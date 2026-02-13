#' Get XKCD comic
#'
#' @description
#' Fetches comic metadata and image from the XKCD JSON API.
#' 
#' @param which A single string or a single numeric value. Specifies which comic to retrieve.
#'   Can be `"current"` for the latest comic, `"random"` for a random comic,
#'   or a comic number. Optional.
#' @param display A single logical value. If `TRUE`, the comic image will be displayed
#'   in the R graphics device. Optional.
#' @param html A single logical value. If `TRUE`, the comic's URL will be opened in
#'   the default web browser. Optional.
#' @param saveImg A single logical value. If `TRUE`, the comic image will be saved
#'   to a file in the current working directory. Optional.
#'
#' @returns 
#' A list containing details about the XKCD comic (number, title, date, image URL,
#' alt text, link, and transcript). If `html` is `TRUE`, the list is returned
#' invisibly. If the comic is not found or connection fails, it returns `NULL` invisibly.
#' Errors will occur if `which` is invalid or if the XKCD API cannot be reached
#' when attempting to get a random comic.
#'
#' @export
getXKCD <- function(which = "current", display = TRUE, html = FALSE, saveImg = FALSE) {
  
  base_url <- "https://xkcd.com"
  
  # 1. Determine URL
  if (which == "current") {
    url <- paste0(base_url, "/info.0.json")
  } else if (which == "random") {
    current_res <- tryCatch(jsonlite::fromJSON(paste0(base_url, "/info.0.json")),
                            error = function(e) NULL)
    if (is.null(current_res)) stop("Could not connect to XKCD.")
    max_id <- current_res$num
    rand_id <- sample(1:max_id, 1)
    url <- paste0(base_url, "/", rand_id, "/info.0.json")
  } else if (is.numeric(which) || grepl("^[0-9]+$", which)) {
    url <- paste0(base_url, "/", which, "/info.0.json")
  } else {
    stop("Invalid 'which' argument.")
  }
  
  # 2. Fetch JSON
  req <- tryCatch(httr::GET(url), error = function(e) NULL)
  if (is.null(req) || req$status_code != 200) {
    message("Comic not found or connection failed.")
    return(invisible(NULL))
  }
  
  comic_data <- httr::content(req, as = "parsed", type = "application/json")
  
  res <- list(
    num = comic_data$num,
    title = comic_data$title,
    date = paste(comic_data$year, comic_data$month, comic_data$day, sep = "-"),
    img = comic_data$img,
    alt = comic_data$alt,
    link = paste0("https://xkcd.com/", comic_data$num),
    transcript = comic_data$transcript
  )
  
  if (html) {
    utils::browseURL(res$link)
    return(invisible(res))
  }
  
  if (display || saveImg) {
    ext <- tools::file_ext(res$img)
    if (ext == "") ext <- "png"
    temp_img <- tempfile(fileext = paste0(".", ext))
    
    dl <- try(httr::GET(res$img, httr::write_disk(temp_img, overwrite = TRUE)), silent = TRUE)
    
    if (!inherits(dl, "try-error") && dl$status_code == 200) {
      img_data <- NULL
      if (grepl("png", ext, ignore.case = TRUE)) {
        img_data <- png::readPNG(temp_img)
      } else if (grepl("jpe?g", ext, ignore.case = TRUE)) {
        img_data <- jpeg::readJPEG(temp_img)
      }
      
      if (display && !is.null(img_data)) {
        old_par <- graphics::par(no.readonly = TRUE)
        on.exit(graphics::par(old_par))
        graphics::par(mar = c(0, 0, 2, 0))
        img_dim <- dim(img_data)
        graphics::plot(c(0, img_dim[2]), c(0, img_dim[1]), type = "n", 
                       axes = FALSE, asp = 1, xlab = "", ylab = "", main = res$title)
        graphics::rasterImage(img_data, 0, 0, img_dim[2], img_dim[1])
        message(paste0("Alt Text: ", res$alt))
      }
      
      if (saveImg && !is.null(img_data)) {
        safe_title <- gsub("[^a-zA-Z0-9]", "_", res$title)
        filename <- paste0(safe_title, ".png")
        png::writePNG(img_data, target = filename)
        message(paste0("Saved image to: ", filename))
      }
    }
  }
  return(res)
}

#' Update XKCD Database (Smart Sync)
#'
#' @description
#' Connects to the local DuckDB. Checks for missing comics and fetches 
#' full metadata (Title, Alt Text, Transcript) from the JSON API for any gaps.
#'
#' @returns
#' `TRUE`, invisibly, if the database is successfully updated or determined to be
#' up-to-date. The function will stop with an error if it cannot connect to
#' `xkcd.com`.
#'
#' @export
updateConfig <- function() {
  
  # 1. Setup Database
  home_dir <- Sys.getenv("HOME")
  db_dir <- file.path(home_dir, ".RXKCD")
  if (!dir.exists(db_dir)) dir.create(db_dir)
  db_path <- file.path(db_dir, "xkcd.duckdb")
  
  con <- DBI::dbConnect(duckdb::duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con))
  
  # Create Table if missing
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS xkcd (
    num INTEGER PRIMARY KEY, 
    title VARCHAR, 
    date VARCHAR, 
    alt VARCHAR, 
    img VARCHAR, 
    transcript VARCHAR
  )")
  
  # 2. Determine Missing Comics
  message("Checking for new comics...")
  
  # Get latest comic ID from web
  latest_json <- tryCatch(jsonlite::fromJSON("https://xkcd.com/info.0.json"), error = function(e) NULL)
  if (is.null(latest_json)) stop("Could not connect to xkcd.com")
  max_id <- latest_json$num
  
  # Get existing IDs from local DB
  existing_ids <- DBI::dbGetQuery(con, "SELECT num FROM xkcd")$num
  
  # Identify gaps (e.g., new comics or missed ones)
  all_ids <- 1:max_id
  # Exclude 404 (Easter egg: Comic 404 does not exist)
  all_ids <- all_ids[all_ids != 404]
  
  missing_ids <- setdiff(all_ids, existing_ids)
  
  if (length(missing_ids) == 0) {
    message("Database is up to date!")
    return(invisible(TRUE))
  }
  
  message(paste("Found", length(missing_ids), "missing comics. Downloading metadata..."))
  
  # 3. Batch Download
  # Setup progress bar
  pb <- txtProgressBar(min = 0, max = length(missing_ids), style = 3)
  
  # Prepare a list to collect data
  new_data <- list()
  
  counter <- 0
  for (id in missing_ids) {
    url <- paste0("https://xkcd.com/", id, "/info.0.json")
    
    # Use httr::GET with a small timeout to avoid hanging
    req <- tryCatch(httr::GET(url, httr::timeout(10)), error = function(e) NULL)
    
    if (!is.null(req) && req$status_code == 200) {
      json <- httr::content(req, as = "parsed", type = "application/json")
      
      # Handle potential NULLs in JSON fields
      safe_get <- function(x) if (is.null(x)) "" else x
      
      new_data[[length(new_data) + 1]] <- data.frame(
        num = as.integer(json$num),
        title = safe_get(json$title),
        date = paste(json$year, json$month, json$day, sep = "-"),
        alt = safe_get(json$alt),
        img = safe_get(json$img),
        transcript = safe_get(json$transcript),
        stringsAsFactors = FALSE
      )
    }
    
    counter <- counter + 1
    setTxtProgressBar(pb, counter)
    
    # Be polite to the server
    Sys.sleep(0.05) 
  }
  close(pb)
  
  # 4. Write to DB
  if (length(new_data) > 0) {
    df_new <- do.call(rbind, new_data)
    DBI::dbWriteTable(con, "xkcd", df_new, append = TRUE)
    message(paste("Added", nrow(df_new), "comics to the database."))
  } else {
    message("No valid data retrieved.")
  }
}

#' Search XKCD comics
#'
#' @description
#' A short description...
#'
#' @param query A single string to search for in comic titles, alt text, and transcripts.
#'
#' @returns
#' A data frame containing matching XKCD comics, ordered by comic number in descending order.
#' If no matches are found, an empty data frame is returned invisibly, and a message is printed.
#' The function will stop with an error if the local database is not found.
#'
#' @export
searchXKCD <- function(query) {
  
  home_dir <- Sys.getenv("HOME")
  db_path <- file.path(home_dir, ".RXKCD", "xkcd.duckdb")
  
  if (!file.exists(db_path)) {
    stop("Local database not found. Please run updateConfig() first.")
  }
  
  con <- DBI::dbConnect(duckdb::duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con))
  
  # Search across Title, Alt Text, and Transcript
  # Using parameterized queries is cleaner, but for simple ILIKE pattern matching:
  sql <- sprintf(
    "SELECT num, date, title, alt FROM xkcd 
     WHERE title ILIKE '%%%1$s%%' 
        OR alt ILIKE '%%%1$s%%' 
        OR transcript ILIKE '%%%1$s%%'
     ORDER BY num DESC", 
    query
  )
  
  res <- DBI::dbGetQuery(con, sql)
  
  if (nrow(res) == 0) {
    message("No matches found.")
    return(invisible(res))
  }
  
  return(res)
}