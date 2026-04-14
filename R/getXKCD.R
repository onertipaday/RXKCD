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
#' @examples
#' \dontrun{
#' getXKCD(which = 1)
#' getXKCD(which = "random")
#' }
#' @export
getXKCD <- function(which = "current", display = TRUE, html = FALSE, saveImg = FALSE) {
  
  base_url <- "https://xkcd.com"
  
  # 1. Determine URL
  if (which == "current") {
    url <- paste0(base_url, "/info.0.json")
  } else if (which == "random") {
    # Requires two API calls: one to discover max_id, one to fetch the random comic.
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
        filename <- paste0(safe_title, ".", ext)
        if (grepl("png", ext, ignore.case = TRUE)) {
          png::writePNG(img_data, target = filename)
        } else if (grepl("jpe?g", ext, ignore.case = TRUE)) {
          jpeg::writeJPEG(img_data, target = filename)
        }
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
#' @examples
#' \dontrun{
#' updateConfig()
#' }
#' @importFrom utils txtProgressBar setTxtProgressBar
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
  } else {

  message(paste("Found", length(missing_ids), "missing comics. Downloading metadata..."))
  
  # 3. Batch Download
  # Setup progress bar
  pb <- txtProgressBar(min = 0, max = length(missing_ids), style = 3)

  # Prepare a list to collect data; flush to DB every 100 records
  new_data <- list()
  total_added <- 0L

  safe_get <- function(x) if (is.null(x)) "" else x

  counter <- 0
  for (id in missing_ids) {
    url <- paste0("https://xkcd.com/", id, "/info.0.json")

    # Use httr::GET with a small timeout to avoid hanging
    req <- tryCatch(httr::GET(url, httr::timeout(10)), error = function(e) NULL)

    if (!is.null(req) && req$status_code == 200) {
      json <- httr::content(req, as = "parsed", type = "application/json")

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

    # Flush to DB every 100 records to cap peak memory use
    if (length(new_data) >= 100) {
      df_chunk <- do.call(rbind, new_data)
      DBI::dbWriteTable(con, "xkcd", df_chunk, append = TRUE)
      total_added <- total_added + nrow(df_chunk)
      new_data <- list()
    }

    counter <- counter + 1
    setTxtProgressBar(pb, counter)

    # Be polite to the server
    Sys.sleep(0.05)
  }
  close(pb)

  # 4. Write remaining records to DB
  if (length(new_data) > 0) {
    df_chunk <- do.call(rbind, new_data)
    DBI::dbWriteTable(con, "xkcd", df_chunk, append = TRUE)
    total_added <- total_added + nrow(df_chunk)
  }

  if (total_added > 0) {
    message(paste("Added", total_added, "comics to the database."))
  } else {
    message("No valid data retrieved.")
  }

  } # end if (missing_ids)

  # 5. Build full-text search index
  DBI::dbExecute(con, "INSTALL fts")
  DBI::dbExecute(con, "LOAD fts")
  tryCatch(DBI::dbExecute(con, "PRAGMA drop_fts_index('xkcd')"), error = function(e) NULL)
  DBI::dbExecute(con, "PRAGMA create_fts_index('xkcd', 'num', 'title', 'alt', 'transcript',
                 stemmer = 'porter', stopwords = 'english', lower = 1, strip_accents = 1)")
  message("Full-text search index updated.")

  # 6. Build GloVe embeddings for similarXKCD()
  comics <- DBI::dbGetQuery(con, "SELECT num, title, alt, transcript FROM xkcd ORDER BY num")
  corpus <- paste(comics$title, comics$alt, comics$transcript, sep = " ")
  corpus <- tolower(trimws(corpus))

  it <- text2vec::itoken(corpus, tokenizer = text2vec::space_tokenizer, progressbar = FALSE)
  vocab <- text2vec::create_vocabulary(it)
  vocab <- text2vec::prune_vocabulary(vocab, term_count_min = 2L)
  vectorizer <- text2vec::vocab_vectorizer(vocab)

  # Rebuild iterator for TCM (iterators are consumed)
  it <- text2vec::itoken(corpus, tokenizer = text2vec::space_tokenizer, progressbar = FALSE)
  tcm <- text2vec::create_tcm(it, vectorizer, skip_grams_window = 5L)

  glove <- text2vec::GlobalVectors$new(rank = 100L, x_max = 10L)
  wv_main <- glove$fit_transform(tcm, n_iter = 25L, convergence_tol = 0.001)
  wv_context <- glove$components
  word_vectors <- wv_main + t(wv_context)

  # Compute document embeddings (average of word vectors per comic)
  wv_vocab <- rownames(word_vectors)
  embed_dim <- ncol(word_vectors)
  embed_matrix <- matrix(0, nrow = nrow(comics), ncol = embed_dim)

  for (i in seq_len(nrow(comics))) {
    tokens <- unlist(strsplit(corpus[i], "\\s+"))
    matched <- tokens[tokens %in% wv_vocab]
    if (length(matched) > 0) {
      embed_matrix[i, ] <- colMeans(word_vectors[matched, , drop = FALSE])
    }
  }
  rownames(embed_matrix) <- comics$num

  saveRDS(word_vectors, file.path(db_dir, "glove_vectors.rds"))
  saveRDS(embed_matrix, file.path(db_dir, "embeddings.rds"))
  message("GloVe embeddings updated.")

  return(invisible(TRUE))
}

#' Search XKCD comics
#'
#' @description
#' Searches the local DuckDB cache using full-text search (BM25 ranking) across
#' comic titles, alt text, and transcripts. Results are ranked by relevance.
#' Supports stemming (e.g., "running" matches "run") and stopword removal.
#' Run \code{updateConfig()} first to populate the database and build the search index.
#'
#' @param query A single string to search for in comic titles, alt text, and transcripts.
#'
#' @returns
#' A data frame containing matching XKCD comics with columns \code{num}, \code{date},
#' \code{title}, \code{alt}, and \code{score} (BM25 relevance), ordered by relevance.
#' If no matches are found, an empty data frame is returned invisibly, and a message is printed.
#' The function will stop with an error if the local database or search index is not found.
#'
#' @examples
#' \dontrun{
#' updateConfig()
#' searchXKCD("satellite")
#' }
#' @export
searchXKCD <- function(query) {
  
  home_dir <- Sys.getenv("HOME")
  db_path <- file.path(home_dir, ".RXKCD", "xkcd.duckdb")
  
  if (!file.exists(db_path)) {
    stop("Local database not found. Please run updateConfig() first.")
  }
  
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con))

  # Ensure FTS extension and index are available
  has_fts <- tryCatch({
    DBI::dbExecute(con, "LOAD fts")
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM duckdb_tables() WHERE schema_name = 'fts_main_xkcd'")$n > 0
  }, error = function(e) FALSE)

  if (!has_fts) {
    stop("Full-text search index not found. Please run updateConfig() to build it.")
  }

  # Full-text search with BM25 ranking across title, alt text, and transcript
  sql <- "SELECT x.num, x.date, x.title, x.alt,
                 fts_main_xkcd.match_bm25(x.num, ?) AS score
          FROM xkcd x
          WHERE score IS NOT NULL
          ORDER BY score DESC"
  res <- DBI::dbGetQuery(con, sql, params = list(query))
  
  if (nrow(res) == 0) {
    message("No matches found.")
    return(invisible(res))
  }

  return(res)
}

#' Find semantically similar XKCD comics
#'
#' @description
#' Uses pre-computed GloVe embeddings to find comics that are semantically
#' similar to the given query. Unlike \code{searchXKCD()}, which matches
#' keywords, this function captures meaning (e.g., "feeling lonely" can match
#' comics about isolation even if they don't contain the word "lonely").
#' Run \code{updateConfig()} first to build the embeddings.
#'
#' @param query A single string describing the topic or feeling to search for.
#' @param n A single integer specifying the number of results to return. Defaults to 5.
#'
#' @returns
#' A data frame with columns \code{num}, \code{date}, \code{title}, \code{alt},
#' and \code{similarity} (cosine similarity score, 0-1), ordered by similarity.
#' If no similar comics are found, an empty data frame is returned invisibly.
#' The function will stop with an error if the embeddings have not been built.
#'
#' @examples
#' \dontrun{
#' updateConfig()
#' similarXKCD("space exploration")
#' }
#' @export
similarXKCD <- function(query, n = 5L) {

  home_dir <- Sys.getenv("HOME")
  db_dir <- file.path(home_dir, ".RXKCD")
  vectors_path <- file.path(db_dir, "glove_vectors.rds")
  embed_path <- file.path(db_dir, "embeddings.rds")

  if (!file.exists(vectors_path) || !file.exists(embed_path)) {
    stop("Embeddings not found. Please run updateConfig() first.")
  }

  # Load GloVe word vectors and pre-computed document embeddings
  word_vectors <- readRDS(vectors_path)
  embed_matrix <- readRDS(embed_path)

  # Embed the query
  tokens <- unlist(strsplit(tolower(trimws(query)), "\\s+"))
  vocab <- rownames(word_vectors)
  matched <- tokens[tokens %in% vocab]

  if (length(matched) == 0) {
    message("No matches found.")
    return(invisible(data.frame(num = integer(), date = character(),
                                title = character(), alt = character(),
                                similarity = numeric())))
  }

  query_vec <- colMeans(word_vectors[matched, , drop = FALSE])

  # Cosine similarity against all comic embeddings
  norms_comics <- sqrt(rowSums(embed_matrix^2))
  norm_query <- sqrt(sum(query_vec^2))
  sims <- as.vector(embed_matrix %*% query_vec) / (norms_comics * norm_query)
  sims[is.nan(sims)] <- 0

  # Get top n
  n <- min(n, length(sims))
  top_idx <- order(sims, decreasing = TRUE)[seq_len(n)]
  top_nums <- as.integer(rownames(embed_matrix)[top_idx])
  top_sims <- sims[top_idx]

  # Fetch metadata from DuckDB
  db_path <- file.path(db_dir, "xkcd.duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con))

  placeholders <- paste(rep("?", length(top_nums)), collapse = ", ")
  sql <- paste0("SELECT num, date, title, alt FROM xkcd WHERE num IN (", placeholders, ")")
  meta <- DBI::dbGetQuery(con, sql, params = as.list(top_nums))

  # Merge similarity scores and sort
  meta$similarity <- top_sims[match(meta$num, top_nums)]
  meta <- meta[order(meta$similarity, decreasing = TRUE), ]
  rownames(meta) <- NULL

  return(meta)
}