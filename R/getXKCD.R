## Wrapper around download.file() (methods: default, wget and curl) or
## RCurl::getBinaryURL(). Intention is to support https URLs when the
## default method fails.
#' @importFrom utils download.file
download.file2 <- function(...) {
    Call <- as.list(match.call(download.file))
    Call[[1]] <- as.symbol("download.file")
    the_url <- eval.parent(Call[["url"]])
    Call[["url"]] <- the_url
    Call[["quiet"]] <- TRUE
    destfile <- eval.parent(Call[["destfile"]])
    tmpfile <- tempfile()
    on.exit(unlink(tmpfile))
    Call[["destfile"]] <- tmpfile
    retval <- try(eval.parent(as.call(Call)), silent = TRUE)
    if (inherits(retval, "try-error") || retval != 0 ||
        !file.exists(tmpfile) || file.info(tmpfile)[["size"]] == 0) {
        success <- FALSE
        for (method in c("wget", "curl")) {
            sw <- Sys.which(method)
            if (is.na(sw) || !grepl(method, sw, fixed = TRUE)) {
                next
            }
            Call[["method"]] <- method
            retval <- try(eval.parent(as.call(Call)), silent = TRUE)
            if (!inherits(retval, "try-error") && retval == 0 &&
                file.exists(tmpfile) && file.info(tmpfile)[["size"]] > 0) {
                success <- TRUE
                break
            }
        }
        ## requireNamespace() does not exist in R < 2.14.0, therefore
        ## loadNamespace()
        if (!success &&
            !inherits(try(loadNamespace("RCurl"), silent = TRUE),
                      "try-error")) {
            bytes <- try(RCurl::getBinaryURL(the_url), silent = TRUE)
            if (!inherits(bytes, "try-error")) {
                if (length(bytes) == 0L &&
                    grepl("^[hH][tT][tT][pP]:", the_url)) {
                    url2 <- sub("^[hH][tT][tT][pP]", "https", the_url)
                    bytes <- try(RCurl::getBinaryURL(url2), silent = TRUE)
                }
                if (!inherits(bytes, "try-error") && length(bytes) > 0L) {
                    writeBin(bytes, tmpfile)
                    success <- TRUE
                }
            }
        }
    } else {
        success <- TRUE
    }
    if (success) {
        file.copy(tmpfile, destfile, overwrite = TRUE)
        invisible(0)
    } else {
        warning(gettextf("could not download %s", the_url),
                domain = NA)
        invisible(1)
    }
}

## Wrapper around RJSONIO::fromJSON, using download.file2()
#' @importFrom RJSONIO fromJSON
fromJSON2 <- function(...) {
    Call <- as.list(match.call(fromJSON))
    Call[[1]] <- as.symbol("fromJSON")
    the_url <- eval.parent(Call[["content"]])
    tmpfile <- tempfile()
    on.exit(unlink(tmpfile))
    stopifnot(download.file2(the_url, tmpfile) == 0)
    Call[["content"]] <- tmpfile
    eval.parent(as.call(Call))
}

#' @importFrom utils read.csv
read.xkcd <- function(file = NULL)
{
  if(!is.null(file) && file.exists(file)) {
    xkcd <- file
  } else {
    path <- system.file("xkcd", package = "RXKCD") # fix requested by Brian Ripley
    datafiles <- list.files(path)
    if(!is.null(file) && file.exists(file.path(path, file))) {
      xkcd <- file.path(path, file)
    } else {
      if(!is.null(file)) stop("sorry, ", sQuote(file), " not found")
      file <- datafiles
      xkcd <- file.path(path, file)
    }
  }
  out <- readRDS(xkcd)
  return(out)
}

load.xkcd <- function(file = NULL)
{
  if(!is.null(file) && file.exists(file)) {
    xkcd <- file
  } else {
    path <- system.file("xkcd", package = "RXKCD") # fix requested by Brian Ripley
    datafiles <- list.files(path)
    if(!is.null(file) && file.exists(file.path(path, file))) {
      xkcd <- file.path(path, file)
    } else {
      if(!is.null(file)) stop("sorry, ", sQuote(file), " not found")
      file <- datafiles
      xkcd <- file.path(path, file)
    }
  }
  out <-readRDS(xkcd)
  return(out)
  
}

#'
#' Update the XKCD database saved in the user directory
#'
#' This function updates the local version of the XKCD database used
#' by searchXKCD().
#'
#' @references \url{https://xkcd.com/license.html}
#'
#' @export
#'
#' @importFrom plyr rbind.fill
#' @importFrom utils tail
updateConfig <- function(){
  home <- Sys.getenv("HOME") # user's home directory
  if( !file.exists( paste(home, ".Rconfig/rxkcd.rda", sep="/") ) ) {
    stop("Use saveConfig() to save your xkcd database locally!")
  } else xkcd.df <- readRDS( paste(home, ".Rconfig/rxkcd.rda", sep="/") )
  from <- as.numeric(tail(xkcd.df$num,n=1))
  current <- getXKCD("current", display=FALSE)
  if ( current$num == from ) stop("Your local xkcd is already updated!")
  tmp <- NULL
  for( i in c((from+1):(current$num)) ){
    if (is.null(tmp)) tmp <- data.frame(unclass(getXKCD(i, display=FALSE)))
    else tmp <- plyr::rbind.fill(tmp, data.frame(unclass(getXKCD(i, display=FALSE))))
  }
  xkcd2add <- cbind(
    "month"=unlist(tmp[["month"]]),
    "num"=unlist(tmp[["num"]]),
    "link"=unlist(tmp[["link"]]),
    "year"=unlist(tmp[["year"]]),
    "news"=unlist(tmp[["news"]]),
    "safe_title"=unlist(tmp[["safe_title"]]),
    "transcript"=unlist(tmp[["transcript"]]),
    "alt"=unlist(tmp[["alt"]]),
    "img"=unlist(tmp[["img"]]),
    "title"=unlist(tmp[["title"]]),
    "day"=unlist(tmp[["day"]])
  )
  suppressWarnings(xkcd2add <- data.frame(xkcd2add))
  xkcd.updated <- rbind(xkcd.df,xkcd2add)
  xkcd.updated <- plyr::rbind.fill(xkcd.df,xkcd2add)
  xkcd.df <- xkcd.updated
  saveRDS( xkcd.df, file=paste(home, ".Rconfig/rxkcd.rda", sep="/") , compress=TRUE)
}
#'
#' Save XKCD database info into a file in the user directory
#'
#' This function saves the XKCD database as a file in the user's home
#' directory.
#'
#' @references \url{https://xkcd.com/license.html}
#'
#' @export
#'
saveConfig <- function(){
	home <- Sys.getenv("HOME") # home dir of the user
	if( file.exists( paste(home, ".Rconfig/rxkcd.rda", sep="/") ) ) stop("Use updateConfig() for updating your local xkcd database")
	else {
		dir.create( paste(home, ".Rconfig", sep="/") )
		xkcd.df <- read.xkcd()
		saveRDS( xkcd.df, file=paste(home, ".Rconfig/rxkcd.rda", sep="/") , compress=TRUE)
	}
}
#'
#' Search your favorite XKCD comic strip by title/transcript
#'
#' This function uses grep to inspect the title and transcript for all
#' the occurrences of a specified string and returns a data.frame with
#' both the number and the title of the XKCD comic strips.
#'
#' @param which string.
#'
#' @return a data.frame containing the following fields:
#' \itemize{
#'     \item num The num of the XKCD comic strip
#'     \item title The title of the XKCD comic strip
#' }
#'
#' @references \url{https://xkcd.com/license.html}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library("RXKCD")
#' searchXKCD(which="significant")
#' searchXKCD(which="someone is wrong") }
#'
searchXKCD <- function(which="significant"){
	xkcd.df <- NULL # Thanks to Duncan Murdoch
	home <- Sys.getenv("HOME") # user's home directory
	if( file.exists( paste(home, ".Rconfig/rxkcd.rda", sep="/") ) ) {
	  tryCatch(readRDS( paste(home, ".Rconfig/rxkcd.rda", sep="/")), error = function(e) {
	             e$message <- paste0(e$message, "(RXKCD < 1.9) archive input format! You need to delete", home, "/.Rconfig/rxkcd.rda by typing, for example, file.remove('~/.Rconfig/rxkcd.rda')")
	             stop(e)  
	             })
		xkcd.df <- readRDS( paste(home, ".Rconfig/rxkcd.rda", sep="/"))
	} else	xkcd.df <- read.xkcd()
	if(is.character(which)) {
		if(length(which) > 1) which <- sample(which)
	which.tt <- grep(which, xkcd.df["title"][[1]], ignore.case = TRUE, useBytes = TRUE)
	which.tr <- grep(which, xkcd.df["transcript"][[1]], ignore.case =TRUE, useBytes = TRUE)
	which.all <- unique(c(which.tr, which.tt))
	}
	out <- data.frame(num=xkcd.df[which.all, "num"], title=xkcd.df[which.all, "title"])
	return(out)
}

#'
#' Display your favorite XKCD comic in R
#'
#' This function fetches a XKCD comic strip (randomly or by number)
#' and displays it on screen.
#'
#' Old \R versions may have problems accessing HTTPS URLs such as
#' those used on the XKCD website. In case the default method of
#' \code{\link{download.file}} fails, alternatives will be tried. For
#' the best chance to succeed, ensure that the command line tools
#' \code{"wget"} and \code{"curl"} as well as the \code{"RCurl"}
#' package are installed.
#'
#' @param which string: either "current" or "random"; or a number
#'     indicating the specific strip.
#' @param display logical; TRUE (default) if you like to display the
#'     strip on the screen.
#' @param html logical; TRUE if you like to open the XKCD web page for
#'     the selected comic in your browser: if TRUE it sets display and
#'     saveImg arguments to FALSE. Default FALSE.
#' @param saveImg logical; TRUE if you want to save image in the
#'     current directory. Default FALSE.
#'
#' @return a list containing the following fields: \itemize{
#'     \item img URL of the XKCD comic strip image (PNG)
#'     \item title Title of the XKCD comic strip
#'     \item month
#'     \item num Number of the XKCD comic strip
#'     \item link
#'     \item year Year of publication
#'     \item safe_title
#'     \item transcript
#'     \item alt
#'     \item day
#'     \item news
#' }
#'
#' @references \url{https://xkcd.com/license.html}
#'
#' @export
#' @importFrom graphics plot
#' @importFrom graphics rasterImage
#' @importFrom utils browseURL
#' @importFrom jpeg readJPEG
#' @importFrom png readPNG
#' @importFrom png writePNG
#'
#' @examples
#'
#' library("RXKCD")
#' significant <- getXKCD(882, display=FALSE)
#'
getXKCD <- function(which = "current", display = TRUE, html = FALSE, saveImg = FALSE) {
	if (which=="current") xkcd <- fromJSON2("https://xkcd.com/info.0.json")
	else if(which=="random" || which=="") {
		current <- fromJSON2("https://xkcd.com/info.0.json")
		num <- sample(1:current["num"][[1]], 1)
		xkcd <- fromJSON2(paste("https://xkcd.com/",num,"/info.0.json",sep=""))
	}
	else xkcd <- fromJSON2(paste("https://xkcd.com/",which,"/info.0.json",sep=""))
	class(xkcd) <- "rxkcd"
	if(html) {
		display <- FALSE
		browseURL( paste("https://xkcd.com/", as.numeric(xkcd["num"][[1]]),sep="") )
	}
	if (display || saveImg) {
		if(grepl(".png",xkcd["img"][[1]])){
			download.file2(url=xkcd["img"][[1]], quiet=TRUE, mode="wb", destfile=paste(tempdir(),"xkcd.png",sep="/"))
			xkcd.img <- readPNG( paste(tempdir(),"xkcd.png",sep="/") )
		}
		else if(grepl(".jpg",xkcd["img"][[1]])){
			download.file2(url=xkcd["img"][[1]], quiet=TRUE, mode="wb", destfile=paste(tempdir(),"xkcd.jpg",sep="/"))
			xkcd.img <- readJPEG( paste(tempdir(),"xkcd.jpg",sep="/") )
		} else stop("Unsupported image format! Try html = TRUE")
		# show the image if the format is supported
		if(display){
			img_dim <- dim(xkcd.img)
			plot(c(0, img_dim[2]), c(0, img_dim[1]), type = "n",
			     axes = FALSE, asp = 1, xaxs = "i", yaxs = "i",
			     xaxt = "n", yaxt = "n", xlab = "", ylab = "")
			rasterImage(xkcd.img, xleft = 0, ybottom = 0,
				    xright = img_dim[2], ytop = img_dim[1])
		}
		# save the image
		if(saveImg) writePNG( image=xkcd.img, target=paste(xkcd$title,".png",sep="") )
	}
	return(xkcd)
}

print.rxkcd <- function(x, ...){
	cat("image.url = ", x$img, "\n", sep="")
	cat("title =  ", x$title, "\n", sep="")
	cat("num = ", x$num, "\n", sep="")
	cat("year = ", x$year, "\n", sep="")
	cat("transcript = ", x$transcript,"\n", sep="")
	cat("alt = ", x$alt, "\n", sep="")
}
