\name{searchXKCD}
\alias{searchXKCD}
\title{Search your favorite XKCD comic strip by title/trascript}
\usage{
  searchXKCD(which = "significant")
}
\arguments{
  \item{which}{string.}
}
\value{
  a data.frame containing the following fields: \itemize{
  \item num The num of the XKCD comic strip \item title The
  title of the XKCD comic strip }
}
\description{
  This function use grep to inspect the title and trascript
  for all the occurrences of a specified string and return
  a data.frame with both the number and the title of the
  XKCD comic strips.
}
\examples{
\dontrun{
library("RXKCD")
searchXKCD(which="significant")
searchXKCD(which="someone is wrong")
}
}
\references{
  http://xkcd.com/license.html
}

