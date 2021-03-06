% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.r
\name{utils_clean_addresses}
\alias{utils_clean_addresses}
\title{Clean directory addresses}
\usage{
utils_clean_addresses(directory)
}
\arguments{
\item{directory}{A directory dataframe. Columns must include
\code{address.house.number}, \code{address.house.number} and/or
\code{address.trade.number}, \code{address.trade.number}.}
}
\value{
A dataframe.
}
\description{
Clean all address records in provided directory dataframe.
}
\examples{
\dontrun{
  directory <- data.frame(
    page = c("71", "71", "71"),
    surname = c("ABOT", "ABRCROMBIE", "BLAI"), forename = c("Wm.", "Alex", "Jn Huh"),
    occupation = c("Wine and spirit merchant", "Baker", "Victualer"),
    address.trade.number = c(" -; 1820", "", "280"),
    address.trade.body = c("London st. ; house, Mary hill.*", "", "High stret"),
    stringsAsFactors = FALSE
  )
  utils_clean_addresses(directory)
}
}
