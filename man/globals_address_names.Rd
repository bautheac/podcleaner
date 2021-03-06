% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/globals.r
\docType{data}
\name{globals_address_names}
\alias{globals_address_names}
\title{Place names in address entries}
\format{
A data frame with 3 variables:
\describe{
\item{pattern}{regex for place name matching}
\item{replacement}{replacement pattern for substitution operations}
\item{ignore_case}{
boolean operator indicating whether the corresponding
regex is case sensitive or not.
}
}
}
\usage{
globals_address_names
}
\description{
A dataset containing regular expression meant to match commonly (OCR) misread
place names in directory address entries. For each place name a replacement
pattern is provided for used in substitution operations as well as a boolean
operator indicating whether the corresponding regex is case sensitive or not.
}
\keyword{datasets}
