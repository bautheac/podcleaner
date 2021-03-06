% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trades.r
\name{trades_clean_directory_progress}
\alias{trades_clean_directory_progress}
\title{Mutate operation(s) in Scottish post office trades directory data.frame
column(s)}
\usage{
trades_clean_directory_progress(directory, verbose)
}
\arguments{
\item{directory}{A Scottish post office trades directory in the form
of a data.frame or other object that inherits from the data.frame class
such as a \code{\link[tibble]{tibble}}. Columns must at least include
\code{forename}, \code{surname}, \verb{occupation``address.trade.number} and
\code{address.trade.body}.}

\item{verbose}{Whether the function should be executed silently (\code{FALSE}) or
not (\code{TRUE}).}
}
\value{
A data.frame of the same class as the one provided in \code{directory};
columns include at least \code{forename}, \code{surname}, \code{occupation},
\code{address.trade.number} and \code{address.trade.body}. Entries are cleaned of
optical character recognition (OCR) errors and subject to a number of
standardisation operations.
}
\description{
Attempts to clean the provided Scottish post office trades directory
data.frame. Shows a progress bar indicating function progression.
}
