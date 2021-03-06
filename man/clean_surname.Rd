% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean.r
\name{clean_surname}
\alias{clean_surname}
\title{Clean entry(/ies) surname}
\usage{
clean_surname(names)
}
\arguments{
\item{names}{A character string vector of surname(s).}
}
\value{
A character string vector of cleaned surname(s).
}
\description{
Attempts to clean provided surname.
}
\section{Details}{

Multiple spelling names are standardised to that of the capital letter header
in the general directory. i.e. Abercrombie, Abercromby -> Abercromby;
Bayne, Baynes -> Bayne; Beattie, Beatty -> Beatty; etc.
}

