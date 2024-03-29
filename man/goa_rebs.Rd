% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/goa_rockfish.R
\name{goa_rebs}
\alias{goa_rebs}
\title{raw data query for GOA rougheye/blackspotted rockfish}
\usage{
goa_rebs(year, off_yr = FALSE)
}
\arguments{
\item{year}{assessment year}

\item{off_yr}{if this is an off-year assessment change to TRUE}
}
\value{
a suite of raw data .csv files and a time stamp of when the query was done
}
\description{
raw data query for GOA rougheye/blackspotted rockfish
}
\examples{
\dontrun{
goa_rebs(year = 2021, off_yr = FALSE)
}
}
