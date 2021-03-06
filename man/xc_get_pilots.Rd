% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xc_summary.R
\name{xc_get_pilots}
\alias{xc_get_pilots}
\title{Retrieve Pilot Data Given a start point for a url}
\usage{
xc_get_pilots(x, url_pg)
}
\arguments{
\item{x}{numeric value for starting point in url}

\item{url_pg}{character vector containing url base}
}
\value{
dataframe of pilot data from page url_pg pasted with "?list[start]=",x
}
\description{
Retrieve Pilot Data Given a start point for a url
}
\examples{
df <- xc_get_pilots(50, "https://www.xcontest.org/world/en/pilots/")
}
