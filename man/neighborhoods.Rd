% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spatiotemporal.R
\name{neighborhoods}
\alias{neighborhoods}
\title{Neighborhoods from real travel distances in Colombia}
\usage{
neighborhoods(query_vector, threshold = 2)
}
\arguments{
\item{query_vector}{Codes of the municipalities to consider for the
neighborhoods.}

\item{threshold}{Maximum traveling time around each municipality.}
}
\value{
neighborhood object according to the introduced threshold.
}
\description{
Function to build neighborhoods from real travel distances
inside Colombia by land or river transportation.
}
\examples{
query_vector <- c("05001", "05002", "05004", "05021", "05030", "05615")
neighborhoods(query_vector, 2)

}
