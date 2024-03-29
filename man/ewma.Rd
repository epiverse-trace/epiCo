% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ewma.R
\name{ewma}
\alias{ewma}
\title{Returns the EWMA smoothed of a vector}
\usage{
ewma(x, lambda = 0.2, delta = 0.001)
}
\arguments{
\item{x}{A numeric vector}

\item{lambda}{= 0.2 (default) The smoothing parameter}

\item{delta}{= 1e-3 (default) The lowest weight to include in the calculation}
}
\value{
The EWMA smoothed vector
}
\description{
Function that performs EWMA smoothing on a vector
}
