% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{geometric_sd}
\alias{geometric_sd}
\title{Returns the geometric standard deviation of a vector of real numbers.}
\usage{
geometric_sd(
  x,
  method = c("positive", "shifted", "optimized", "weighted"),
  shift = 1,
  delta = 0.001
)
}
\arguments{
\item{x}{A numeric vector of real values}

\item{method}{Description of methods:
\itemize{
\item positive = only positive values within x are used in the calculation.
\item shifted = positive and zero values within x are used by adding a shift
value before the calculation and subtracting it to the final result.
\item optimized = optimized shifted method. See: De La Cruz, R., & Kreft, J. U.
(2018). Geometric mean extension for data sets with zeros. arXiv preprint
arXiv:1806.06403.
\item weighted = a probability weighted calculation of gm for negative, positive,
and zero values. See: Habib, E. A. (2012). Geometric mean for negative and
zero values. International Journal of Research and Reviews in Applied
Sciences, 11(3), 419-432.
}}

\item{shift}{a positive value to use in the shifted method}

\item{delta}{an positive value (shift) used in the optimized method.}
}
\value{
The geometric mean of the x vector, and the epsilon value if
optimized method is used.
}
\description{
Function that returns the geometric standard deviation of a
vector of real numbers according to the selected method.
}
\examples{
x <- c(4, 5, 3, 7, 8)
geometric_sd(x, method = "optimized")

}
