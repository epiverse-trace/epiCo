% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/demographics.R
\name{population_pyramid}
\alias{population_pyramid}
\title{Returns the population pyramid of the consulted region}
\usage{
population_pyramid(
  divipola_code,
  year,
  sex = TRUE,
  range = 5,
  total = TRUE,
  plot = FALSE
)
}
\arguments{
\item{divipola_code}{A code from the divipola table representing a department
or municipality. To obtain values at the national level, code '0' is used}

\item{year}{A numeric input for the year of interest}

\item{sex}{A boolean to consult data disaggregated by sex. The default value
is TRUE}

\item{range}{A numeric value from 1 to 100 for the age range to use. The
default value is 5}

\item{total}{A boolean for returning the total number rather than the
proportion of the country's population. The default value is TRUE}

\item{plot}{A boolean for displaying a plot. The default value is TRUE}
}
\value{
A dataframe with the proportion or total count of individuals
}
\description{
Function that returns the population pyramid of the municipality
or department of a specific year
}
\examples{
population_pyramid("15001", 2015, sex = TRUE, total = TRUE, plot = TRUE)
}
