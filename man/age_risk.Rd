% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/demographics.R
\name{age_risk}
\alias{age_risk}
\title{Returns the specific rates associated with being infected given age and sex}
\usage{
age_risk(age, population_pyramid, sex = NULL, plot = FALSE)
}
\arguments{
\item{age}{A vector with the ages of cases in years from 0 to 100 years}

\item{population_pyramid}{A dataframe with the count of individuals with the
columns age, population and sex}

\item{sex}{A vector with the sex of cases 'F' and 'M'. The default value
is NULL}

\item{plot}{A boolean for displaying a plot. The default value is FALSE}
}
\value{
A dataframe with the proportion or total count of individuals
}
\description{
Function that returns the specific rates of being infected given
age and sex
}
\examples{
pop_pyramid <- population_pyramid("15001", 2015,
  sex = TRUE, total = TRUE,
  plot = FALSE
)
ages <- round(runif(150, 0, 100))
sex <- c(rep("M", 70), rep("F", 80))
age_risk(
  age = ages, sex = sex, population_pyramid = pop_pyramid,
  plot = TRUE
)
}
