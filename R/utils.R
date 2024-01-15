#' Get the epidemiological calendar of a consulted year.
#'
#' @description Function that returns the starting date of the epidemiological
#' weeks in a year of interest.
#'
#' @param year A numeric value for the year of interest.
#' @param jan_days Number of January days that the first epidemiological week
#' must contains.
#'
#' @return A character array with the starting dates of the epidemiological
#' weeks of the given year.
#'
#' @examples
#' \dontrun{
#' epi_calendar(2016)
#' }
#'
#' @export
epi_calendar <- function(year, jan_days = 4) {
  # Input must be numeric
  stopifnot(
    "`year` must be numeric" = (is.numeric(year)),
    "`jan_days` must be numeric" = (is.numeric(jan_days))
  )
  
  # By definition, the first epidemiological week of the year contains at least
  # four days in January.
  epi_calendar <- NULL
  
  sec_day <- 24 * 60 * 60 # seconds in a day
  
  first_date <- as.POSIXlt(paste0("01-01-", toString(year)),
                           format = "%d-%m-%Y"
  )
  last_date <- as.POSIXlt(paste0("31-12-", toString(year)),
                          format = "%d-%m-%Y"
  )
  first_week_day <- first_date$wday # 0 to 6 starting on Sundays
  last_week_day <- last_date$wday # 0 to 6 starting on Sundays
  
  if (first_week_day < jan_days) {
    temp_date <- first_date - first_week_day * sec_day
  } else {
    temp_date <- first_date + (7 * sec_day - first_week_day * sec_day)
  }
  
  while (as.numeric(format(temp_date, "%Y")) <= year) {
    epi_calendar <- c(epi_calendar, as.character.Date(temp_date))
    temp_date <- temp_date + 7 * sec_day
  }
  
  if (last_week_day < 3) {
    epi_calendar <- utils::head(epi_calendar, -1)
  }
  
  return(as.Date.character(epi_calendar))
}

#' Extends an incidence class object with incidence rates estimations.
#'
#' @description Function that estimates incidence rates from a incidence class
#' object and population projections.
#' @param incidence_object An incidence object.
#' @param level Administration level at which incidence counts are grouped
#' (0 = national, 1 = state/department, 2 = city/municipality).
#' @param scale Scale to consider when calculating the incidence_rate.
#'
#' @return A modified incidence object where counts are normalized with the
#' population.
#'
#' @examples
#' \dontrun{
#' incidence_rate(incidence_object, 2)
#' }
#'
#' @export
incidence_rate <- function(incidence_object, level, scale = 100000) {
  # Input check
  stopifnot(
    "`incidence_object` must have incidence class" =
      (inherits(incidence_object, "incidence")),
    "`level` must be numeric" = (is.numeric(level)),
    "`scale` must be numeric" = (is.numeric(scale))
  )
  dates <- lubridate::year(incidence_object$dates)
  years <- unique(dates)
  if (level == 0) {
    path_0 <- system.file("extdata", "population_projection_col_0.rda",
                          package = "epiCo"
    )
    load(path_0)
    population_projection_col_0 <- population_projection_col_0
    populations <- population_projection_col_0
    populations$code <- population_projection_col_0$DP
    groups <- 0
  } else if (level == 1) {
    path_1 <- system.file("extdata", "population_projection_col_1.rda",
                          package = "epiCo"
    )
    load(path_1)
    population_projection_col_1 <- population_projection_col_1
    populations <- population_projection_col_1
    populations$code <- population_projection_col_1$DP
    groups <- as.numeric(colnames(incidence_object$counts))
  } else if (level == 2) {
    path_2 <- system.file("extdata", "population_projection_col_2.rda",
                          package = "epiCo"
    )
    load(path_2)
    population_projection_col_2 <- population_projection_col_2
    populations <- population_projection_col_2
    populations$code <- population_projection_col_2$DPMP
    groups <- as.numeric(colnames(incidence_object$counts))
  } else {
    stop("Error in Administrative Level selection")
  }
  
  # if (sum(!(years %in% unique(populations$ANO))) +
  #     sum(!(groups %in% unique(populations$code))) > 0) {
  #   stop("No population projections found.
  #          Incidence groups and administration level may not match or
  #          dates may be out of the projections (2005-2023)")
  # } else {
  populations <- dplyr::filter(
    populations,
    .data$code %in% groups,
    .data$ANO %in% years
  )
  incidence_rates <- incidence_object$counts
  
  for (group in groups) {
    for (year in years) {
      year_population <- dplyr::filter(populations, .data$ANO == year)
      group_population <- year_population[
        year_population$code == group,
        "Total_General"
      ]
      if (level == 0) {
        incidence_rates[which(dates == year)] <-
          incidence_rates[which(dates == year)] * scale / group_population
      } else {
        incidence_rates[which(dates == year), as.character(group)] <-
          incidence_rates[which(dates == year), as.character(group)] *
          scale / group_population
      }
    }
  }
  
  incidence_rate_object <- incidence_object
  incidence_rate_object$rates <- incidence_rates
  return(incidence_rate_object)
}

#' Returns the geometric mean of a vector of real numbers.
#'
#' @description Function that returns the geometric mean of a vector of real
#' numbers according to the selected method.
#'
#' @param x A numeric vector of real values
#' @param method
#' Description of methods:
#' - positive = only positive values within x are used in the calculation.
#' - shifted = positive and zero values within x are used by adding a shift
#' value before the calculation and subtracting it to the final result.
#' - optimized = optimized shifted method. See: De La Cruz, R., & Kreft, J. U.
#' (2018). Geometric mean extension for data sets with zeros. arXiv preprint
#' arXiv:1806.06403.
#' - weighted = a probability weighted calculation of gm for negative, positive,
#' and zero values. See: Habib, E. A. (2012). Geometric mean for negative and
#' zero values. International Journal of Research and Reviews in Applied
#' Sciences, 11(3), 419-432.
#' @param shift = 1 (default) a positive value to use in the shifted method
#' @param epsilon = 1e-5 (default) the minimum positive value to consider in the
#' optimized method.
#'
#' @return The geometric mean of the x vector, and the epsilon value if
#' optimized method is used.
#'
#' @examples
#' \dontrun{
#' x <- c(4, 5, 3, 7, 8)
#' geom_mean(x, method = "optimized")
#' }
#'
#' @export
geom_mean <- function(x, method = "positive", shift = 1, epsilon = 1e-5) {
  stopifnot(
    "`x`must be numeric" = (is.numeric(x)),
    "`method` must be positive, shifted, otimized or wehighted" =
      (method %in% c("positive", "shifted", "optimized", "weighted")),
    "`shift` must be numeric" = (is.numeric(shift)),
    "`epsilon` must be numeric" = (is.numeric(epsilon)),
    "`x` includes zero or negative values, check the geom_mean methods"
    = (any(x<=0) & method == "positive")
  )
  
  if (method == "positive") {
    gm <- exp(mean(log(x)))
  } else if (method == "shifted") {
    x_shifted <- x + shift
    if (any(x_shifted<=0)) {
      stop("shifted `x` still includes zero or negative values,
              reconsider the shifting parameter")
    }
    gm <- exp(mean(log(x_shifted))) - shift
  } else if (method == "weighted") {
    n_x <- length(x)
    
    x_positive <- x[x > 0]
    w_positive <- length(x_positive) / n_x
    x_negative <- x[x < 0]
    w_negative <- length(x_negative) / n_x
    x_zeros <- x[x == 0]
    w_zeros <- length(x_zeros) / n_x
    
    gm_positive <- exp(sum(log(x_positive))/n_x)
    gm_negative <- -1 * exp(sum(log(abs(x_negative)))/n_x)
    gm_zeros <- 0
    
    gm <- w_positive * gm_positive + w_negative * gm_negative + w_zeros *
      gm_zeros
  } else if (method == "optimized") {
    # The formula is:
    # exp(mean(log(x+delta)))-delta (Eq. I)
    # where delta is the maximum value such that:
    # abs([exp(mean(log(x_positive+delta)))-delta]-geomean(x_positive))<
    # epsilon*geomean(x_positive) (Eq. II)
    if (any(x<0)) {
      stop("`x` includes negative values, check the geom_mean methods")
    }
    
    x_positive <- x[x > 0]
    gm_positive <- exp(mean(log(x_positive)))
    epsilon <- epsilon * gm_positive
    
    # Simple bisection  method to calculate delta: (Eq. I) is increasing as
    # consequence of the Superaddivity of the Geometric Mean
    
    delta_min <- 0
    delta_max <- gm_positive + epsilon
    
    while (exp(mean(log(x_positive+delta_max)))-delta_max < epsilon){
      delta_min <- delta_max
      delta_max <- delta_max*2
    }
    
    delta <- (delta_min + delta_max) / 2
    
    # Define aus_exp to not repeat operations
    aus_exp <- exp(mean(log(x_positive + delta))) - delta
    
    while ((aus_exp - gm_positive) > epsilon) {
      if ((aus_exp < gm_positive)) {
        delta_min <- delta
      } else {
        delta_max <- delta
      }
      
      delta <- (delta_min + delta_max) / 2
      aus_exp <- exp(mean(log(x_positive + delta))) - delta
      print(delta)
    }
    gm <- round(exp(mean(log(x + delta))) - delta, 5)
    delta <- round(delta, 5)
    
    return(c(gm, delta))
  }
  gm <- round(gm, 5)
  return(gm)
}
