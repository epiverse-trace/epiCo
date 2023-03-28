#' Get the epidemiological calendar of a consulted year.
#'
#' @definition Function that returns the starting date of the epidemiological 
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
#' epi_calendar(2016)
#' 
#' @export
epi_calendar <- function(year, jan_days = 4) {
  # By definition, the first epidemiological week of the year contains at least
  # four days in January.
  epi_calendar <- c()
  
  sec_day <- 24 * 60 * 60 # seconds in a day
  
  first_date <- as.POSIXlt(paste0("01-01-", toString(year)), 
                           format = "%d-%m-%Y")
  last_date <- as.POSIXlt(paste0("31-12-", toString(year)), 
                          format = "%d-%m-%Y")
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
    epi_calendar <- head(epi_calendar, -1)
  }
  
  return(as.Date.character(epi_calendar))
}



#' Complete georeference data from an epidemiologic linelist.
#'
#' @description function that complete a structure of municipality names, 
#' DIVIPOLA codes, and coordinatesfrom an input of one of these variables.
#'
#' @param query_vector A vector of DIVIPOLA codes, names or coordinates.
#'
#' @return A dataframe with DIVIPOLA codes, names, longitude and latitude from 
#' the query.
#' 
#' @examples
#' \donttest{
#' epigeoref(query_vector)
#' } 
#' 
#' @export
epi_georef <- function(query_vector) {
  query_labels <- colnames(query_vector)
  data("divipola_table", package = "epiCo")
  output_labels <- c("NOM_MPIO", "COD_MPIO", "LONGITUD", "LATITUD")
  if (sum(query_labels == c("LONGITUD", "LATITUD")) == 2) {
    dist_matrix <- geosphere::distm(query_vector, 
                                    divipola_table[, c("LONGITUD", "LATITUD")])
    min_d <- apply(dist_matrix, 1, function(x) order(x, decreasing = F)[1])
    geo_ref <- divipola_table[min_d, output_labels]
  } else {
    geo_ref <- merge(query_vector, divipola_table, by = query_labels)
    geo_ref <- geo_ref[, output_labels]
  }
  return(geo_ref)
}

#' Extends an incidence class object with incidence rates estimations.
#'
#' @definition Function that estimates incidence rates from a incidence class 
#' object and population projections.
#' @param incidence An incidence object.
#' @param country 3 letter ISO code for the country of interest.
#' @param level Administration level at which incidence counts are grouped 
#' (0=national, 1=state/department, 2=city/municipality).
#'
#' @return A modified incidence object where counts are normalized with the 
#' population.
#' 
#' @examples
#' \donttest{
#' incidence_rate(incidence_object, 2)
#' }
#' 
#' @export
estimate_incidenceRate <- function(incidence_object, level, scale = 100000) {
  data("population_projection_col_0", packages = "epiCo")
  data("population_projection_col_1", packages = "epiCo")
  data("population_projection_col_2", packages = "epiCo")
  dates_years <- lubridate::year(incidence_object$dates)
  years <- unique(dates_years)
  groups <- colnames(incidence_object$counts)
  
  if (level == 0) {
    populations <- dplyr::filter(population_projection_col_0, ANO %in% years)
  } else if (level == 1) {
    populations <- dplyr::filter(population_projection_col_1, DP %in% groups 
                                 & ANO %in% years)
  } else {
    populations <- dplyr::filter(population_projection_col_2, DPMP %in% groups 
                                 & ANO %in% years)
  }
  
  inc_rates <- incidence_object$counts
  
  for (gr in groups)
  {
    for (ye in years)
    {
      pop <- dplyr::filter(populations, ANO == ye)
      pop <- pop[pop$DPMP == gr, "Total_General"]
      inc_rates[which(dates_years == ye), gr] <- 
        inc_rates[which(dates_years == ye), gr] * scale / pop
    }
  }
  
  incidence_rate_object <- incidence_object
  incidence_rate_object$rates <- inc_rates
  
  return(incidence_rate_object)
}

#' Returns the geometric mean of a vector of real numbers.
#'
#' @definition Function that returns the geometric mean of a vector of real 
#' numbers according to the selected method.
#' 
#' @param x A numeric vector of real values
#' @param method
#' Description of methods:
#' - positive = only positive values within x are used in the calculation.
#' - shifted = positive and zero values within x are used by adding a shift 
#' value before the calculation and subtratcting it to the final result.
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
#' x <- c(4,5,3,7,8)
#' geom_mean(x, method = "optimized")
#' 
#' @export
geom_mean <- function(x, method = "optimized", shift = 1, epsilon = 1e-5) {
  if (method == "positive") {
    x_positive <- x[x > 0]
    
    gm <- exp(mean(log(x_positive)))
  } else if (method == "shifted") {
    x_shifted <- x[x >= 0] + shift
    
    gm <- exp(mean(log(x_shifted))) - shift
  } else if (method == "weighted") {
    N <- length(x)
    
    x_positive <- x[x > 0]
    w_positive <- length(x_positive) / N
    x_negative <- x[x < 0]
    w_negative <- length(x_negative) / N
    x_zeros <- x[x == 0]
    w_zeros <- length(x_zeros) / N
    
    gm_positive <- exp(mean(log(x_positive)))
    gm_negative <- -1 * exp(mean(log(abs(x_negative))))
    gm_zeros <- 0
    
    gm <- w_positive * gm_positive + w_negative * gm_negative + w_zeros * 
      gm_zeros
  } else if (method == "optimized") {
    # The formula is:
    # exp(mean(log(x+delta)))-delta (Eq. I)
    # where delta is the maximum value such that:
    # abs([exp(mean(log(x_positive+delta)))-delta]-geomean(x_positive))<
    # epsilon*geomean(x_positive) (Eq. II)
    
    x <- x[x >= 0]
    x_positive <- x[x > 0]
    gm_positive <- exp(mean(log(x_positive)))
    epsilon <- epsilon * gm_positive
    
    # Simple bisection  method to calculate delta: (Eq. I) is increasing as
    # consequence of the Superaddivity of the Geometric Mean
    
    delta_min <- 0
    delta_max <- gm_positive + epsilon
    
    # while (exp(mean(log(x_positive+deltamax)))-deltamax < epsilon)
    # { #Just for data set with very small standard desviation
    #   delta_min <- delta_max
    #   delta_max <- delta_max*2
    # }
    
    delta <- (delta_min + delta_max) / 2
    
    # Define aus_exp to not repeat operations
    aus_exp <- exp(mean(log(x_positive + delta))) - delta 
    
    while ((aus_exp - gm_positive) > epsilon) {
      if ((aus_exp < gm_positive)) {
        deltamin <- delta
      } else {
        deltamax <- delta
      }
      
      delta <- (delta_min + delta_max) / 2
      aus_exp <- exp(mean(log(x_positive + delta))) - delta
    }
    
    gm <- exp(mean(log(x + delta))) - delta
    
    return(c(gm, delta))
  } else {
    return("Error in selection of geometric mean calculation method")
  }
  
  return(gm)
}