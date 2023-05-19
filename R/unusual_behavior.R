#' Identifies unusual behavior by performing a Poisson test on expected and
#' observed incidence
#'
#' @description Function that detects if there have been a significant increment
#' or decrement of cases in hypoendemic territories using a Poisson distribution
#' test
#'
#' @param incidence_historic An incidence object with the historic weekly
#' observations
#' @param outlier_years A numeric vector with the outlier years
#' @param outliers_handling A string with the handling decision regarding
#' outlier years
#' @param ci = 0.95 A numeric value to specify the confidence interval to use
#' with the geometric method
#'
#' @return 
#'
#' @examples
#' \dontrun{
#' unusual_behaviour(incidence_historic)
#' }
#'
#' @export
unusual_behaviour <- function(historic,
                              outlier_years = NULL,
                              outliers_handling = "ignored",
                              ci = 0.95) {
  
  years <- unique(lubridate::epiyear(incidence::get_dates(incidence_historic)))
  
  counts_historic <- incidence::get_counts(incidence_historic)
  
  historic <- as.data.frame(matrix(counts_historic,
                                   nrow = length(years),
                                   byrow = TRUE
  ))
  colnames(historic) <- seq(1, 12)
  rownames(historic) <- years
  historic2 <- endemic_outliers(historic, outlier_years, outliers_handling,
                                geom_method)
  
  central <- as.numeric(apply(historic, MARGIN = 2, FUN = mean))
  up_lim <- c()
  low_lim <- c()
  for (c in central){
    poiss_test <- poisson.test(round(c), alternative = "two.sided", conf.level = ci)
    up_lim <- c(up_lim,poiss_test$conf.int[2])
    low_lim <- c(low_lim,poiss_test$conf.int[1])
  }
  unusual_behavior_data <- list(central = central, up_lim = up_lim,
                                low_lim = low_lim)
  return(unusual_behavior_data)
  
}
