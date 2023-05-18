#' Identifies unusual behavior by performing a Poisson test on expected and
#' observed incidence
#'
#' @description Function that detects if there have been a significant increment
#' or decrement of cases in hypoendemic territories using a Poisson distribution
#' test
#'
#' @param 
#'
#' @return 
#'
#' @examples
#' \dontrun{
#' 
#' }
#'
#' @export
unusual_behaviour <- function(observations, incidence_historic,
                                 window = 0, outlier_years = NULL,
                                 outliers_handling = "ignored",
                                 ci = 0.95, plot = FALSE) {
  obs <- c(observations, rep(NA, 12 - length(observations)))
  years <- unique(lubridate::epiyear(incidence::get_dates(incidence_historic)))
  
  counts_historic <- incidence::get_counts(incidence_historic)
  
  historic <- as.data.frame(matrix(counts_historic,
                                   nrow = length(years),
                                   byrow = TRUE
  ))
  colnames(historic) <- seq(1, 12)
  rownames(historic) <- years
  historic <- outliers_handling(historic, outlier_years, outliers_handling,
                                geom_method)
  
  central <- as.numeric(apply(historic, MARGIN = 2, FUN = mean))
  poiss_test <- poisson.test(central, alternative = "two.sided", conf.level = ci)
  up_lim <- poiss_test$conf.level[2]
  low_lim <- poiss_test$conf.level[1]
  
}