#' Returns the endemic channel of a disease
#' 
#' Function that builds the endemic channel of a disease time series based on the selected method and windows of observation
#' @param incidence An incidence or incidenceRate object with the current observations
#' @param historic An incidence or incidenceRate object with the historic observations
#' @param method = "geometric" (default) A string with the mean calculation method of preference (geometric or arithmetic)
#' @param period_window = 0 (default) A numeric value to specify the number of previous and posterior periods to include in the calculation of the current period mean
#' @param CI = 0.95 (default) A numeric calue to specify the confidence interval to use with the geometric method
#' @param plot = FALSE (default) A boolean for displaying a plot  
#' @return A dataframe with the observation, historial mean, and confidence intervals (or risk areas)
#' @examples
#' endemicChannel(incidence, period_window = 3, CI = 0.95, cumu = FALSE, plot = TRUE)
#' @export
endemicChannel <- function(incidence, historic, method = "geometric", period_window = 0, CI = 0.95, plot = FALSE) {
  
  
  if (method == "geometric"){
    
    gmean = geomMean(historic)
    
  } else if (method == "arithmetic"){
    
    amean = mean(historic)
    
  } else {
    
    print("Error in method selection")
  }
  
  
}
