#' Returns the endemic channel of a disease
#' 
#' Function that builds the endemic channel of a disease time series based on the selected method and windows of observation
#' @param observations A numeric vector with the current observations
#' @param incidence_historic An incidence object with the historic observations
#' @param method = "geometric" (default) A string with the mean calculation method of preference (median, mean, or geometric)
#' @param geom_method = NULL (default) A string with the selected method for geometric mean calculation, see: geomMean
#' @param window = 0 (default) A numeric value to specify the number of previous and posterior periods to include in the calculation of the current period mean
#' @param outlier_years = NULL (default) A numeric vector with the oulier years
#' @param outliers_handling = "ignore" (default) A string with the handling decision regarding outlier years
#' - ignore = data from outlier years will not take into account for the calculations
#' - include = data from outlier years will take into account for the calculations
#' - replace_with_median = data from outlier years will be replaced with the median and take into account for the calculations
#' - replace_with_mean = data from outlier years will be replaced with the mean and take into account for the calculations
#' - replace_with_geom_mean = data from outlier years will be replaced with the geometric mean and take into account for the calculations
#' @param CI = 0.95 (default) A numeric value to specify the confidence interval to use with the geometric method
#' @param plot = FALSE (default) A boolean for displaying a plot  
#' @return A dataframe with the observation, historical mean, and confidence intervals (or risk areas)
#' @examples
#' endemicChannel(observations, incidence_historic, method = "geometric", outliers_handling = "replace_with_median", plot = TRUE)
#' @export
endemicChannel <- function(observations, incidence_historic, method = "geometric", geom_method = "shifted", window = 0, outlier_years = NULL, outliers_handling = "ignore", CI = 0.95, plot = FALSE) {
  
  observations <- c(observations,rep(NA,52-length(observations)))
  years <- unique(epiyear(incidence::get_dates(incidence_historic)))
  
  extra_weeks <- which(epiweek(incidence_historic$dates)==53)
  
  dates_historic <- incidence::get_dates(incidence_historic)[-extra_weeks]
  counts_historic <- incidence::get_counts(incidence_historic)[-extra_weeks]
  
  historic <- as.data.frame(matrix(counts_historic, nrow = length(years), byrow = TRUE))
  colnames(historic) <- seq(1,52)
  rownames(historic) <- years
  
  
  if (outliers_handling == "include"){
    
    historic <- historic
    
  } else if (outliers_handling == "ignore"){
    
    historic <- historic[!(row.names(historic)%in%outlier_years),]
    
  } else if (outliers_handling == "replace_with_median"){
    
    handling <- as.numeric(apply(historic, MARGIN = 2, FUN = median))
    handling <- t(replicate(length(outlier_years),handling))
    
    historic[outlier_years,] <- handling
    
  } else if("replace_with_mean"){
    
    handling <- as.numeric(apply(historic, MARGIN = 2, FUN = mean))
    handling <- t(replicate(length(outlier_years),handling))
    
    historic[outlier_years,] <- handling
    
  } else if("replace_with_geom_mean"){
    
    handling <- as.numeric(apply(historic, MARGIN = 2, FUN = geomMean, method = geom_method))
    handling <- t(replicate(length(outlier_years),handling))
    
    historic[outlier_years,] <- handling
    
  } else {
    
    print("Error")
    
  }
  
  
  if (method == "median"){
    
    central <- as.numeric(apply(historic, MARGIN = 2, FUN = quantile, p = c(0.5)))
    UL <- as.numeric(apply(historic, MARGIN = 2, FUN = quantile, p = c(0.25)))
    LL <- as.numeric(apply(historic, MARGIN = 2, FUN = quantile, p = c(0.75)))
    
  } else if (method == "mean"){
    
    central <- as.numeric(apply(historic, MARGIN = 2, FUN = mean))
    interval <- as.numeric(apply(historic, MARGIN = 2, FUN = function(x) qt(p = c((1-CI)/2),df = length(x)-1)*sd(x)/sqrt(length(x))))
    UL <- central + abs(interval)
    LL <- central - abs(interval)
    
  } else if (method == "geometric") {
    
    central <- as.numeric(apply(historic, MARGIN = 2, FUN = geomMean, method = geom_method))
    interval <- as.numeric(apply(historic, MARGIN = 2, FUN = function(x) qt(p = c((1-CI)/2),df = length(x)-1)*sd(x)/sqrt(length(x))))
    UL <- central + abs(interval)
    LL <- central - abs(interval)
    
  } else {
    
    print("Error")
    
  }
  
  endemic_channel <- data.frame(central = central,
                                UL = UL,
                                LL = LL)
  
  if (plot = TRUE)
  {
    endemic_channel_plot <- ggplot(endemic_channel, aes(x=as.numeric(row.names(endemic_channel)))) +
      geom_line(aes(y=central)) +
      geom_line(aes(y=LL), color = "green") +
      geom_line(aes(y=UL), color = "red") + theme_minimal()
    
    plot(endemic_channel_plot)
    
  }
  
  return(endemic_channel)
  
}
