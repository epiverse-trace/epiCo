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
#' - ignored = data from outlier years will not take into account for the calculations
#' - included = data from outlier years will take into account for the calculations
#' - replaced_by_median = data from outlier years will be replaced with the median and take into account for the calculations
#' - replaced_by_mean = data from outlier years will be replaced with the mean and take into account for the calculations
#' - replaced_by_geom_mean = data from outlier years will be replaced with the geometric mean and take into account for the calculations
#' @param CI = 0.95 (default) A numeric value to specify the confidence interval to use with the geometric method
#' @param plot = FALSE (default) A boolean for displaying a plot  
#' @return A dataframe with the observation, historical mean, and confidence intervals (or risk areas)
#' @examples
#' endemicChannel(observations, incidence_historic, method = "geometric", outliers_handling = "replace_with_median", plot = TRUE)
#' @export
endemicChannel <- function(observations, incidence_historic, method = "geometric", geom_method = "shifted", window = 0, outlier_years = NULL, outliers_handling = "ignored", CI = 0.95, plot = FALSE) {
  
  observations <- c(observations,rep(NA,52-length(observations)))
  years <- unique(epiyear(incidence::get_dates(incidence_historic)))
  
  extra_weeks <- which(epiweek(incidence_historic$dates)==53)
  
  dates_historic <- incidence::get_dates(incidence_historic)[-extra_weeks]
  counts_historic <- incidence::get_counts(incidence_historic)[-extra_weeks]
  
  historic <- as.data.frame(matrix(counts_historic, nrow = length(years), byrow = TRUE))
  colnames(historic) <- seq(1,52)
  rownames(historic) <- years
  
  
  if (outliers_handling == "included"){
    
    historic <- historic
    
  } else if (outliers_handling == "ignored"){
    
    historic <- historic[!(row.names(historic)%in%outlier_years),]
    
  } else if (outliers_handling == "replaced_by_median"){
    
    handling <- as.numeric(apply(historic, MARGIN = 2, FUN = median))
    handling <- t(replicate(length(outlier_years),handling))
    
    historic[outlier_years,] <- handling
    
  } else if(outliers_handling == "replaced_by_mean"){
    
    handling <- as.numeric(apply(historic, MARGIN = 2, FUN = mean))
    handling <- t(replicate(length(outlier_years),handling))
    
    historic[outlier_years,] <- handling
    
  } else if(outliers_handling == "replaced_by_geom_mean"){
    
    if(geom_method == "optimized"){
      handling <- apply(historic, MARGIN = 2, FUN = geomMean, method = geom_method)
      handling <- as.numeric(handling[1,])
    } else {
      handling <- as.numeric(apply(historic, MARGIN = 2, FUN = geomMean, method = geom_method))
    }
    
    handling <- t(replicate(length(outlier_years),handling))
    
    historic[outlier_years,] <- handling
    
  } else {
    
    return("Error in outlier years handling")
    
  }
  
  
  if (method == "median"){
    
    central <- as.numeric(apply(historic, MARGIN = 2, FUN = quantile, p = c(0.5)))
    UL <- as.numeric(apply(historic, MARGIN = 2, FUN = quantile, p = c(0.75)))
    LL <- as.numeric(apply(historic, MARGIN = 2, FUN = quantile, p = c(0.25)))
    
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
    
    return("Error in central tendency method")
    
  }
  
  endemic_channel <- data.frame(central = central,
                                UL = UL,
                                LL = LL,
                                obs = observations)
  
  endemic_channel$LL[which(endemic_channel$LL<0)] <- 0
  
  if (plot == TRUE)
  {
    
    endemic_channel_plot <- ggplot(endemic_channel, aes(x=as.numeric(row.names(endemic_channel)))) +
      geom_area(aes(y=rep(max(UL)*1.05,52), fill = "Epidemic")) +
      geom_area(aes(y=UL, fill = "Warning")) +
      geom_area(aes(y=central, fill = "Safety")) +
      geom_area(aes(y=LL, fill = "Success")) +
      geom_vline(xintercept = seq(1, 52, 1), color="azure2", size=.1) +
      geom_hline(yintercept = seq(0, max(UL)*1.05, 5), color="azure2", size=.1) +
      geom_line(aes(y=UL), linewidth = 1, color = "brown4") +
      geom_line(aes(y=central), linewidth = 1, color = "darkorange3") +
      geom_line(aes(y=LL), linewidth = 1, color = "darkgreen") +
      geom_line(aes(y=obs), linetype = "dashed", linewidth = 0.75) +
      geom_point(aes(y=obs), size = 2) +
      scale_x_continuous(breaks=seq(1, 52, 1), limits = c(1, 52),
                         expand = c(0,0)) +
      scale_y_continuous(breaks=seq(0, max(UL)*1.05, 10), limits = c(0, max(UL)*1.05),
                         expand = c(0,0)) +
      labs(title = "Endemic channel", 
           caption = paste("Method: ", method, " | Epidemic years: ",
                           paste(outlier_years, collapse = ', '), " are ", outliers_handling)) + 
      xlab(label = "Epidemiological week") + ylab("Number of cases") + 
      scale_fill_manual("", values=c(Epidemic = "brown3", Warning = "darkorange",
                                     Safety = "darkgoldenrod1", Success = "darkolivegreen4"),
                        limits = c("Epidemic","Warning","Safety","Success")) +
      theme(
        plot.background = element_rect(fill = "white"),
        plot.margin = margin(20, 20, 20, 20),
        plot.title = element_text(size = 16),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12),
        plot.caption = element_text(size = 10, hjust = 0),
        legend.position = "bottom")
    
    plot(endemic_channel_plot)
    
  }
  
  return(endemic_channel)
  
}
