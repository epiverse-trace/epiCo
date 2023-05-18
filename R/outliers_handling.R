#' Modifies the historic incidence to handle with the observations of epidemic
#' years
#'
#' @description Function that modifies an historic incidence by including, 
#' ignoring, or replacing the observations of epidemic years
#' 
#' @param historic Historic incidence counts
#' @param outlier_years A numeric vector with the outlier years
#' @param outliers_handling A string with the handling decision regarding
#' outlier years
#' - ignored = data from outlier years will not take into account
#' - included = data from outlier years will take into account
#' - replaced_by_median = data from outlier years will be replaced with the
#' median and take into account
#' - replaced_by_mean = data from outlier years will be replaced with the
#' mean and take into account
#' - replaced_by_geom_mean = data from outlier years will be replaced with the
#' geometric mean and take into account
#' @param geom_method A string with the selected method for geometric mean
#' calculation, see: geom_mean
#'
#' @return A modified historic incidence
#'
#' @examples
#' \dontrun{
#' outliers_handling(historic, outlier_years, outliers_handling,
#' geom_method = "shifted")
#' }
#'
#' @export
outliers_handling <- function(historic, outlier_years, outliers_handling,
                              geom_method = "shifted"){
  if (outliers_handling == "included") {
    historic <- historic
  } else if (outliers_handling == "ignored") {
    historic <- historic[!(row.names(historic) %in% outlier_years), ]
  } else if (outliers_handling == "replaced_by_median") {
    handling <- as.numeric(apply(historic, MARGIN = 2, FUN = stats::median))
    handling <- t(replicate(length(outlier_years), handling))
    historic[outlier_years, ] <- handling
  } else if (outliers_handling == "replaced_by_mean") {
    handling <- as.numeric(apply(historic, MARGIN = 2, FUN = mean))
    handling <- t(replicate(length(outlier_years), handling))
    historic[outlier_years, ] <- handling
  } else if (outliers_handling == "replaced_by_geom_mean") {
    
    if (geom_method == "optimized") {
      handling <- apply(historic,
                        MARGIN = 2, FUN = geom_mean,
                        method = geom_method
      )
      handling <- as.numeric(handling[1, ])
    } else {
      handling <- as.numeric(apply(historic,
                                   MARGIN = 2, FUN = geom_mean,
                                   method = geom_method
      ))
    }
    
    handling <- t(replicate(length(outlier_years), handling))
    
    historic[outlier_years, ] <- handling
  } else {
    return("Error in outlier years handling")
  }
  return(historic)
}