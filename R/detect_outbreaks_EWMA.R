#' Detects Outbreaks with SPC schemes
#'
#' Function that performs EWMA Statistical Process Control to detect outliers
#' @param epidata A data frame with the incidence or incidence rate time series
#' @param lambda An smoothing parameter 0 <= lambda <= 1
#' @param nsigmas The numeric value of sigmas to use for control limits
#'
#' @return A list with Y smoothed values and limits UCL & LCL
#' @export
detect_outbreaks_EWMA <- function(epidata, lambda, nsigmas) {

  qcc_list <- qcc::ewma(epidata, lambda = lambda, nsigmas = nsigmas)

  outbreaks_EWMA <- list(epidata = NULL, y = NULL, up_lim = NULL, low_lim = NULL, outliers = NULL)

  outbreaks_EWMA$epidata <- epidata
  outbreaks_EWMA$y <- qcc_list$y
  outbreaks_EWMA$up_lim <- qcc_list$limits[, 2]
  outbreaks_EWMA$low_lim <- qcc_list$limits[, 1]
  outbreaks_EWMA$outliers <- qcc_list$violations

  return(outbreaks_EWMA)
}
