#' Detects Outbreaks with SPC schemes
#'
#' Function that performs EWMA Statistical Process Control to detect outliers
#' @param epidata A data frame with the incidence or incidence rate time series
#' @param lambda An smoothing parameter 0 <= lambda <= 1
#' @param nsigmas The numeric value of sigmas to use for control limits
#'
#' @return A list with Y smoothed values and limits UCL & LCL
#' @export
detect_outbreaks_ewma <- function(epidata, lambda, nsigmas) {
  qcc_list <- qcc::ewma(epidata, lambda = lambda, nsigmas = nsigmas)
  outbreaks_ewma <- list(
    epidata = NULL, y = NULL, up_lim = NULL,
    low_lim = NULL, outliers = NULL
  )
  outbreaks_ewma$epidata <- epidata
  outbreaks_ewma$y <- qcc_list$y
  outbreaks_ewma$up_lim <- qcc_list$limits[, 2]
  outbreaks_ewma$low_lim <- qcc_list$limits[, 1]
  outbreaks_ewma$outliers <- qcc_list$violations
  return(outbreaks_ewma)
}
