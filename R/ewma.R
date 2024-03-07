#' Returns the EWMA smoothed of a vector
#'
#' Function that performs EWMA smoothing on a vector
#' @param x A numeric vector
#' @param lambda = 0.2 (default) The smoothing parameter
#' @param delta = 1e-3 (default) The lowest weight to include in the calculation
#' @return The EWMA smoothed vector
#' @export
ewma <- function(x, lambda = 0.2, delta = 1e-3) {
    weights <- lambda * (1 - lambda)^(seq_along(x)-1)
    set_to_zero <- which(weights <= delta)[-1]
    weights[set_to_zero] <- 0

  y <- x

  for (i in 2:length(x)) {
    y[i] <- sum(rev(y[1:i]) * weights[1:i])
  }

  y <- y[-1]

  return(list(y, weights))
}

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
