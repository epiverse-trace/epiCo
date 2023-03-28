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
  ## TO TRY: perform EWMA smoothing and limits calculation lighter

  # outbreaks_EWMA <- list(y=NULL, UCL=NULL, LCL=NULL)
  #
  # ndata <- length(epidata)
  # indices <- 1:length(epidata)
  #
  # y_EWMA <- c(epidata[1],epidata)
  #
  # for (i in 2:(ndata + 1))
  #   y_EWMA[i] <- lambda * y_EWMA[i] + (1 - lambda) * y_EWMA[i - 1]
  #
  # sigma2 <- sd(epidata)^2 * ((lambda/(2-lambda))*(1-(1-lambda)^(2*(1:ndata))))
  # UCL <- mean(epidata) + nsigmas*sqrt(sigma2)
  # LCL <- mean(epidata) - nsigmas*sqrt(sigma2)
  #
  # outbreaks_EWMA$y <- y_EWMA[-1]
  # outbreaks_EWMA$UCL <- UCL
  # outbreaks_EWMA$LCL <- LCL

  qcc_list <- qcc::ewma(epidata, lambda = lambda, nsigmas = nsigmas)

  outbreaks_EWMA <- list(epidata = NULL, y = NULL, UCL = NULL, LCL = NULL, outliers = NULL)

  outbreaks_EWMA$epidata <- epidata
  outbreaks_EWMA$y <- qcc_list$y
  outbreaks_EWMA$UCL <- qcc_list$limits[, 2]
  outbreaks_EWMA$LCL <- qcc_list$limits[, 1]
  outbreaks_EWMA$outliers <- qcc_list$violations

  return(outbreaks_EWMA)
}
