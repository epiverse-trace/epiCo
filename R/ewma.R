#' Returns the EWMA smoothed of a vector
#'
#' Function that performs EWMA smoothing on a vector
#' @param x A numeric vector
#' @param lambda = 0.2 (default) The smoothing parameter
#' @param delta = 1e-3 (default) The lowest weight to include in the calculation
#' @return The EWMA smoothed vector
#' @examples
#' ewma(x, lambda = 0.2)
#' @export
ewma <- function(x, lambda = 0.2, delta = 1e-3) {
  weights <- rep(0, length(x))
  weights[1] <- lambda

  n <- 1

  while (n < length(x) && weights[n] > delta) {
    weights[n + 1] <- lambda * (1 - lambda)^n
    n <- n + 1
  }

  y <- x

  for (i in 2:length(x)) {
    y[i] <- sum(rev(y[1:i]) * weights[1:i])
  }

  y <- y[-1]

  return(list(y, weights))
}
