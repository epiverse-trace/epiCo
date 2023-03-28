#' Estimates the mode of an input vector
#'
#' Function that estimates the mode of an input vector
#' @param x A numeric or character vector
#' @return The mode of the vector
#' @examples
#' get_mode(x)
#' @export
get_mode <- function(x) {
  un <- unique(x)
  tab <- tabulate(match(x, un))
  return(un[tab == max(tab)])
}
