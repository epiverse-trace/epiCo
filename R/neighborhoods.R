#' Get ISCO-88 occupation labels from codes
#'
#' Function that translates a vector of ISCO-88 occupation codes into a vector
#' of labels.
#' @param codes Codes of the municipalities to consider for the neighborhoods.
#' @param threshold Maximum traveling time around each city

#' @return neighborhood object according to the introduced threshold
#' @examples
#' neighborhood(codes, 2)
#' @export
neighborhoods <- function(codes = c("all"), threshold = 2) {
  data("distance_matrix")
  distance <- distance_matrix[
    which(row.names(distance_matrix) %in% codes),
    which(names(distance_matrix) %in% codes)
  ]
  adjacency_matrix <- as.matrix(ifelse(distance <= threshold, 1, 0))
  list_weights <- mat2listw(adjacency_matrix)
  neighborhoods <- list_weights$neighbours
  rm(distance_matrix, envir = .GlobalEnv)
  return(neighborhoods)
}
