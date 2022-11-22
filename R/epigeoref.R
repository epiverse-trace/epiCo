#' Complete georeference data from an epidemiologic linelist
#' 
#' Function that complete a structure of municipality names, DIVIPOLA codes, and coordinates
#' from an input of one of these variables
#' 
#' @param query A vector of DIVIPOLA codes, names or coordinates
#'  
#' @return A dataframe with DIVIPOLA codes, names, longitude and latitude from the query
#' @examples
#' epigeoref(query_vector)
#' @export
#' 
epigeoref <- function(query_vector) {
  
  query_labels <- colnames(query_vector)
  
  output_labels <- c("NOM_MPIO", "COD_MPIO", "LONGITUD", "LATITUD")
  
  if (sum(query_labels == c("LONGITUD", "LATITUD"))==2)
  {
    dist_matrix <- geosphere::distm(query_vector, DIVIPOLA_table[,c("LONGITUD","LATITUD")])
    min.d <- apply(dist_matrix, 1, function(x) order(x, decreasing=F)[1])
    epiGeoRef <- DIVIPOLA_table[min.d,output_labels]
  } else {
    epiGeoRef <- merge(query_vector, DIVIPOLA_table, by = query_labels)
    epiGeoRef <- epiGeoRef[,output_labels]
  }
  
  return(epiGeoRef)
}
