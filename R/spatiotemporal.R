#' Neighborhoods from real travel distances
#' 
#' @description Function to build neighborhoods from real travel distances 
#' inside Colombia by land or river transport.
#' 
#' @param query_vector Codes of the municipalities to consider for the 
#' neighborhoods.
#' @param threshold Maximum traveling time around each municipality.

#' @return neighborhood object according to the introduced threshold.
#' 
#' @examples
#' \donttest{
#'   query_vector <- c(5001, 5002, 5004, 5021, 5030, 5615, 5607)
#'   neighborhood(query_vector, 2)
#' }
#' 
#' @export
neighborhoods <- function(query_vector, threshold = 2){
  data("distance_matrix", package = "epiCo")
  distance <- distance_matrix[which(row.names(distance_matrix) %in% 
                                      query_vector), 
                              which(names(distance_matrix) %in% 
                                      query_vector)]
  adjacency_matrix <- as.matrix(ifelse(distance <= threshold, 1, 0))
  list_weights <- spdep::mat2listw(adjacency_matrix)
  neighborhoods <- list_weights$neighbours
  return(neighborhoods)
}

#' Calculate spatial correlation of given municipalities in an incidence_rate
#' object.
#' 
#' @description Function to calculate spatial autocorrelation via Moran's Index
#' from a given incidence_rate object grouped by municipality.
#' @param incidence_rate Incidence rate object with only one observation for a 
#' group of municipalities.
#' @param threshold Maximum traveling time around each municipality.
#' @param plot if TRUE, returns a plot of influential observations in the 
#' Moran's plot.

#' @return List of Moran's I clustering analysis, giving the quadrant of each 
#' observation, influential values.
#' 
#' @examples
#' \donttest{
#'   morans_index(incidence_rate, 2, FALSE)
#' }
#' @export
morans_index <- function(incidence_rate,threshold = 2, plot = TRUE){
  data("divipola_table", package = "epiCo")
  # Match with DIVIPOLA order
  incidence_rate_ordered <- incidence_rate[,order(match(
    colnames(incidence_rate$counts),divipola_table$COD_MPIO))]
  mpios <- colnames(incidence_rate_ordered$counts)
  
  # Logarithmic transformation
  incidence_rate_log <- log(incidence_rate_ordered$rates)
  mpios_filtered <- mpios[which(incidence_rate_log > -Inf)]
  incidence_rate_log <- incidence_rate_log[which(incidence_rate_log > -Inf)]
  
  # Neighborhood structure
  nb <- neighborhoods(mpios_filtered, threshold)
  weights <- spdep::nb2listw(nb, style = "W")
  
  # Moran's I
  morans_i <- spdep::moran.plot(incidence_rate_log, 
                                listw = weights, plot = F)
  moran_data_frame <- as.data.frame(morans_i)
  #Clusters
  moran_data_frame <- dplyr::mutate(moran_data_frame, 
                                    cluster = case_when(x > mean(x) & wx > 
                                                          mean(wx) ~ "HH",
                                                        x < mean(x) & wx < 
                                                          mean(wx) ~ "LL",
                                                        x > mean(x) & wx < 
                                                          mean(wx) ~ "HL",
                                                        x < mean(x) & wx > 
                                                          mean(wx) ~ "LH"))
  influential_mpios = moran_data_frame[which(moran_data_frame$is_inf == TRUE),]
  #Slope
  #slope_moran <- lm(wx ~ x, morans_i)$coefficients[2]
  
  morans_index = c(list(municipios = moran_data_frame$labels), 
                   list(quadrant = moran_data_frame$cluster),
                   list(influential = moran_data_frame$is_inf),
                   list(logIncidence = moran_data_frame$x),
                   list(lagIncidence = moran_data_frame$wx))
  cat(paste("Influential municipalities are:", "\n"))
  #Influential observations
  for(i in 1:nrow(influential_mpios)){
    relative_incidence <- ifelse(substr(
      influential_mpios$cluster[i],1,1) == "H","high", "low")
    relative_correlation <- ifelse(substr(
      influential_mpios$cluster[i],2,2) == "H","high", "low")
    cat(paste(influential_mpios$labels[i], "con", relative_incidence, 
              "incidence and", relative_correlation, "spatial correlation",
              "\n"))
  }
  #Plot
  if(plot == TRUE){
    data("spatial_polygons_col_2", package = "epiCo")
    library(leaflet)
    pal <- leaflet::colorFactor(palette = c("#ba0001", "#357a38", "#2c7c94", 
                                                     "#fbe45b"), 
                                domain = c("HH", "LL", "LH", "HL"),
                                ordered = TRUE)
    shapes <- spatial_polygons_col_2[spatial_polygons_col_2$MPIO_CDPMP %in% 
                                      as.integer(influential_mpios$labels),]
    shapes_plot <- shapes[,order(match(as.integer(influential_mpios$labels),
                                       shapes$MPIO_CDPMP))]
    shapes_plot$CLUSTER <- influential_mpios$cluster
    shapes_plot$NOM_MPIO <- divipola_table$NOM_MPIO[divipola_table$COD_MPIO %in% 
                                                      shapes_ordered$MPIO_CDPMP]
    popup_data <- paste0("<b>","Municipality Name: ","</b>", 
                         shapes_plot$NOM_MPIO, "<br>",
                         "<b>","Municipality Code: ","</b>", 
                         shapes_plot$MPIO_CDPMP, "<br>",
                         "<b>","Cluster: ", "</b>", 
                         shapes_plot$CLUSTER, "<br>")
    leaflet::leaflet(shapes_plot) %>% addTiles() %>% 
      addPolygons(stroke=TRUE,
                   weight=1,
                   smoothFactor = 0.2,
                   fillColor  = ~ pal(CLUSTER),
                   popup = popup_data,
                   color = "white",
                   fillOpacity = .75)
    # rm(spatial_polygons_col_2, envir = .GlobalEnv)
    # rm(divipola_table, envir = .GlobalEnv)
  }
  return(morans_index)
}


