#' Get ISCO-88 occupation labels from codes
#' 
#' Function that translates a vector of ISCO-88 occupation codes into a vector 
#' of labels
#' @param incidence_rate Incidence rate object with only one observation for a 
#' group of locations.
#' @param threshold Maximum traveling time around each city.
#' @param plot if TRUE, returns a plot of influential observations in the 
#' Moran's plot.

#' @return List of Moran's I clustering analysis, giving the quadrant of each 
#' observation, influential values and slope of the fitted regression.
#' @examples
#' morans_index(incidence_rate, 2, FALSE)
#' @export
morans_index <- function(incidence_rate,threshold = 2, plot = TRUE){
  data("DIVIPOLA_table")
  incidence_rate_ordered <- incidence_rate[,order(match(
    colnames(incidence_rate$counts),DIVIPOLA_table$COD_MPIO))]
  mpios <- colnames(incidence_rate_ordered$counts)
  incidence_rate_log <- log(incidence_rate_ordered$rates)
  mpios_filtered <- mpios[which(incidence_rate_log > -Inf)]
  incidence_rate_log <- incidence_rate_log[which(incidence_rate_log > -Inf)]
  nb <- neighborhoods(mpios_filtered, threshold)
  weights <- spdep::nb2listw(nb, style = "W")
  morans_i <- spdep::moran.plot(incidence_rate_log, listw = weights, plot = plot)
  moran_data_frame <- as.data.frame(morans_i)
  #Clusters
  moran_data_frame <- dplyr::mutate(moran_data_frame, 
                        cluster = case_when(x > mean(x) & wx > mean(wx) ~ "HH",
                                            x < mean(x) & wx < mean(wx) ~ "LL",
                                            x > mean(x) & wx < mean(wx) ~ "HL",
                                            x < mean(x) & wx > mean(wx) ~ "LH"))
  influential_mpios = moran_data_frame[which(moran_data_frame$is_inf == TRUE),]
  #Slope
  slope_moran <- lm(wx ~ x, morans_i)$coefficients[2]
  
  output = c(list(municipios = moran_data_frame$labels), 
             list(quadrant = moran_data_frame$cluster),
             list(influential = moran_data_frame$is_inf),
             list(logIncidence = moran_data_frame$x),
             list(lagIncidence = moran_data_frame$wx),
             slope = slope_moran)
  cat(paste("Los municipios influyentes son:", "\n"))
  #Influential observations
  for(i in 1:nrow(influential_mpios)){
    relative_incidence <- ifelse(substr(
      influential_mpios$cluster[i],1,1) == "H","alta", "baja")
    relative_correlation <- ifelse(substr(
      influential_mpios$cluster[i],2,2) == "H","alta", "baja")
    cat(paste(influential_mpios$labels[i], "con", relative_incidence, 
             "incidencia y", relative_correlation, "correlacion espacial","\n"))
  }
  #Plot
  if(plot == TRUE){
    data("spatialPolygons_COL_2")
    library(leaflet)
    pal <- leaflet::colorFactor(palette = c("#ba0001", "#357a38", "#2c7c94", 
                                            "#fbe45b"), 
                       domain = c("HH", "LL", "LH", "HL"),ordered = TRUE)
    shapes <- spatialPolygons_COL_2[spatialPolygons_COL_2$MPIO_CDPMP %in% 
                                      as.integer(influential_mpios$labels),]
    shapes_plot <- shapes[,order(match(as.integer(influential_mpios$labels),
                                       shapes$MPIO_CDPMP))]
    shapes_plot$CLUSTER <- influential_mpios$cluster
    shapes_plot$NOM_MPIO <- DIVIPOLA_table$NOM_MPIO[DIVIPOLA_table$COD_MPIO %in% 
                                                      shapes_ordered$MPIO_CDPMP]
    popup_data <- paste0("<b>","Nombre Municipio: ","</b>", 
                         shapes_plot$NOM_MPIO, "<br>",
                         "<b>","Código Municipio: ","</b>", 
                         shapes_plot$MPIO_CDPMP, "<br>",
                         "<b>","Cluster: ", "</b>", 
                         shapes_plot$CLUSTER, "<br>")
    leaflet::leaflet(shapes_plot) %>% addTiles() %>% addPolygons(stroke=TRUE,
                                                    weight=1,
                                                    smoothFactor = 0.2,
                                                    fillColor  = ~ pal(CLUSTER),
                                                    popup = popup_data,
                                                    color = "white",
                                                    fillOpacity = .75)
    rm(spatialPolygons_COL_2, envir = .GlobalEnv)
    rm(DIVIPOLA_table, envir = .GlobalEnv)
  }
  return(output)
}
