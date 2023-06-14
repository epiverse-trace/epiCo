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
#' \dontrun{
#' query_vector <- c(5001, 5002, 5004, 5021, 5030, 5615, 5607)
#' neighborhood(query_vector, 2)
#' }
#'
#' @export
neighborhoods <- function(query_vector, threshold = 2) {
  stopifnot("`query_vector` must be numeric" = (is.numeric(query_vector)))
  path <- system.file("data", "distance_matrix.rda", package = "epiCo")
  load(path)
  distance_matrix <- distance_matrix
  distance <- distance_matrix[
    which(row.names(distance_matrix) %in%
            query_vector),
    which(names(distance_matrix) %in%
            query_vector)
  ]
  excluded <- query_vector[!query_vector %in% rownames(distance)]
  for(i in excluded){
    warning(paste("municipality", i, "was not found"))
  }
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
#'
#' @importFrom magrittr %>%
#'
#' @param incidence_rate Incidence rate object with only one observation for a
#' group of municipalities.
#' @param threshold Maximum traveling time around each municipality.
#' @param plot if TRUE, returns a plot of influential observations in the
#' Moran's plot.

#' @return List of Moran's I clustering analysis, giving the quadrant of each
#' observation, influential values.
#'
#' @examples
#' \dontrun{
#' morans_index(incidence_rate, 2, FALSE)
#' }
#' @export
morans_index <- function(incidence_rate, threshold = 2, plot = TRUE) {
  stopifnot("`incidence_rate` must have observations for only one given date" =
              ncol(incidence_rate$rates)==length(incidence_rate$rates))
  path_1 <- system.file("data", "divipola_table.rda", package = "epiCo")
  load(path_1)
  divipola_table <- divipola_table
  # Match with DIVIPOLA order
  incidence_rate_ordered <- incidence_rate[, order(match(
    colnames(incidence_rate$counts), divipola_table$COD_MPIO
  ))]
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
                                listw = weights, plot = FALSE
  )
  moran_data_frame <- as.data.frame(morans_i)
  # Clusters
  moran_data_frame <- dplyr::mutate(moran_data_frame,
                                    cluster = dplyr::case_when(
                                      x > mean(x) & wx >
                                        mean(wx) ~ "HH",
                                      x < mean(x) & wx <
                                        mean(wx) ~ "LL",
                                      x > mean(x) & wx <
                                        mean(wx) ~ "HL",
                                      x < mean(x) & wx >
                                        mean(wx) ~ "LH"
                                    )
  )
  inf_mpios <- moran_data_frame[which(moran_data_frame$is_inf == TRUE), ]
  morans_index <- c(
    list(municipios = moran_data_frame$labels),
    list(quadrant = moran_data_frame$cluster),
    list(influential = moran_data_frame$is_inf),
    list(logIncidence = moran_data_frame$x),
    list(lagIncidence = moran_data_frame$wx)
  )
  if (!all(is.na(morans_index$quadrant))){
    cat(paste("Influential municipalities are:", "\n"))
    # Influential observations
    for (i in seq_len(nrow(inf_mpios))) {
      if(!is.na(inf_mpios$cluster[i])){
        relative_incidence <- ifelse(substr(
          inf_mpios$cluster[i], 1, 1
        ) == "H", "high", "low")
        relative_correlation <- ifelse(substr(
          inf_mpios$cluster[i], 2, 2
        ) == "H", "high", "low")
        cat(paste(
          inf_mpios$labels[i], "with", relative_incidence,
          "incidence and", relative_correlation, "spatial correlation",
          "\n"
        ))
      }
    }
  }
  # Plot
  if (plot == TRUE) {
    if(!all(is.na(morans_index$quadrant))){
      path_2 <- system.file("data", "spatial_polygons_col_2.rda",
                            package = "epiCo"
      )
      load(path_2)
      spatial_polygons_col_2 <- spatial_polygons_col_2
      pal <- leaflet::colorFactor(
        palette = c(
          "#ba0001", "#357a38", "#2c7c94",
          "#fbe45b"
        ),
        domain = c("HH", "LL", "LH", "HL"),
        ordered = TRUE
      )
      pal_test <- pal(c("LL", "HH"))
      rm(pal_test)
      shapes <- spatial_polygons_col_2[spatial_polygons_col_2$MPIO_CDPMP %in%
                                         as.integer(inf_mpios$labels), ]
      shapes_plot <- shapes[, order(match(
        as.integer(inf_mpios$labels),
        shapes$MPIO_CDPMP
      ))]
      shapes_plot$CLUSTER <- inf_mpios$cluster
      shapes_plot$MPIO_CDPMP <- inf_mpios$labels
      shapes_plot$NOM_MPIO <- divipola_table$NOM_MPIO[divipola_table$COD_MPIO %in%
                                                        shapes_plot$MPIO_CDPMP]
      # shapes_ordered
      popup_data <- paste0(
        "<b>", "Municipality Name: ", "</b>",
        shapes_plot$NOM_MPIO, "<br>",
        "<b>", "Municipality Code: ", "</b>",
        shapes_plot$MPIO_CDPMP, "<br>",
        "<b>", "Cluster: ", "</b>",
        shapes_plot$CLUSTER, "<br>"
      )
      leaflet::leaflet(shapes_plot) %>%
        leaflet::addTiles() %>%
        leaflet::addPolygons(
          stroke = TRUE,
          weight = 1,
          smoothFactor = 0.2,
          fillColor = ~ pal(CLUSTER),
          popup = popup_data,
          color = "white",
          fillOpacity = .75
        )
    }
    else{
      warning("There are no influential municipalities to plot")
    }
  }
  return(morans_index)
}
