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
  path <- system.file("extdata", "distance_matrix.rda", package = "epiCo")
  load(path)
  distance_matrix <- distance_matrix
  distance <- distance_matrix[
    which(row.names(distance_matrix) %in%
      query_vector),
    which(names(distance_matrix) %in%
      query_vector)
  ]
  excluded <- query_vector[!query_vector %in% rownames(distance)]
  for (i in excluded) {
    warning("municipality ", i, " was not found")
  }
  adjacency_matrix <- as.matrix(distance <= threshold)
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
#' @param incidence_object An object is the incidence of an observation for the
#' different locations.
#' @param level Administration level at which incidence counts are grouped.
#' (0=national, 1=state/department, 2=city/municipality).
#' @param scale Scale to consider when calculating the incidence_rate.
#' @param threshold Maximum traveling time around each municipality.
#' @param plot if TRUE, returns a plot of influential observations in the
#' Moran's plot.

#' @return List of Moran's I clustering analysis, giving the quadrant of each
#' observation, influential values.
#'
#' @examples
#' \dontrun{
#' morans_index(incidence_object, 2, FALSE)
#' }
#' @export
morans_index <- function(incidence_object, level, scale = 100000, threshold = 2,
                         plot = TRUE) {
  stopifnot(
    "`incidence_object` must have incidence class" =
      (inherits(incidence_object, "incidence")),
    "`incidence_object` must account for a single cumulative observation" =
      (length(incidence_object$dates) == 1),
    "`level` must be 0, 1, or 2" =
      (level %in% c(0, 1, 2)),
    "`scale` must be numeric" = (is.numeric(scale)),
    "`threshold` must be numeric" = (is.numeric(threshold)),
    "`plot` must be boolean" = (is.logical(plot))
  )
  incidence_rate <- incidence_rate(
    incidence_object = incidence_object,
    level = level, scale = scale
  )
  path_1 <- system.file("extdata", "divipola_table.rda", package = "epiCo")
  load(path_1)
  divipola_table <- divipola_table
  # Match with DIVIPOLA order
  incidence_rate_ordered <- incidence_rate[, order(match(
    colnames(incidence_rate$counts), divipola_table$COD_MPIO
  ))]
  mpios <- colnames(incidence_rate_ordered$counts)

  # Logarithmic transformation
  incidence_rate_log <- log(incidence_rate_ordered$rates)
  mpios_filtered <- as.numeric(mpios[which(incidence_rate_log > -Inf)])
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
        mean(wx) ~ "High-High",
      x < mean(x) & wx <
        mean(wx) ~ "Low-Low",
      x > mean(x) & wx <
        mean(wx) ~ "High-Low",
      x < mean(x) & wx >
        mean(wx) ~ "Low-High"
    )
  )
  infl_mpios <- moran_data_frame[which(moran_data_frame$is_inf), ]
  morans_index <- c(
    list(municipios = moran_data_frame$labels),
    list(quadrant = moran_data_frame$cluster),
    list(influential = moran_data_frame$is_inf),
    list(logIncidence = moran_data_frame$x),
    list(lagIncidence = moran_data_frame$wx)
  )
  if (!all(is.na(morans_index$quadrant))) {
    cat(paste("Influential municipalities are:", "\n"))
    # Influential observations
    for (i in seq_len(nrow(infl_mpios))) {
      if (!is.na(infl_mpios$cluster[i])) {
        # nolint start: string_boundary_linter
        relative_incidence <- ifelse(substr(
          infl_mpios$cluster[i], 1, 1
        ) == "H", "high", "low")
        # nolint end: string_boundary_linter
        relative_correlation <- ifelse(substr(
          infl_mpios$cluster[i], 2, 2
        ) == "H", "high", "low")
        cat(paste(
          infl_mpios$labels[i], "with", relative_incidence,
          "incidence and", relative_correlation, "spatial correlation",
          "\n"
        ))
      }
    }
  }
  # Plot
  if (plot) {
    if (all(is.na(morans_index$quadrant))) {
      warning("There are no influential municipalities to plot")
    } else {
      path_2 <- system.file("extdata", "spatial_polygons_col_2.rda",
        package = "epiCo"
      )
      load(path_2)
      spatial_polygons_col_2 <- spatial_polygons_col_2

      shapes <- spatial_polygons_col_2[spatial_polygons_col_2$MPIO_CDPMP %in%
        as.integer(morans_index$municipios), ]
      shapes_order <- match(shapes$MPIO_CDPMP, morans_index$municipios)
      shapes$MPIO_CDPMP <- morans_index$municipios[shapes_order]
      shapes$CLUSTER <- morans_index$quadrant[shapes_order]
      shapes_names <- rep(NA, length(morans_index$municipios))
      index <- 1
      for (n in morans_index$municipios[shapes_order]) {
        s_name <- divipola_table$NOM_MPIO[which(
          divipola_table$COD_MPIO == n
        )]
        shapes_names[index] <- s_name
        index <- index + 1
      }
      shapes$NOM_MPIO <- shapes_names

      popup_data <- paste0(
        "<b>", "Municipality Name: ", "</b>",
        shapes$NOM_MPIO, "<br>",
        "<b>", "Municipality Code: ", "</b>",
        shapes$MPIO_CDPMP, "<br>",
        "<b>", "Cluster: ", "</b>",
        shapes$CLUSTER, "<br>"
      )
      # nolint start
      pal <- leaflet::colorFactor(
        palette = c(
          "#ba0001", "#357a38", "#2c7c94",
          "#fbe45b"
        ),
        domain = c("High-High", "Low-Low", "Low-High", "High-Low"),
        ordered = TRUE
      )

      tile_provider <- "https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}"

      clusters_plot <- leaflet::leaflet(shapes) %>%
        leaflet::addTiles(tile_provider) %>%
        leaflet::addPolygons(
          stroke = TRUE,
          weight = 2,
          smoothFactor = 0.2,
          fillColor = ~ pal(shapes$CLUSTER),
          popup = popup_data,
          color = "black",
          fillOpacity = ifelse(shapes$CLUSTER == "High-High" | 
                                 shapes$CLUSTER == "Low-Low", 0.65, 0
          )
        ) %>%
        leaflet::addLegend("bottomright",
          pal = pal, values = ~ c("High-High", "Low-Low"),
          title = "Local Moran's Index Clusters",
          opacity = 1
        )
      # nolint end
      return(list(moran_data = morans_index, leaflet_map = clusters_plot))
    }
  }
  return(morans_index)
}
