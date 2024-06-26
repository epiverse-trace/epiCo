#' Neighborhoods from real travel distances in Colombia
#'
#' @description Function to build neighborhoods from real travel distances
#' inside Colombia by land or river transportation.
#'
#' @param query_vector Codes of the municipalities to consider for the
#' neighborhoods.
#' @param threshold Maximum traveling time around each municipality.
#' @return neighborhood object according to the introduced threshold.
#'
#' @examples
#' query_vector <- c("05001", "05002", "05004", "05021", "05030", "05615")
#' neighborhoods(query_vector, 2)
#'
#' @export
neighborhoods <- function(query_vector, threshold = 2) {
  stopifnot("`query_vector` must be a character vector" = (is.character(
    query_vector
  )))
  path <- system.file("extdata", "distance_matrix.rds", package = "epiCo")
  distance_matrix <- as.matrix(readRDS(path))
  distance <- distance_matrix[
    which(row.names(distance_matrix) %in%
      query_vector),
    which(colnames(distance_matrix) %in%
      query_vector)
  ]
  excluded <- query_vector[!query_vector %in% rownames(distance)]
  for (i in excluded) {
    warning("municipality ", i, " was not found")
  }
  distance[!distance <= threshold] <- 0
  list_weights <- spdep::mat2listw(distance, style = "W", zero.policy = TRUE)
  null_municipalities <- row.names(distance)[unlist(lapply(
    list_weights$weights, is.null
  ))]
  if (length(null_municipalities) > 0) {
    msg <- paste(
      "Municipalities", toString(null_municipalities),
      "are not part of the neighborhood according to the selected",
      "thershold in hours. It wil be displayed as 'Not significant'",
      "but it was not included in the local moran's index analysis."
    )
    message(msg)
  }
  return(list_weights)
}

#' Calculate spatial correlation of given municipalities in an incidence_rate
#' object.
#'
#' @description Function to calculate spatial autocorrelation via Moran's Index
#' from a given incidence_rate object grouped by municipality.
#'
#' @importFrom magrittr %>%
#'
#' @param incidence_object An incidence object with one observation for the
#' different locations (groups).
#' @param scale Scale to consider when calculating the incidence_rate.
#' @param threshold Maximum traveling time around each municipality.
#' @param plot if TRUE, returns a plot of influential observations in the
#' Moran's plot.
#' @param language Language for plot components

#' @return List of Moran's I clustering analysis, giving the quadrant of each
#' observation, influential values.
#'
#' @examples
#' data_event <- epiCo::epi_data
#' incidence_historic <- incidence::incidence(data_event$fec_not,
#'   groups = data_event$cod_mun_o,
#'   interval = "4 year"
#' )
#' incidence_object <- subset(incidence_historic,
#'   from = "2015-01-04",
#'   to = "2018-12-27"
#' )
#' morans_index(incidence_object, scale = 100000, threshold = 2, plot = TRUE)
#' @export
morans_index <- function(incidence_object, scale = 100000, threshold = 2,
                         plot = TRUE, language = c("EN", "ES")) {
  stopifnot(
    "`incidence_object` must have incidence class" =
      (inherits(incidence_object, "incidence")),
    "`incidence_object` must account for a single cumulative observation" =
      (length(incidence_object$dates) == 1),
    "`scale` must be numeric" = (is.numeric(scale)),
    "`threshold` must be numeric" = (is.numeric(threshold)),
    "`plot` must be boolean" = (is.logical(plot))
  )
  language <- match.arg(language)

  incidence_rate <- incidence_rate(
    incidence_object = incidence_object,
    level = 2, scale = scale
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
  mpios_filtered <- mpios[which(incidence_rate_log > -Inf)]
  incidence_rate_log <- incidence_rate_log[which(incidence_rate_log > -Inf)]

  # Neighborhood structure
  weights <- neighborhoods(mpios_filtered, threshold)

  # Moran's I
  morans_i <- spdep::localmoran(incidence_rate_log,
    listw = weights
  )
  moran_data_frame <- as.data.frame(morans_i)
  moran_data_frame$quadr <- attributes(morans_i)$quadr$mean
  moran_data_frame$quadr <- factor(moran_data_frame$quadr,
    levels = c(
      "High-High", "High-Low",
      "Low-High", "Low-Low",
      "Not Significant"
    )
  )
  moran_data_frame[
    moran_data_frame$`Pr(z != E(Ii))` > 0.05 |
      is.na(moran_data_frame$`Pr(z != E(Ii))`),
    "quadr"
  ] <- "Not Significant"

  morans_index <- list(
    municipios = row.names(moran_data_frame),
    quadrant = moran_data_frame$quadr
  )

  if (all(morans_index$quadrant == "Not Significant")) {
    message("There are no influential municipalities to plot")
    return(morans_index)
  } else {
    message("Significant municipalities are:")
    # Influential observations
    significant_municipalities <- paste(
      sprintf(
        "%s with %s (incidence - spatial correlation)",
        row.names(
          moran_data_frame[moran_data_frame$quadr != "Not Significant", ]
        ),
        moran_data_frame[moran_data_frame$quadr != "Not Significant", "quadr"]
      ),
      collapse = "\n"
    )
    message(significant_municipalities)
  }
  # Plot
  if (plot) {
    map_title <- paste0(
      "Local Moran's Index Clusters </br>",
      lubridate::epiyear(incidence_object$dates),
      "-W", lubridate::epiweek(incidence_object$dates),
      " to ", incidence_object$interval, " later"
    )
    if (language == "ES") {
      map_title <- paste0(
        "Clusters del Indice de Moran Local </br>",
        lubridate::epiyear(incidence_object$dates),
        "-SE", lubridate::epiweek(incidence_object$dates),
        " a ", incidence_object$interval, " despues"
      )
    }
    path_2 <- system.file("extdata", "spatial_polygons_col_2.rda",
      package = "epiCo"
    )
    load(path_2)
    spatial_polygons_col_2 <- spatial_polygons_col_2

    shapes <- spatial_polygons_col_2[spatial_polygons_col_2$MPIO_CDPMP %in%
      morans_index$municipios, ]
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
        "#ba0001", "#00992C", "#80CC96",
        "#F08E94", "#c8c8c8"
      ),
      domain = c(
        "High-High", "Low-Low", "Low-High", "High-Low",
        "Not Significant"
      ),
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
        fillOpacity = 0.65
      ) %>%
      leaflet::addLegend("bottomright",
        pal = pal,
        values = ~ c(
          "High-High",
          "Low-Low",
          "Low-High",
          "High-Low",
          "Not Significant"
        ),
        title = map_title,
        opacity = 1
      )
    # nolint end
    return(list(data = morans_index, plot = clusters_plot))
  } else {
    return(morans_index)
  }
}
