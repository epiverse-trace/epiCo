#' Returns an automated endemic channel of a disease
#'
#' @description Function that performs automated data wrangling necessary for
#' an automated endemic channel given a method and an specific disease, location
#' and year.
#'
#' @param incidence_historic An incidence object with the historic weekly cases
#' @param outlier_years A numeric vector with the outlier years
#' @param outliers_handling A string with the handling decision regarding
#' outlier years
#' @param ci = 0.95 A numeric value to specify the confidence interval to use
#' with the geometric method
#'
#' @return TBD
#'
#' @examples
#' \dontrun{
#' unusual_behaviour(incidence_historic)
#' }
#'
#' @export
auto_endemic_channel <- function(disease_name, divipola_code, year,
                                 observations = NULL,
                                 location = "O", window = 7,
                                 method = "geometric", geom_method = "shifted",
                                 outlier_years = NULL,
                                 outliers_handling = "ignored",
                                 ci = 0.95,
                                 plot = TRUE) {
  ## Data import and cleaning ####

  years_to_analyze <- seq(year - window + 1, year)

  events_to_analyze <- disease_name

  tags_to_analyze <- c(
    "FEC_NOT",
    "COD_PAIS_O", "COD_DPTO_O", "COD_MUN_O",
    "COD_DPTO_R", "COD_MUN_R",
    "COD_DPTO_N", "COD_MUN_N"
  )

  disease_data <- data.frame(matrix(ncol = length(tags_to_analyze), nrow = 0))
  colnames(disease_data) <- tags_to_analyze

  for (y in years_to_analyze)
  {
    for (e in events_to_analyze)
    {
      temp_data <- sivirep::import_data_disease_by_year(y, e)
      temp_data$FEC_NOT <- as.character(temp_data$FEC_NOT)
      temp_data$FEC_NOT <- format(
        as.Date(temp_data$FEC_NOT,
          tryFormats = c("%Y-%m-%d", "%d/%m/%Y")
        ),
        "%Y-%m-%d"
      )
      disease_data <- rbind(
        disease_data,
        dplyr::select(
          temp_data,
          tags_to_analyze
        )
      )
    }
  }

  ## Dates and DIVIPOLA codes preparation and cleaning

  disease_data <- disease_data %>%
    mutate(
      COD_MUN_R = ifelse(COD_DPTO_R == 1,
        COD_PAIS_O, # 1 indicates residence abroad
        ifelse(nchar(COD_MUN_R) == 1,
          as.numeric(paste(COD_DPTO_R,
            COD_MUN_R,
            sep = "00"
          )),
          ifelse(nchar(COD_MUN_R) == 2,
            as.numeric(paste(COD_DPTO_R,
              COD_MUN_R,
              sep = "0"
            )),
            ifelse(nchar(COD_MUN_R) == 3,
              as.numeric(paste(COD_DPTO_R,
                COD_MUN_R,
                sep = ""
              )),
              NA
            )
          )
        )
      ),
      COD_MUN_O = ifelse(COD_DPTO_O == 1,
        COD_PAIS_O, # 1 indicates infection occurred abroad
        ifelse(nchar(COD_MUN_O) == 1,
          as.numeric(paste(COD_DPTO_O,
            COD_MUN_O,
            sep = "00"
          )),
          ifelse(nchar(COD_MUN_O) == 2,
            as.numeric(paste(COD_DPTO_O,
              COD_MUN_O,
              sep = "0"
            )),
            ifelse(nchar(COD_MUN_O) == 3,
              as.numeric(paste(COD_DPTO_O,
                COD_MUN_O,
                sep = ""
              )),
              NA
            )
          )
        )
      ),
      EPI_WEEK = epiweek(FEC_NOT),
      EPI_MONTH = month(FEC_NOT),
      EPI_YEAR = epiyear(FEC_NOT)
    )

  # Cleaning of cases without specified municipalities

  disease_data <- dplyr::filter(disease_data, !is.na(disease_data$COD_MUN_O))
  disease_data <- dplyr::filter(disease_data, !is.na(disease_data$COD_MUN_R))
  disease_data <- dplyr::filter(disease_data, !is.na(disease_data$COD_MUN_N))

  # Cleaning of cases out of the years range

  disease_data <- dplyr::filter(disease_data, EPI_YEAR %in% years_to_analyze)

  # Cleaning of cases from abroad

  disease_data <- dplyr::filter(disease_data, COD_PAIS_O == 170)

  # Cleaning of typos

  data("divipola_table", package = "epiCo")

  typos <- which(disease_data$COD_PAIS_O == 170 &
    !(disease_data$COD_MUN_O %in% divipola_table$COD_MPIO))

  disease_data <- disease_data[-typos, ]

  #####


  disease_data <- dplyr::filter(disease_data, COD_MUN_O == divipola_code)

  interval <- ifelse(method == "unusual_behavior", "1 month", "1 epiweek")

  incidence_historic <- incidence::incidence(disease_data$FEC_NOT,
    interval = interval
  )

  endemic_channel(observations, incidence_historic, plot = TRUE)
}
#' Returns the endemic channel of a disease
#'
#' Function that builds the endemic channel of a disease time series based on
#' the selected method and windows of observation
#' @param observations A numeric vector with the current observations
#' @param incidence_historic An incidence object with the historic weekly
#' observations
#' @param method A string with the mean calculation method of preference
#' (median, mean, or geometric) or to use unusual behavior method (Poisson
#' Distribution Test for hypoendemic settings)
#' @param geom_method A string with the selected method for geometric mean
#' calculation, see: geom_mean
#' @param window A numeric value to specify the number of previous and
#' posterior periods to include in the calculation of the current period mean
#' @param outlier_years A numeric vector with the outlier years
#' @param outliers_handling A string with the handling decision regarding
#' outlier years, see: outliers_handling function
#' @param ci = 0.95 A numeric value to specify the confidence interval to use
#' with the geometric method
#' @param plot A boolean for displaying a plot
#' @return A dataframe with the observation, historical mean, and confidence
#' intervals (or risk areas)
#' @examples
#' \dontrun{
#' endemic_channel(observations, incidence_historic,
#'   method = "geometric",
#'   outliers_handling = "replace_with_median", plot = TRUE
#' )
#' }
#' @export
endemic_channel <- function(observations, incidence_historic,
                            method = "geometric", geom_method = "shifted",
                            outlier_years = NULL, outliers_handling = "ignored",
                            ci = 0.95,
                            plot = FALSE) {
  ifelse(incidence_historic$interval == "1 month",
    period <- 12,
    period <- 52
  )
  obs <- c(observations, rep(NA, period - length(observations)))
  years <- unique(lubridate::epiyear(incidence::get_dates(incidence_historic)))

  extra_weeks <- which(lubridate::epiweek(incidence_historic$dates) == 53)

  ifelse(incidence_historic$interval == "1 month",
    counts_historic <- as.numeric(incidence::get_counts(
      incidence_historic
    )),
    counts_historic <- as.numeric(incidence::get_counts(
      incidence_historic
    )[-extra_weeks])
  )

  historic <- as.data.frame(matrix(counts_historic,
    nrow = length(years),
    byrow = TRUE
  ))
  colnames(historic) <- seq(1, period)
  rownames(historic) <- years

  historic <- endemic_outliers(
    historic, outlier_years, outliers_handling,
    geom_method
  )

  if (method == "median") {
    central <- as.numeric(apply(historic,
      MARGIN = 2,
      FUN = stats::quantile, p = c(0.5)
    ))
    up_lim <- as.numeric(apply(historic,
      MARGIN = 2,
      FUN = stats::quantile, p = c(0.75)
    ))
    low_lim <- as.numeric(apply(historic,
      MARGIN = 2,
      FUN = stats::quantile, p = c(0.25)
    ))
  } else if (method == "mean") {
    central <- as.numeric(apply(historic, MARGIN = 2, FUN = mean))
    interval <- as.numeric(apply(historic,
      MARGIN = 2, FUN = function(x) {
        stats::qt(
          p = c((1 - ci) / 2),
          df = length(x) - 1
        ) *
          stats::sd(x) / sqrt(length(x))
      }
    ))
    up_lim <- central + abs(interval)
    low_lim <- central - abs(interval)
  } else if (method == "geometric") {
    central <- as.numeric(apply(historic,
      MARGIN = 2,
      FUN = geom_mean, method = geom_method
    ))
    interval <- as.numeric(apply(historic,
      MARGIN = 2, FUN = function(x) {
        stats::qt(
          p = c((1 - ci) / 2),
          df = length(x) - 1
        ) *
          stats::sd(x) / sqrt(length(x))
      }
    ))
    up_lim <- central + abs(interval)
    low_lim <- central - abs(interval)
  } else if (method == "unusual_behavior") {
    central <- as.numeric(apply(historic, MARGIN = 2, FUN = mean))
    up_lim <- c()
    low_lim <- c()
    for (c in central) {
      poiss_test <- poisson.test(round(c),
        alternative = "two.sided",
        conf.level = ci
      )
      up_lim <- c(up_lim, poiss_test$conf.int[2])
      low_lim <- c(low_lim, poiss_test$conf.int[1])
    }
  } else {
    return("Error in central tendency method")
  }

  channel_data <- data.frame(
    central = central,
    up_lim = up_lim,
    low_lim = low_lim,
    obs = obs
  )

  channel_data$low_lim[which(channel_data$low_lim < 0)] <- 0

  if (plot == TRUE) {
    endemic_channel_plot <- endemic_plot(
      channel_data, method,
      outlier_years, outliers_handling
    )
    plot(endemic_channel_plot)
    endemic_channel_data <- list(
      endemic_channel_plot = endemic_channel_plot,
      channel_data = channel_data
    )
    return(endemic_channel_data)
  } else {
    return(channel_data)
  }
}

#' Modifies the historic incidence to handle with the observations of epidemic
#' years
#'
#' @description Function that modifies an historic incidence by including,
#' ignoring, or replacing the observations of epidemic years
#'
#' @param historic Historic incidence counts
#' @param outlier_years A numeric vector with the outlier years
#' @param outliers_handling A string with the handling decision regarding
#' outlier years
#' - ignored = data from outlier years will not take into account
#' - included = data from outlier years will take into account
#' - replaced_by_median = data from outlier years will be replaced with the
#' median and take into account
#' - replaced_by_mean = data from outlier years will be replaced with the
#' mean and take into account
#' - replaced_by_geom_mean = data from outlier years will be replaced with the
#' geometric mean and take into account
#' @param geom_method A string with the selected method for geometric mean
#' calculation, see: geom_mean
#'
#' @return A modified historic incidence
#'
#' @examples
#' \dontrun{
#' endemic_outliers(historic, outlier_years, outliers_handling,
#'   geom_method = "shifted"
#' )
#' }
#'
#' @export
endemic_outliers <- function(historic, outlier_years, outliers_handling,
                             geom_method = "shifted") {
  if (outliers_handling == "included") {
    historic <- historic
  } else if (outliers_handling == "ignored") {
    historic <- historic[!(row.names(historic) %in% outlier_years), ]
  } else if (outliers_handling == "replaced_by_median") {
    handling <- as.numeric(apply(historic, MARGIN = 2, FUN = stats::median))
    handling <- t(replicate(length(outlier_years), handling))
    historic[outlier_years, ] <- handling
  } else if (outliers_handling == "replaced_by_mean") {
    handling <- as.numeric(apply(historic, MARGIN = 2, FUN = mean))
    handling <- t(replicate(length(outlier_years), handling))
    historic[outlier_years, ] <- handling
  } else if (outliers_handling == "replaced_by_geom_mean") {
    if (geom_method == "optimized") {
      handling <- apply(historic,
        MARGIN = 2, FUN = geom_mean,
        method = geom_method
      )
      handling <- as.numeric(handling[1, ])
    } else {
      handling <- as.numeric(apply(historic,
        MARGIN = 2, FUN = geom_mean,
        method = geom_method
      ))
    }

    handling <- t(replicate(length(outlier_years), handling))

    historic[outlier_years, ] <- handling
  } else {
    return("Error in outlier years handling")
  }
  return(historic)
}

#' Creates the endemic channel plot
#'
#' @description Function creates the endemic channel plot
#'
#' @param channel_data Data frame with the central tendency, upper limit,
#' lower limit, and observations to plot
#' @param method A string with method used in the endemic channel calculation
#' @param outlier_years A numeric vector with the outlier years
#' @param outliers_handling A string with the handling decision regarding
#' outlier years
#'
#' @return The ggplot object with the endemic channel plot
#'
#' @examples
#' \dontrun{
#' endemic_plot(channel_data, method, outlier_years, outliers_handling)
#' }
#'
#' @export
endemic_plot <- function(channel_data, method,
                         outlier_years, outliers_handling) {
  endemic_channel_plot <- ggplot2::ggplot(
    channel_data,
    ggplot2::aes(x = as.numeric(
      row.names(channel_data)
    ))
  ) +
    ggplot2::geom_area(ggplot2::aes(
      y = rep(max(c(max(up_lim), obs), na.rm = T) * 1.05, nrow(channel_data)),
      fill = "Epidemic"
    )) +
    ggplot2::geom_area(ggplot2::aes(
      y = up_lim,
      fill = "Warning"
    )) +
    ggplot2::geom_area(ggplot2::aes(
      y = central,
      fill = "Safety"
    )) +
    ggplot2::geom_area(ggplot2::aes(
      y = low_lim,
      fill = "Success"
    )) +
    ggplot2::geom_vline(
      xintercept = seq(1, nrow(channel_data), 1),
      color = "azure2", size = .1
    ) +
    ggplot2::geom_hline(
      yintercept = seq(0, max(c(max(channel_data$up_lim), channel_data$obs),
        na.rm = T
      ) * 1.05, length.out = 8),
      color = "azure2", size = .1
    ) +
    ggplot2::geom_line(ggplot2::aes(y = up_lim),
      linewidth = 1,
      color = "brown4"
    ) +
    ggplot2::geom_line(ggplot2::aes(y = central),
      linewidth = 1,
      color = "darkorange3"
    ) +
    ggplot2::geom_line(ggplot2::aes(y = low_lim),
      linewidth = 1,
      color = "darkgreen"
    ) +
    ggplot2::scale_y_continuous(
      breaks = round(seq(0, max(c(max(channel_data$up_lim), channel_data$obs),
        na.rm = T
      ) * 1.05, length.out = 8)),
      limits = c(0, max(c(max(channel_data$up_lim), channel_data$obs),
        na.rm = T
      ) * 1.05),
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      title = "Endemic channel",
      caption = paste(
        "Method: ", method, " | Epidemic years: ",
        paste(outlier_years, colow_limapse = ", "), " are ", outliers_handling
      )
    ) +
    ggplot2::xlab(label = "Epidemiological period") +
    ggplot2::ylab("Number of cases") +
    ggplot2::scale_fill_manual("",
      values = c(
        Epidemic = "brown3",
        Warning = "darkorange",
        Safety = "darkgoldenrod1",
        Success = "darkolivegreen4"
      ),
      limits = c(
        "Epidemic",
        "Warning",
        "Safety",
        "Success"
      )
    ) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white"),
      plot.margin = ggplot2::margin(20, 20, 20, 20),
      plot.title = ggplot2::element_text(size = 16),
      axis.title.x = ggplot2::element_text(size = 14),
      axis.title.y = ggplot2::element_text(size = 14),
      axis.text = ggplot2::element_text(size = 12),
      plot.caption = ggplot2::element_text(size = 10, hjust = 0),
      legend.position = "bottom"
    )

  if (all(!is.na(channel_data$obs))) {
    endemic_channel_plot <- endemic_channel_plot +
      ggplot2::geom_line(ggplot2::aes(y = channel_data$obs),
        linetype = "dashed",
        linewidth = 0.75
      ) +
      ggplot2::geom_point(ggplot2::aes(y = channel_data$obs), size = 2) +
      ggplot2::scale_x_continuous(
        breaks = seq(1, nrow(channel_data), 1),
        limits = c(1, nrow(channel_data)),
        expand = c(0, 0)
      )
  }

  return(endemic_channel_plot)
}
