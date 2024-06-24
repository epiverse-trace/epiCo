#' Create and return the endemic channel of a disease from an incidence object
#'
#' @description Function that builds the endemic channel of a disease time
#' series based on the selected method and windows of observation
#'
#' @param incidence_historic An incidence object with the historic weekly
#' observations
#' @param observations A numeric vector with the current observations
#' @param method A string with the mean calculation method of preference
#' (median, mean, or geometric) or to use the unusual behavior method (Poisson
#' Distribution Test for Hypoendemic settings)
#' @param geometric_method A string with the selected method for geometric mean
#' calculation; see: geometric_mean
#' @param outlier_years A numeric vector with the outlier years
#' @param outliers_handling A string with the handling decision regarding
#' outlier years, see: outliers_handling function
#' @param ci = 0.95 A numeric value to specify the confidence interval to use
#' with the geometric method
#' @param plot A boolean for displaying a plot
#' @param language Language for plot components
#'
#' @return A dataframe with the observation, historical mean, and confidence
#' intervals (or risk areas)
#'
#' @examples
#' data_event <- epiCo::epi_data
#' data_ibague <- data_event[data_event$cod_mun_o == 73001, ]
#' incidence_historic <- incidence::incidence(data_ibague$fec_not,
#'   interval = "1 epiweek"
#' )
#' endemic_channel(incidence_historic,
#'   method = "geometric", plot = TRUE
#' )
#' @export
endemic_channel <- function(incidence_historic, observations = NULL,
                            method = c(
                              "geometric", "median", "mean",
                              "unusual_behavior"
                            ),
                            geometric_method = "shifted",
                            outlier_years = NULL,
                            outliers_handling = c(
                              "ignored", "included",
                              "replaced_by_median",
                              "replaced_by_mean",
                              "replaced_by_geometric_mean"
                            ),
                            ci = 0.95, plot = FALSE, language = c("EN", "ES")) {
  stopifnot(
    "`incidence_historic` must be an incidence object" =
      inherits(incidence_historic, "incidence"),
    "incidence interval should be `1 week` or `1 epiweek`" =
      incidence_historic$interval %in%
        c("1 week", "1 epiweek"),
    "`incidence_historic` must contain at least one complete epidemiological
    year" =
      ((incidence_historic$interval == "1 week" &
        length(incidence_historic$dates) >= 52)),
    "`ci` must be a number between 0 and 1" =
      (ci >= 0 & ci <= 1 & is.numeric(ci)),
    "`plot` must be a boolean" =
      (is.logical(plot))
  )
  method <- match.arg(method)
  outliers_handling <- match.arg(outliers_handling)
  language <- match.arg(language)

  if (!is.null(observations)) {
    stopifnot(
      "`observations` must be numeric and positive" =
        (is.numeric(observations) &&
          all(observations >= 0)),
      "`observations` size doesn't correspond to incidence interval" =
        ((incidence_historic$interval == "1 week" &
          length(observations) == 52))
    )
  }

  if (incidence_historic$interval %in% c("1 week", "1 epiweek")) {
    period <- 52
    first_year <- lubridate::epiyear(incidence_historic$dates[1])
    if (incidence_historic$dates[1] != epi_calendar(first_year)[1]) {
      new_date <- epi_calendar(first_year + 1)[1]
      incidence_historic <- incidence_historic[incidence_historic$dates >=
        new_date]
      message(
        "Data prior to ", new_date,
        " were not used for the endemic channel calculation."
      )
    }
    last_year <- lubridate::epiyear(incidence_historic$dates[
      length(incidence_historic$dates)
    ])
    last_epidate <- epi_calendar(last_year)[
      length(epi_calendar(last_year))
    ]
    if (incidence_historic$dates[length(incidence_historic$dates)] !=
      last_epidate) {
      new_date <- epi_calendar(last_year - 1)[
        length(epi_calendar(last_year - 1))
      ]
      incidence_historic <- incidence_historic[incidence_historic$dates <=
        as.Date(new_date)]
      message(
        "Data after", new_date,
        " were not used for the endemic channel calculation."
      )
    }
    extra_weeks <- which(lubridate::epiweek(incidence_historic$dates) == 53)
    counts_historic <- as.numeric(incidence::get_counts(
      incidence_historic
    ))
    if (length(extra_weeks) > 0) {
      counts_historic <- counts_historic[-extra_weeks]
    }
  }
  obs <- c(observations, rep(NA, period - length(observations)))
  years <- unique(lubridate::epiyear(incidence::get_dates(incidence_historic)))

  stopifnot(
    "`outlier_years` include years outside the historic range" =
      all(outlier_years %in% years)
  )

  historic <- as.data.frame(matrix(counts_historic,
    nrow = length(years),
    byrow = TRUE
  ))
  colnames(historic) <- seq(1, period)
  rownames(historic) <- years

  historic <- endemic_outliers(
    historic, outlier_years, outliers_handling,
    geometric_method
  )

  switch(method,
    median = {
      central <- as.numeric(apply(historic,
        MARGIN = 2,
        FUN = stats::quantile, p = 0.5
      ))
      up_lim <- as.numeric(apply(historic,
        MARGIN = 2,
        FUN = stats::quantile, p = 0.75
      ))
      low_lim <- as.numeric(apply(historic,
        MARGIN = 2,
        FUN = stats::quantile, p = 0.25
      ))
    },
    mean = {
      central <- as.numeric(colMeans(historic))
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
    },
    geometric = {
      central <- as.numeric(apply(historic,
        MARGIN = 2,
        FUN = geometric_mean, method = geometric_method
      ))
      interval <- as.numeric(apply(historic,
        MARGIN = 2, FUN = function(x) {
          stats::qt(
            p = c((1 - ci) / 2),
            df = length(x) - 1
          ) *
            geometric_sd(x, method = geometric_method) / sqrt(length(x))
        }
      ))
      up_lim <- exp(log(central) + abs(interval))
      low_lim <- exp(log(central) - abs(interval))
    },
    unusual_behavior = {
      central <- as.numeric(colMeans(historic))
      up_lim <- NULL
      low_lim <- NULL
      for (c in central) {
        poiss_test <- stats::poisson.test(round(c),
          alternative = "two.sided",
          conf.level = ci
        )
        up_lim <- c(up_lim, poiss_test$conf.int[2])
        low_lim <- c(low_lim, poiss_test$conf.int[1])
      }
    }
  )

  stopifnot(
    "Insufficient information to estimate 'endemic channel' limits, check for at
    least two years of data" = !all(is.na(c(low_lim, up_lim)))
  )

  channel_data <- data.frame(
    central = central,
    up_lim = up_lim,
    low_lim = low_lim,
    obs = obs
  )

  channel_data$low_lim[which(channel_data$low_lim < 0)] <- 0

  if (plot) {
    endemic_channel_plot <- endemic_plot(
      channel_data, method,
      outlier_years, outliers_handling, language
    )
    plot(endemic_channel_plot)
    return(list(data = channel_data, plot = endemic_channel_plot))
  } else {
    return(channel_data)
  }
}

#' Modifies the historic incidence to handle with the observations of epidemic
#' years
#'
#' @description Function that modifies an historic incidence by including,
#' ignoring or replacing the observations of epidemic years
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
#' - replaced_by_geometric_mean = data from outlier years will be replaced with
#' the geometric mean and take into account
#' @param geometric_method A string with the selected method for geometric mean
#' calculation; see: geometric_mean
#'
#' @return A modified historic incidence
#'
#' @keywords internal
endemic_outliers <- function(historic, outlier_years, outliers_handling,
                             geometric_method = "shifted") {
  if (outliers_handling == "included") {
    historic <- historic
  } else if (outliers_handling == "ignored") {
    historic <- historic[!(row.names(historic) %in% outlier_years), ]
  } else if (outliers_handling == "replaced_by_median") {
    handling <- as.numeric(apply(historic, MARGIN = 2, FUN = stats::median))
    handling <- t(replicate(length(outlier_years), handling))
    historic[as.character(outlier_years), ] <- handling
  } else if (outliers_handling == "replaced_by_mean") {
    handling <- as.numeric(colMeans(historic))
    handling <- t(replicate(length(outlier_years), handling))
    historic[as.character(outlier_years), ] <- handling
  } else if (outliers_handling == "replaced_by_geometric_mean") {
    handling <- apply(historic,
      MARGIN = 2, FUN = geometric_mean,
      method = geometric_method
    )
    if (geometric_method == "optimized") {
      handling <- as.numeric(handling[1, ])
    } else {
      handling <- as.numeric(handling)
    }

    handling <- t(replicate(length(outlier_years), handling))

    historic[as.character(outlier_years), ] <- handling
  }
  return(historic)
}

#' Creates the endemic channel plot
#'
#' @description Function that creates the endemic channel plot
#'
#' @param channel_data Data frame with the central tendency, upper limit,
#' lower limit, and observations to plot
#' @param method A string with the method used in the endemic channel
#' calculation
#' @param outlier_years A numeric vector with the outlier years
#' @param outliers_handling A string with the handling decision regarding
#' outlier years
#' @param language Language for plot components
#'
#' @return The ggplot object with the endemic channel plot
#'
#' @keywords internal
endemic_plot <- function(channel_data, method,
                         outlier_years, outliers_handling, language) {
  endemic_channel_plot <- ggplot2::ggplot(
    channel_data,
    ggplot2::aes(x = as.numeric(
      row.names(channel_data)
    ))
  ) +
    ggplot2::geom_line(ggplot2::aes(y = .data$up_lim, color = "Epidemic"),
      linewidth = 1
    ) +
    ggplot2::geom_line(ggplot2::aes(y = .data$central, color = "Warning"),
      linewidth = 1
    ) +
    ggplot2::geom_line(ggplot2::aes(y = .data$low_lim, color = "Safety"),
      linewidth = 1
    ) +
    ggplot2::scale_y_continuous(
      breaks = round(seq(0, max(c(max(channel_data$up_lim), channel_data$obs),
        na.rm = TRUE
      ) * 1.05, length.out = 8)),
      limits = c(0, max(c(max(channel_data$up_lim), channel_data$obs),
        na.rm = TRUE
      ) * 1.05),
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      title = "Endemic channel",
      caption = ifelse(length(outlier_years) > 1,
        paste(
          "Method: ", method, " | Epidemic years: ",
          toString(outlier_years), " are ", outliers_handling
        ),
        paste(
          "Method: ", method, " | Epidemic year: ",
          outlier_years, " is ", outliers_handling
        )
      )
    ) +
    ggplot2::xlab(label = "Epidemiological week") +
    ggplot2::ylab("Number of cases") +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white"),
      plot.margin = ggplot2::margin(20, 20, 20, 20),
      plot.title = ggplot2::element_text(size = 16),
      axis.title.x = ggplot2::element_text(size = 14),
      axis.title.y = ggplot2::element_text(size = 14),
      axis.text = ggplot2::element_text(size = 12),
      plot.caption = ggplot2::element_text(size = 10, hjust = 0),
      legend.position = "bottom",
      panel.background = ggplot2::element_blank()
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, nrow(channel_data), 2),
      limits = c(1, nrow(channel_data)),
      expand = c(0.01, 0.01)
    )

  if (language == "ES") {
    endemic_channel_plot <- endemic_channel_plot +
      ggplot2::geom_line(ggplot2::aes(y = .data$up_lim, color = "Epidemia"),
        linewidth = 1
      ) +
      ggplot2::geom_line(ggplot2::aes(y = .data$central, color = "Alerta"),
        linewidth = 1
      ) +
      ggplot2::geom_line(ggplot2::aes(y = .data$low_lim, color = "Seguridad"),
        linewidth = 1
      ) +
      ggplot2::labs(
        title = "Canal endemico",
        caption = ifelse(length(outlier_years) > 1,
          paste(
            "Metodo: ", method, " | Temporada epidemica: ",
            toString(outlier_years), " son ", outliers_handling
          ),
          paste(
            "Metodo: ", method, " | Temporadas epidemicas: ",
            outlier_years, " es ", outliers_handling
          )
        )
      ) +
      ggplot2::xlab(label = "Semana epidemiologica") +
      ggplot2::ylab("Numero de casos")
  }

  if (anyNA(channel_data$obs)) {
    endemic_channel_plot <- endemic_channel_plot +
      # nolint start
      ggplot2::scale_color_manual(
        name = "",
        values = c(
          "Epidemic" = "brown4",
          "Warning" = "darkorange3",
          "Safety" = "darkgreen"
        )
      )
    # nolint end
    if (language == "ES") {
      endemic_channel_plot <- endemic_channel_plot +
        # nolint start
        ggplot2::scale_color_manual(
          name = "",
          values = c(
            "Epidemia" = "brown4",
            "Alerta" = "darkorange3",
            "Seguridad" = "darkgreen"
          )
        )
    }
    # nolint end
  } else {
    endemic_channel_plot <- endemic_channel_plot +
      ggplot2::geom_line(ggplot2::aes(y = .data$obs, color = "Observed cases"),
        linetype = "dashed",
        linewidth = 0.75
      ) +
      ggplot2::geom_point(ggplot2::aes(y = .data$obs, color = "Observed cases"),
        size = 2
      ) +
      # nolint start
      ggplot2::scale_color_manual(
        name = "",
        values = c(
          "Epidemic" = "brown4",
          "Warning" = "darkorange3",
          "Safety" = "darkgreen",
          "Observed cases" = "black"
        )
      )
    # nolint end
    if (language == "ES") {
      endemic_channel_plot <- endemic_channel_plot +
        ggplot2::geom_line(
          ggplot2::aes(
            y = .data$obs,
            color = "Casos observados"
          ),
          linetype = "dashed",
          linewidth = 0.75
        ) +
        ggplot2::geom_point(
          ggplot2::aes(
            y = .data$obs,
            color = "Casos observados"
          ),
          size = 2
        ) +
        # nolint start
        ggplot2::scale_color_manual(
          name = "",
          values = c(
            "Epidemia" = "brown4",
            "Alerta" = "darkorange3",
            "Seguridad" = "darkgreen",
            "Casos observados" = "black"
          )
        )
    }
    # nolint end
  }

  return(endemic_channel_plot)
}
