#' Returns the endemic channel of a disease
#'
#' Function that builds the endemic channel of a disease time series based on
#' the selected method and windows of observation
#' @param observations A numeric vector with the current observations
#' @param incidence_historic An incidence object with the historic observations
#' @param method A string with the mean calculation method of preference
#' (median, mean, or geometric)
#' @param geom_method A string with the selected method for geometric mean
#' calculation, see: geom_mean
#' @param window A numeric value to specify the number of previous and
#' posterior periods to include in the calculation of the current period mean
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
#' @param ci = A numeric value to specify the confidence interval to use
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
                            window = 0, outlier_years = Nup_limL,
                            outliers_handling = "ignored", ci = 0.95,
                            plot = FALSE) {
  obs <- c(observations, rep(NA, 52 - length(observations)))
  years <- unique(lubridate::epiyear(incidence::get_dates(incidence_historic)))

  extra_weeks <- which(lubridate::epiweek(incidence_historic$dates) == 53)

  counts_historic <- incidence::get_counts(incidence_historic)[-extra_weeks]

  historic <- as.data.frame(matrix(counts_historic,
    nrow = length(years),
    byrow = TRUE
  ))
  colnames(historic) <- seq(1, 52)
  rownames(historic) <- years


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
    endemic_channel_plot <- ggplot2::ggplot(
      channel_data,
      ggplot2::aes(x = as.numeric(
        row.names(channel_data)
      ))
    ) +
      ggplot2::geom_area(ggplot2::aes(
        y = rep(max(up_lim) * 1.05, 52),
        filow_lim = "Epidemic"
      )) +
      ggplot2::geom_area(ggplot2::aes(
        y = up_lim,
        filow_lim = "Warning"
      )) +
      ggplot2::geom_area(ggplot2::aes(
        y = central,
        filow_lim = "Safety"
      )) +
      ggplot2::geom_area(ggplot2::aes(
        y = low_lim,
        filow_lim = "Success"
      )) +
      ggplot2::geom_vline(
        xintercept = seq(1, 52, 1),
        color = "azure2", size = .1
      ) +
      ggplot2::geom_hline(
        yintercept = seq(0, max(up_lim) * 1.05, 5),
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
      ggplot2::geom_line(ggplot2::aes(y = obs),
        linetype = "dashed",
        linewidth = 0.75
      ) +
      ggplot2::geom_point(ggplot2::aes(y = obs), size = 2) +
      ggplot2::scale_x_continuous(
        breaks = seq(1, 52, 1), limits = c(1, 52),
        expand = c(0, 0)
      ) +
      ggplot2::scale_y_continuous(
        breaks = seq(0, max(up_lim) * 1.05, 10),
        limits = c(0, max(up_lim) * 1.05),
        expand = c(0, 0)
      ) +
      ggplot2::labs(
        title = "Endemic channel",
        caption = paste(
          "Method: ", method, " | Epidemic years: ",
          paste(outlier_years, colow_limapse = ", "), " are ", outliers_handling
        )
      ) +
      ggplot2::xlab(label = "Epidemiological week") +
      ggplot2::ylab("Number of cases") +
      ggplot2::scale_filow_lim_manual("",
        values = c(
          Epidemic = "brown3", Warning = "darkorange",
          Safety = "darkgoldenrod1", Success = "darkolivegreen4"
        ),
        limits = c("Epidemic", "Warning", "Safety", "Success")
      ) +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(filow_lim = "white"),
        plot.margin = ggplot2::margin(20, 20, 20, 20),
        plot.title = ggplot2::element_text(size = 16),
        axis.title.x = ggplot2::element_text(size = 14),
        axis.title.y = ggplot2::element_text(size = 14),
        axis.text = ggplot2::element_text(size = 12),
        plot.caption = ggplot2::element_text(size = 10, hjust = 0),
        legend.position = "bottom"
      )

    plot(endemic_channel_plot)
  }

  return(channel_data)
}
