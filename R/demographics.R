#' Returns the population pyramid of the consulted region
#'
#' @description Function that returns the population pyramid of the municipality
#' or department of a specific year
#' @param divipola_code A numeric code accounting for the territory of interest
#' @param year A numeric input for the year of interest
#' @param gender A boolean to consult data disaggregated by gender
#' @param range A numeric value from 1 to 100 for the age range to use
#' @param total A boolean for returning the total number rather than the
#' proportion of the country's population
#' @param plot A boolean for displaying a plot
#'
#' @importFrom rlang .data
#'
#' @return A dataframe with the proportion or total count of individuals
#' @examples
#' \dontrun{
#' population_pyramid(15001, 2015, total = TRUE, plot = TRUE)
#' }
#' @export
population_pyramid <- function(divipola_code, year, gender = TRUE, range = 5,
                               total = TRUE, plot = FALSE) {
  stopifnot(
    "`year` is only available from 2005 to 2026,
            please select a valid year" = year %in% seq(2005, 2026),
    "`divipola_code` must be numeric" = (is.numeric(divipola_code) &
      length(divipola_code) == 1),
    "`range` must be an integer value between 1 and 100" = is.numeric(range) &
      range %in% seq(1, 100)
  )
  path <- system.file("extdata", "divipola_table.rda", package = "epiCo")
  load(path)
  divipola_table <- divipola_table

  if (divipola_code == 0) {
    path_0 <- system.file("extdata", "population_projection_col_0.rda",
      package = "epiCo"
    )
    load(path_0)
    population_projection_col_0 <- population_projection_col_0
    pop_data_dpto <- dplyr::filter(
      population_projection_col_0,
      ((population_projection_col_0$DP == divipola_code) &
        (population_projection_col_0$ANO == year))
    )

    female_counts <- as.numeric(pop_data_dpto[104:204])
    male_counts <- as.numeric(pop_data_dpto[3:103])
  } else if (divipola_code %in% divipola_table$COD_DPTO) {
    path_1 <- system.file("extdata", "population_projection_col_1.rda",
      package = "epiCo"
    )
    load(path_1)
    population_projection_col_1 <- population_projection_col_1
    pop_data_dpto <- dplyr::filter(
      population_projection_col_1,
      ((.data$DP == divipola_code) & (.data$ANO == year))
    )

    female_counts <- as.numeric(pop_data_dpto[104:204])
    male_counts <- as.numeric(pop_data_dpto[3:103])
  } else if (divipola_code %in% divipola_table$COD_MPIO) {
    path_2 <- system.file("extdata", "population_projection_col_2.rda",
      package = "epiCo"
    )
    load(path_2)
    population_projection_col_2 <- population_projection_col_2
    pop_data_mun <- dplyr::filter(
      population_projection_col_2,
      ((.data$DPMP == divipola_code) & (.data$ANO == year))
    )

    female_counts <- as.numeric(pop_data_mun[89:174])
    male_counts <- as.numeric(pop_data_mun[3:88])
  } else {
    stop("There is no location assigned to the consulted DIVIPOLA code")
  }

  female_total <- vector(length = length(seq(
    0, length(female_counts) - range,
    range
  )))
  male_total <- vector(length = length(seq(
    0, length(female_counts) - range,
    range
  )))
  cont <- 1
  for (h in seq(1, length(female_counts) - range, range)) {
    female_total[cont] <- sum(female_counts[h:(h + range - 1)])
    male_total[cont] <- sum(male_counts[h:(h + range - 1)])
    cont <- cont + 1
  }

  if (!total) {
    female_total <- female_total / sum(female_total)
    male_total <- male_total / sum(male_total)
  }

  if (gender) {
    pop_pyramid <- data.frame(
      age = rep(seq(0, length(female_counts) - range, range), 2),
      population = c(female_total, male_total),
      gender = c(
        rep("F", floor((length(female_counts)) / range)),
        rep("M", floor((length(male_counts)) / range))
      )
    )
  } else {
    pop_pyramid <- data.frame(
      age = seq(0, length(female_counts) - range, range),
      population = c(female_total + male_total)
    )
  }

  if (plot) {
    if (gender) {
      pop_pyramid$population <- c(-1 * female_total, male_total)
      dist_pop_f <- stats::quantile(female_total)[2:5]
      dist_pop_m <- stats::quantile(male_total)[2:5]
      dist_pop <- c(rev(-1 * dist_pop_f), 0, dist_pop_m)

      pop_pyramid_plot <- ggplot2::ggplot(
        pop_pyramid,
        ggplot2::aes(
          x = .data$age,
          y = .data$population,
          fill = .data$gender
        )
      ) +
        ggplot2::geom_bar(
          data = dplyr::filter(pop_pyramid, .data$gender == "F"),
          stat = "identity"
        ) +
        ggplot2::geom_bar(
          data = dplyr::filter(pop_pyramid, .data$gender == "M"),
          stat = "identity"
        ) +
        ggplot2::scale_y_continuous(
          breaks = c(dist_pop)[c(1, 3, 5, 7, 9)],
          labels = c(abs(round(dist_pop, 5)[c(1, 3, 5, 7, 9)]))
        ) +
        ggplot2::scale_x_continuous(
          name = "Age",
          breaks = unique(pop_pyramid$age),
          labels = unique(pop_pyramid$age)
        ) +
        ggplot2::coord_flip()

      pop_pyramid$population <- c(female_total, male_total)
    } else {
      dist_pop <- stats::quantile(pop_pyramid$population)
      pop_pyramid_plot <- ggplot2::ggplot(
        pop_pyramid,
        ggplot2::aes(
          x = .data$age,
          y = .data$population
        )
      ) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::scale_y_continuous(
          breaks = c(dist_pop),
          labels = c(round(dist_pop, 5))
        ) +
        ggplot2::scale_x_continuous(
          name = "Age",
          breaks = pop_pyramid$age,
          labels = pop_pyramid$age
        ) +
        ggplot2::coord_flip()
    }

    if (total) {
      pop_pyramid_plot <- pop_pyramid_plot +
        ggplot2::ylab("Total population")
    } else {
      pop_pyramid_plot <- pop_pyramid_plot +
        ggplot2::ylab("Proportion of population")
    }

    print(pop_pyramid_plot)
  }

  return(pop_pyramid)
}

#' Returns the probability mass function of being infected given age and gender
#'
#' @description Function that returns the probability of being infected given
#' age and gender
#'
#' @param age A vector with the ages of cases in years from 0 to 100 years
#' @param gender A vector with the gender of cases 'F' and 'M'
#' @param population_pyramid A dataframe with the count of individuals
#' @param plot A boolean for displaying a plot
#'
#' @importFrom rlang .data
#'
#' @return A dataframe with the proportion or total count of individuals
#' @examples
#' \dontrun{
#' age_risk(c(15, 22), c("M", "F"), population_pyramid, plot = TRUE)
#' }
#' @export
age_risk <- function(age, gender = NULL, population_pyramid, plot = FALSE) {
  stopifnot("`age` must be an integer numeric vector with values
            between 0 and 100" = all(age %in% seq(0, 100)))
  if (!is.null(gender)) {
    stopifnot(
      "`gender` does not have the same number of elements as `age`" =
        (length(gender) == length(age)),
      "`population_pyramid` should include gender" =
        (length(population_pyramid) == 3)
    )
    age_female <- age[gender == "F"]
    pyramid_female <- dplyr::filter(population_pyramid, .data$gender == "F")
    hist_female <- graphics::hist(age_female,
      breaks = c(
        seq(0, pyramid_female$age[length(pyramid_female$age)],
          by = (pyramid_female$age[2] - pyramid_female$age[1])
        ),
        Inf
      ),
      plot = FALSE
    )

    age_risk_female <- data.frame(
      age = pyramid_female$age,
      prop = hist_female$counts / pyramid_female$population,
      gender = rep("F", length(pyramid_female$age)),
      stringsAsFactors = FALSE
    )

    age_male <- age[gender == "M"]
    pyramid_male <- dplyr::filter(population_pyramid, .data$gender == "M")
    hist_male <- graphics::hist(age_male,
      breaks = c(
        seq(0, pyramid_male$age[length(pyramid_male$age)],
          by = (pyramid_male$age[2] - pyramid_male$age[1])
        ),
        Inf
      ),
      plot = FALSE
    )

    age_risk_male <- data.frame(
      age = pyramid_male$age,
      prop = hist_male$counts / pyramid_male$population,
      gender = rep("M", length(pyramid_male$age)),
      stringsAsFactors = FALSE
    )

    age_risk <- rbind(age_risk_female, age_risk_male)
  } else {
    if (length(population_pyramid) == 3) {
      population_pyramid <- stats::aggregate(
        population ~ age,
        population_pyramid, sum
      )
    }
    hist_total <- graphics::hist(age,
      breaks = c(
        0,
        population_pyramid$age +
          (population_pyramid$age[2] -
            population_pyramid$age[1])
      ),
      plot = FALSE
    )

    age_risk <- data.frame(
      age = population_pyramid$age,
      prop = hist_total$counts / population_pyramid$population
    )
  }


  if (plot) {
    if (!is.null(gender)) {
      age_risk$prop <- c(-1 * age_risk_female$prop, age_risk_male$prop)
      dist_prop_f <- stats::quantile(age_risk_female$prop)[2:5]
      dist_prop_m <- stats::quantile(age_risk_male$prop)[2:5]
      dist_prop <- c(rev(-1 * dist_prop_f), 0, dist_prop_m)

      age_risk_plot <- ggplot2::ggplot(
        age_risk,
        ggplot2::aes(
          x = .data$age,
          y = .data$prop,
          fill = .data$gender
        )
      ) +
        ggplot2::geom_bar(
          data = dplyr::filter(age_risk, .data$gender == "F"),
          stat = "identity"
        ) +
        ggplot2::geom_bar(
          data = dplyr::filter(age_risk, .data$gender == "M"),
          stat = "identity"
        ) +
        ggplot2::scale_y_continuous(
          breaks = c(dist_prop)[c(1, 3, 5, 7, 9)],
          labels = c(round(abs(dist_prop)[c(1, 3, 5, 7, 9)], 5))
        ) +
        ggplot2::scale_x_continuous(
          name = "Age",
          breaks = unique(population_pyramid$age),
          labels = unique(population_pyramid$age)
        ) +
        ggplot2::coord_flip() +
        # nolint start
        ggplot2::ylab("Cases / Population")
      # nolint end
    } else {
      dist_prop <- stats::quantile(age_risk$prop)
      age_risk_plot <- ggplot2::ggplot(age_risk, ggplot2::aes(
        x = .data$age,
        y = .data$prop
      )) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::scale_y_continuous(
          breaks = c(dist_prop),
          labels = c(round(dist_prop, 5))
        ) +
        ggplot2::scale_x_continuous(
          name = "Age",
          breaks = unique(population_pyramid$age),
          labels = unique(population_pyramid$age)
        ) +
        ggplot2::coord_flip() +
        # nolint start
        ggplot2::ylab("Cases / Population")
      # nolint end
    }

    print(age_risk_plot)
  }


  return(age_risk)
}

#' Provides the sociological description of ethnicities in Colombia
#'
#' @description Function that returns the description of the consulted
#' ethnicities
#' @param ethnic_labels A numeric vector with the codes of ethnicities to
#' consult
#' @param language "ES" for description in Spanish "EN" for English
#'
#' @return A printed message with ethnicities descriptions
#' @examples
#' \dontrun{
#' describe_ethnicity(c(1, 2, 3, 4))
#' }
#' @export
describe_ethnicity <- function(ethnic_labels, language = "ES") {
  stopifnot(
    "`ethnic_labels` must be a numeric vector" =
      is.numeric(ethnic_labels),
    "The only languages allowed are ES and EN" = language %in% c("ES", "EN")
  )
  ethnic_labels <- as.data.frame(ethnic_labels)
  # nolint start
  #### ESPA<U+00D1>OL ####
  indigena_es <- paste(
    "Persona de ascendencia amerindia que comparten sentimientos de",
    "identificacion con su pasado aborigen, manteniendo rasgos y valores",
    "propios de su cultura tradicional, asi como formas de organizacion y",
    "control social propios"
  )

  rom_es <- paste(
    "Son comunidades que tienen una identidad etnica y cultural propia; se",
    "caracterizan por una tradicion nomada, y tienen su propio idioma que es",
    "el romanes"
  )

  raizal_es <- paste(
    "Poblacion ubicada en el Archipielago de San Andres, Providencia y Santa",
    "Catalina, con raices culturales afroanglo-antillanas, cuyos integrantes",
    "tienen rasgos socioculturales y linguisticos claramente diferenciados del",
    "resto de la poblacion afrocolombiana"
  )

  palenquero_es <- paste(
    "Poblacion ubicada en el municipio de San Basilio de Palenque,",
    "departamento de Bolivar, donde se habla el palenquero, lenguaje criollo"
  )

  afro_es <- paste(
    "Persona de ascendencia afrocolombiana que poseen una cultura propia, y",
    "tienen sus propias tradiciones y costumbre dentro de la relacion",
    "campo-poblado"
  )

  #### ENGLISH ####
  indigena_en <- paste(
    "A person of Amerindian descent who shares feelings of identification with",
    "their aboriginal past, maintaining traits and values of their traditional",
    "culture, as well as their own forms of organization and social control"
  )

  rom_en <- paste(
    "They are communities that have their own ethnic and cultural identity;",
    "They are characterized by a nomadic tradition, and have their own",
    "language, which is Romanesque"
  )

  raizal_en <- paste(
    "Population located in the Archipelago of San Andres, Providencia and",
    "Santa Catalina, with Afro-Anglo-Antillean cultural roots, whose members",
    "have clearly differentiated sociocultural and linguistic traits from the",
    "rest of the Afro-Colombian population"
  )

  palenquero_en <- paste(
    "Population located in the municipality of San Basilio de Palenque,",
    "department of Bolivar, where palenquero is spoken, a Creole language"
  )

  afro_en <- paste(
    "Person of Afro-Colombian descent who have their own culture, and have",
    "their own traditions and customs within the rural-populatedrelationship"
  )

  descriptions_es <- c(indigena_es, rom_es, raizal_es, palenquero_es, afro_es)
  descriptions_en <- c(indigena_en, rom_en, raizal_en, palenquero_en, afro_en)

  labels <- sort(unique(ethnic_labels$ethnic_labels))
  descrip_en <- descriptions_en[labels]
  descrip_es <- descriptions_es[labels]

  if (language == "EN") {
    return(data.frame(Label = labels, Description = descrip_es))
  } else {
    return(data.frame(Etiqueta = labels, Descripcion = descrip_es))
  }
}

#' Get ISCO-88 occupation labels from codes
#'
#' @description Function that translates a vector of ISCO-88 occupation codes
#' into a vector of labels
#' @param isco_codes A numeric vector of ISCO-88 occupation codes
#' (major, submajor, minor, or unit)
#' @param gender A vector with the isco_codes vector genders
#' @param plot A type of plot between treemap and circular  packing
#' @return A string vector of ISCO-88 labels
#' @examples
#' \dontrun{
#' describe_occupation(c(3221, 6111), c("F", "M"), plot = "treeemap")
#' }
#' @export
describe_occupation <- function(isco_codes, gender = NULL, plot = NULL) {
  path <- system.file("extdata", "isco88_table.rda", package = "epiCo")
  load(path)
  isco88_table <- isco88_table
  valid_unit_codes <- isco_codes[isco_codes %in% isco88_table[, 7]]
  valid_minor_codes <- isco_codes[isco_codes %in% isco88_table[, 5]]
  valid_sub_major_codes <- isco_codes[isco_codes %in% isco88_table[, 3]]
  valid_major_codes <- isco_codes[isco_codes %in% isco88_table[, 1]]
  invalid_codes <- isco_codes[!isco_codes %in% c(
    isco88_table[, 1], isco88_table[, 3],
    isco88_table[, 5], isco88_table[, 7]
  )]

  stopifnot(
    "`isco_codes` must be a numeric vector" = is.numeric(isco_codes),
    "`plot` must be circular or treemap" = plot %in% c(
      NULL,
      "circular",
      "treemap"
    ),
    "`isco_codes` must have at least one valid code" =
      (length(isco_codes) != length(invalid_codes))
  )

  if (length(invalid_codes) > 0) {
    msg <- paste(
      length(invalid_codes),
      "codes are invalid."
    )
    warning(msg)
  }

  if (!is.null(gender)) {
    stopifnot(
      "`gender` must have the same size as `isco_codes`" =
        (length(gender) == length(isco_codes))
    )
    occupation_data_unit <- data.frame(
      occupation = valid_unit_codes,
      gender = gender[isco_codes %in% valid_unit_codes]
    )
    occupation_data_unit <- occupation_data_unit %>%
      dplyr::count(.data$gender, .data$occupation)
    occupation_data_unit <- unique(merge(occupation_data_unit, isco88_table,
      by.x = "occupation", by.y = "unit"
    ))
    names(occupation_data_unit)[
      names(occupation_data_unit) == "occupation"
    ] <- "unit"
    names(occupation_data_unit)[names(occupation_data_unit) == "n"] <- "count"


    occupation_data_minor <- data.frame(
      occupation = valid_minor_codes,
      gender = gender[isco_codes %in% valid_minor_codes]
    )
    occupation_data_minor <- occupation_data_minor %>%
      dplyr::count(.data$gender, .data$occupation)
    occupation_data_minor <- unique(merge(occupation_data_minor,
      isco88_table[, seq(1, 6)],
      by.x = "occupation", by.y = "minor"
    ))
    names(occupation_data_minor)[
      names(occupation_data_minor) == "occupation"
    ] <- "minor"
    names(occupation_data_minor)[names(occupation_data_minor) == "n"] <- "count"

    occupation_data_sub_major <- data.frame(
      occupation = valid_sub_major_codes,
      gender = gender[isco_codes %in% valid_sub_major_codes]
    )
    occupation_data_sub_major <- occupation_data_sub_major %>%
      dplyr::count(.data$gender, .data$occupation)
    occupation_data_sub_major <- unique(merge(occupation_data_sub_major,
      isco88_table[, seq(1, 4)],
      by.x = "occupation",
      by.y = "sub_major"
    ))
    names(occupation_data_sub_major)[
      names(occupation_data_sub_major) == "occupation"
    ] <- "sub_major"
    names(occupation_data_sub_major)[
      names(occupation_data_sub_major) == "n"
    ] <- "count"

    occupation_data_major <- data.frame(
      occupation = valid_major_codes,
      gender = gender[isco_codes %in% valid_major_codes]
    )
    occupation_data_major <- occupation_data_major %>%
      dplyr::count(.data$gender, .data$occupation)
    occupation_data_major <- unique(merge(occupation_data_major,
      isco88_table[, c(1, 2)],
      by.x = "occupation", by.y = "major"
    ))
    names(occupation_data_major)[
      names(occupation_data_major) == "occupation"
    ] <- "major"
    names(occupation_data_major)[names(occupation_data_major) == "n"] <- "count"

    occupation_data <- data.frame(
      gender = NA, major = NA,
      major_label = NA, sub_major = NA,
      sub_major_label = NA, minor = NA,
      minor_label = NA, unit = NA,
      unit_label = NA, count = length(invalid_codes)
    )
    occupation_data <- merge(occupation_data,
      occupation_data_unit,
      all = T
    )
    occupation_data <- merge(occupation_data,
      occupation_data_minor,
      all = T
    )
    occupation_data <- merge(occupation_data,
      occupation_data_sub_major,
      all = T
    )
    occupation_data <- merge(occupation_data,
      occupation_data_major,
      all = T
    )
    occupation_data <- list(occupation_data[, c(
      "major", "major_label",
      "sub_major", "sub_major_label", "minor",
      "minor_label", "unit", "unit_label",
      "gender", "count"
    )])
    if (!is.null(plot)) {
      if (plot == "treemap") {
        occupation_data$occupation_plot <- occupation_plot(
          occupation_data,
          gender = TRUE
        )
        plot(occupation_data$occupation_plot)
      } else if (plot == "circular") {
        warning(
          "Remember that the circular plot does not distinguish by gender."
        )
        occupation_data$occupation_plot <- occupation_plot_circular(occupation_data)
        plot(occupation_data$occupation_plot)
      }
    }
  } else {
    occupation_data_unit <- data.frame(
      occupation = valid_unit_codes
    )
    occupation_data_unit <- occupation_data_unit %>%
      dplyr::count(.data$occupation)
    occupation_data_unit <- unique(merge(occupation_data_unit, isco88_table,
      by.x = "occupation", by.y = "unit"
    ))
    names(occupation_data_unit)[
      names(occupation_data_unit) == "occupation"
    ] <- "unit"
    names(occupation_data_unit)[names(occupation_data_unit) == "n"] <- "count"


    occupation_data_minor <- data.frame(
      occupation = valid_minor_codes
    )
    occupation_data_minor <- occupation_data_minor %>%
      dplyr::count(.data$occupation)
    occupation_data_minor <- unique(merge(occupation_data_minor,
      isco88_table[, seq(1, 6)],
      by.x = "occupation", by.y = "minor"
    ))
    names(occupation_data_minor)[
      names(occupation_data_minor) == "occupation"
    ] <- "minor"
    names(occupation_data_minor)[names(occupation_data_minor) == "n"] <- "count"

    occupation_data_sub_major <- data.frame(
      occupation = valid_sub_major_codes
    )
    occupation_data_sub_major <- occupation_data_sub_major %>%
      dplyr::count(.data$occupation)
    occupation_data_sub_major <- unique(merge(occupation_data_sub_major,
      isco88_table[, seq(1, 4)],
      by.x = "occupation",
      by.y = "sub_major"
    ))
    names(occupation_data_sub_major)[
      names(occupation_data_sub_major) == "occupation"
    ] <- "sub_major"
    names(occupation_data_sub_major)[
      names(occupation_data_sub_major) == "n"
    ] <- "count"

    occupation_data_major <- data.frame(
      occupation = valid_major_codes,
      gender = gender[isco_codes %in% valid_major_codes]
    )
    occupation_data_major <- occupation_data_major %>%
      dplyr::count(.data$occupation)
    occupation_data_major <- unique(merge(occupation_data_major,
      isco88_table[, c(1, 2)],
      by.x = "occupation", by.y = "major"
    ))
    names(occupation_data_major)[
      names(occupation_data_major) == "occupation"
    ] <- "major"
    names(occupation_data_major)[names(occupation_data_major) == "n"] <- "count"

    occupation_data <- data.frame(
      major = NA,
      major_label = NA, sub_major = NA,
      sub_major_label = NA, minor = NA,
      minor_label = NA, unit = NA,
      unit_label = NA, count = length(invalid_codes)
    )
    occupation_data <- merge(occupation_data,
      occupation_data_unit,
      all = T
    )
    occupation_data <- merge(occupation_data,
      occupation_data_minor,
      all = T
    )
    occupation_data <- merge(occupation_data,
      occupation_data_sub_major,
      all = T
    )
    occupation_data <- merge(occupation_data,
      occupation_data_major,
      all = T
    )
    occupation_data <- list(occupation_data[, c(
      "major", "major_label",
      "sub_major", "sub_major_label", "minor",
      "minor_label", "unit", "unit_label", "count"
    )])
    if (!is.null(plot)) {
      if (plot == "treemap") {
        occupation_data$occupation_plot <- occupation_plot(occupation_data)
        plot(occupation_data$occupation_plot)
      } else if (plot == "circular") {
        occupation_data$occupation_plot <- occupation_plot_circular(occupation_data)
        plot(occupation_data$occupation_plot)
      }
    }
  }
  return(occupation_data)
}

#' Distribution plots for ISCO-88 occupation labels
#'
#' @description Function that makes a treemap plot of a vector of ISCO-88
#' occupation codes
#' @param occupation_data A dataframe
#' @param gender A boolean for gender data
#' @param q A number that represents the quantile
#' @return A plot to summarize the distribution of ISCO-88 labels
#' @examples
#' \dontrun{
#' occupation_plot(occupation_data, gender = TRUE)
#' }
#' @export
occupation_plot <- function(occupation_data, gender = FALSE, q = 0.9) {
  occupation_data <- occupation_data[[1]]
  occupation_data_q <- subset(
    occupation_data,
    !is.na(occupation_data$unit_label)
  ) %>%
    subset(occupation_data$count >= stats::quantile(
      occupation_data$count,
      q
    ))

  label_count <- dplyr::count(occupation_data_q, .data$sub_major_label)

  label_count <- label_count[order(label_count$n, decreasing = TRUE), ]
  n_labels <- ifelse(nrow(label_count) < 12,
    nrow(label_count), 12
  )

  labels <- label_count[1:n_labels, ]

  sub_occupation_data <- subset(
    occupation_data_q,
    occupation_data_q$sub_major_label %in% labels$sub_major_label
  )

  if (gender) {
    occupation_treemap <- ggplot2::ggplot(sub_occupation_data, ggplot2::aes(
      area = .data$count,
      fill = .data$sub_major_label,
      label = .data$unit_label,
      subgroup = .data$gender
    )) +
      treemapify::geom_treemap() +
      ggplot2::scale_fill_manual(
        name = "Major Group",
        values = RColorBrewer::brewer.pal(n = 12, name = "Set3")
      ) +
      treemapify::geom_treemap_subgroup_border(colour = "white", size = 10) +
      treemapify::geom_treemap_subgroup_text(
        place = "centre", grow = TRUE,
        alpha = 0.1, colour = "black"
      ) +
      treemapify::geom_treemap_text(
        colour = "grey16", place = "centre",
        size = 15, fontface = "italic",
        grow = TRUE, reflow = TRUE
      ) +
      ggplot2::theme(legend.position = "bottom")
  } else {
    sub_occupation_data <- sub_occupation_data %>%
      dplyr::group_by(.data$sub_major_label, .data$unit_label) %>%
      dplyr::summarise(count = sum(.data$count))

    occupation_treemap <- ggplot2::ggplot(sub_occupation_data, ggplot2::aes(
      area = .data$count,
      fill = .data$sub_major_label,
      label = .data$unit_label
    )) +
      treemapify::geom_treemap() +
      ggplot2::scale_fill_manual(
        name = "Major Group",
        values = RColorBrewer::brewer.pal(n = 12, name = "Set3")
      ) +
      treemapify::geom_treemap_text(
        colour = "grey16", place = "centre",
        size = 20, fontface = "italic",
        grow = TRUE, reflow = TRUE
      ) +
      ggplot2::theme(legend.position = "bottom")
  }

  return(occupation_treemap)
}

#' Distribution plots for ISCO-88 occupation labels
#'
#' @description Function that makes a circular packing plot of a vector of
#' ISCO-88 occupation codes
#' @param occupation_data A dataframe
#' @param q A number that represents the quantile
#' @return A plot to summarize the distribution of ISCO-88 labels
#' @examples
#' \dontrun{
#' occupation_plot_circular(occupation_data)
#' }
#' @export
occupation_plot_circular <- function(occupation_data, q = 0.9) {
  occupation_data <- occupation_data[[1]]
  occupation_data_q <- subset(
    occupation_data,
    !is.na(occupation_data$unit_label)
  )
  occupation_data_q <- subset(
    occupation_data_q,
    occupation_data_q$count >= stats::quantile(
      occupation_data$count,
      q
    )
  )

  label_count <- dplyr::count(occupation_data_q, .data$sub_major_label)

  label_count <- label_count[order(label_count$n, decreasing = TRUE), ]
  n_labels <- ifelse(nrow(label_count) < 12,
    nrow(label_count), 12
  )

  labels <- label_count[1:n_labels, ]

  sub_occupation_data <- subset(
    occupation_data_q,
    occupation_data_q$sub_major_label %in% labels$sub_major_label
  )

  sub_occupation_data <- sub_occupation_data %>%
    dplyr::group_by(.data$sub_major_label, .data$unit_label) %>%
    dplyr::summarise(count = sum(.data$count))

  occupation_data_group <- sub_occupation_data %>%
    dplyr::group_by(.data$sub_major_label) %>%
    dplyr::summarise(count = sum(.data$count))

  circle_edges <- data.frame(
    from = as.character(sub_occupation_data$sub_major_label),
    to = as.character(sub_occupation_data$unit_label)
  )

  circle_vertices <- data.frame(
    id = c(
      unique(as.character(sub_occupation_data$sub_major_label)),
      as.character(sub_occupation_data$unit_label)
    ),
    size = c(
      occupation_data_group$count,
      sub_occupation_data$count
    ),
    sub_major = c(
      unique(as.character(sub_occupation_data$sub_major_label)),
      as.character(sub_occupation_data$sub_major_label)
    ),
    unit = c(
      rep(NA, length(unique(sub_occupation_data$sub_major_label))),
      as.character(sub_occupation_data$unit_label)
    )
  )

  mygraph <- igraph::graph_from_data_frame(circle_edges,
    vertices = circle_vertices
  )

  p <- ggraph::ggraph(mygraph,
    layout = "circlepack",
    weight = .data$size
  ) +
    ggraph::geom_node_circle(ggplot2::aes(fill = .data$sub_major)) +
    ggplot2::scale_fill_manual(
      name = "Major Group",
      values = RColorBrewer::brewer.pal(n = 12, name = "Set3"),
      labels = circle_vertices$sub_major
    ) +
    ggraph::geom_node_text(ggplot2::aes(label = .data$unit)) +
    ggplot2::theme_void()

  return(p)
}
