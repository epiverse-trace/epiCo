#' Returns the population pyramid of the consulted region
#'
#' @description Function that returns the population pyramid of the municipality
#' or department of a specific year
#' @param divipola_code A code from the divipola table representing a department
#' or municipality. To obtain values at the national level, code '0' is used
#' @param year A numeric input for the year of interest
#' @param sex A boolean to consult data disaggregated by sex. The default value
#' is TRUE
#' @param range A numeric value from 1 to 100 for the age range to use. The
#' default value is 5
#' @param total A boolean for returning the total number rather than the
#' proportion of the country's population. The default value is TRUE
#' @param plot A boolean for displaying a plot. The default value is TRUE
#' @param language Language for plot components
#' @importFrom rlang .data
#' @return A dataframe with the proportion or total count of individuals
#' @examples
#' population_pyramid("15001", 2015, sex = TRUE, total = TRUE, plot = TRUE)
#' @export
population_pyramid <- function(divipola_code, year, sex = TRUE, range = 5,
                               total = TRUE, plot = FALSE, language = c(
                                 "EN",
                                 "ES"
                               )) {
  stopifnot(
    "`year` must be an unique value" = (length(year) == 1),
    "`divipola_code` must be a character of a positive integer" = (
      is.character(divipola_code) & (as.numeric(divipola_code) %% 1 == 0) &
        length(divipola_code) == 1 & as.numeric(divipola_code) >= 0),
    "`range` must be an integer value between 1 and 100" = is.numeric(range) &
      range %in% seq(1, 100)
  )
  language <- match.arg(language)
  path <- system.file("extdata", "divipola_table.rda", package = "epiCo")
  load(path)
  divipola_table <- divipola_table

  if (divipola_code == 0) {
    path_0 <- system.file("extdata", "population_projection_col_0.rds",
      package = "epiCo"
    )
    load(path_0)
    population_projection_col_0 <- population_projection_col_0
    stopifnot(
      "`year` must be a unique valid year between the range of the population
      projection file; please select a unique valid year. See demographics
      vignette" = (year %in% unique(population_projection_col_0$ano))
    )
    pop_data_dpto <- dplyr::filter(
      population_projection_col_0,
      ((population_projection_col_0$dp == divipola_code) &
        (population_projection_col_0$ano == year))
    )

    female_counts <- as.numeric(pop_data_dpto[104:204])
    male_counts <- as.numeric(pop_data_dpto[3:103])

    name <- "Colombia"
  } else if (divipola_code %in% divipola_table$COD_DPTO) {
    path_1 <- system.file("extdata", "population_projection_col_1.rds",
      package = "epiCo"
    )
    load(path_1)
    population_projection_col_1 <- population_projection_col_1
    stopifnot(
      "`year` must be a unique valid year between the range of the population
      projection file; please select a unique valid year. See demographics
      vignette" = (year %in% unique(population_projection_col_1$ano))
    )
    pop_data_dpto <- dplyr::filter(
      population_projection_col_1,
      ((.data$dp == divipola_code) & (.data$ano == year))
    )

    female_counts <- as.numeric(pop_data_dpto[104:204])
    male_counts <- as.numeric(pop_data_dpto[3:103])

    name <- dplyr::filter(
      divipola_table,
      .data$COD_DPTO == divipola_code
    ) %>%
      dplyr::slice(1) %>%
      dplyr::pull(.data$NOM_DPTO)
    name <- tolower(substr(name, 1, 1)) %>%
      toupper() %>%
      paste0(tolower(substr(name, 2, nchar(name))))
  } else if (divipola_code %in% divipola_table$COD_MPIO) {
    path_2 <- system.file("extdata", "population_projection_col_2.rds",
      package = "epiCo"
    )
    load(path_2)
    population_projection_col_2 <- population_projection_col_2
    stopifnot(
      "`year` must be a unique valid year between the range of the population
      projection file; please select a unique valid year. See demographics
      vignette" = (year %in% unique(population_projection_col_2$ano))
    )
    pop_data_mun <- dplyr::filter(
      population_projection_col_2,
      ((.data$dpmp == divipola_code) & (.data$ano == year))
    )

    female_counts <- as.numeric(pop_data_mun[89:174])
    male_counts <- as.numeric(pop_data_mun[3:88])

    name <- dplyr::filter(
      divipola_table,
      .data$COD_MPIO == divipola_code
    ) %>%
      dplyr::pull(.data$NOM_MPIO)
    name <- tolower(substr(name, 1, 1)) %>%
      toupper() %>%
      paste0(tolower(substr(name, 2, nchar(name))))
  } else {
    stop("There is no location assigned to the consulted DIVIPOLA code")
  }

  female_total <- as.vector(
    table(
      findInterval(
        rep(seq_along(female_counts) - 1, female_counts),
        seq(0, length(female_counts), range)
      )
    )
  )
  male_total <- as.vector(
    table(
      findInterval(
        rep(seq_along(male_counts) - 1, male_counts),
        seq(0, length(male_counts), range)
      )
    )
  )


  if (!total) {
    female_total <- female_total / sum(female_total)
    male_total <- male_total / sum(male_total)
  }

  if (sex) {
    pop_pyramid <- data.frame(
      age = rep(seq(0, length(female_counts), range), 2),
      population = c(female_total, male_total),
      sex = c(
        rep("F", length(female_total)),
        rep("M", length(male_total))
      )
    )
  } else {
    pop_pyramid <- data.frame(
      age = seq(0, length(female_counts), range),
      population = c(female_total + male_total)
    )
  }

  if (plot) {
    pop_pyramid_plot <- population_pyramid_plot(pop_pyramid,
      language = language, sex = sex
    )
    if (total) {
      if (language == "EN") {
        pop_pyramid_plot <- pop_pyramid_plot +
          ggplot2::ylab("Total population") +
          ggplot2::ggtitle(paste(name, "population pyramid"))
      } else {
        pop_pyramid_plot <- pop_pyramid_plot +
          ggplot2::ylab("Poblacion total") +
          ggplot2::ggtitle(paste("Piramide poblacional", name))
      }
    } else {
      if (language == "EN") {
        pop_pyramid_plot <- pop_pyramid_plot +
          ggplot2::ylab("Proportion of population") +
          ggplot2::ggtitle(paste(name, "population pyramid"))
      } else {
        pop_pyramid_plot <- pop_pyramid_plot +
          ggplot2::ylab("Proporcion de la poblacion") +
          ggplot2::ggtitle(paste("Piramide poblacional", name))
      }
    }
    print(pop_pyramid_plot)
    return(list(data = pop_pyramid, plot = pop_pyramid_plot))
  } else {
    return(pop_pyramid)
  }
}


#' Returns the population pyramid plot
#'
#' @description Function that returns the population pyramid plot of the
#' municipality or department of a specific year
#'
#' @param pop_pyramid A dataframe with the age counts
#' @param sex A boolean to consult data disaggregated by sex. The default value
#' is TRUE
#' @param language Language for plot components
#'
#' @return the population pyramid plot
#'
#' @keywords internal
population_pyramid_plot <- function(pop_pyramid, language, sex = TRUE) {
  if (sex) {
    female_total <- dplyr::filter(pop_pyramid, .data$sex == "F")$population
    male_total <- dplyr::filter(pop_pyramid, .data$sex == "M")$population
    pop_pyramid$population <- c(-1 * female_total, male_total)

    pop_pyramid_plot <- ggplot2::ggplot(
      pop_pyramid,
      ggplot2::aes(
        x = .data$age,
        y = .data$population,
        fill = .data$sex
      )
    ) +
      ggplot2::geom_col(
        data = dplyr::filter(pop_pyramid, .data$sex == "F")
      ) +
      ggplot2::geom_col(
        data = dplyr::filter(pop_pyramid, .data$sex == "M")
      ) +
      ggplot2::scale_y_continuous(
        breaks = scales::breaks_extended(n = 8),
        labels = function(x) {
          as.character(abs(as.numeric(x)))
        }
      ) +
      ggplot2::coord_flip()
    if (language == "EN") {
      pop_pyramid_plot <- pop_pyramid_plot +
        ggplot2::scale_x_continuous(
          name = "Age",
          breaks = unique(pop_pyramid$age),
          labels = unique(pop_pyramid$age)
        )
    } else {
      pop_pyramid_plot <- pop_pyramid_plot +
        ggplot2::scale_x_continuous(
          name = "Edad",
          breaks = unique(pop_pyramid$age),
          labels = unique(pop_pyramid$age)
        )
    }

    pop_pyramid$population <- c(female_total, male_total)
  } else {
    pop_pyramid_plot <- ggplot2::ggplot(
      pop_pyramid,
      ggplot2::aes(
        x = .data$age,
        y = .data$population
      )
    ) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_y_continuous(
        breaks = scales::breaks_extended(n = 8),
        labels = function(x) {
          as.character(abs(as.numeric(x)))
        }
      ) +
      ggplot2::scale_x_continuous(
        name = "Age",
        breaks = pop_pyramid$age,
        labels = pop_pyramid$age
      ) +
      ggplot2::coord_flip()
  }
  return(pop_pyramid_plot)
}

#' Returns the specific rates associated with being infected given age and sex
#'
#' @description Function that returns the specific rates of being infected given
#' age and sex
#' @param age A vector with the ages of cases in years from 0 to 100 years
#' @param sex A vector with the sex of cases 'F' and 'M'. The default value
#' is NULL
#' @param population_pyramid A dataframe with the count of individuals with the
#' columns age, population and sex
#' @param plot A boolean for displaying a plot. The default value is FALSE
#' @param language Language for plot components
#' @importFrom rlang .data
#' @return A dataframe with the proportion or total count of individuals
#' @examples
#' pop_pyramid <- population_pyramid("15001", 2015,
#'   sex = TRUE, total = TRUE,
#'   plot = FALSE
#' )
#' ages <- round(runif(150, 0, 100))
#' sex <- c(rep("M", 70), rep("F", 80))
#' age_risk(
#'   age = ages, sex = sex, population_pyramid = pop_pyramid,
#'   plot = TRUE
#' )
#' @export
age_risk <- function(age, population_pyramid, sex = NULL, plot = FALSE,
                     language = c("EN", "ES")) {
  stopifnot("`age` must be an integer numeric vector with values
            between 0 and 100" = all(age %in% seq(0, 100)))
  language <- match.arg(language)
  if (inherits(population_pyramid, what = "list")) {
    population_pyramid <- population_pyramid$data
  }
  if (!is.null(sex)) {
    stopifnot(
      "`sex` does not have the same number of elements as `age`" =
        (length(sex) == length(age)),
      "`population_pyramid` should include sex" =
        ("sex" %in% colnames(population_pyramid))
    )

    age_risk_female <- get_age_risk_sex(age, sex, population_pyramid,
      sex = "F"
    )
    age_risk_male <- get_age_risk_sex(age, sex, population_pyramid,
      sex = "M"
    )
    age_risk <- rbind(age_risk_female, age_risk_male)
  } else {
    if (length(population_pyramid) == 3) {
      population_pyramid <- stats::aggregate(
        population ~ age,
        population_pyramid, sum
      )
    }
    hist_total <- graphics::hist(
      age,
      breaks = c(
        seq(0, population_pyramid$age[length(population_pyramid$age)],
          by = (population_pyramid$age[2] - population_pyramid$age[1])
        ),
        Inf
      ),
      plot = FALSE
    )

    age_risk <- data.frame(
      age = population_pyramid$age,
      population = hist_total$counts / population_pyramid$population
    )
  }

  if (plot) {
    if (!is.null(sex)) {
      age_risk_plot <- population_pyramid_plot(age_risk,
        language = language,
        sex = TRUE
      )
      if (language == "EN") {
        # nolint start
        age_risk_plot <- age_risk_plot +
          ggplot2::ylab("Cases / Population") +
          ggplot2::ggtitle("Specific rate by age group")
        # nolint end
      } else {
        # nolint start
        age_risk_plot <- age_risk_plot +
          ggplot2::ylab("Casos / Poblacion") +
          ggplot2::ggtitle("Tasa especifica por grupo de edad")
        # nolint end
      }
    } else {
      age_risk_plot <- population_pyramid_plot(age_risk,
        language = language,
        sex = FALSE
      )
      if (language == "EN") {
        age_risk_plot <- age_risk_plot +
          # nolint start
          ggplot2::ylab("Cases / Population")
        # nolint end
      } else {
        age_risk_plot <- age_risk_plot +
          # nolint start
          ggplot2::ylab("Casos / Poblacion")
        # nolint end
      }
    }
    print(age_risk_plot)
    return(list(data = age_risk, plot = age_risk_plot))
  } else {
    return(age_risk)
  }
}

#' Auxiliary function to calculate the proportion by age according to the total
#' population and sex
#' @param age A vector with the ages of cases in years from 0 to 100 years
#' @param sex_vector A vector with the sex of cases 'F' and 'M'
#' @param pyramid A dataframe with the count of individuals
#' @param sex A string specifying the sex being calculated
#' @return A dataframe with the proportion by age according to the total
#' population and sex
#' @keywords internal
get_age_risk_sex <- function(age, sex_vector, pyramid, sex) {
  age_sex <- age[sex_vector == sex]
  pyramid_sex <- dplyr::filter(pyramid, .data$sex == !!sex)
  hist_sex <- graphics::hist(age_sex,
    breaks = c(
      seq(0, pyramid_sex$age[length(pyramid_sex$age)],
        by = (pyramid_sex$age[2] - pyramid_sex$age[1])
      ),
      Inf
    ),
    plot = FALSE
  )
  age_risk_sex <- data.frame(
    age = pyramid_sex$age,
    population = hist_sex$counts / pyramid_sex$population,
    sex = rep(sex, length(pyramid_sex$age)),
    stringsAsFactors = FALSE
  )
  return(age_risk_sex)
}

#' Provides the sociological description of ethnicities in Colombia
#'
#' @description Function that returns the description of the consulted
#' ethnicities
#' @param ethnic_codes A numeric vector with the codes of ethnicities to
#' consult
#' @param language "ES" for description in Spanish "EN" for English. The default
#' value is EN
#' @return A printed message with ethnicities descriptions
#' @examples
#' describe_ethnicity(round(runif(n = 150, min = 1, max = 4)))
#' @export
describe_ethnicity <- function(ethnic_codes, language = c("EN", "ES")) {
  stopifnot(
    "`ethnic_codes` must be a numeric vector" =
      is.numeric(ethnic_codes)
  )
  language <- match.arg(language)
  ethnic_codes <- as.data.frame(ethnic_codes)
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

  codes <- sort(unique(ethnic_codes$ethnic_codes))
  descrip_en <- descriptions_en[codes]
  descrip_es <- descriptions_es[codes]

  if (language == "EN") {
    return(data.frame(code = codes, description = descrip_en))
  } else {
    return(data.frame(codigo = codes, descripcion = descrip_es))
  }
}

#' Get ISCO-88 occupation labels from codes
#'
#' @description Function that translates a vector of ISCO-88 occupation codes
#' into a vector of labels
#' @param isco_codes A numeric vector of ISCO-88 occupation codes
#' (major, submajor, minor, or unit)
#' @param sex A vector with the respective sex for isco_codes vector. The
#' default value is NULL
#' @param plot A type of plot between treemap and circular  packing. The default
#' value is NULL
#' @return A string vector of ISCO-88 labels
#' @examples
#' demog_data <- data.frame(
#'   occupation_label =
#'     c(6111, 3221, 5113, 5133, 6111, 23, 25),
#'   sex = c("F", "M", "F", "F", "M", "M", "F")
#' )
#' describe_occupation(
#'   isco_codes = demog_data$occupation_label,
#'   sex = demog_data$sex, plot = "treemap"
#' )
#' @export
describe_occupation <- function(isco_codes, sex = NULL, plot = NULL) {
  path <- system.file("extdata", "isco88_table.rda", package = "epiCo")
  load(path)
  isco88_table <- isco88_table
  invalid_codes <- isco_codes[!isco_codes %in% c(
    isco88_table$major, isco88_table$sub_major,
    isco88_table$minor, isco88_table$unit
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
    message(
      length(invalid_codes),
      " codes are invalid."
    )
  }

  valid_unit_codes <- isco_codes[isco_codes %in% isco88_table$unit]
  valid_minor_codes <- isco_codes[isco_codes %in% isco88_table$minor]
  valid_sub_major_codes <- isco_codes[isco_codes %in% isco88_table$sub_major]

  if (!is.null(sex)) {
    stopifnot(
      "`sex` must have the same size as `isco_codes`" =
        (length(sex) == length(isco_codes))
    )
    occupation_data_unit <- get_occupation_data(valid_unit_codes,
      isco_codes,
      isco88_table,
      name_occupation = "unit",
      sex = sex
    )

    occupation_data_minor <- get_occupation_data(valid_minor_codes,
      isco_codes,
      isco88_table[, seq(1, 6)],
      name_occupation = "minor",
      sex = sex
    )

    occupation_data_sub_major <- get_occupation_data(valid_sub_major_codes,
      isco_codes,
      isco88_table[, seq(1, 4)],
      name_occupation = "sub_major",
      sex = sex
    )

    occupation_data_major <- get_occupation_data(valid_sub_major_codes,
      isco_codes,
      isco88_table[, seq(1, 2)],
      name_occupation = "major",
      sex = sex
    )

    occupation_data <- data.frame(
      sex = NA, major = NA,
      major_label = NA, sub_major = NA,
      sub_major_label = NA, minor = NA,
      minor_label = NA, unit = NA,
      unit_label = NA, count = length(invalid_codes)
    )
    occupation_data <- merge(occupation_data,
      occupation_data_unit,
      all = TRUE
    )
    occupation_data <- merge(occupation_data,
      occupation_data_minor,
      all = TRUE
    )
    occupation_data <- merge(occupation_data,
      occupation_data_sub_major,
      all = TRUE
    )
    occupation_data <- merge(occupation_data,
      occupation_data_major,
      all = TRUE
    )
    occupation_data <- occupation_data[, c(
      "major", "major_label",
      "sub_major", "sub_major_label", "minor",
      "minor_label", "unit", "unit_label",
      "sex", "count"
    )]
    if (!is.null(plot)) {
      if (plot == "treemap") {
        occupation_plot <- occupation_plot(
          occupation_data,
          sex = TRUE
        )
        plot(occupation_plot)
      } else if (plot == "circular") {
        message(
          "Remember that the circular plot does not distinguish by sex."
        )
        occupation_plot <- occupation_plot_circular(
          occupation_data
        )
        plot(occupation_plot)
      }
      return(list(data = occupation_data, plot = occupation_plot))
    } else {
      return(occupation_data)
    }
  } else {
    occupation_data_unit <- get_occupation_data(valid_unit_codes,
      isco_codes,
      isco88_table,
      name_occupation = "unit",
      sex = sex
    )

    occupation_data_minor <- get_occupation_data(valid_minor_codes,
      isco_codes,
      isco88_table[, seq(1, 6)],
      name_occupation = "minor",
      sex = sex
    )

    occupation_data_sub_major <- get_occupation_data(valid_sub_major_codes,
      isco_codes,
      isco88_table[, seq(1, 4)],
      name_occupation = "sub_major",
      sex = sex
    )

    occupation_data_major <- get_occupation_data(valid_sub_major_codes,
      isco_codes,
      isco88_table[, seq(1, 2)],
      name_occupation = "major",
      sex = sex
    )

    occupation_data <- data.frame(
      major = NA,
      major_label = NA, sub_major = NA,
      sub_major_label = NA, minor = NA,
      minor_label = NA, unit = NA,
      unit_label = NA, count = length(invalid_codes)
    )
    occupation_data <- merge(occupation_data,
      occupation_data_unit,
      all = TRUE
    )
    occupation_data <- merge(occupation_data,
      occupation_data_minor,
      all = TRUE
    )
    occupation_data <- merge(occupation_data,
      occupation_data_sub_major,
      all = TRUE
    )
    occupation_data <- merge(occupation_data,
      occupation_data_major,
      all = TRUE
    )
    occupation_data <- occupation_data[, c(
      "major", "major_label",
      "sub_major", "sub_major_label", "minor",
      "minor_label", "unit", "unit_label", "count"
    )]
    if (!is.null(plot)) {
      if (plot == "treemap") {
        occupation_plot <- occupation_plot(occupation_data)
        print(occupation_plot)
      } else if (plot == "circular") {
        occupation_plot <- occupation_plot_circular(
          occupation_data
        )
        print(occupation_plot)
      }
      return(list(data = occupation_data, plot = occupation_plot))
    } else {
      return(occupation_data)
    }
  }
}

#' Auxiliary function to obtain the information of the occupations
#' @param valid_codes A numeric vector with the valid codes from the ISCO-88
#' table
#' @param isco_codes A numeric vector of ISCO-88 occupation codes
#' (major, submajor, minor, or unit)
#' @param sex A vector with the respective sex for isco_codes vector. The
#' default value is NULL
#' @param isco88_table The ISCO-88 table columns of the information for that
#' group of occupations
#' @param name_occupation The category of occupations to be consulted. These can
#' be: major, submajor, minor, or unit
#' @return A dataframe with the information of the occupations
#' @keywords internal
get_occupation_data <- function(valid_codes, isco_codes, isco88_table,
                                name_occupation, sex = NULL) {
  if (!is.null(sex)) {
    occupation_data <- data.frame(
      occupation = valid_codes,
      sex = sex[isco_codes %in% valid_codes]
    )
    occupation_data <- occupation_data %>%
      dplyr::count(.data$sex, .data$occupation)
    occupation_data <- unique(merge(occupation_data, isco88_table,
      by.x = "occupation", by.y = name_occupation
    ))
  } else {
    occupation_data <- data.frame(
      occupation = valid_codes
    )
    occupation_data <- occupation_data %>%
      dplyr::count(.data$occupation)
    occupation_data <- unique(merge(occupation_data, isco88_table,
      by.x = "occupation", by.y = name_occupation
    ))
  }
  names(occupation_data)[
    names(occupation_data) == "occupation"
  ] <- name_occupation
  names(occupation_data)[names(occupation_data) == "n"] <- "count"
  return(occupation_data)
}

#' Distribution plots for ISCO-88 occupation labels
#'
#' @description Function that makes a treemap plot of a vector of ISCO-88
#' occupation codes
#' @param occupation_data A dataframe
#' @param sex A boolean for sex data. The default value is FALSE
#' @param q A number that represents the quantile. The default value is 0.9
#' @return A plot to summarize the distribution of ISCO-88 labels
#' @keywords internal
occupation_plot <- function(occupation_data, sex = FALSE, q = 0.9) {
  occupation_data <- stats::na.omit(occupation_data)
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
  n_labels <- pmin(nrow(label_count), 12)

  labels <- label_count[1:n_labels, ]

  sub_occupation_data <- subset(
    occupation_data_q,
    occupation_data_q$sub_major_label %in% labels$sub_major_label
  )

  if (sex) {
    occupation_treemap <- ggplot2::ggplot(sub_occupation_data, ggplot2::aes(
      area = .data$count,
      fill = .data$sub_major_label,
      label = .data$unit_label,
      subgroup = .data$sex
    )) +
      treemapify::geom_treemap_subgroup_border(colour = "white", size = 10) +
      treemapify::geom_treemap_subgroup_text(
        place = "centre", grow = TRUE,
        alpha = 0.1, colour = "black"
      )
  } else {
    sub_occupation_data <- sub_occupation_data %>%
      dplyr::group_by(.data$sub_major_label, .data$unit_label) %>%
      dplyr::summarise(count = sum(.data$count))

    occupation_treemap <- ggplot2::ggplot(sub_occupation_data, ggplot2::aes(
      area = .data$count,
      fill = .data$sub_major_label,
      label = .data$unit_label
    ))
  }
  occupation_treemap <- occupation_treemap +
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
  return(occupation_treemap)
}

#' Distribution plots for ISCO-88 occupation labels
#'
#' @description Function that makes a circular packing plot of a vector of
#' ISCO-88 occupation codes
#' @param occupation_data A dataframe
#' @param q A number that represents the quantile. The default value is 0.9
#' @return A plot to summarize the distribution of ISCO-88 labels
#' @keywords internal
occupation_plot_circular <- function(occupation_data, q = 0.9) {
  occupation_data <- stats::na.omit(occupation_data)
  occupation_data_q <- subset(
    occupation_data,
    !is.na(occupation_data$unit_label)
  )
  occupation_data_q <- subset(
    occupation_data_q,
    occupation_data_q$count >= stats::quantile(
      occupation_data_q$count,
      q
    )
  )

  label_count <- dplyr::count(occupation_data_q, .data$sub_major_label)

  label_count <- label_count[order(label_count$n, decreasing = TRUE), ]
  n_labels <- pmin(nrow(label_count), 12)

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
    to = as.character(sub_occupation_data$unit_label),
    stringsAsFactors = FALSE
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
