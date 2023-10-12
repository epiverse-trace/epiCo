#' Returns the population pyramid of the consulted region
#'
#' @description Function that returns the population pyramid of the municipality
#' or department of an specific year
#' @param divipola_code A numeric code accounting for the territory of interest
#' @param year A numeric input for year of interest
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
#'
population_pyramid <- function(divipola_code, year,
                               gender = TRUE, range = 5, total = TRUE,
                               plot = FALSE) {
  stopifnot(
    "`year` is only available from 2005 to 2026,
            please select a valid year" = (year >= 2005 & year <= 2026),
    "`divipola_code` must be numeric" = (is.numeric(divipola_code) &
      length(divipola_code) == 1),
    "`range` must be a numeric value between 1 and 100" = (is.numeric(range))
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

    female_counts <- as.numeric(pop_data_mun[104:204])
    male_counts <- as.numeric(pop_data_mun[3:103])
  } else {
    stop("There is no location assigned to the consulted DIVIPOLA code")
  }

  female_total <- vector(length = length(seq(1, length(female_counts) - range,
                                             range)))
  male_total <- vector(length = length(seq(1, length(female_counts) - range,
                                           range)))
  cont <- 1
  for (h in seq(1, length(female_counts) - range, range)) {
    female_total[cont] <- sum(female_counts[h:h + range])
    male_total[cont] <- sum(male_counts[h:h + range])
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
        rep("F", ceiling((length(female_counts) - range) / range)),
        rep("M", ceiling((length(male_counts) - range) / range))
      )
    )
  } else {
    pop_pyramid <- data.frame(
      age = seq(1, length(female_counts) - range, range),
      population = c(female_total + male_total)
    )
  }

  if (plot) {
    if (gender) {
      pop_pyramid$population <- c(-1 * female_total, male_total)

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
        ggplot2::coord_flip()

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
#' @param age A vector with the ages of cases in years
#' @param gender A vector with the gender of cases 'F' and 'M'
#' @param population_pyramid A dataframe with the count of individuals
#' @param plot A boolean for displaying a plot
#'
#' @importFrom rlang .data
#'
#' @return A dataframe with the proportion or total count of individuals
#' @export
#'
#'
age_risk <- function(age, gender = NULL, population_pyramid, plot = FALSE) {
  stopifnot("`age` must be a numeric vector" = is.numeric(age))
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
        0,
        pyramid_female$age +
          (pyramid_female$age[2] -
            pyramid_female$age[1])
      ),
      plot = FALSE
    )

    age_risk_female <- data.frame(
      age = pyramid_female$age,
      prob = hist_female$counts / pyramid_female$population,
      gender = rep("F", length(pyramid_female$age)),
      stringsAsFactors = FALSE
    )

    age_male <- age[gender == "M"]
    pyramid_male <- dplyr::filter(population_pyramid, .data$gender == "M")
    hist_male <- graphics::hist(age_male,
      breaks = c(
        0,
        pyramid_male$age +
          (pyramid_male$age[2] -
            pyramid_male$age[1])
      ),
      plot = FALSE
    )

    age_risk_male <- data.frame(
      age = pyramid_male$age,
      prob = hist_male$counts / pyramid_male$population,
      gender = rep("M", length(pyramid_male$age)),
      stringsAsFactors = FALSE
    )

    age_risk <- rbind(age_risk_female, age_risk_male) ######
  } else {
    if (length(population_pyramid) == 3){
      population_pyramid <- aggregate(population ~ age, population_pyramid, sum)
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
      prob = hist_total$counts / population_pyramid$population
    )
  }


  if (plot) {
    if (!is.null(gender)) {
      age_risk$prob <- c(-1 * age_risk_female$prob, age_risk_male$prob)

      age_risk_plot <- ggplot2::ggplot(
        age_risk,
        ggplot2::aes(
          x = .data$age,
          y = .data$prob,
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
        ggplot2::coord_flip()
    } else {
      age_risk_plot <- ggplot2::ggplot(age_risk, ggplot2::aes(
        x = .data$age,
        y = .data$prob
      )) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::coord_flip()
    }

    print(age_risk_plot)
  }


  return(age_risk)
}

#' Provides the sociological description of ethnicities in Colombia
#'
#' @description Function that returns the description of consulted ethnicities
#' @param ethnic_labels A numeric vector with the codes of ethnicities to
#' consult
#' @param language "ES" for description in spanish "EN" for english
#'
#' @return A printed message with the description of the ethnicities
#' @examples
#' \dontrun{
#' describe_ethnicity(c(1, 2, 3, 4))
#' }
#' @export
describe_ethnicity <- function(ethnic_labels, language = "ES") {
  stopifnot(
    "`ethnic_labels` must be a numeric vector" =
      is.numeric(ethnic_labels)
  )
  ethnic_labels <- as.data.frame(ethnic_labels)

  #### ESPAÑOL ####
  indigena_es <- "Persona de ascendencia amerindia que comparten sentimientos
  de identificacion con su pasado aborigen, manteniendo rasgos y valores
  propios de su cultura tradicional, asi como formas de organizacion
  y control social propios"

  rom_es <- "Son comunidades que tienen una identidad etnica y cultural propia;
  se caracterizan por una tradicion nomada, y tienen su propio idioma
  que es el romanes"

  raizal_es <- "Poblacion ubicada en el Archipielago de San Andres, Providencia
  y Santa Catalina, con raices culturales afroanglo-antillanas,
  cuyos integrantes tienen rasgos socioculturales y linguisticos
  claramente diferenciados del resto de la poblacion afrocolombiana"

  palenquero_es <- "Poblacion ubicada en el municipio de San Basilio de
  Palenque, departamento de Bolivar, donde se habla el palenquero,
  lenguaje criollo"

  afro_es <- "Persona de ascendencia afrocolombiana que poseen una cultura
  propia, y tienen sus propias tradiciones y costumbre dentro de la relacion
  campo-poblado"

  #### ENGLISH ####
  indigena_en <- "A person of Amerindian descent who shares feelings of
  identification with their aboriginal past, maintaining traits and values
  of their traditional culture, as well as their own forms of organization
  and social control"

  rom_en <- "They are communities that have their own ethnic and cultural
  identity; They are characterized by a nomadic tradition, and have their own
  language, which is Romanesque"

  raizal_en <- "Population located in the Archipelago of San Andres, Providencia
  and Santa Catalina, with Afro-Anglo-Antillean cultural roots, whose members
  have clearly differentiated sociocultural and linguistic traits
  from the rest of the Afro-Colombian population"

  palenquero_en <- "Population located in the municipality of San Basilio de
  Palenque, department of Bolivar, where palenquero is spoken,
  a Creole language"

  afro_en <- "Person of Afro-Colombian descent who have their own culture,
  and have their own traditions and customs within the
  rural-populated relationship"

  #####

  descriptions_es <- c(indigena_es, rom_es, raizal_es, palenquero_es, afro_es)
  description_en <- c(indigena_en, rom_en, raizal_en, palenquero_en, afro_en)

  labels <- order(unique(ethnic_labels$ethnic_labels))

  if (language == "EN") {
    return(description_en[labels])
  } else {
    return(descriptions_es[labels])
  }
}

# nolint start
#' Get ISCO-88 occupation labels from codes
#'
#' @description Function that translates a vector of ISCO-88 occupation codes
#' into a vector of labels
#' @param isco_codes A numeric vector of ISCO-88 occupation codes
#' (major, submajor, minor or unit)
#' @param output_level A string parameter that defines the level of the desired
#' label (major, submajor, minor or unit)
#'
#' @return A string vector of ISCO-88 labels
#' @examples
#' \dontrun{
#' describe_occupation(1111, level = 1)
#' }
#' @export
describe_occupation <- function(isco_codes, output_level) {
  stopifnot("`isco_codes` must be a numeric vector" = is.numeric(isco_codes))
  path <- system.file("extdata", "isco88_table.rda", package = "epiCo")
  load(path)
  isco88_table <- isco88_table
  input_level <- dplyr::case_when(
    isco_codes %in% c(0, 110) ~ "Armed Forces",
    nchar(isco_codes) == 1 ~ "major",
    nchar(isco_codes) == 2 ~ "sub_major",
    nchar(isco_codes) == 3 ~ "minor",
    nchar(isco_codes) == 4 ~ "unit",
    TRUE ~ NA_character_
  )
  tryCatch(
    {
      output_level_index <- as.numeric(sapply(output_level, switch,
        "major" = 1,
        "major_label" = 1,
        "sub_major" = 2,
        "sub_major_label" = 2,
        "minor" = 3,
        "minor_label" = 3,
        "unit" = 4,
        "unit_label" = 4,
        simplify = "array"
      ))
    },
    error = function(e) {
      stop(
        "Output level does not exist, please check your input",
        call. = FALSE
      )
    }
  )


  input_level_index <- sapply(input_level, switch,
    "major" = 1,
    "sub_major" = 2,
    "minor" = 3,
    "unit" = 4,
    "Armed Forces" = 0,
    simplify = "array"
  )

  isco88_labels <- as.list(input_level)
  for (i in seq(1, length(isco_codes))) {
    tryCatch(
      {
        isco_code <- isco_codes[i]
        if (isco_code == 0 | isco_code == 110) {
          isco88_labels[[i]] <- as.array(isco88_table[
            isco88_table$major == 0,
            output_level
          ])
        } else if (input_level_index[i] < output_level_index) {
          index_start <- match(isco_code, isco88_table[, input_level[i]])
          n_match <- sum(isco88_table[, input_level[i]] == isco_code)
          index_end <- index_start + n_match - 1
          isco88_labels[[i]] <- as.array(isco88_table[
            index_start:index_end,
            output_level
          ])
        } else {
          isco88_labels[i] <- isco88_table[which(isco88_table[input_level[i]]
          == isco_code)[1], output_level]
        }
      },
      error = function(e) {
        stop(
          "Code ", isco_code, " does not exist, please check your input",
          call. = FALSE
        )
      }
    )
  }
  return(isco88_labels)
}
# nolint end