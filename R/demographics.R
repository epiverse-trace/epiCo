#' Returns the population pyramid of the consulted region
#'
#' @description Function that returns the population pyramid of the municipality
#' or department of an specific year
#' @param divipola_code A numeric code accounting for the territory of interest
#' @param year A numeric input for year of interest
#' @param gender A boolean to consult data disaggregated by gender
#' @param total A boolean for returning the total number rather than the
#' proportion of the populations
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
                               gender = TRUE, total = TRUE, plot = FALSE) {
  path <- system.file("data", "divipola_table.rda", package = "epiCo")
  load(path)
  divipola_table <- divipola_table
  
  if (divipola_code == 0) {
    path_0 <- system.file("data", "population_projection_col_0.rda",
      package = "epiCo"
    )
    load(path_0)
    population_projection_col_0 <- population_projection_col_0
    pop_data_dpto <- subset(
      population_projection_col_0,
      population_projection_col_0$DP == divipola_code &
        population_projection_col_0$ANO == year
    )

    female_total <- as.numeric(pop_data_dpto[104:204])
    male_total <- as.numeric(pop_data_dpto[3:103])
  } else if (divipola_code %in% divipola_table$COD_DPTO) {
    path_1 <- system.file("data", "population_projection_col_1.rda",
      package = "epiCo"
    )
    load(path_1)
    population_projection_col_1 <- population_projection_col_1
    pop_data_dpto <- subset(
      population_projection_col_1,
      .data$DP == divipola_code & .data$ANO == year
    )

    female_total <- as.numeric(pop_data_dpto[104:204])
    male_total <- as.numeric(pop_data_dpto[3:103])
  } else if (divipola_code %in% divipola_table$COD_MPIO) {
    path_2 <- system.file("data", "population_projection_col_2.rda",
      package = "epiCo"
    )
    load(path_2)
    population_projection_col_2 <- population_projection_col_2
    pop_data_mun <- subset(
      population_projection_col_2,
      .data$DPMP == divipola_code & .data$ANO == year
    )

    female_total <- as.numeric(pop_data_mun[104:204])
    male_total <- as.numeric(pop_data_mun[3:103])
  } else {
    warning("There is no location assigned to the consulted DIVIPOLA code")
    return(NA)
  }

  if (total == FALSE) {
    female_total <- female_total / sum(female_total)
    male_total <- male_total / sum(male_total)
  }

  if (gender == TRUE) {
    pop_pyramid <- data.frame(
      Age = rep(c(0:100), 2),
      Population = c(female_total, male_total),
      Gender = c(rep("F", 101), rep("M", 101))
    )
  } else {
    pop_pyramid <- data.frame(
      Age = c(0:100),
      Population = c(female_total + male_total)
    )
  }

  if (plot == TRUE) {
    if (gender == TRUE) {
      pop_pyramid$Population <- c(-1 * female_total, male_total)

      pop_pyramid_plot <- ggplot2::ggplot(
        pop_pyramid,
        ggplot2::aes(
          x = .data$Age,
          y = .data$Population,
          fill = .data$Gender
        )
      ) +
        ggplot2::geom_bar(
          data = subset(pop_pyramid, .data$Gender == "F"),
          stat = "identity"
        ) +
        ggplot2::geom_bar(
          data = subset(pop_pyramid, .data$Gender == "M"),
          stat = "identity"
        ) +
        ggplot2::coord_flip()

      pop_pyramid$Population <- c(female_total, male_total)
    } else {
      pop_pyramid_plot <- ggplot2::ggplot(
        pop_pyramid,
        ggplot2::aes(
          x = .data$Age,
          y = .data$Population
        )
      ) +
        ggplot2::geom_bar(stat = "identity")
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
age_risk <- function(age, gender = NULL, population_pyramid, plot = FALSE) {
  if (!is.null(gender)) {
    ages_f <- age[gender == "F"]
    pyramid_f <- subset(population_pyramid, .data$Gender == "F")
    hist_f <- graphics::hist(ages_f,
      breaks = c(0:101),
      right = FALSE, plot = FALSE
    )

    age_risk_f <- data.frame(
      Age = pyramid_f$Age,
      Prob = hist_f$counts / pyramid_f$Population,
      Gender = rep("F", 101)
    )

    ages_m <- age[gender == "M"]
    pyramid_m <- subset(population_pyramid, .data$Gender == "M")
    hist_m <- graphics::hist(ages_m,
      breaks = c(0:101),
      right = FALSE, plot = FALSE
    )

    age_risk_m <- data.frame(
      Age = pyramid_m$Age,
      Prob = hist_m$counts / pyramid_m$Population,
      Gender = rep("M", 101)
    )

    age_risk <- rbind(age_risk_f, age_risk_m)
  } else {
    hist_t <- graphics::hist(age,
      breaks = c(0:101), right = FALSE,
      plot = FALSE
    )

    age_risk <- data.frame(
      Age = population_pyramid$Age,
      Prob = hist_t$counts / population_pyramid$Population
    )
  }


  if (plot == TRUE) {
    if (!is.null(gender)) {
      age_risk$Prob <- c(-1 * age_risk_f$Prob, age_risk_m$Prob)

      age_risk_plot <- ggplot2::ggplot(
        age_risk,
        ggplot2::aes(
          x = .data$Age,
          y = .data$Prob,
          fill = .data$Gender
        )
      ) +
        ggplot2::geom_bar(
          data = subset(age_risk, .data$Gender == "F"),
          stat = "identity"
        ) +
        ggplot2::geom_bar(
          data = subset(age_risk, .data$Gender == "M"),
          stat = "identity"
        ) +
        ggplot2::coord_flip()

      age_risk$Prob <- c(age_risk_f$Prob, age_risk_m$Prob)
    } else {
      age_risk_plot <- ggplot2::ggplot(age_risk, ggplot2::aes(
        x = .data$Age,
        y = .data$Prob
      )) +
        ggplot2::geom_bar(stat = "identity")
    }

    print(age_risk_plot)
  }


  return(age_risk)
}

#' Provides the sociological description of ethnicities in Colombia
#'
#' @description Function that returns the description of consulted ethnicities
#' @param ethniclabels A numeric vector with the codes of ethnicities to consult
#' @param language "ES" for description in spanish "EN" for english
#' @param plot A boolean for displaying an histogram plot
#'
#' @return A printed message with the description of the ethnicities
#' @examples
#' \dontrun{
#' describe_ethnicity(c(1, 2, 3, 4))
#' }
#' @export
describe_ethnicity <- function(ethniclabels, language = "ES", plot = FALSE) {
  ethniclabels <- as.data.frame(ethniclabels)

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

  if (plot) {
    ethn_hist <- ggplot2::ggplot(ethniclabels, ggplot2::aes(ethniclabels)) +
      ggplot2::geom_histogram() +
      ggplot2::theme_minimal()

    print(ethn_hist)
  }

  labels <- order(unique(ethniclabels$ethniclabels))

  if (language == "EN") {
    return(description_en[labels])
  } else {
    return(descriptions_es[labels])
  }
}

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
  path <- system.file("data", "isco88_table.rda", package = "epiCo")
  load(path)
  isco88_table <- isco88_table
  input_level <- ifelse(isco_codes == 0 | isco_codes == 110, "Armed Forces",
    ifelse(nchar(isco_codes) == 1, "major",
      ifelse(nchar(isco_codes) == 2, "sub_major",
        ifelse(nchar(isco_codes) == 3, "minor",
          ifelse(nchar(isco_codes) == 4, "unit",
            NA
          )
        )
      )
    )
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
      stop(paste0("Output level does not exist, please check your input"),
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
        stop(paste0("Code ", isco_code, " does not exist,
                    please check your input"), call. = FALSE)
      }
    )
  }
  return(isco88_labels)
}
