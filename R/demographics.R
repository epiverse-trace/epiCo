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
#' @return A dataframe with the proportion or total count of individuals
#' @examples
#' population_pyramid(15001, 2015, total = TRUE, plot = TRUE)
#' @export
#'
population_pyramid <- function(divipola_code, year,
                              gender = TRUE, total = TRUE, plot = FALSE) {
  
  data("divipola_table", package = "epiCo")
  
  if (divipola_code %in% divipola_table$COD_DPTO) {
    data("population_projection_col_1", package = "epiCo")
    pop_data_dpto <- subset(population_projection_col_1,
                           population_projection_col_1$DP == divipola_code
                           & population_projection_col_1$ANO == year)
    
    female_total <- as.numeric(pop_data_dpto[106:206])
    male_total <- as.numeric(pop_data_dpto[5:105])
    
  } else if (divipola_code %in% divipola_table$COD_MPIO) {
    data("population_projection_COL_2", package = "epiCo")
    pop_data_mun <- subset(population_projection_COL_2,
                           population_projection_COL_2$DPMP == divipola_code
                           & population_projection_COL_2$ANO == year)
    
    female_total <- as.numeric(pop_data_mun[105:205])
    male_total <- as.numeric(pop_data_mun[4:104])
    
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
      Gender = c(rep('F', 101), rep('M', 101))
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
      
      pop_pyramid_plot <- ggplot2::ggplot(pop_pyramid,
                                          ggplot2::aes(x=Age,y=Population
                                                       ,fill=Gender)) +
        ggplot2::geom_bar(data=subset(pop_pyramid,Gender=='F'),stat='identity')+
        ggplot2::geom_bar(data=subset(pop_pyramid,Gender=='M'),stat='identity')+
        ggplot2::coord_flip()
      
      pop_pyramid$Population <- c(female_total, male_total)
      
    } else {
      pop_pyramid_plot <- ggplot2::ggplot(pop_pyramid,aes(x=Age,y=Population)) +
        ggplot2::geom_bar(stat='identity')
    }
    
    if (total) {
      pop_pyramid_plot <- pop_pyramid_plot + ylab("Total population")
    } else {
      pop_pyramid_plot <- pop_pyramid_plot + ylab("Proportion of population")
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
#' @return A dataframe with the proportion or total count of individuals
#' @export
age_risk <- function(age, gender = NULL, population_pyramid, plot = FALSE) {
  
  if (!is.null(gender)) {
    
    ages_F <- age[gender=='F']
    pyramid_F <- subset(population_pyramid, Gender == "F")
    hist_F <- graphics::hist(ages_F, breaks = c(0:101),
                             right = FALSE, plot = FALSE)
    
    age_risk_F <- data.frame(
      Age = pyramid_F$Age,
      Prob = hist_F$counts / pyramid_F$Population,
      Gender = rep("F", 101)
    )
    
    ages_M <- age[gender=='M']
    pyramid_M <- subset(population_pyramid, Gender == "M")
    hist_M <- graphics::hist(ages_M, breaks = c(0:101),
                             right = FALSE, plot = FALSE)
    
    age_risk_M <- data.frame(
      Age = pyramid_M$Age,
      Prob = hist_M$counts / pyramid_M$Population,
      Gender = rep("M", 101)
    )
    
    age_risk <- rbind(age_risk_F, age_risk_M)
    
  } else {
    
    hist_T <- graphics::hist(age, breaks = c(0:101), right = FALSE, 
                             plot = FALSE)
    
    age_risk <- data.frame(
      Age = population_pyramid$Age,
      Prob = hist_T$counts / population_pyramid$Population
    )
  }
  
  
  if (plot == TRUE) {
    if (!is.null(gender)) {
      age_risk$Prob <- c(-1 * age_risk_F$Prob, age_risk_M$Prob)
      
      age_risk_plot <- ggplot2::ggplot(age_risk, 
                                       ggplot2::aes(x=Age,y=Prob,fill=Gender)) +
        ggplot2::geom_bar(data=subset(age_risk,Gender=='F'),stat="identity") +
        ggplot2::geom_bar(data=subset(age_risk,Gender=='M'),stat="identity") +
        ggplot2::coord_flip()
      
      age_risk$Prob <- c(age_risk_F$Prob, age_risk_M$Prob)
      
    } else {
      age_risk_plot <- ggplot2::ggplot(age_risk, ggplot2::aes(x=Age,y=Prob)) +
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
#' describe_ethnicity(c(1, 2, 3, 4))
#' @export
describe_ethnicity <- function(ethniclabels, language = "ES", plot = FALSE) {
  
  ethniclabels <- as.data.frame(ethniclabels)
  
  #### ESPAÑOL ####
  indigena_ES <-"Persona de ascendencia amerindia que comparten sentimientos
  de identificación con su pasado aborigen, manteniendo rasgos y valores
  propios de su cultura tradicional, así como formas de organización
  y control social propios"
  
  rom_ES <-"Son comunidades que tienen una identidad étnica y cultural propia;
  se caracterizan por una tradición nómada, y tienen su propio idioma
  que es el romanés"
  
  raizal_ES <-"Población ubicada en el Archipiélago de San Andrés, Providencia
  y Santa Catalina, con raíces culturales afroanglo-antillanas,
  cuyos integrantes tienen rasgos socioculturales y lingüísticos
  claramente diferenciados del resto de la población afrocolombiana"
  
  palenquero_ES <-"Población ubicada en el municipio de San Basilio de Palenque,
  departamento de Bolívar, donde se habla el palenquero, lenguaje criollo"
  
  afro_ES <-"Persona de ascendencia afrocolombiana que poseen una cultura
  propia, y tienen sus propias tradiciones y costumbre dentro de la relación
  campo-poblado"
  
  #### ENGLISH ####
  indigena_EN <-"A person of Amerindian descent who shares feelings of
  identification with their aboriginal past, maintaining traits and values
  of their traditional culture, as well as their own forms of organization
  and social control"
  
  rom_EN <-"They are communities that have their own ethnic and cultural
  identity; They are characterized by a nomadic tradition, and have their own
  language, which is Romanesque"
  
  raizal_EN <-"Population located in the Archipelago of San Andrés, Providencia
  and Santa Catalina, with Afro-Anglo-Antillean cultural roots, whose members
  have clearly differentiated sociocultural and linguistic traits
  from the rest of the Afro-Colombian population"
  
  palenquero_EN <-"Population located in the municipality of San Basilio de
  Palenque, department of Bolívar, where palenquero is spoken,
  a Creole language"
  
  afro_EN <- "Person of Afro-Colombian descent who have their own culture,
  and have their own traditions and customs within the
  rural-populated relationship"
  
  #####
  
  descriptions_ES <- c(indigena_ES, rom_ES, raizal_ES, palenquero_ES, afro_ES)
  description_EN <- c(indigena_EN, rom_EN, raizal_EN, palenquero_EN, afro_EN)
  
  if (plot) {
    ethnHist <- ggplot(ethniclabels, aes(ethniclabels)) +
      geom_histogram() +
      theme_minimal()
    
    print(ethnHist)
  }
  
  labels <- order(unique(ethniclabels$ethniclabels))
  
  if (language == "EN") {
    return(description_EN[labels])
  } else {
    return(descriptions_ES[labels])
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
#' describe_occupation(1111, level = 1)
#' @export
describe_occupation <- function(isco_codes, output_level) {
  data(isco88_table)
  input_level <- ifelse(isco_codes==0 | isco_codes==110,"Armed Forces",
                        ifelse(nchar(isco_codes)==1,"major",
                               ifelse(nchar(isco_codes)==2,"sub_major",
                                      ifelse(nchar(isco_codes)==3,"minor",
                                             ifelse(nchar(isco_codes)==4,"unit",
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
           call. = FALSE)
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
  for (i in seq(1, length(isco_codes)))
  {
    tryCatch(
      {
        isco_code <- isco_codes[i]
        if (isco_code == 0 | isco_code == 110) {
          isco88_labels[[i]] <- as.array(isco88_table[isco88_table$major == 0,
                                                      output_level])
        } else if (input_level_index[i] < output_level_index) {
          index_start <- match(isco_code, isco88_table[, input_level[i]])
          n_match <- sum(isco88_table[, input_level[i]] == isco_code)
          index_end <- index_start + n_match - 1
          isco88_labels[[i]] <- as.array(isco88_table[index_start:index_end,
                                                      output_level])
        } else {
          isco88_labels[i] <- isco88_table[which(isco88_table[input_level[i]]
                                                 == isco_code)[1], output_level]
        }
      },
      error = function(e) {
        stop(paste0("Code ",isco_code," does not exist, 
                    please check your input"), call. = FALSE)
      }
    )
  }
  return(isco88_labels)
}
