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
      disease_data <- rbind(disease_data, dplyr::select(temp_data, tags_to_analyze))
    }
  }

  ## Dates and DIVIPOLA codes preparation and cleaning

  disease_data <- disease_data %>%
    mutate(
      COD_MUN_R = ifelse(COD_DPTO_R == 1, COD_PAIS_O, # 1 indicates residence abroad
        ifelse(nchar(COD_MUN_R) == 1, as.numeric(paste(COD_DPTO_R, COD_MUN_R, sep = "00")),
          ifelse(nchar(COD_MUN_R) == 2, as.numeric(paste(COD_DPTO_R, COD_MUN_R, sep = "0")),
            ifelse(nchar(COD_MUN_R) == 3, as.numeric(paste(COD_DPTO_R, COD_MUN_R, sep = "")),
              NA
            )
          )
        )
      ),
      COD_MUN_O = ifelse(COD_DPTO_O == 1, COD_PAIS_O, # 1 indicates infection occurred abroad
        ifelse(nchar(COD_MUN_O) == 1, as.numeric(paste(COD_DPTO_O, COD_MUN_O, sep = "00")),
          ifelse(nchar(COD_MUN_O) == 2, as.numeric(paste(COD_DPTO_O, COD_MUN_O, sep = "0")),
            ifelse(nchar(COD_MUN_O) == 3, as.numeric(paste(COD_DPTO_O, COD_MUN_O, sep = "")),
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

  disease_data <- subset(disease_data, !is.na(disease_data$COD_MUN_O))
  disease_data <- subset(disease_data, !is.na(disease_data$COD_MUN_R))
  disease_data <- subset(disease_data, !is.na(disease_data$COD_MUN_N))

  # Cleaning of cases out of the years range

  disease_data <- subset(disease_data, EPI_YEAR %in% years_to_analyze)

  # Cleaning of cases from abroad

  disease_data <- subset(disease_data, COD_PAIS_O == 170)

  # Cleaning of typos

  data("divipola_table", package = "epiCo")

  typos <- which(disease_data$COD_PAIS_O == 170 & !(disease_data$COD_MUN_O %in% divipola_table$COD_MPIO))

  disease_data <- disease_data[-typos, ]

  #####


  disease_data <- subset(disease_data, COD_MUN_O == divipola_code)

  interval <- ifelse(method == "unusual_behavior", "1 month", "1 epiweek")

  incidence_historic <- incidence::incidence(disease_data$FEC_NOT,
    interval = interval
  )

  endemic_channel(observations, incidence_historic, plot = TRUE)
}
