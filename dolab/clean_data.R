
library("tidyverse")
library("tidyr")
library("dplyr")

incidence_data <- readRDS("inc_dep.rds")
google_data <- read.csv("google_dengue.csv", header = T, sep = ";")

clean_data <- function(google_data, incidence_data){
  df_incidence <- as.data.frame(incidence_data[["counts"]])
  df_filtered <- df_incidence %>% select("25", "15", "73", "5", "68", "54", 
                                         "41", "63", "17", "66")
  google_data <- google_data %>% mutate(Region = recode(Region, "CO-CUN" = "25", 
                                                            "CO-BOY" = "15", 
                                                            "CO-TOL" = "73", 
                                                            "CO-ANT" = "5",
                                                            "CO-SAN" = "68", 
                                                            "CO-NSA" = "54", 
                                                            "CO-HUI" = "41", 
                                                            "CO-QUI" = "63",
                                                            "CO-CAL" = "17", 
                                                            "CO-RIS" = "66"))
  
  date_rep <- sort(rep(incidence_data$dates, times = length(unique(google_data$Region))))
  
  pivoted_data <- df_filtered %>% 
                    pivot_longer(cols = c(1:length(df_filtered)), 
                                 names_to = "Region",
                                 values_to = "Incidence") %>% 
                    mutate(Date = as.character(date_rep)) %>% 
                    filter(Date %in% google_data$date)
  colnames(pivote_data) <- epitrix::clean_labels(colnames(pivote_data))
  colnames(google_data) <- epitrix::clean_labels(colnames(google_data))
  
  data <- merge(google_data, pivoted_data, by.x = c("region", "date"), by.y = c("region", "date"))
    
  return (data)
}

