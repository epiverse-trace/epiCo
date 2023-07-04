

# install.packages("remotes")
remotes::install_github("epiverse-trace/epiCo")

library(epiCo)
library(incidence)

View(inc_dep)
inc_dep[["counts"]]

incidencia_dengue <- as.data.frame(inc_dep$dates)
plot(inc_dep)
library(ggplot2)
ggplot(google_dengue, aes(x=date, y=dengue)) 


library(epiparameter)
library(bpmodels)
library(incidence2)
library(tidyverse)

set_calendar_date <- as.Date("2023-01-01")

numbers <- c(34, 56, 67)

test_data_dengue <- data.frame(COD = sample(numbers, 100, replace = T)) %>% 
  mutate(date = sample(seq(as.Date("2023-01-01"), by = "day", length.out = n()))) %>% 
  mutate(I = sample(0:10, replace = T, n())) %>%
  mutate(dengue = sample(0:100, replace = T, n())) %>%
  mutate(hemorragia = sample(0:100, replace = T, n())) %>%
  mutate(mosquito = sample(0:100, replace = T, n()))
                   

  
  



