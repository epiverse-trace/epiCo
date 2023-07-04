

library(corrplot)
library(dplyr)
data_challenge <- readRDS("dolab/data_epiCo_challenge.RDS")

data_challenge_1 <- data_challenge %>% filter(region == 1)
# correlacion <- cor(test_data_dengue[, c('I','dengue','hemorragia',"mosquito")])

cor_matrix <- cor(data_challenge_bog[,c(3:10)])
cor_plot_1 <- corrplot(cor_matrix, method = "number")


cor_matrix_region <- function(data, 
                              region_code, 
                              col_range,
                              method = "number"){
  data_region <- data %>% filter(region == region_code)
  cor_matrix <- cor(data[, col_range])
  cor_plot <- corrplot(cor_matrix, method = method)
  return(cor_plot)  
}

cor_plot_73 <- cor_matrix_region(data = data_challenge, 
                                region_code = 73, 
                                col_range = c(3:10))
