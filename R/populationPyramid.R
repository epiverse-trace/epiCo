#' Returns the population pyramid of the consulted region
#' 
#' Function that returns the population pyramid of the municipality or department of an specific year
#' @param DIVIPOLA_code A numeric code accounting for the territory of interest
#' @param year A numeric input for year of interest
#' @param gender = TRUE (default) A boolean to indicate that data is disaggregated by gender
#' @param total = TRUE (default) A boolean for returning the total number rather than the porportion of the populations
#' @param plot = FALSE (default) A boolean for displaying a plot
#' 
#' @return A dataframe with the proportion or total count of individuals
#' @examples
#' populationPyramid(15001, 2015, total = TRUE, plot = TRUE)
#' @export
#' 
populationPyramid <- function(DIVIPOLA_code, year, gender = TRUE, total = TRUE, plot = FALSE) {
  
  data(DIVIPOLA_table)
  
  if(DIVIPOLA_code %in% DIVIPOLA_table$COD_DPTO){
    
    data("population_projection_COL_1")
    popData_dpto <- subset(population_projection_COL_1,population_projection_COL_1$DP == DIVIPOLA_code & population_projection_COL_1$ANO == year)
    
    female_total <- as.numeric(popData_dpto[106:206])
    male_total <- as.numeric(popData_dpto[5:105])
    
    rm(DIVIPOLA_table, envir = .GlobalEnv)
    rm(population_projection_COL_1, envir = .GlobalEnv)
    
  } else if (DIVIPOLA_code %in% DIVIPOLA_table$COD_MPIO){
    
    data("population_projection_COL_2")
    popData_mun <- subset(population_projection_COL_2,population_projection_COL_2$DPMP == DIVIPOLA_code & population_projection_COL_2$ANO == year)
    
    female_total <- as.numeric(popData_mun[105:205])
    male_total <- as.numeric(popData_mun[4:104])
    
    rm(DIVIPOLA_table, envir = .GlobalEnv)
    rm(population_projection_COL_2, envir = .GlobalEnv)
    
  } else {
    print("There is no location assigned to the consulted DIVIPOLA code")
    return(NA)
  }
  
  if(total == FALSE){
    female_total = female_total/sum(female_total)
    male_total = male_total/sum(male_total)
  }
  
  if(gender == TRUE){
    popPyramid = data.frame(Age = rep(c(0:100),2),
                            Population = c(female_total,male_total),
                            Gender = c(rep("F",101),rep("M",101)))
  } else {
    popPyramid = data.frame(Age = c(0:100),
                            Population = c(female_total+male_total))
  }
  
  if(plot == TRUE){
    
    if(gender == TRUE){
      
    popPyramid$Population = c(-1*female_total,male_total)
    
    popPyramid_plot <- ggplot(popPyramid, aes(x = Age, y = Population, fill = Gender)) + 
      geom_bar(data = subset(popPyramid, Gender == "F"), stat = "identity") + 
      geom_bar(data = subset(popPyramid, Gender == "M"), stat = "identity") +
      coord_flip()
    
    popPyramid$Population = c(female_total,male_total)
    
    } else {
      
      popPyramid_plot <- ggplot(popPyramid, aes(x = Age, y = Population)) + 
        geom_bar(stat = "identity")
      
    }
    
    
    if(total)
    {
      popPyramid_plot <- popPyramid_plot + ylab("Total population")
    } else {
      popPyramid_plot <- popPyramid_plot + ylab("Proportion of population")
    }
    
    print(popPyramid_plot)
    
  }
  
  return(popPyramid)
  
}
