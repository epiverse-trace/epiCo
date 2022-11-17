#' Estimates incidence rates from a incidence class object and 
#' 
#' Function that estimates incidence rates from a incidence class object and population projections
#' @param incidence An incidence object
#' @param country 3 letter ISO code for the country of interest
#' @param level Administration level at which incidence counts are grouped (0=national, 1=state/department, 2=city/municipality)
#' 
#' @return A modified incidence object where counts are normalized with population
#' @examples
#' estimate_incidenceRate(incidence, level=1)
#' @export
estimate_incidenceRate <- function(incidence_object, level, scale = 100000) {
  
  dates_years <- lubridate::year(incidence_object$dates)
  years <- unique(dates_years)
  groups <- colnames(incidence_object$counts)
  
  if(level==0){
    populations <- dplyr::filter(population_projection_COL_0, ANO %in% years)
  } else if(level==1){
    populations <- dplyr::filter(population_projection_COL_1, DP %in% groups & ANO %in% years) 
  } else{
    populations <- dplyr::filter(population_projection_COL_2, DPMP %in% groups & ANO %in% years)
  }
  
  rownames(populations) <- groups
  incRates <- incidence_object$counts
  
  for (gr in groups)
  {
    for (ye in years)
    {
      pop <- dplyr::filter(populations, ANO==ye) 
      pop <- pop[gr,"Total_General"]
      incRates[which(dates_years==ye),gr] <- incRates[which(dates_years==ye),gr]*scale/pop
    }
  }
  
  incidenceRate_object <- incidence_object
  incidenceRate_object$rates <- incRates
  
  return(incidenceRate_object)
}
