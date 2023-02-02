#' Get the epidemiological calendar of a consulted year
#' 
#' Function that returns the starting date of the epidemiological weeks in a year of interest
#' @param year A numeric value for the year of interest
#' @param daysJan = 4 (default) Number of January days that the first epidemiological week must contains
#' 
#' @return A character array with the starting dates of the epidemiological weeks
#' @examples
#' epiCalendar(2016)
#' @export
epiCalendar <- function(year, daysJan = 4) {
  
  # By definition, the first epidemiological week of the year contains at least four days in January
  
  epiCalendar <- c()
  
  secday <- 24*60*60
  
  firstDate <- as.POSIXlt(paste0("01-01-",toString(year)),format = "%d-%m-%Y")
  lastDate <- as.POSIXlt(paste0("31-12-",toString(year)),format = "%d-%m-%Y")
  firstWeekDay <- firstDate$wday # 0 to 6 starting on Sundays
  lastWeekDay <- lastDate$wday # 0 to 6 starting on Sundays
  
  if(firstWeekDay<daysJan)
  {
    temp_date <- firstDate-firstWeekDay*secday
  }  else {
    temp_date <- firstDate+(7*secday-firstWeekDay*secday)
  }
  
  while(as.numeric(format(temp_date,"%Y"))<=year)
  {
    epiCalendar <- c(epiCalendar,as.character.Date(temp_date))
    temp_date <- temp_date+7*secday
  }
  
  if(lastWeekDay<3)
  {
    epiCalendar <- head(epiCalendar,-1)
  }
  
  return(epiCalendar)
}
