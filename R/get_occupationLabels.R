#' Get ISCO-88 occupation labels from codes
#' 
#' Function that translates a vector of ISCO-88 occupation codes into a vector of labels
#' @param isco_codes A numeric vector of ISCO-88 occupation codes (major, submajor, minor or unit)
#' @param output_level A string parameter that defines the level of the desired label (major, submajor, minor or unit)
#' 
#' @return A string vector of ISCO-88 labels
#' @examples
#' estimate_incidenceRate(incidence, level=1)
#' @export
get_occupationLabels <- function(isco_codes, output_level) {
  
  input_level <- ifelse((floor(log10(isco_codes)) + 1)==1, "major",
                        ifelse((floor(log10(isco_codes)) + 1)==2, "sub_major",
                               ifelse((floor(log10(isco_codes)) + 1)==3, "minor",
                                      ifelse((floor(log10(isco_codes)) + 1)==4, "unit",
                                             ifelse(isco_codes==0, "Armed Forces",
                                                    NA)))))
  
  ISCO88_labels <- input_level
  
  for (i in seq(1,length(isco_codes)))
  {
    isco_code <- isco_codes[i]
    
    if(isco_code==0 | is.na(isco_code)){
      next
    }
    
    ISCO88_labels[i] <- ISCO88_table[which(ISCO88_table[input_level[i]]==isco_code)[1], output_level]
  }
  
  return(ISCO88_labels)
}
