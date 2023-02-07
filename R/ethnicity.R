#' Provides the sociological description of ethnicities in Colombia
#' 
#' Function that returns the description of consulted ethnicities
#' @param ethniclabels A numeric vector with the codes of ethnicities to consult
#' @param plot = FALSE (defalut) A boolean for displaying a plot
#' 
#' @return A printed message with the description of the ethnicities
#' @examples
#' ethnicity(c(1,2,3,4))
#' @export
ethnicity <- function(ethniclabels, language = "ES", plot = FALSE) {

  ethniclabels <- as.data.frame(ethniclabels)
  
  #### ESPAÑOL ####
  indigena_ES <- "Persona de ascendencia amerindia que comparten sentimientos de identificación con su pasado aborigen, manteniendo rasgos y valores propios de su cultura tradicional, así como formas de organización y control social propios"
  
  rom_ES <- "Son comunidades que tienen una identidad étnica y cultural propia; se caracterizan por una tradición nómada, y tienen su propio idioma que es el romanés"
  
  raizal_ES <- "Población ubicada en el Archipiélago de San Andrés, Providencia y Santa Catalina, con raíces culturales afroanglo-antillanas, cuyos integrantes tienen rasgos socioculturales y lingüísticos claramente diferenciados del resto de la población afrocolombiana"
  
  palenquero_ES <- "Población ubicada en el municipio de San Basilio de Palenque, departamento de Bolívar, donde se habla el palenquero, lenguaje criollo"
  
  afro_ES <- "Persona de ascendencia afrocolombiana que poseen una cultura propia, y tienen sus propias tradiciones y costumbre d entro de la relación campo-poblado"
  
  #### ENGLISH ####
  indigena_EN <- "A person of Amerindian descent who shares feelings of identification with their aboriginal past, maintaining traits and values of their traditional culture, as well as their own forms of organization and social control"
  
  rom_EN <- "They are communities that have their own ethnic and cultural identity; They are characterized by a nomadic tradition, and have their own language, which is Romanesque"
  
  raizal_EN <- "Population located in the Archipelago of San Andrés, Providencia and Santa Catalina, with Afro-Anglo-Antillean cultural roots, whose members have clearly differentiated sociocultural and linguistic traits from the rest of the Afro-Colombian population"
  
  palenquero_EN <- "Population located in the municipality of San Basilio de Palenque, department of Bolívar, where palenquero is spoken, a Creole language"
  
  afro_EN <- "Person of Afro-Colombian descent who have their own culture, and have their own traditions and customs within the rural-populated relationship"
  
  #####
  
  
  descriptions_ES <- c(indigena_ES,rom_ES,raizal_ES,palenquero_ES,afro_ES)
  description_EN <- c(indigena_EN,rom_EN,raizal_EN,palenquero_EN,afro_EN)

  if(plot){
    
    ethnHist <- ggplot(ethniclabels, aes(ethniclabels)) +
      geom_histogram() + theme_minimal()
    
    print(ethnHist)
    
  }
  
  labels <- order(unique(ethniclabels$ethniclabels))
  
  if (language == "EN"){
    return(description_EN[labels])
  } else {
    return(descriptions_ES[labels])
  }
  
  
}
