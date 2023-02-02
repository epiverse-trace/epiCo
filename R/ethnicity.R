#' Provides the sociological description of ethnicities in Colombia
#' 
#' Function that returns the description of consulted ethnicities
#' @param ethniclabels A numeric vector with the codes of ethnicities to consult
#' 
#' @return A printed message with the description of the ethnicities
#' @examples
#' ethnicity(c(1,2,3,4))
#' @export
ethnicity <- function(ethniclabels) {

  indigena <- "Persona de ascendencia amerindia que comparten sentimientos de 
  identificación con su p asado aborigen, manteniendo rasgos y valores propios 
  de su cultura tradicional, así como formas de organización y control social
  propios"
  
  rom <- "Son comunidades que tienen una identidad étnica y cultural propia;
  se caracterizan por una tradición nómada, y tienen su propio idioma 
  que es el romanés"
  
  raizal <- "Población ubicada en el Archipiélago de San Andrés, Providencia 
  y Santa Catalina, con raíces culturales afroanglo-antillanas, cuyos integrantes 
  tienen rasgos socioculturales y lingüísticos claramente diferenciados del resto
  de la población afrocolombiana"
  
  palenquero <- "Población hubicada en el municipio de San Basilio de Palenque, 
  departamento de Bolívar, donde se habla el palenquero, lenguaje criollo"
  
  afro <- "Persona de ascendencia afrocolombiana que poseen una cultura propia, 
  y tienen sus propias tradiciones y costumbre d entro de la relación campo-poblado"
  
  descriptions <- c(indigena,rom,raizal,palenquero,afro)
  
  labels <- unique(ethniclabels)  
  descriptions[labels]
}
