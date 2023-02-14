#' Returns the conditional probability mass function of being infected given an age and gender
#' 
#' Function that returns the probability og being infected given an age
#' @param cases A data frame with a numeric vector with the ages of cases in years and a vector with gender
#' @param populationPyramid A dataframe with the count of individuals
#' @param gender = TRUE (default) A boolean to indicate the disaggregation of age risk by gender
#' @param plot = FALSE (default) A boolean for displaying a plot
#' 
#' @return A dataframe with the proportion or total count of individuals
#' @examples
#' populationPyramid(15001, 2015, total = TRUE, plot = TRUE)
#' @export
ageRisk <- function(cases, populationPyramid, gender = TRUE, plot = FALSE) {
  
  if(gender == TRUE){
    
    cases_F <- subset(cases, Gender == "F")
    pyramid_F <- subset(populationPyramid, Gender == "F")
    
    hist_age_I_F <- hist(cases_F$Age, breaks = c(0:101), right = FALSE, plot = FALSE)
    P_age_I_F <- data.frame(Age = c(0:100),
                          Prob = hist_age_I_F$density)
    
    P_age_F <- data.frame(Age = pyramid_F$Age,
                        Prob = pyramid_F$Population/sum(pyramid_F$Population))
    
    P_I_F <- length(cases_F$Age)/sum(pyramid_F$Population)
    
    P_I_age_F <- data.frame(Age = pyramid_F$Age,
                          Prob = P_I_F*P_age_I_F$Prob/P_age_F$Prob,
                          Gender = rep("F",101))
    
    
    cases_M <- subset(cases, Gender == "M")
    pyramid_M <- subset(populationPyramid, Gender == "M")
    
    hist_age_I_M <- hist(cases_M$Age, breaks = c(0:101), right = FALSE, plot = FALSE)
    P_age_I_M <- data.frame(Age = c(0:100),
                            Prob = hist_age_I_M$density)
    
    P_age_M <- data.frame(Age = pyramid_M$Age,
                          Prob = pyramid_M$Population/sum(pyramid_M$Population))
  
    P_I_M <- length(cases_M$Age)/sum(pyramid_M$Population)
    
    P_I_age_M <- data.frame(Age = pyramid_M$Age,
                          Prob = P_I_M*P_age_I_M$Prob/P_age_M$Prob,
                          Gender = rep("M",101))
    
    P_I_age <- rbind(P_I_age_F,P_I_age_M)
    
  } else {
    
    hist_age_I <- hist(cases$Age, breaks = c(0:101), right = FALSE, plot = FALSE)
    P_age_I <- data.frame(Age = c(0:100),
                          Prob = hist_age_I$density)
    
    P_age <- data.frame(Age = populationPyramid$Age,
                        Prob = populationPyramid$Population/sum(populationPyramid$Population))
    
    P_I <- length(ages)/sum(populationPyramid$Population)
    
    P_I_age <- data.frame(Age = populationPyramid$Age,
                          Prob = P_I*P_age_I$Prob/P_age$Prob)
  }
  
  
  if(plot == TRUE){
    
    if(gender == TRUE){
      
      P_I_age$Prob = c(-1*P_I_age_M$Prob, P_I_age_M$Prob)
      
      P_I_age_plot <- ggplot(P_I_age, aes(x = Age, y = Prob, fill = Gender)) + 
        geom_bar(data = subset(P_I_age, Gender == "F"), stat = "identity") + 
        geom_bar(data = subset(P_I_age, Gender == "M"), stat = "identity") +
        coord_flip()
      
      P_I_age$Prob = c(P_I_age_M$Prob, P_I_age_M$Prob)
      
    } else {
      
      P_I_age_plot <- ggplot(P_I_age, aes(x = Age, y = Prob)) + 
        geom_bar(stat = "identity")
      
    }
    
   print(P_I_age_plot)
    
  }
  
  
  return(P_I_age)
  
}
