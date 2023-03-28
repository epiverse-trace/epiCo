#' Returns the conditional probability mass function of being infected given an age and gender
#'
#' Function that returns the probability of being infected given an age
#' @param cases A data frame with a numeric vector with the ages of cases in years and a vector with gender
#' @param populationPyramid A dataframe with the count of individuals
#' @param gender = TRUE (default) A boolean to indicate the disaggregation of age risk by gender
#' @param plot = FALSE (default) A boolean for displaying a plot
#'
#' @return A dataframe with the proportion or total count of individuals
#' @export
ageRisk <- function(cases, populationPyramid, gender = TRUE, plot = FALSE) {
  if (gender == TRUE) {
    cases_F <- subset(cases, eval(parse(text = 'Gender == "F"')))
    pyramid_F <- subset(populationPyramid, Gender == "F")

    hist_F <- graphics::hist(cases_F$Age, breaks = c(0:101), right = FALSE, plot = FALSE)

    riskAge_F <- data.frame(
      Age = pyramid_F$Age,
      Prob = hist_F$counts / pyramid_F$Population,
      Gender = rep("F", 101)
    )


    cases_M <- subset(cases, Gender == "M")
    pyramid_M <- subset(populationPyramid, Gender == "M")

    hist_M <- graphics::hist(cases_M$Age, breaks = c(0:101), right = FALSE, plot = FALSE)

    riskAge_M <- data.frame(
      Age = pyramid_M$Age,
      Prob = hist_M$counts / pyramid_M$Population,
      Gender = rep("M", 101)
    )

    riskAge <- rbind(riskAge_F, riskAge_M)
  } else {
    hist_T <- graphics::hist(cases$Age, breaks = c(0:101), right = FALSE, plot = FALSE)

    riskAge <- data.frame(
      Age = populationPyramid$Age,
      Prob = hist_T$counts / populationPyramid$Population
    )
  }


  if (plot == TRUE) {
    if (gender == TRUE) {
      riskAge$Prob <- c(-1 * riskAge_F$Prob, riskAge_M$Prob)

      riskAge_plot <- ggplot2::ggplot(riskAge, ggplot2::aes(x = Age, y = Prob, fill = Gender)) +
        ggplot2::geom_bar(data = subset(riskAge, Gender == "F"), stat = "identity") +
        ggplot2::geom_bar(data = subset(riskAge, Gender == "M"), stat = "identity") +
        ggplot2::coord_flip()

      riskAge$Prob <- c(riskAge_F$Prob, riskAge_M$Prob)
    } else {
      riskAge_plot <- ggplot(riskAge, aes(x = Age, y = Prob)) +
        geom_bar(stat = "identity")
    }

    print(riskAge_plot)
  }


  return(riskAge)
}
