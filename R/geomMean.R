#' Returns the geometric mean of a vector of real numbers
#' 
#' Function that returns the geometric mean of a vector of real numbers according to the selected method
#' @param x A numeric vector of real values
#' @param method = "positive" (default)
#' Description of methods:
#' - positive = only positive values within x are used in the calculation
#' - shifted = positive and zero values within x are used by adding a shift value before the calculation and subtratcting it to the final result
#' - optimized = optimized shifted method. See: De La Cruz, R., & Kreft, J. U. (2018). Geometric mean extension for data sets with zeros. arXiv preprint arXiv:1806.06403.
#' - weighted = a probability weighted calculation of GM for negative, positive, and zero values. See: Habib, E. A. (2012). Geometric mean for negative and zero values. International Journal of Research and Reviews in Applied Sciences, 11(3), 419-432. 
#' @param shift = 1 (default) a positive value to use in the shifted method
#' @param epsilon = 1e-5 (default) the minimum positive value to consider in the optimized method
#' @return The geometric mean of the x vector, and the epsilon value if optimized method is used
#' @examples
#' geomMean(x, method = "optimized")
#' @export
geomMean <- function(x, method = "optimized", shift = 1, epsilon = 1e-5) {
  
  if (method == "positive"){
    
    x_positive <- x[x>0]
    
    GM <- exp(mean(log(x_positive)))
    
  } else if (method == "shifted"){
    
    x_shifted <- x[x>=0] + shift
    
    GM <- exp(mean(log(x_shifted))) - shift
    
  } else if (method == "weighted"){
    
    N <- length(x)
    
    x_positive <- x[x>0]
    w_positive <- length(x_positive)/N
    x_negative <- x[x<0]
    w_negative <- length(x_negative)/N
    x_zeros <- x[x==0]
    w_zeros <- length(x_zeros)/N
    
    GM_positive <- exp(mean(log(x_positive)))
    GM_negative <- -1*exp(mean(log(abs(x_negative))))
    GM_zeros <- 0
    
    GM <- w_positive*GM_positive + w_negative*GM_negative + w_zeros*GM_zeros
    
  } else if (method == "optimized") {
    
    # The formula is:
    # exp(mean(log(x+delta)))-delta (Eq. I)
    # where delta is the maximum value such that:
    # abs([exp(mean(log(x_positive+delta)))-delta]-geomean(x_positive))<epsilon*geomean(x_positive) (Eq. II)
    
    x <- x[x>=0]
    
    x_positive <- x[x>0]
    GM_positive <- exp(mean(log(x_positive)))
    epsilon <- epsilon*GM_positive 
    
    
    # Simple bisection  method to calculate delta: (Eq. I) is increasing as
    # consequence of the Superaddivity of the Geometric Mean
    
    deltamin <- 0
    deltamax <- GM_positive+epsilon
    
    # while (exp(mean(log(x_positive+deltamax)))-deltamax < epsilon){ #Just for data set with very small standard desviation
    #   deltamin <- deltamax
    #   deltamax <- deltamax*2
    # }
    
    delta <- (deltamin+deltamax)/2
    
    auxExp <- exp(mean(log(x_positive+delta)))-delta #Define auxExp to not repeat operations
    
    while ((auxExp-GM_positive)>epsilon){
      
      if ((auxExp<GM_positive)){
        deltamin <- delta
      } else {
        deltamax <- delta
      }
      
      delta <- (deltamin+deltamax)/2
      auxExp <- exp(mean(log(x_positive+delta)))-delta
      
    }
    
    GM <- exp(mean(log(x+delta)))-delta
    
    return(c(GM,delta))
    
  } else {
    
    print("Error")
    
  }
  
  return(GM)
  
}
