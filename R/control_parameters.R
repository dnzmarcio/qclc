### Control parameters

#' Control parameters for the Moving Average 
#' 
#' Returns a list of values containing the target mean of the sample, the variance of 
#' the Moving Average when the process is under control, and the value of omega.
#'
#' @param x A numeric vector containing the data points for which to calculate the MA.
#' @param omega An integer
#'
#' @return Returns mean and variance of the sample
#'
control_parm_ma <- function(x, omega){
  
  t <- 1:omega
  
  if (is.matrix(x)){
    n <- ncol(x)
    x <- rowMeans(x)
  } else {
    n <- 1
  }
  
  mu <- mean(x)
  
  sigma2 <- ifelse(t >= omega, var(x)/(n*omega), var(x)/(n*t))
  out <- list(mu = mu, sigma2 = sigma2, omega = omega)
  return(out)
}

#' Control parameters for the Exponential Weighted Moving Average 
#' 
#' Returns a list of values containing the target mean of the sample and the 
#' variance of the Exponential Weighted Moving Average when the process is 
#' under control, and the value of lambda.
#'
#' @param x A numeric vector containing the data points for which to calculate the EWMA.
#' @param lambda A numeric value between 0 and 1 inclusive that indicates the smoothing constant of EWMA charts.
#'               It determines the weight given to recent data points. A smaller \(\lambda\) gives more weight to recent observations,
#'               making the chart more sensitive to shifts.
#' @param max.arl integer; Maximum Average Run Length.
#'
#' @return Returns mean and variance of the sample
#'

control_parm_ewma <- function(x, lambda, max.arl){
  
  if (is.matrix(x)){
    n <- ncol(x)
    x <- rowMeans(x)
  } else {
    n <- 1
  }
  
  mu <- mean(x)
  sigma2 <- (var(x)/n)*(lambda/(2 - lambda))*(1 - (1 - lambda)^(2*(1:max.arl)))
  
  out <- list(mu = mu, sigma2 = sigma2, lambda = lambda)
}