#' Control parameters for the Moving Average (MA)
#' 
#' \code{control_parm_ma} computes the essential parameters required for EWMA statistic calculation. 
#' It includes the process mean, the variance of MA statistic
#' when the process is in control, and the value of weighting factor \code{omega}.
#'
#' @param x A numeric vector of individual observations obtained from the process.
#' In some cases, \code{x} can also indicate sample averages according to a specified sampling plan.
#' @param omega integer; the weighting factor of MA charts.
#'
#' @return A list containing the Process Mean \code{mu}, the variance of MA statistic \code{sigma2} 
#' and the weighting factor \code{omega}.
#' 
#' @examples
#' x <- rnorm(100)
#' omega <- 10
#' control_parm_ma(x, omega)
#'
#'
control_parm_ma <- function(x, omega){
  
  t <- 1:omega
  
  if (is.matrix(x)){
    n <- ncol(x)
    x <- rowMeans(x)
  } else {
    n <- 1
  }
  
  mu <- mean(x, na.rm = TRUE)
  
  sigma2 <- ifelse(t >= omega, var(x, na.rm = TRUE)/(n*omega), var(x, na.rm = TRUE)/(n*t))
  out <- list(mu = mu, sigma2 = sigma2, omega = omega)
  return(out)
}

#' Control parameters for the Exponential Weighted Moving Average (EWMA)
#' 
#' \code{control_parm_ewma} computes the essential parameters required for EWMA statistic calculation. 
#' It includes the process mean, the variance of EWMA statistic
#' when the process is in control, and the value of weighting factor \code{lambda}.
#'
#' @param x A numeric vector of individual observations obtained from the process.
#' In some cases, \code{x} can also indicate sample averages according to a specified sampling plan.
#' @param lambda A numeric value between 0 and 1 inclusive that indicates the smoothing constant of EWMA charts.
#'               It determines the weight given to recent data points. A smaller \code{lambda} gives more weight to recent observations,
#'               making the chart more sensitive to shifts.
#' @param max.rl integer; Maximum Run Length. It refers to the total number of 
#' observations to be collected from \code{x} in each process replicate. \code{max.rl} 
#' is denoted by \code{i} in the EWMA variance formula. See Details for more information.
#' 
#'
#' @return A list containing the process mean \code{mu}, the variance of EWMA statistic \code{sigma2} 
#' and the weighting factor \code{lambda}.
#' 
#' @export
#' 
#' @details The variance of the EWMA statistic \code{sigma2} is calculated using the formula:
#' \deqn{\sigma^2(Z_i) = \left[(1-(1-\lambda)^{2i})\frac{{\lambda}}{{2-\lambda}}\right]\sigma^2_X}
#' The Process Mean \code{mu} is the average of the individual observations obtained from \code{X}:
#' \deqn{\bar{X} = \frac{\sum_{j=0}^{n}X_j}{n}}
#' 
#' @examples
#' x <- rnorm(100)
#' lambda <- 0.2
#' max.rl <- 500
#' control_parm_ewma(x, lambda, max.rl)
#'

control_parm_ewma <- function(x, lambda, max.rl){
  
  if (is.matrix(x)){
    n <- ncol(x)
    x <- rowMeans(x)
  } else {
    n <- 1
  }
  
  mu <- mean(x, na.rm = TRUE)
  sigma2 <- (var(x, na.rm = TRUE)/n)*(lambda/(2 - lambda))*(1 - (1 - lambda)^(2*(1:max.rl)))
  
  out <- list(mu = mu, sigma2 = sigma2, lambda = lambda)
}