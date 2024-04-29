
#' Exponentially Weighted Moving Average statistic
#'
#' @param x A numeric vector of individual observations obtained from the process.
#' In some cases, \code{x} can also indicate sample averages according to a specified sampling plan.
#' @param t An integer vector; representing a sequence from 1 to 
#' n, where n denotes the number of observations used in the Exponential 
#' Weighted Moving Average.
#' @param lambda A numeric value between 0 and 1 inclusive that indicates the weighting factor of EWMA charts.
#'               It determines the weight given to recent data points. A smaller \code{lambda} gives more weight to recent observations,
#'               making the chart more sensitive to shifts.
#' @param x0 double; process mean. Also known as the target mean or the average of the historical data.
#'
#' @return A numeric vector of Exponential Weighted Moving average statistic.
#' @export
#'
#' @examples 
#' x = rnorm(100)
#' ewma_statistic(x, t = 1:length(x), lambda = 0.15, x0 = mean(x))
ewma_statistic <- function(x, t, lambda, x0) {
  
  n = length(t)
  Z = c()
  
  for (i in 1:n) {
    sum_term <- 0
    for (j in 0:(i - 1)) {
      sum_term <- sum_term + (1 - lambda)^j * x[i - j]
    }
    Z <- c(Z, lambda * sum_term + (1 - lambda)^i * x0)
  }
  
  return(Z)
}


#' Moving Average statistic
#'
#' @param x A numeric vector of individual observations obtained from the process.
#' In some cases, \code{x} can also indicate sample averages according to a specified sampling plan.
#' @param t An integer vector; representing a sequence from 1 to 
#' n, where n denotes the number of observations used in the Moving Average.
#' @param omega integer; the weighting factor of MA charts.
#'
#' @return A numeric vector of Moving average statistic.
#' @export
#'
#' @examples 
#' x = rnorm(100)
#' ma_statistic(x, t = 1:length(x), omega = 0.15)
ma_statistic <- function(x, t, omega) {
  n <- length(t)
  out <- c()
  
  for (i in 1:n) {
    z <- 0
    
    if (t[i] >= omega) {
      for (j in (t[i] - omega + 1):t[i]) {
        z <- z + x[j] / omega
      }
    } else {
      for (j in 1:t[i]) {
        z <- z + x[j] / t[i]
      }
    }
    
    out <- c(out, z)
  }
  
  return(out)
}

