
#' Exponentially Weighted Moving Average statistic
#'
#' \code{ewma_statistic()} computes exponentially weighted moving averages for a given 
#' time series, assigning greater importance to recent observations while still considering 
#' older data points.
#' 
#' @param x A numeric vector of individual observations obtained from the process.
#' In some cases, \code{x} can also indicate sample averages according to a specified sampling plan.
#' @param t An integer vector; representing a sequence from 1 to 
#' n, where n denotes the number of observations used in the Exponential 
#' Weighted Moving Average. \code{t} is denoted by \code{i} in the EWMA statistic formula. 
#' See Details for more information.
#' @param lambda A numeric value between 0 and 1 inclusive that indicates the weighting factor of EWMA charts.
#'               It determines the weight given to recent data points. A smaller \code{lambda} gives more weight to recent observations,
#'               making the chart more sensitive to shifts.
#' @param x0 double; process mean. Also known as the target mean or the average of the 
#' historical data. \code{x0} is denoted by \code{Z_0} in the EWMA statistic formula. 
#' See Details for more information.
#'
#' @return A numeric vector of Exponential Weighted Moving average statistic.
#' @export
#' 
#' @details
#' The EWMA statistic is given by:
#' \deqn{Z_i = \lambda \sum_{j=0}^{i-1} (1-\lambda)X_{i-j} + (1-\lambda)^i Z_0}
#'
#' @seealso \code{\link{arl_ewma}}, \code{\link{lower_limit_ewma}}, 
#' \code{\link{upper_limit_ewma}}, \code{\link{ma_statistic}}
#' 
#' @examples 
#' x = rnorm(100)
#' ewma_values <- ewma_statistic(x, t = 1:length(x), lambda = 0.15, x0 = mean(x))
#' print(ewma_values)
#' 
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
#' \code{ma_statistic()} calculates moving averages for a provided time series vector, 
#' using a defined window size.
#' 
#' @param x A numeric vector of individual observations obtained from the process.
#' In some cases, \code{x} can also indicate sample averages according to a specified sampling plan.
#' @param t An integer vector; representing a sequence from 1 to 
#' n, where n denotes the number of observations used in the Moving Average.
#' @param omega integer; the weighting factor of MA charts, also known as the window size.
#'
#' @return A numeric vector of Moving average statistics.
#' @export
#'
#' @details
#' The moving average statistic is given by:
#' \deqn{Z_i = \left\{
#' \begin{array}{ll}
#' \frac{1}{\omega} \sum_{j=t[i]-\omega+1}^{t[i]} x_j & t[i] \geq \omega \\\\
#' \frac{1}{t[i]} \sum_{j=1}^{t[i]} x_j & t[i] < \omega
#' \end{array}
#' \right.}
#'
#' where \code{t[i]} is the observation index at time \code{i}, \eqn{\omega} is the weighting factor
#' \code{omega}, also known as the window size, on which averages are calculated, 
#' and \code{x_j} is the individual observation of the process \code{x} at time \code{j}.
#' 
#' @seealso \code{\link{arl_ma}}, \code{\link{lower_limit_ma}}, 
#' \code{\link{upper_limit_ma}}, \code{\link{ewma_statistic}}
#' 
#' @examples 
#' x = rnorm(100)
#' ma_values <- ma_statistic(x, t = 1:length(x), omega = 10)
#' print(ma_values)
#' 
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

