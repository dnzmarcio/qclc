#' Lower Control Limits (LCL) for Moving Average 
#'
#' @param t An integer vector; representing a sequence from 1 to 
#' n, where n denotes the number of observations used in the Moving Average.
#' @param omega integer; the weighting factor of MA charts.
#' @param mu double; process mean. Also known as the target mean or the average of the historical data.
#' @param sigma2 double: estimate of the process variance.
#' @param L A numeric value; the control limit factor. \code{L} represents the width of 
#' the control limits in terms of multiples of the standard deviation.  
#'
#' @return A vector of the MA Lower Control Limit.
#' @export
#' 
#' @details
#' The choice of \code{L} affects the chart's ability to detect process shifts; 
#' larger L values set wider control limits from the center line, reducing false alarms but also 
#' potentially delaying the detection of real shifts.
#' 
#'
#' @examples 
#' x = rnorm(100)
#' aux = control_parm_ma(x, omega = 10)
#' lower_limit_ma(t = 1:length(x), omega =10, mu = aux$mu, sigma2 = aux$sigma2, L = 3)
#' 
lower_limit_ma <- function(t, omega, mu, sigma2, L){
  temp <- ifelse(t < omega, sigma2[t], sigma2[length(sigma2)])
  out <- mu - L*sqrt(temp)
  return(out)
}

#' Upper Control Limits (UCL) for Moving Average
#'
#' @param t An integer vector; representing a sequence from 1 to 
#' n, where n denotes the number of observations used in the Moving Average.
#' @param omega An integer value
#' @param mu double; process mean. Also known as the target mean or the average of the historical data.
#' @param sigma2 double: estimate of the process variance.
#' @param L A numeric value; the control limit factor. \code{L} represents the width of 
#' the control limits in terms of multiples of the standard deviation.
#'
#' @return A vector of the MA Upper Control Limit.
#' @export
#'
#' @details
#' The choice of \code{L} affects the chart's ability to detect process shifts; 
#' larger L values set wider control limits from the center line, reducing false alarms but also 
#' potentially delaying the detection of real shifts.
#' 
#' @examples 
#' upper_limit_ma(t = 1:30, omega =10, mu = 2, sigma2 = 4, L = 1)
#'
upper_limit_ma <- function(t, omega, mu, sigma2, L){
  temp <- ifelse(t < omega, sigma2[t], sigma2[length(sigma2)])
  out <- mu + L*sqrt(temp)
  return(out)
}

#' Lower Control Limits (LCL) for Exponentially Weighted Moving Average 
#'
#' @param t An integer vector; representing a sequence from 1 to 
#' n, where n denotes the number of observations used in the Exponential 
#' Weighted Moving Average. \code{t} is denoted by \code{i} in the LCL formula. 
#' See Details for more information.
#' @param mu double; process mean. Also known as the target mean or the average of the historical data.
#' @param sigma2 double: variance of the EWMA statistic.
#' @param L A numeric value; the control limit factor. \code{L} represents the width of 
#' the control limits in terms of multiples of the standard deviation.
#' 
#' @return A vector of the EWMA Lower Control Limit.
#' @export
#' 
#' @details
#' The choice of \code{L} affects the chart's ability to detect process shifts; 
#' larger L values set wider control limits from the center line, reducing false alarms but also 
#' potentially delaying the detection of real shifts. 
#' The LCL of the EWMA is given by:
#' \deqn{LCL(Z_i) = Z_0 - L \sigma^2(Z_i)}
#'
#' @examples 
#' x = rnorm(100)
#' aux = control_parm_ewma(x, lambda = 0.15, max.rl = 500)
#' lower_limit_ewma(t = 1:length(x), mu = aux$mu, sigma2 = aux$sigma2, L = 3)

lower_limit_ewma <- function(t, mu, sigma2, L){
  out <- mu - L*sqrt(sigma2[1:length(t)])
  return(out)
}

#' Upper Control Limits (UCL) for Exponentially Weighted Moving Average 
#'
#' @param t An integer vector; representing a sequence from 1 to 
#' n, where n denotes the number of observations used in the Exponential 
#' Weighted Moving Average. \code{t} is denoted by \code{i} in the UCL formula. 
#' See Details for more information.
#' @param mu double; process mean. Also known as the target mean or the average of the historical data.
#' @param sigma2 double: variance of the EWMA statistic.
#' @param L A numeric value; the control limit factor. \code{L} represents the width of 
#' the control limits in terms of multiples of the standard deviation.
#' 
#' @return A vector of the EWMA Upper Control Limit.
#' @export
#'
#' @details
#' The choice of \code{L} affects the chart's ability to detect process shifts; 
#' larger L values set wider control limits from the center line, reducing false alarms but also 
#' potentially delaying the detection of real shifts.
#' The UCL of the EWMA is given by:
#' \deqn{UCL(Z_i) = Z_0 + L \sigma^2(Z_i)}
#' 
#' @examples 
#' x = rnorm(100)
#' aux = control_parm_ewma(x, lambda = 0.15, max.rl = 500)
#' upper_limit_ewma(t = 1:length(x), mu = aux$mu, sigma2 = aux$sigma2, L = 3)
#' 
upper_limit_ewma <- function(t, mu, sigma2, L){
  out <-  mu + L*sqrt(sigma2[1:length(t)])
  return(out)
}