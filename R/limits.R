#' Lower Control Limits (LCL) for Moving Average 
#'
#' @param t An integer vector; representing a sequence from 1 to 
#' n, where n denotes the sample size used for calculating the Moving Average 
#' (MA) statistic.
#' @param omega An integer value
#' @param mu double; target mean.
#' @param sigma2 double: variance of the Moving Average statistic.
#' @param L integer: the control limit factor; represents the width of the control 
#' limits in terms of multiples of the standard deviation. 
#'
#' @return MA Lower Limit
#' @export
#'
#' @examples lower_limit_ma(t = 1:30, omega =10, mu = 2, sigma2 = 4, L = 1)
#' 
lower_limit_ma <- function(t, omega, mu, sigma2, L){
  temp <- ifelse(t < omega, sigma2[t], sigma2[length(sigma2)])
  out <- mu - L*sqrt(temp)
  return(out)
}

#' Upper Control Limits (UCL) for Moving Average
#'
#' @param t An integer vector; representing a sequence from 1 to 
#' n, where n denotes the sample size used for calculating the Moving Average 
#' (MA) statistic.
#' @param omega An integer value
#' @param mu double; target mean.
#' @param sigma2 double: variance of the MA statistic.
#' @param L integer: the control limit factor; represents the width of the control 
#' limits in terms of multiples of the standard deviation. 
#'
#' @return MA Upper Limit
#' @export
#'
#' @examples upper_limit_ma(t = 1:30, omega =10, mu = 2, sigma2 = 4, L = 1)
#'
upper_limit_ma <- function(t, omega, mu, sigma2, L){
  temp <- ifelse(t < omega, sigma2[t], sigma2[length(sigma2)])
  out <- mu + L*sqrt(temp)
  return(out)
}

#' Lower Control Limits (LCL) for Exponentially Weighted Moving Average 
#'
#' @param t An integer vector; representing a sequence from 1 to 
#' n, where n denotes the sample size used for calculating the Exponentially 
#' Weighted Moving Average (EWMA) statistic.
#' @param mu double; target mean.
#' @param sigma2 double: variance of the EWMA statistic.
#' @param L integer: the control limit factor; represents the width of the control limits in terms of multiples of the standard deviation.
#'          In EWMA charts, control limits are set based on the standard deviation of the EWMA statistic, which incorporates the smoothing constant 
#'          \code{lambda}. \code{L} determines how wide the control limits are set from the center line.
#' @return EWMA Lower Limit
#' @export
#'
#' @examples lower_limit_ewma(t = 1:30, mu = 2, sigma2 = 4, L = 1)

lower_limit_ewma <- function(t, mu, sigma2, L){
  out <- mu - L*sqrt(sigma2[1:length(t)])
  return(out)
}

#' Upper Control Limits (UCL) for Exponentially Weighted Moving Average 
#'
#' @param t An integer vector; representing a sequence from 1 to 
#' n, where n denotes the sample size used for calculating the Exponentially 
#' Weighted Moving Average (EWMA) statistic.
#' @param mu double; target mean.
#' @param sigma2 double: variance of the EWMA statistic.
#' @param L integer: the control limit factor; represents the width of the control limits in terms of multiples of the standard deviation.
#'          In EWMA charts, control limits are set based on the standard deviation of the EWMA statistic, which incorporates the smoothing constant 
#'          \code{lambda}. \code{L} determines how wide the control limits are set from the center line.
#' @return EWMA Upper Limit
#' @export
#'
#' @examples upper_limit_ewma (t = 1:30, mu = 2, sigma2 = 4, L = 1)
#' 
upper_limit_ewma <- function(t, mu, sigma2, L){
  out <-  mu + L*sqrt(sigma2[1:length(t)])
  return(out)
}