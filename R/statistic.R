
#' Exponentially Weighted Moving Average statistic
#'
#' @param x A numeric vector containing the data points for which to calculate the Exponentially Weighted Moving Averages.
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
#' @examples ewma_statistic(x = 1:50, t = 1:30, lambda = 0.15, x0 = 2)
ewma_statistic <- function(x, t, lambda, x0) {
  ewma_statistic_C(x, t, lambda, x0)
}

#' Moving Average statistic
#'
#' @param x A numeric vector containing the data points for which to calculate the Moving Averages.
#' @param t An integer vector; representing a sequence from 1 to 
#' n, where n denotes the number of observations used in the Exponential 
#' Weighted Moving Average.
#' @param omega integer; the weighting factor of MA charts.
#'
#' @return A numeric vector of Moving average statistic.
#' @export
#'
#' @examples ma_statistic(x = 1:50, t = 1:30, omega = 0.15)
ma_statistic <- function(x, t, omega) {
  ma_statistic_C(x, t, omega)
}

