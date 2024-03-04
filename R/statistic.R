
#' Exponentially Weighted Moving Average statistic
#'
#' @param x A numeric vector
#' @param t An integer vector
#' @param lambda A numeric value between 0 and 1 inclusive that indicates the smoothing constant of EWMA charts.
#'               It determines the weight given to recent data points. A smaller \(\lambda\) gives more weight to recent observations,
#'               making the chart more sensitive to shifts.
#' @param x0 numeric: target mean. 
#'
#' @return A vector of EWMA
#' @export
#'
#' @examples ewma_statistic(x = 1:50, t = 1:30, lambda = 0.15, x0 = 2)
ewma_statistic <- function(x, t, lambda, x0) {
  ewma_statistic_C(x, t, lambda, x0)
}

#' Moving Average statistic
#'
#' @param x A numeric vector
#' @param t An integer vector
#' @param omega An integer value
#'
#' @return A vector of MA
#' @export
#'
#' @examples ma_statistic(x = 1:50, t = 1:30, omega = 0.15)
ma_statistic <- function(x, t, omega) {
  ma_statistic_C(x, t, omega)
}

