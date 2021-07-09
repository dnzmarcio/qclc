
#' Exponentially Weighted Moving Average statistic
#'
#' @param x
#' @param t
#' @param lambda
#' @param x0
#'
#' @return
#' @export
#'
#' @examples
ewma_statistic <- function(x, t, lambda, x0) {
  ewma_statistic_C(x, t, lambda, x0)
}

#' Moving Average statistic
#'
#' @param x
#' @param t
#' @param omega
#'
#' @return
#' @export
#'
#' @examples
ma_statistic <- function(x, t, omega) {
  ma_statistic_C(x, t, omega)
}

