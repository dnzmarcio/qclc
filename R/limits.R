#' Moving Average Lower limit
#'
#' @param t
#' @param omega
#' @param mu
#' @param sigma2
#' @param L
#'
#' @return
#' @export
#'
#' @examples
lower_limit_ma <- function(t, omega, mu, sigma2, L){
  temp <- ifelse(t < omega, sigma2[t], sigma2[length(sigma2)])
  out <- mu - L*sqrt(temp)
  return(out)
}

#' Moving Average Upper limit
#'
#' @param t
#' @param omega
#' @param mu
#' @param sigma2
#' @param L
#'
#' @return
#' @export
#'
#' @examples
upper_limit_ma <- function(t, omega, mu, sigma2, L){
  temp <- ifelse(t < omega, sigma2[t], sigma2[length(sigma2)])
  out <- mu + L*sqrt(temp)
  return(out)
}

#' Exponentially Weighted Moving Average Lower limit
#'
#' @param t
#' @param omega
#' @param mu
#' @param sigma2
#' @param L
#'
#' @return
#' @export
#'
#' @examples
lower_limit_ewma <- function(t, mu, sigma2, L){
  out <- mu - L*sqrt(sigma2[1:length(t)])
  return(out)
}

#' Exponentially Weighted Moving Average Upper limit
#'
#' @param t
#' @param omega
#' @param mu
#' @param sigma2
#' @param L
#'
#' @return
#' @export
#'
#' @examples
upper_limit_ewma <- function(t, mu, sigma2, L){
  out <-  mu + L*sqrt(sigma2[1:length(t)])
  return(out)
}