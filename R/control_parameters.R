#' Control parameters for the Moving Average (MA)
#' 
#' \code{control_parm_ma} computes the essential parameters required for generating
#' the Operating Characteristics of MA quality control charts. It includes the process 
#' mean and the variance of MA statistic when the process is in control.
#'
#' @param x A numeric vector of individual observations obtained from the process.
#' In some cases, \code{x} can also indicate sample averages according to a specified sampling plan.
#' @param omega integer; the weighting factor of MA charts.
#' 
#' @return A list containing the following components:
#' \itemize{
#'   \item \code{mu}: The process mean. Also known as the target mean or the average of the historical data \code{x}.
#'   \item \code{sigma2}: The variance of MA statistic.
#'   \item \code{omega}: the weighting factor of MA charts.
#' }
#' 
#'
#' @details
#' The Variance of the MA statistic \code{sigma2} is given by: 
#' \deqn{\sigma^2(t) = \left\{ 
#' \begin{array}{ll} 
#' \frac{\text{var}(x)}{n \cdot \omega} & t \geq \omega \\\\
#' \frac{\text{var}(x)}{n \cdot t} & \text{otherwise}
#' \end{array} 
#' \right.}
#' If \code{x} is provided as an input vector to \code{control_parm_ma()}, \code{n} is regarded as 1,
#' else \code{n} indicates number of columns of a given matrix.
#' 
#' The Process Mean \code{mu} is the average of the individual observations obtained from \code{X}:
#' \deqn{\bar{X} = \frac{\sum_{j=0}^{n}X_j}{n}}
#' 
#' @seealso \code{\link{arl_ma}}, \code{\link{ma_statistic}},
#' \code{\link{lower_limit_ma}}, \code{\link{upper_limit_ma}}  
#' 
#' @import stats
#' @examples
#' x <- rnorm(100)
#' omega <- 10
#' control_parm_ma(x, omega)
#'
#' @export
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
#' \code{control_parm_ewma} computes the essential parameters required for generating
#' the Operating Characteristics of EWMA quality control charts. It includes the process 
#' mean and the variance of EWMA statistic when the process is in control.
#'
#' @param x A numeric vector of individual observations obtained from the process.
#' In some cases, \code{x} can also indicate sample averages according to a specified sampling plan.
#' @param lambda A numeric value between 0 and 1 inclusive that indicates the weighting 
#' factor of EWMA charts.It determines the weight given to recent data points. 
#' A smaller \code{lambda} gives more weight to recent observations, making the 
#' chart more sensitive to shifts.
#' @param max.rl integer; Maximum Run Length. It refers to the total number of 
#' observations to be collected from \code{x} in each process replicate. \code{max.rl} 
#' is denoted by \code{i} in the EWMA variance formula. See Details for more information.
#' 
#' @return A list containing the following components:
#' \itemize{
#'   \item \code{mu}: The process mean. Also known as the target mean or the average of the historical data \code{x}.
#'   \item \code{sigma2}: The variance of EWMA statistic.
#'   \item \code{lambda}: the weighting factor of EWMA charts.
#' }
#' 
#' 
#' @details The variance of the EWMA statistic \code{sigma2} is given by:
#' \deqn{\sigma^2(Z_i) = \left[(1-(1-\lambda)^{2i})\frac{{\lambda}}{{2-\lambda}}\right]\sigma^2_X}
#' The Process Mean \code{mu} is the average of the individual observations obtained from \code{X}:
#' \deqn{\bar{X} = \frac{\sum_{j=0}^{n}X_j}{n}}
#' 
#' @seealso \code{\link{arl_ewma}}, \code{\link{ewma_statistic}},
#' \code{\link{lower_limit_ewma}}, \code{\link{upper_limit_ewma}}  
#' 
#' @import stats
#' 
#' @examples
#' x <- rnorm(100)
#' lambda <- 0.2
#' max.rl <- 500
#' control_parm_ewma(x, lambda, max.rl)
#'
#' @export

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
  return(out)
}
