#' Control parameters for the Moving Average (MA)
#' 
#' \code{control_parm_ma} computes the essential parameters required for generating
#' the Operating Characteristics of MA quality control charts. It includes the process 
#' mean and the variance of MA statistic when the process is under control.
#'
#' @param x A numeric vector of individual observations obtained from the process.
#' In some cases, \code{x} can also indicate sample averages according to a specified sampling plan.
#' @param omega integer; refers to the the window size for MA charts that determines how many consecutive observations are weighted equally. For example, with Omega = 10, each of the last 10 observations receives equal weight of 0.1 (⅒). This parameter directly impacts the chart’s smoothing behavior and sensitivity to process changes.
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
  
  if (any(is.na(x))) {
    stop("Input vector 'x' contains NA values. Please ensure all missing values are removed or replaced before using this function.")
  }
  
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
#' mean and the variance of EWMA statistic when the process is under control.
#'
#' @param x A numeric vector of individual observations obtained from the process.
#' In some cases, \code{x} can also indicate sample averages according to a specified sampling plan.
#' @param lambda numeric; a weighting factor between 0 and 1 that determines how much emphasis is placed on recent versus historical data in EWMA charts. Smaller λ values give more weight to recent observations, making the chart more sensitive to small process shifts.
#' @param max.rl integer; Maximum Run Length. It refers to the expected maximum number of observations in the experiment.
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
  
  if (any(is.na(x))) {
    stop("Input vector 'x' contains NA values. Please ensure all missing values are removed or replaced before using this function.")
  }
  
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
