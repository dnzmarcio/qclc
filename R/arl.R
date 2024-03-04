#' Calibration parameters for MA quality control charts
#'
#' This function produces a data frame for the calibration of Moving Average 
#' charts, which includes the control limit factor \code{L}, 
#' Average Run Length \code{ARL}, and either the False Positive rate 
#' (when the process is in control) or the True Negative rate 
#' (when the process is out of control).
#'
#' @param x A numeric vector containing the data points for which to calculate the MA.
#' @param mu double; target mean.
#' @param sigma2 double; variance of the MA statistic.
#' @param L integer: the control limit factor; represents the width of the control limits in terms of multiples of the standard deviation.
#' @param omega An integer
#' @param max.arl integer; Maximum Average Run Length.
#' @param arl integer; Average Run Length.
#' @param scenario A logical evaluating to TRUE or FALSE indicating whether the process is under control \code{scenario} = 0, or the process out of control \code{scenario} = 1.
#' @param nboot Number of iterations to calculate the MA statistics
#' @param ncores integer; the number of CPU cores to use for parallel processing. 
#' @param seed integer; to initialize the Random Number Generator to a known state.
#'
#' @return This function produces a data frame for calibration, 
#' which includes the control limit factor \code{L}, Average Run Length \code{ARL}, 
#' and either the False Positive rate (when the process is in control) 
#' or the True Negative rate (when the process is out of control).
#' 
#' @export
#'
#' @examples 

arl_ma <- function(x, mu, sigma2, omega,
                   L = seq(1, 3, by = 0.25), 
                   max.arl = 10000, arl = 100,
                   scenario = 0,
                   nboot = 100, ncores = 1, seed = 1234){
  
  registerDoParallel(ncores)
  
  arl.tab <- matrix(NA, ncol = (3 + length(arl)), nrow = length(L))
  
  
  for (i in 1:length(L)){
    #print(i)
    upper <- mu + L[i]*sqrt(sigma2[c(1:(omega-1), rep(omega, (max.arl - omega + 2)))])
    lower <- mu - L[i]*sqrt(sigma2[c(1:(omega-1), rep(omega, (max.arl - omega + 2)))])
    
    arl.tab[i, 1] <- L[i]
    
    set.seed(seed)
    arl.temp <- foreach (j = 1:nboot, .combine = `c`) %dopar% {
                           x.boot <- sample(x, size = max.arl, replace = TRUE)
                           statistic <- ma_statistic_C(x = x.boot, t = 1:max.arl, omega = omega)
                           
                           aux <- which(statistic < lower | statistic > upper)[1]
                           
                           if (!is.na(aux)){
                             out <- aux
                           } else {
                             out <- max.arl
                           }
                         }
    
    arl.tab[i, 2] <- mean(arl.temp[!is.na(arl.temp)], na.rm = TRUE)
    
    arl.tab[i, 3:(3+length(arl)-1)] <-
      sapply(arl, function(t) mean(arl.temp < t | is.na(arl.temp), na.rm = TRUE))
    
    arl.tab[i, (3+length(arl))] <- length(arl.temp[!is.na(arl.temp)])
  }
  stopImplicitCluster()
  
  if (scenario == 0){
    colnames(arl.tab) <- c("L", "ARL0", paste0("FP_", arl), "nboot")
  } else {
    colnames(arl.tab) <- c("L", "ARL1", paste0("TP_", arl), "nboot")
  }
  
  return(arl.tab)
}


#' Calibration parameters for EWMA quality control charts
#'
#' This function produces a data frame for the calibration of Exponential 
#' Weighted Moving Average charts, which includes the control limit factor \code{L}, Average Run Length \code{ARL}, 
#' and either the False Positive rate (when the process is in control) 
#' or the True Negative rate (when the process is out of control).
#' 
#' @param x A numeric vector containing the data points for which to calculate the EWMA.
#' @param mu double; target mean.
#' @param sigma2 double: variance of the EWMA statistic.
#' @param L integer: the control limit factor; represents the width of the control limits in terms of multiples of the standard deviation.
#'          In EWMA charts, control limits are set based on the standard deviation of the EWMA statistic, which incorporates the smoothing constant 
#'          \code{lambda}. \code{L} determines how wide the control limits are set from the center line.
#' @param lambda A numeric value between 0 and 1 inclusive that indicates the smoothing constant of EWMA charts.
#' @param max.arl integer; Maximum Average Run Length.
#' @param arl integer; Average Run Length.
#' @param scenario A logical evaluating to TRUE or FALSE indicating whether the process is under control \code{scenario} = 0, or the process out of control \code{scenario} = 1.
#' @param nboot integer; Number of iterations to calculate the MA statistics
#' @param ncores integer; the number of CPU cores to use for parallel processing. 
#' @param seed integer; to initialize the Random Number Generator to a known state.
#'
#' @return This function produces a data frame for calibration, 
#' which includes the control limit factor \code{L}, Average Run Length \code{ARL}, 
#' and either the False Positive rate (when the process is in control) 
#' or the True Negative rate (when the process is out of control).
#' 
#' @export
#'
#' @examples 

arl_ewma <- function(x, mu, sigma2, lambda,
                     L = seq(1, 3, by = 0.25), 
                     max.arl = 10000, arl = 100,
                     scenario = 0,
                     nboot = 100, ncores = 1, seed = 1234){
  
  registerDoParallel(ncores)
  
  arl.tab <- matrix(NA, ncol = (3 + length(arl)), nrow = length(L))
  
  for (i in 1:length(L)){
    #print(i)
    upper <- mu + L[i]*sqrt(sigma2)
    lower <- mu - L[i]*sqrt(sigma2)
    
    arl.tab[i, 1] <- L[i]
    
    set.seed(seed)
    arl.temp <- foreach (j = 1:nboot, .combine = `c`) %dopar% {
                           x.boot <- sample(x, size = max.arl, replace = TRUE)
                           statistic <- ewma_statistic_C(x = x.boot, t = 1:max.arl, lambda = lambda, x0 = mu)
                           
                           aux <- which(statistic < lower | statistic > upper)[1]
                           
                           if (!is.na(aux)){
                             out <- aux
                           } else {
                             out <- max.arl
                           }
                         }
    
    arl.tab[i, 2] <- mean(arl.temp[!is.na(arl.temp)], na.rm = TRUE)
    
    arl.tab[i, 3:(3+length(arl)-1)] <- 
      sapply(arl, function(t) mean(arl.temp < t | is.na(arl.temp), na.rm = TRUE))
    
    arl.tab[i, (3+length(arl))] <- length(arl.temp[!is.na(arl.temp)])
  }
  stopImplicitCluster()
  
  if (scenario == 0){
    colnames(arl.tab) <- c("L", "ARL0", paste0("FP_", arl), "nboot")
  } else {
    colnames(arl.tab) <- c("L", "ARL1", paste0("TP_", arl), "nboot")
  }
  
  return(arl.tab)
}