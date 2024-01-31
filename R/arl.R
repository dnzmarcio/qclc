#' Average Length Run for Moving Average 
#'
#' @param x
#' @param mu
#' @param sigma2
#' @param L
#' @param omega
#' @param max.arl
#' @param arl
#' @param nboot
#' @param ncores
#' @param seed
#'
#' @return
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


#' Average Length Run for Exponentially Weighted Moving Average
#'
#' @param x
#' @param mu
#' @param sigma2
#' @param L
#' @param omega
#' @param max.arl
#' @param arl
#' @param nboot
#' @param ncores
#' @param seed
#'
#' @return
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