### Control parameters

control_parm_ma <- function(x, omega){
  
  t <- 1:omega
  
  if (is.matrix(x)){
    n <- ncol(x)
    x <- rowMeans(x)
  } else {
    n <- 1
  }
  
  mu <- mean(x)
  
  sigma2 <- ifelse(t >= omega, var(x)/(n*omega), var(x)/(n*t))
  out <- list(mu = mu, sigma2 = sigma2, omega = omega)
  return(out)
}

control_parm_ewma <- function(x, lambda, max.arl){
  
  if (is.matrix(x)){
    n <- ncol(x)
    x <- rowMeans(x)
  } else {
    n <- 1
  }
  
  mu <- mean(x)
  sigma2 <- (var(x)/n)*(lambda/(2 - lambda))*(1 - (1 - lambda)^(2*(1:max.arl)))
  
  out <- list(mu = mu, sigma2 = sigma2, lambda = lambda)
}