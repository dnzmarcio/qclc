#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector ewma_statistic_C(NumericVector x, IntegerVector t, double lambda, double x0) {
  
  int n = t.size();
  NumericVector out(n);
  
  for (int i = 0; i < n; i++){

    double z0 = x0*pow((1-lambda), t[i]);
    double z = 0;  
    
    for(int j = 0; j < t[i]; j++){
     z += pow((1-lambda),j)*x[(t[i]-(j+1))]; 
    }
  
    out[i] = z0 + lambda*z;
  }
  
  return out;
}

// [[Rcpp::export]]
NumericVector ma_statistic_C(NumericVector x, IntegerVector t, int omega) {
  
  int n = t.size();
  NumericVector out(n);
  
  for (int i = 0; i < n; i++){
    double z = 0;  
    
    if (t[i] >= omega){
      for(int j = (t[i]-omega); j < t[i]; j++){
        z += x[j]/omega; 
      }
    } else {
      for(int j = 0; j < t[i]; j++){
        z += x[j]/t[i]; 
      }
    }
    
    out[i] = z;
  }
  
  return out;
}




