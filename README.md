# qclc

An [R](https://www.r-project.org/) package to generate the Operating Characteristics of ordinary Moving Average (MA) and Exponentially Weighted Moving Average (EWMA) quality control limit charts; supporting the monitoring of both in-control and out-of-control processes.

## Table of Contents 

[Introduction](#introduction)<br>
[Functions](functions)<br>
[Installtion Instructions](#installation-instructions)<br>
[Usage](#usage)<br>
[Integration in Shiny](#integration-in-shiny)<br>
[References](#references)<br>

## Introduction 

The **qclc** package serves as a statistical resource specifically tailored for MA and EWMA analyses, enabling you to:

- Produce Operating Characteristics for in-control processes, including Average Run Length and the rate of False Positives.
- Produce Operating Characteristics for out-of-control processes, focusing on Average Run Length and the rate of True Positives.
- Compute both MA and EWMA statistics.
- Establish both the upper and lower control limits for the respective charts.

## Functions

`control_parm_ma():` calculates the  process mean and variance needed for the Operating Characteristics of MA.

`control_parm_ewma():` computes the process mean and variance required for generating the Operating Characteristics of EWMA.

`arl_ma():` estimates the Operating Characteristics for the the MA.

`arl_ewma():` estimates the Operating Characteristics for the the EWMA.

`ma_statistic():` calculates moving averages for a provided time series vector, 
using a defined window size.

`ewma_statistic():` computes exponentially weighted moving averages for a given time series.

`lower_limit_ma():` generates the Lower Control Limits for the MA.

`upper_limit_ma():` generates the Upper Control Limits for the MA.

`lower_limit_ewma():` generates the Lower Control Limits for the EWMA.

`upper_limit_ewma():` generates the Upper Control Limits for the EWMA.

## Installtion Instructions

You can install the development version of **qclc** from Github:

``` r
# install.packages("devtools")
devtools::install_github("dnzmarcio/qclc", dependencies = TRUE, ref = "dev")
```

## Usage
``` r
library(qclc)

## Moving Averages

### Calculating Process Mean and Variance
x <- rnorm(100)
omega <- 10
aux <- control_parm_ma(x, omega)
aux

### Calibration
#### process under control
max.rl = 110
arl_ma(x, mu = aux$mu, aux$sigma2, omega = aux$omega, 
        L = seq(1, 3, by = 0.25), 
        max.rl = 110, rl = 100, 
        scenario = 0, nboot = 100, 
        ncores = 1, seed = 1234)
        
#### process out of control 
x.shift = x + 0.5*(sqrt(var(x)))
arl_ma(x.shift, mu = aux$mu, aux$sigma2, omega, 
        L = seq(1, 3, by = 0.25), 
        max.rl = 110, rl = 100, 
        scenario = 1, nboot = 100, 
        ncores = 1, seed = 1234)
        

### Moving Average Statistic
ma_statistic(x, t = 1:length(x), omega = 10)

### Control Limits for MA
lower_limit_ma(t = 1:length(x), omega =10, mu = aux$mu, sigma2 = aux$sigma2, L = 3)
upper_limit_ma(t = 1:length(x), omega =10, mu = aux$mu, sigma2 = aux$sigma2, L = 3)
```

## Integration in Shiny

The features of the **qclc** package have been incorporated into an R Shiny application, which is available for viewing and can be download from [R Shiny](https://github.com/KHBHH/RShiny_qclc).

## References