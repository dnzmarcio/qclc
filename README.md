# qclc

An [R](https://www.r-project.org/) package to generate the Operating Characteristics of ordinary Moving Average (MA) and Exponentially Weighted Moving Average (EWMA) quality control limit charts. 

[Overview](#overview)<br>
[Functions](functions)<br>
[Installtion Instructions](#installation-instructions)<br>
[Usage](#usage)<br>
[Integration in Shiny](#integration-in-shiny)<br>
[References](#references)<br>



## Overview 


## Functions

`control_parm_ma():` calculates the key parameters needed for the Operating Characteristics of MA quality control charts, including the process mean and the variance of the MA statistic when the process is stable.

`control_parm_ewma():` computes the essential parameters required for generating the Operating Characteristics of EWMA quality control charts. It includes the process mean and the variance of EWMA statistic when the process is in control.

`arl_ma():` estimates the Operating Characteristics for the the MA.

`arl_ewma():` estimates the Operating Characteristics for the the EWMA.

`ma_statistic():` calculates moving averages for a provided time series vector, 
using a defined window size.

`ewma_statistic():` computes exponentially weighted moving averages for a given time series.

## Installtion Instructions

You can install the development version of **qclc** from Github:

``` r
# install.packages("devtools")
devtools::install_github("dnzmarcio/qclc", dependencies = TRUE, ref = "dev")
```

## Usage

## Integration in Shiny

## References