#' qclc: Operating Characteristics for Quality Control Limit Charts
#' 
#' @docType package 
#' @name qclc
#' 
#' @aliases qclc
#' 
#' 
#' @description
#' qclc is an R package for analyzing and generating the operating characteristics of 
#' quality control limit charts, including Moving Averages (MA) and
#' exponentially weighted moving averages (EWMA). It supports
#' monitoring of both in-control and out-of-control processes.
#'
#' }
#' 
#' @section Functions:
#' \describe{
#'   \item{\code{control_parm_ma}}{Computes the process mean and variance essential for determining the operating characteristics of a Moving Average (MA).}
#'   \item{\code{control_parm_ewma}}{Computes the process mean and variance essential for determining the operating characteristics of a Exponential Weighted Moving Average (EWMA).}
#'   \item{\code{arl_ma}}{Implements bootstrap-based Average Run Length (ARL) calculations for MA based on a specified control limit factor. This function helps assess the performance of the MA chart for monitoring process stability and detecting shifts in the process mean.}
#'   \item{\code{arl_ewma}}{Implements bootstrap-based Average Run Length (ARL) calculations for EWMA based on a specified control limit factor. This function helps assess the performance of the MA chart for monitoring process stability and detecting shifts in the process mean.}
#'   \item{\code{ma_statistic}}{Calculates moving average values.}
#'   \item{\code{ewma_statistic}}{ Calculates exponentially weighted moving averages.}
#'   \item{\code{lower_limit_ma}}{Computes lower control boundaries for MA charts.}
#'   \item{\code{upper_limit_ma}}{Computes upper control boundaries for MA charts.}
#'   \item{\code{lower_limit_ewma}}{Computes lower control boundaries for EWMA charts.}
#'   \item{\code{upper_limit_ewma}}{Computes upper control boundaries for EWMA charts.}
#'   
#' }
#' 
#' @details
#' The main functions in qclc are `ma_statistic()` for Moving Average analysis and 
#' `ewma_statistic()` for Exponentially Weighted Moving Average analysis. These functions
#' allow users to specify control limits and generate relevant statistics to help
#' in quality assessment and control processes.
#' 
#' @export

