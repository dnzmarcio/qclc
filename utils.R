library(tidyverse)
library(DT)
library(ntimes) # to install devtools::install_github(dnzmarcio/ntimes)
library(lubridate)
library(multcomp)
library(gamlss)
library(gamlss.tr)
library(gamlss.inf)
library(gt)
library(janitor)
library(ggeffects)
library(DescTools)
library(glue)
library(pim)
#library(PMCMRplus) see if i can figure this out
library(mice)
library(ggmice)
library(RColorBrewer)
library(scales)
library(VIM)
library(patchwork)
library(viridis)
library(heatmaply)
library(ggsignif)
#library(kableExtra) see if i can figure this out
library(plotly)

rcs <- rms::rcs
summarize <- dplyr::summarize

tidy <- broom.mixed::tidy
select <- dplyr::select

colors <- brewer.pal(8, "Set1")
colors[c(6, 8)] <- brewer.pal(8, "Accent")[c(6, 8)]
cc <- scales::seq_gradient_pal("blue", "red", "Lab")(seq(0,1,length.out=11))

helper_perc_count2 <- function(var, digits, ...){
  
  ldots <- list(...)
  
  #h <- fct_explicit_na(var, na_level = "Missing")
  h <- var
  lh <- levels(h)
  
  count <- tapply(h, h, length)
  count <- ifelse(is.na(count), 0, count)
  n <- length(h)
  perc <- 100*prop.table(count)
  perc <- ifelse(!is.finite(perc), NA, 
                 format(round(perc, digits), nsmall = digits))
  
  perc_count <- paste0(count, " (", perc, ")")
  
  lh <- c(lh, "Effective Sample Size")
  perc_count <- c(perc_count, sum(!is.na(h)))
  
  lh <- paste0("\t ", lh)
  
  out <- list(name = lh, value = perc_count)
}

helper_missing2 <- function(var, ...){
  
  ldots <- list(...)
  
  out <- list(name = "\t Effective Sample Size",
              value = sum(!is.na(var)))
  
  return(out)
}


hush <- function(code){
  sink("NUL") # use /dev/null in UNIX
  tmp = code
  sink()
  return(tmp)
}