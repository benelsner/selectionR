####################################################
# Biavaschi & Elsner
# Let's be selective about migrant self-selection
# Revisions for JIE
# Master file
####################################################
# packages

library(pracma)
library(ggplot2)
library(gdata)
library(plyr)
library(readxl)
library(plotly)
library(xtable)

#---------------------------------------------------
# R scripts

  # programs -> all functions to run the model
    source("functions.R")

  # data -> skill distributions, GDP
    source("data.R")

  # distributional plots (to check if data make sense)
    source("plots.R")

  # find fixed costs
    source("findfx.R")

  # compute equilibrium zbar
    source("equil_zbar.R")

  # generate tex outputs
    source("texout.R")

eqvars2[[1]]
eq_expgdp[[1]]

#zbar_opt
    
  # outputs
  #equil(c(0.6,0.75), cbind(skills[[1]][,1],skills[[2]][,1]) , vars)