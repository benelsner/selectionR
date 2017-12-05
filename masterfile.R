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
    #source("plots.R")

  # find fixed costs #  attention: resource-heavy code; 
    source("fxfind.R")

  # compute equilibrium zbar
    source("equil_zbar.R")

  # generate tex outputs
    source("texout.R")

eqvars1[[1]][1,2]/eqvars1[[1]][1,1]
eqvars2[[1]]
eq_expgdp[[1]]
eq_tradegdp

#zbar_opt
    
  # outputs
  #equil(c(0.6,0.75), cbind(skills[[1]][,1],skills[[2]][,1]) , vars)