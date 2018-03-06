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
    #source("fxfind.R")

  # compute equilibrium zbar
    source("equil_zbar.R")

  # generate tex outputs
    source("texout.R")

# share of workers in sector Y
eqvars2[[1]]
# trade as a share of total GDP
eq_tradegdp[[1]]
