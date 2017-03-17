
setwd("/Users/peterkuriyama/School/Research/hlsimulator")

library(devtools)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(doParallel)
library(parallel)
library(foreach)
library(stringr)
#--------------------------------------------------------------------------------------------
#May need to track depletion by drop at some points, this is in conduct_survey
#--------------------------------------------------------------------------------------------
#Options to load the package

#From github straight
install_github('peterkuriyama/hlsimulator')
library(hlsimulator)


#----------------------------------------------------------------------------------------
# What range of catch per hooks provides a relative index of abundance?
# What range of hooks without an aggressive species provides a relative index of abundance.

#To-Do:
#1. Explore patchiness configurations
#3. Scenarios:
  #Increasing/decreasing Trend
  #Gear saturation
  #Aggressive behavior

#Default locations, 15% of available sites

#--------------------------------------------------------------------------------------------
#Define default locations that will be used for other scenarios
# set.seed(3)
# locs <- expand.grid(1:10, 1:10)
# locs$vessel <- 1
# names(locs)[1:2] <- c('x', 'y')
# locs <- locs[, c('vessel', 'x', 'y')]

# samps <- base::sample(1:100, 15)
# def_locs <- locs[samps, ]

#--------------------------------------------------------------------------------------------
#Add option to distribute fish with beta distribution
#normal distribution, 10, 10
#Equal high and low, 1, 1
#Few sites with many fish, .1, 10, very patchy
#Many sites with many fish c(3, 1) - approaches uniform numbers of fish

#--------------------------------------------------------------------------------------------
 