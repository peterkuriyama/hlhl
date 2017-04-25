#Initialize mega runs

#mega run in computer lab
# install.packages('devtools')
# install.packages("sendmailR")

#--------------------------------------------------------------------------------------------
#Load Packages
library(devtools)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(doParallel)
library(parallel)
library(foreach)
library(stringr)
library(sendmailR)

#--------------------------------------------------------------------------------------------
#Update directory

#Automatically detect # of cores
nncores <- detectCores() - 2

#Big Lab Mac
if(Sys.info()['sysname'] == 'Darwin' & nncores == 22){
  #Make sure to login to 
  setwd("/Users/fish/Desktop/peter")
  results_dir <- "/Volumes/udrive/hlsimulator_runs"
  ##Make sure that udrive is functional
}

#My Laptop Mac
if(Sys.info()['sysname'] == 'Darwin' & nncores != 22){
  setwd("/Users/peterkuriyama/School/Research/hlsimulator")  
  type <- 'mac'
  results_dir <- "/Volumes/udrive/hlsimulator_runs"
}

#Whitefish
if(Sys.info()['sysname'] == 'Windows' & nncores == 10){
  results_dir <- "C://Users//Peter//Desktop//hlsimulator"
}

#Smaller Lab computers, save to UDRIVE
if(Sys.info()['sysname'] == 'Windows' & nncores < 10){
  # setwd("C://Users//Peter//Desktop//hlsimulator")
  results_dir <- "Z://hlsimulator_runs"
}

#Big Lab computer, save to UDRIVE
if(Sys.info()['sysname'] == 'Windows' & nncores > 11){
  nncores <- 20
  #Specify somehing here, I think it's U
  results_dir <- "U://hlsimulator_runs"
}

#--------------------------------------------------------------------------------------------
#May need to track depletion by drop at some points, this is in conduct_survey
#--------------------------------------------------------------------------------------------
#From github straight
install_github('peterkuriyama/hlsimulator')
library(hlsimulator)

#----------------------------------------------------------------------------------------
# What range of catch per hooks provides a relative index of abundance?
# What range of hooks without an aggressive species provides a relative index of abundance.

#--------------------------------------------------------------------------------------------
#Define scenarios for all simulations
shape_list1 <- data.frame(scen = c('leftskew', 'rightskew', 'normdist', 'uniform', 'patchy'),
  shapes1 = c(10, 1, 5, 1, .1),
  shapes2 = c(1 , 10 ,5, 1, 10))
shape_list1$for_plot <- c('Left Skew', 'Right Skew', 'Symmetric', 'Uniform', 'Patchy')

#Only run for patchy and normal
# shape_list1 <- subset(shape_list1, scen %in% c('normdist', 'patchy'))

#Keep the same prob1 and prob2
ctl1 <- make_ctl(distribute = 'beta', mortality = 0, move_out_prob = .05, 
      nfish1 = 100000,
      nfish2 = 0, prob1 = .01, prob2 = .01, nyear = 2, scope = 0, seed = 1,
      location = data.frame(vessel = 1, x = 1, y = 1), numrow = 30, numcol = 30,
      shapes = c(.1, .1) , max_prob = 0, min_prob = 0, comp_coeff = .5, niters = 1)    

#--------------------------------------------------------------------------------------------
#Build grid of things to loop over
fishes1 <- seq(0, 200000, by = 20000)
fishes2 <- seq(0, 200000, by = 20000)
comp_coeffs <- c(.3, .5, .7)
shape_rows <- c(3, 5) #Normal and patchy distributions
nsites <- 50

#Build the grid of things to loop over
to_loop <- expand.grid(fishes1, fishes2, comp_coeffs,
  shape_rows, c('pref', 'rand'))
names(to_loop) <- c('nfish1', 'nfish2', 'comp_coeff', 
  'shape_list_row', 'type')
to_loop$nsites <- nsites

#remove the rows with 0 and 0 for numbers of fish
to_loop <- to_loop[-which(to_loop$nfish1 == 0 & to_loop$nfish2 == 0), ]
