#mega run in computer lab

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

if(Sys.info()['sysname'] == 'Darwin' & nncores == 22){
  setwd("/Users/fish/Desktop/peter")
}

#Mac
if(Sys.info()['sysname'] == 'Darwin' & nncores != 22){
  setwd("/Users/peterkuriyama/School/Research/hlsimulator")  
  type <- 'mac'
  results_dir <- "/Volumes/udrive/hlsimulator_runs"
}

if(Sys.info()['sysname'] == 'Windows'){
  setwd("C://Users//Peter//Desktop//hlsimulator")
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
shape_list1$for_plot <- c('Left Skew', 'Right Skew', 'Normal', 'Uniform', 'Patchy')

ctl1 <- make_ctl(distribute = 'beta', mortality = 0, move_out_prob = .05, 
      nfish1 = 100000,
      nfish2 = 0, prob1 = .01, prob2 = .05, nyear = 2, scope = 0, seed = 1,
      location = data.frame(vessel = 1, x = 1, y = 1), numrow = 30, numcol = 30,
      shapes = c(.1, .1) , max_prob = 0, min_prob = 0, comp_coeff = .5, niters = 1)    

fishes1 <- seq(0, 200000, by = 20000)
fishes2 <- seq(0, 200000, by = 20000)
nsites <- 50

#Number of repetitions is important
nreps <- 10

#--------------------------------------------------------------------------------------------
#Build the grid of things to loop over
to_loop <- expand.grid(fishes1, fishes2, c(.3, .5, .7),
  1:5, c('pref', 'rand'))
names(to_loop) <- c('nfish1', 'nfish2', 'comp_coeff', 
  'shape_list_row', 'type')
to_loop$nsites <- nsites
to_loop$nreps <- nreps

#--------------------------------------------------------------------------------------------

#Specify the number of fish1, the number of sites to sample,
#the number of replicates
#the shape of the initial distribution

dd <- fixed_parallel(index = 3000, ctl1 = ctl1)

if(sys == 'Windows'){
  registerDoParallel(nncores)

  thing1_outs <- foreach(ii = 3000:3005, 
    .packages = c('plyr', 'dplyr', 'reshape2'), .export = c('ctl1')) %dopar%
    fixed_parallel(index = ii, ctl1 = ctl1)

  stopImplicitCluster()

} 