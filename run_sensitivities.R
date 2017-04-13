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

#Sensitivity to prob1
#--------------------------------------------------------------------------------------------
#Define scenarios for all simulations
shape_list1 <- data.frame(scen = c('leftskew', 'rightskew', 'normdist', 'uniform', 'patchy'),
  shapes1 = c(10, 1, 5, 1, .1),
  shapes2 = c(1 , 10 ,5, 1, 10))
shape_list1$for_plot <- c('Left Skew', 'Right Skew', 'Normal', 'Uniform', 'Patchy')

#Keep the same prob1 and prob2
ctl1 <- make_ctl(distribute = 'beta', mortality = 0, move_out_prob = .05, 
      nfish1 = 100000,
      nfish2 = 0, prob1 = .01, prob2 = .01, nyear = 2, scope = 0, seed = 1,
      location = data.frame(vessel = 1, x = 1, y = 1), numrow = 30, numcol = 30,
      shapes = c(.1, .1) , max_prob = 0, min_prob = 0, comp_coeff = .5, niters = 1)    

fishes1 <- seq(0, 200000, by = 20000)
nsites <- 50

#Number of repetitions is important
nreps <- 50

#--------------------------------------------------------------------------------------------
#Build the grid of things to loop over
prob1s <- seq(.01, .1, by = .01)

to_loop <- expand.grid(fishes1, prob1s, c(.3, .5, .7),
  1:5, c('pref', 'rand'))
names(to_loop) <- c('nfish1', 'prob1', 'comp_coeff', 
  'shape_list_row', 'type')
to_loop$nsites <- nsites
to_loop$nreps <- nreps

#--------------------------------------------------------------------------------------------
#To Do for lab computers

#Create indices for each computer, plan is to do this on five computers
tot <- 1:nrow(to_loop)
tots <- split(tot, ceiling(seq_along(tot) / nrow(to_loop)))

# tots <- split(tot, ceiling(seq_along(tot) / 605)) #break this up into six

#Specify Index for each computer
#-----------------
run_this_ind <- 1

# run_this_ind <- 1:2
#-----------------

if(length(run_this_ind) == 1) to_run <- tots[[run_this_ind]]
if(length(run_this_ind) > 1){
  to_run <- unlist(tots[run_this_ind])
  names(to_run) <- NULL
} 

start_time <- Sys.time()

clusters <- parallel::makeCluster(nncores)
doParallel::registerDoParallel(clusters)

twospp <- foreach(ii = to_run,
  .packages = c('plyr', 'dplyr', 'reshape2', 'hlsimulator'), .export = c("shape_list1")) %dopar% {
    fixed_parallel(index = ii, ctl1 = ctl1, to_loop = to_loop, 
      change_these = c('nfish1', 'prob1', 'comp_coeff'))  
}

#Close clusters
stopImplicitCluster()

#Record run time
run_time <- Sys.time() - start_time

#Format output
twospp <- ldply(twospp)  

#Remove species 2
twospp <- twospp %>% filter(spp == 'spp1')

if(length(run_this_ind) > 1) run_this_ind <- paste(run_this_ind, collapse = "")

assign(paste0("twospp", run_this_ind ), twospp)
filename <- paste0("twospp", run_this_ind )

#Save output in U drive
save(list = filename, file = paste0(results_dir, "//" , paste0(filename, "_", nreps, '.Rdata')))

#Send email that run is done
send_email(body = paste(paste('run', run_this_ind, 'done'), 
  '\n', run_time, units(run_time),  '\n'))

#Clear workspace for others
rm(list = ls())
