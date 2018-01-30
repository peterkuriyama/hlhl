#Initialize mega runs for 150 hooks

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

#Specify results directory
results_dir <- "C://Users//Peter//Dropbox//phd//research//hlsimulator//output"

#--------------------------------------------------------------------------------------------
#Update directory

#Automatically detect # of cores
nncores <- detectCores() - 2

#Big Lab Mac
if(Sys.info()['sysname'] == 'Darwin' & nncores == 22){
  #Make sure to login to 
  results_dir <- "/Volumes/udrive/hlsimulator_runs"
  sys <- 'mac'
  nncores <- 20
  ##Make sure that udrive is functional
}

#My Laptop Mac
if(Sys.info()['sysname'] == 'Darwin' & nncores < 10){
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
  sys <- 'pc'
}

#--------------------------------------------------------------------------------------------
#May need to track depletion by drop at some points, this is in conduct_survey
#--------------------------------------------------------------------------------------------
#From github straight
# install_github('peterkuriyama/hlsimulator')
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

#Specify Number of hooks
num_hooks <- 10 # num_hooks * 150
hook_run <- num_hooks * 15

##Double the number of hooks##
ctl1 <- make_ctl(distribute = 'beta', mortality = 0, move_out_prob = .05, 
      nfish1 = 100000,
      nfish2 = 0, prob1 = .01, prob2 = .01, nyear = 1, scope = 0, seed = 1,
      location = data.frame(vessel = 1, x = 1, y = 1), numrow = 30, numcol = 30,
      shapes = c(.1, .1) , max_prob = 0, min_prob = 0, comp_coeff = .5, niters = 1, 
      nhooks = num_hooks)   

#--------------------------------------------------------------------------------------------
#Functions to create to_loop values
#Function to that returns rounded numbers of fish1 at evenly spaced proportions
calc_fish1_prop <- function(nfish2, prop = seq(0, .9, .1)){
  fishes <- prop * nfish2 / (1 - prop)
  fishes <- round(fishes, digits = 0)
  return(fishes)
}

#Function to create to_loop data frame
create_to_loop <- function(fishes1, fishes2, comp_coeffs = c(.3, .5, .7),
  shape_rows = c(3, 5), nsites = 50){

  to_loop <- expand.grid(fishes1, fishes2, comp_coeffs, shape_rows, c('pref', 'rand'))
  names(to_loop) <- c('nfish1', 'nfish2', 'comp_coeff', 
    'shape_list_row', 'type')
  to_loop$nsites <- nsites
  to_loop$c1_sum <- .01
  return(to_loop)
}

#--------------------------------------------------------------------------------------------
#To loop Key
# 1 - leftskew
# 2 - rightskew
# 3 - normdist
# 4 - uniform
# 5 - patchy

#--------------------------------------------------------------------------------------------
#0 - 200,000 in increments of 20,000
fishes1 <- seq(0, 200000, by = 20000)
# fishes2 <- seq(0, 200000, by = 20000)
fishes2 <- seq(0, 0, by = 0)

to_loop <- create_to_loop(fishes1 = fishes1, fishes2 = fishes2, comp_coeffs = .5,
  shape_rows = 5, nsites = 50)
#remove the rows with 0 and 0 for numbers of fish
to_loop <- to_loop[-which(to_loop$nfish1 == 0 & to_loop$nfish2 == 0), ]

#--------------------------------------------------------------------------------------------
#Only do 
#If there is a check data file there, remove it before running this again
file.remove(paste0(results_dir, '//',  'twospp1_newcc_check_5.Rdata'))

#For testing the new comp coefficient curves
nreps <- 500

#Adjust number of reps
to_loop$nreps <- nreps

#--------------------------------------------------------------------------------------------

#Create indices for each computer, plan is to do this on five computers
tot <- 1:nrow(to_loop)

#Specify one run for each core
tots <- split(tot, ceiling(seq_along(tot) / (nrow(to_loop) / ((nrow(to_loop) / nncores)))))

#Specify Index for each computer
#-----------------
run_this_ind <- 1:2

if(length(run_this_ind) == 1) to_run <- tots[[run_this_ind]]
if(length(run_this_ind) > 1){
  to_run <- unlist(tots[run_this_ind])
  names(to_run) <- NULL
} 

#--------------------------------------------------------------------------------------------
start_time <- Sys.time()

clusters <- parallel::makeCluster(nncores)
doParallel::registerDoParallel(clusters)

twospp <- foreach(ii = to_run,
  .packages = c('plyr', 'dplyr', 'reshape2', 'hlsimulator'), .export = c("shape_list1")) %dopar% {
    fixed_parallel(index = ii, ctl1 = ctl1, to_loop = to_loop, 
      change_these = c('nfish1', 'nfish2', 'comp_coeff'))  
}

#Close clusters
stopCluster(clusters)

#Record run time
run_time <- Sys.time() - start_time

#Format output
site_cpues <- lapply(twospp, FUN = function(x) x[[2]])
twospp <- lapply(twospp, FUN = function(x) x[[1]])

twospp <- ldply(twospp)  

if(length(run_this_ind) > 1) run_this_ind <- paste(run_this_ind, collapse = "")

# assign(paste0("onespp_150_hooks"), twospp)
onespp <- twospp

#From previous runs
# filename <- paste0("twospp", run_this_ind )

#Run now, run2
filename <- paste0("onespp", "_", hook_run, "_hooks") #for new competition coefficient

#Save output in U drive
save(list = onespp, file = paste0(results_dir, "//" , paste0(filename, nreps, '.Rdata')))

#--------------------------------------------------------------------------------------------
#process the data


#For 150 hooks
load('output/onespp_150_hooks500.Rdata')
onespp <- onespp_150_hooks
onespp <- onespp %>% filter(spp == 'spp1')
onespp$dep <- onespp$nfish_orig / 200000


#
onespp %>% filter(iter == 1, init_dist == 'patchy', type == 'pref') 

ggplot(onespp, aes(x = dep, y = cpue)) + geom_point() + facet_wrap(~type)






