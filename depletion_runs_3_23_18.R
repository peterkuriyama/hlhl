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
library(beepr)
library(tidyverse)

setwd("C://Users//Peter//Dropbox//phd//research//hlsimulator//")
setwd("/Users/peterkuriyama/Dropbox/phd/research/hlsimulator")
#--------------------------------------------------------------------------------------------
#Update directory

#Automatically detect # of cores
nncores <- detectCores() - 2

#--------------------------------------------------------------------------------------------
#May need to track depletion by drop at some points, this is in conduct_survey
#--------------------------------------------------------------------------------------------
#From github straight
install_github('peterkuriyama/hlsimulator')
library(hlsimulator)

#----------------------------------------------------------------------------------------
#Function to create to_loop data frame
create_to_loop <- function(fishes1, fishes2, comp_coeffs = c(.3, .5, .7),
  shape_rows = c(3, 5), nsites = 50, dep_type = 'none'){

  to_loop <- expand.grid(fishes1, fishes2, comp_coeffs, shape_rows, c('pref', 'rand'),
    dep_type)
  names(to_loop) <- c('nfish1', 'nfish2', 'comp_coeff', 
    'shape_list_row', 'type', 'dep_type')
  to_loop$nsites <- nsites
  to_loop$c1_sum <- .01
  return(to_loop)
}

#----------------------------------------------------------------------------------------
# What range of catch per hooks provides a relative index of abundance?
# What range of hooks without an aggressive species provides a relative index of abundance.

#--------------------------------------------------------------------------------------------
#Define scenarios for all simulations
shape_list1 <- data.frame(scen = c('uniform' ,'patchy'),
  shapes1 = c(1, .1),
  shapes2 = c(1, 10))
shape_list1$for_plot <- c("Uniform", 'Patchy')

shape_list1 <- shape_list1[2, ]

#Adjust the number of hooks here
#Number of hooks really high
ctl1 <- make_ctl(distribute = 'beta', mortality = 0, move_out_prob = .05, 
      nfish1 = 100000,
      nfish2 = 0, prob1 = .01, prob2 = .01, nyear = 1, scope = 0, seed = 1,
      location = data.frame(vessel = 1, x = 1, y = 1), numrow = 30, numcol = 30,
      shapes = c(.1, .1) , max_prob = 0, min_prob = 0, comp_coeff = .5, niters = 1, 
      nhooks = 10, par_func = 'run_scenario', 
      dep_type = "increasing",
      prop_moving = .1)   

#--------------------------------------------------------------------------------------------
#Check functions
# fishes <- c(20000)
# onespp <- run_sampled_locs(shape_list = shape_list1, ncores = 6,
#   ctl_o = ctl1, thing1 = fishes, name1 = 'nfish1', nreps = 1, 
#   nsites_vec = c(50))

# ctl2 <- ctl1
# ctl2$dep_type = 'none'
# onespp2 <- run_sampled_locs(shape_list = shape_list1, ncores = 6,
#   ctl_o = ctl2, thing1 = fishes, name1 = 'nfish1', nreps = 1, 
#   nsites_vec = c(50, 100))

# onespp %>% filter(spp == 'spp1') %>% select(cpue, type)
# onespp2 %>% filter(spp == 'spp1', nsites == 50) %>% select(cpue, type)

#--------------------------------------------------------------------------------------------
#Run simulation
fishes1 <- seq(20000, 200000, by = 20000)
fishes2 <- 0

to_loop <- create_to_loop(fishes1 = fishes1, fishes2 = fishes2, shape_rows = 5,
  dep_type = c('increasing', 'decreasing'))
to_loop$comp_coeff <- NULL
to_loop <- to_loop[which(duplicated(to_loop) == FALSE), ]
to_loop$dep_type <- as.character(to_loop$dep_type)

#Filter to only have preferential sampling
to_loop <- to_loop %>% filter(type == 'pref')

#Define the proportions moving in and out at each nfish1 value
#Note that the prop_moving refers to the numbers of fish outside of fishing areas
#if dep_type == 'increasing'
#if dep_type == 'decreasing' prop_moving moves the proportion of fish in 
#the fished areas out
prop_vec <- seq(.15, 0, length.out = 8)
prop_vec <- c(prop_vec, rep(0, 2))
incs <- data.frame(nfish1 = fishes1, dep_type = 'increasing', 
  prop_moving = prop_vec)
decs <- data.frame(nfish1 = fishes1, dep_type = 'decreasing', 
  prop_moving = rev(prop_vec))
moving_props <- rbind(incs, decs)

to_loop <- to_loop %>% left_join(moving_props, by = c('nfish1' , 'dep_type'))

#Define number of reps
to_loop$nreps <- 1000
#--------------------------------------------------------------------------------------------
#Test run
# onespp_depletion <- fixed_parallel(index = 5, ctl1 = ctl1, to_loop = to_loop,
#   change_these = c('nfish1', 'nfish2', 'prop_moving', 'dep_type'))

#--------------------------------------------------------------------------------------------
to_run <- 1:nrow(to_loop)

start_time <- Sys.time()

clusters <- parallel::makeCluster(nncores)
doParallel::registerDoParallel(clusters)

onespp_depletion <- foreach(ii = to_run,
  .packages = c('plyr', 'dplyr', 'tidyverse', 'reshape2', 'hlsimulator'), 
  .export = c("shape_list1")) %dopar% {
    fixed_parallel(index = ii, ctl1 = ctl1, to_loop = to_loop, 
      change_these = c('nfish1', 'nfish2', 'prop_moving', 'dep_type'))  
}

#Close clusters
stopCluster(clusters)
#Record run time
run_time <- Sys.time() - start_time

#---------------------------------------
#Format output
site_cpues <- lapply(onespp_depletion, FUN = function(x) x[[2]])
onespp_depletion <- lapply(onespp_depletion, FUN = function(x) x[[1]])

onespp_depletion <- ldply(onespp_depletion)  
onespp_depletion$dep <- onespp_depletion$nfish1 / 200000

save(onespp_depletion, file = "output/onespp_depletion.Rdata")
#--------------------------------------------------------------------------------------------
#Plot the results
onespp_depletion %>% filter(spp == "spp1") %>% 
  ggplot(aes(x = dep, y = cpue, colour = type)) + geom_point() + 
  facet_grid(init_dist ~ nsites + dep_type) + ylim(c(0, 1)) +
  geom_abline(slope = 1, intercept = 0, lty = 2)
# save(onespp, file = 'output/many_hooks_onespp.Rdata')
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#For only uniform distribution

# shape_list_uniform <- shape_list1[1, ]
# ctl_uniform <- ctl1
# ctl_uniform$nhooks <- 10
# 
# 
# start_time <- Sys.time()
# onespp <- run_sampled_locs(shape_list = shape_list_uniform, ncores = nncores,
#   ctl_o = ctl_uniform, thing1 = fishes, name1 = 'nfish1', nreps = 12, 
#   nsites_vec = c(50, 100))
# onespp <- onespp %>% filter(spp == 'spp1')
# (run_time <- Sys.time() - start_time)
# beep()
# onespp %>% ggplot(aes(x = dep, y = cpue, colour = type)) + geom_point() + facet_grid(init_dist ~ nsites) + 
#   geom_abline(slope = 1, intercept = 0, lty = 2)
# 
# 




