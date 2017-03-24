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

#Mac
if(Sys.info()['sysname'] == 'Darwin'){
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

#--------------------------------------------------------------------------------------------
#Check the sampling of sites
#Check that the sampling will work?
#Manually update seed index

for(ss in 1:nrow(shape_list1)){
  print(ss)
  ctl2$shapes <- c(shape_list1[ss, 'shapes1'], shape_list1[ss, 'shapes2'])
  
  init1 <- initialize_population(ctl = ctl2, ctl2$nfish1)
  loc_list <- pick_locs1
  
  #define fishing locations
  locs <- lapply(1:nrow(loc_list), FUN = function(ll){
    print(ll)
  
    pick_sites(nbest = loc_list[ll, 1], nmed = loc_list[ll, 2],
      nbad = loc_list[ll, 3], fish_mat = init1)
  
  })  
}
#----------------------------------------------------------------------------------------
# What range of catch per hooks provides a relative index of abundance?
# What range of hooks without an aggressive species provides a relative index of abundance.


#--------------------------------------------------------------------------------------------
#RUN 1 - Increasing number of sites from 2-20
#--------------------------------------------------------------------------------------------
#Set Up Values for this run
#Try to get range of 10-500 fish per cell

fishes <- seq(1000, 50000, by = 2000)

ctl1 <- make_ctl(distribute = 'beta', mortality = 0, move_out_prob = .05, nfish1 = 10000,
      nfish2 = 0, prob1 = .01, prob2 = .05, nyear = 1, scope = 0, seed = 1,
      location = data.frame(vessel = 1, x = 1, y = 1), numrow = 10, numcol = 10,
      shapes = c(.1, .1) , max_prob = 0, min_prob = 0, comp_coeff = .5, niters = 1)    

shape_list1 <- data.frame(scen = c('patchy','rightskew', 'normdist', 'unif'), 
                          shapes1 = c(.1, 1, 10, 10), 
                          shapes2 = c(10, 10, 10, .10))

pick_locs1 <- data.frame(nbests = c(.7, .7, .7, .6, .6, .7, .8),
                nmeds = c(.2, .3, 0, .3, 0, 0, .1),
                nbads = c(.1, 0, .3, .1, .4, .2, .1))
(pick_locs1 <- pick_locs1 * 20)

#Run the simulation
inc1 <- run_locs(shape_list = shape_list1,
  loc_scenario = 'increasing', loc_vector = seq(2, 20, by = 2),
  ncores = 6, ctl_o = ctl1, thing1 = fishes,
  name1 = 'nfish1')

#Save the data
save(inc1, file = 'output/inc1.Rdata')

#Plot the results
inc1$location <- factor(inc1$location, levels = unique(inc1$location))

inc1 %>% filter(spp == 'spp1') %>% ggplot(aes(x = dep, y = cpue)) + 
  geom_point(aes(colour = init_dist)) + facet_wrap(~ location)
  
inc1 %>% filter(spp == 'spp1') %>% ggplot(aes(x = dep, y = cpue)) + 
  geom_point(aes(colour = location)) + facet_wrap(~ init_dist)

# Increasing number of sites gets closer to true curve
#Least variability as fish distribution gets more even

#---------------------------------------------
#---------------------------------------------
#Scale up this run to larger area

fishes <- seq(2000, 100000, by = 5000)
ctl1$nfish1 <- 20000
ctl1$numrow <- 15
ctl1$numcol <- 15

init1 <- initialize_population(ctl = ctl1, nfish = ctl1$nfish1)

pick_locs1 <- data.frame(nbests = c(.7, .7, .7, .6, .6, .7, .8),
                nmeds = c(.2, .3, 0, .3, 0, 0, .1),
                nbads = c(.1, 0, .3, .1, .4, .2, .1))
(pick_locs1 <- pick_locs1 * 50)

#Run the simulation
inc11 <- run_locs(shape_list = shape_list1,
  loc_scenario = 'increasing', loc_vector = seq(2, 20, by = 2),
  ncores = 6, ctl_o = ctl1, thing1 = fishes,
  name1 = 'nfish1')

#Save the data
save(inc11, file = 'output/inc11.Rdata')

#Plot the results
inc11$location <- factor(inc11$location, levels = unique(inc11$location))

inc11 %>% filter(spp == 'spp1') %>% ggplot(aes(x = dep, y = cpue)) + 
  geom_point(aes(colour = init_dist)) + facet_wrap(~ location)
  
inc11 %>% filter(spp == 'spp1') %>% ggplot(aes(x = dep, y = cpue)) + 
  geom_point(aes(colour = location)) + facet_wrap(~ init_dist)

#Relationship seems to be invariant across matrix dimensions

#---------------------------------------------
#---------------------------------------------
#Increasing number of location with two species, even comp_coeff
#Both species increasing together

fishes1 <- seq(1000, 50000, by = 1000)
fishes2 <- rev(seq(1000, 50000, by = 1000))

ctl1 <- make_ctl(distribute = 'beta', mortality = 0, move_out_prob = .05, nfish1 = 10000,
      nfish2 = 0, prob1 = .01, prob2 = .05, nyear = 1, scope = 0, seed = 1,
      location = data.frame(vessel = 1, x = 1, y = 1), numrow = 10, numcol = 10,
      shapes = c(.1, .1) , max_prob = 0, min_prob = 0, niters = 1, 
      comp_coeff = .7)    

shape_list1 <- data.frame(scen = c('patchy','rightskew', 'normdist', 'unif'), 
                          shapes1 = c(.1, 1, 10, 10), 
                          shapes2 = c(10, 10, 10, .10))

start_time <- Sys.time()

inc12 <- run_locs_2spp(shape_list = shape_list1, loc_scenario = 'increasing',
  loc_vector = seq(2, 20, by = 2), ncores = nncores, ctl_o = ctl1, thing1 = fishes1,
  thing2 = fishes2, name1 = 'nfish1', name2 = 'nfish2')

run_time <- Sys.time() - start_time

send_email()

#Save results
# paste0(results_dir, '//inc12.Rdata')
save(inc12, file = paste0(results_dir, '//inc12.Rdata'))
load('output/inc12.Rdata')

#change formats
inc12$nfish1 <- as.numeric(inc12$nfish1)
inc12$nfish2 <- as.numeric(inc12$nfish2)

#Swing dep by species for plots
dep2 <- inc12 %>% dcast(nfish1 + nfish2 + init_dist + loc ~ spp, value.var = 'dep')
names(dep2)[5:6] <- c('dep1', 'dep2')

inc12 <- inner_join(inc12, dep2, by = c('nfish1', 'nfish2', 'init_dist', 'loc'))


ggplot(inc12, aes(x = dep1, y = dep2)) + geom_point(aes(colour = cpue)) + 
  facet_wrap(~ init_dist + spp, ncol = 2)


#--------------------------------------------------------------------------------------------
#RUN 2
#--------------------------------------------------------------------------------------------
#Patchy Stuff
#20 sites total:
  #70% good, 20% med, 10% bad
  #70% good, 30% med
  #70% good, 30% bad
  #60% good, 30% med, 10% bad
  #60% good, 40% bad
  #80% good, 20% bad
  #80% good, 10% med, 10% bad


#Picking some number of good, med, bad sites
#Increasing number of sites from 2-20
fishes <- seq(1000, 40000, by = 2000)

ctl2 <- make_ctl(distribute = 'beta', mortality = 0, move_out_prob = .05, nfish1 = 10000,
      nfish2 = 0, prob1 = .01, prob2 = .05, nyear = 1, scope = 0, seed = 1,
      location = data.frame(vessel = 1, x = 1, y = 1), numrow = 15, numcol = 15,
      shapes = c(.1, .1) , max_prob = 0, min_prob = 0, comp_coeff = .5, niters = 1)    

fishes <- seq(1000, 50000, by = 2000)

shape_list1 <- data.frame(scen = c('patchy','rightskew', 'normdist', 'unif'), 
                          shapes1 = c(.1, 1, 10, 10), 
                          shapes2 = c(10, 10, 10, .10))

pick_locs1 <- data.frame(nbests = c(.7, .7, .7, .6, .6, .7, .8),
                nmeds = c(.2, .3, 0, .3, 0, 0, .1),
                nbads = c(.1, 0, .3, .1, .4, .2, .1))
pick_locs1 <- pick_locs1 * 30


run2 <- run_locs(shape_list = shape_list1, loc_scenario = 'pick', 
  loc_list = pick_locs1, ncores = 6, ctl_o = ctl2, thing1 = fishes,
  name1 = 'nfish1')
save(run2, file = 'output/run2.Rdata')

descs <- data.frame(location = as.character(unique(res2$location)), 
                    desc = c("70% good, 20% med, 10% bad",
                             "70% good, 30% med",
                             "70% good, 30% bad",
                             "60% good, 30% med, 10% bad",
                             "60% good, 40% bad",
                             "80% good, 20% bad",
                             "80% good, 10% med, 10% bad"))
descs$location <- factor(descs$location, levels = descs$location)

run2 <- inner_join(run2, descs, by = "location")
run2$desc <- as.character(run2$desc)

ggplot(run2, aes(x = dep, y = cpue)) + geom_point(aes(colour = init_dist)) + 
  facet_wrap(~ desc) + ylim(c(0, 1))

##Run 2.1
#Seem to not cover enough locations 
#Use run 3 to sample larger proportion of the area for one species
pick_locs1 <- data.frame(nbests = c(.7, .7, .7, .6, .6, .7, .8),
                nmeds = c(.2, .3, 0, .3, 0, 0, .1),
                nbads = c(.1, 0, .3, .1, .4, .2, .1))
pick_locs1 <- pick_locs1 * 50

run21 <- run_locs(shape_list = shape_list1, loc_scenario = 'pick', 
  loc_list = pick_locs1, ncores = 6, ctl_o = ctl2, thing1 = fishes,
  name1 = 'nfish1')
save(run21, file = 'output/run21.Rdata')

