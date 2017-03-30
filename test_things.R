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

#----------------------------------------------------------------------------------------
# What range of catch per hooks provides a relative index of abundance?
# What range of hooks without an aggressive species provides a relative index of abundance.

#update probabilistic sampling
#--------------------------------------------------------------------------------------------
#RUN 1 - Increasing number of sites from 2-20
#--------------------------------------------------------------------------------------------
#Set Up Values for this run
#Try to get range of 10-500 fish per cell

#----------------------------------------
#Format simulation
fishes <- seq(20000, 200000, by = 20000)

ctl1 <- make_ctl(distribute = 'beta', mortality = 0, move_out_prob = .05, nfish1 = 100000,
      nfish2 = 0, prob1 = .01, prob2 = .05, nyear = 2, scope = 0, seed = 1,
      location = data.frame(vessel = 1, x = 1, y = 1), numrow = 30, numcol = 30,
      shapes = c(.1, .1) , max_prob = 0, min_prob = 0, comp_coeff = .5, niters = 1)    

shape_list1 <- data.frame(scen = c('patchy','rightskew', 'normdist', 'unif'), 
                          shapes1 = c(.1, 1, 10, 10), 
                          shapes2 = c(10, 10, 10, .10))

#----------------------------------------
#Run Simulation
start_time <- Sys.time()
onespp <- run_sampled_locs(shape_list = shape_list1, ncores = nncores,
  ctl_o = ctl1, thing1 = fishes, name1 = 'nfish1', nreps = 100, 
  nsites_vec = c(5, 10, 30, 50, 70, 90, 100))
onespp <- onespp %>% filter(spp == 'spp1')

run_time <- Sys.time() - start_time
send_email()

onespp$dep <- factor(onespp$dep, levels = unique(onespp$dep))
onespp$nsites <- factor(onespp$nsites, levels = unique(onespp$nsites))

save(onespp, file = 'onespp.Rdata')

#first run took 8 hours I think
#----------------------------------------
#Load the data if run already
load("output/onespp.Rdata")

#Calculate mean, variance, and cv of each value
onespp <- onespp %>% group_by(nsites, init_dist, nfish1, spp, type) %>% 
  mutate(mean_cpue = mean(cpue), sd_cpue = sd(cpue), cv_cpue = sd_cpue / mean_cpue) %>%
  as.data.frame

ggplot(onespp) + geom_point(aes(x = nsites, y = cv_cpue, colour = init_dist))

onespp %>% ggplot() + 
  geom_point(aes(dep, cpue, colour = nsites)) + 
  facet_wrap(~ init_dist + type, ncol = 2)

onespp %>% filter(init_dist == 'rightskew') %>% ggplot() + 
  geom_boxplot(aes(dep, cpue, colour = nsites)) + facet_wrap(~ type)

png(width = 11.5, height = 9, units = 'in', res = 150, file = 'figs/onespp_uniform.png')
onespp %>% filter(init_dist == 'unif') %>% ggplot() + 
  geom_boxplot(aes(dep, cpue, colour = type)) + facet_wrap(~ nsites)
dev.off()


png(width = 11.5, height = 9, units = 'in', res = 150, file = 'figs/onespp_rightskew.png')
onespp %>% filter(init_dist == 'patchy') %>% ggplot() + 
  geom_boxplot(aes(dep, cpue, colour = type)) + facet_wrap(~ nsites)
dev.off()

png(width = 11.5, height = 9, units = 'in', res = 150, file = 'figs/onespp_patchy.png')
onespp %>% filter(init_dist == 'patchy') %>% ggplot() + 
  geom_boxplot(aes(dep, cpue, colour = type)) + facet_wrap(~ nsites)
dev.off()

# Increasing number of sites gets closer to true curve
#Least variability as fish distribution gets more even

#---------------------------------------------
#---------------------------------------------
#Increasing number of location with two species, even comp_coeff
#Both species increasing together

fishes1 <- seq(20000, 200000, by = 20000)
fishes2 <- rev(fishes1)

#Change comp_coeff sometime
ctl1 <- make_ctl(distribute = 'beta', mortality = 0, move_out_prob = .05, nfish1 = 100000,
      nfish2 = 0, prob1 = .01, prob2 = .05, nyear = 1, scope = 0, seed = 1,
      location = data.frame(vessel = 1, x = 1, y = 1), numrow = 30, numcol = 30,
      shapes = c(.1, .1) , max_prob = 0, min_prob = 0, niters = 1, 
      comp_coeff = .5)    

shape_list1 <- data.frame(scen = c('patchy','rightskew', 'normdist', 'unif'), 
                          shapes1 = c(.1, 1, 10, 10), 
                          shapes2 = c(10, 10, 10, .10))

start_time <- Sys.time()
twospp <- run_sampled_locs_2spp(shape_list = shape_list1, nsites_vec = c(5, 10, 30, 50, 100),
  ncores = nncores, ctl_o = ctl1, thing1 = fishes1, thing2 = fishes2, name1 = 'nfish1', 
  name2 = 'nfish2', nreps = 50)
run_time <- Sys.time() - start_time
  
send_email()
  
#Save results
# paste0(results_dir, '//inc12.Rdata')
# save(twospp, file = paste0(results_dir, '//inc12.Rdata'))
save(twospp, file = 'twospp.Rdata')


#change formats
inc12$nfish1 <- as.numeric(inc12$nfish1)
inc12$nfish2 <- as.numeric(inc12$nfish2)

#Swing dep by species for plots
dep2 <- inc12 %>% dcast(nfish1 + nfish2 + init_dist + loc ~ spp, value.var = 'dep')
names(dep2)[5:6] <- c('dep1', 'dep2')

inc12 <- inner_join(inc12, dep2, by = c('nfish1', 'nfish2', 'init_dist', 'loc'))

#Look at one fishing location only
inc12 %>% filter(loc == 'loc_list1') %>% ggplot(aes(x = dep1, y = dep2)) + geom_point(aes(colour = cpue)) + 
  facet_wrap(~ init_dist + spp, ncol = 2) + scale_colour_gradient(low = 'white', high = 'red')

png(width = 9, height = 9, units = 'in', res = 150, file = 'figs/fig2_2spp.png')
inc12 %>% filter(loc == 'loc_list10') %>% ggplot(aes(x = dep1, y = dep2)) + geom_point(aes(colour = cpue)) + 
  facet_wrap(~ init_dist + spp, ncol = 2) + scale_colour_gradient(low = 'white', high = 'red')
dev.off()


png(width = 8.58, height = 9, units = 'in', res = 150, file = 'figs/run1_2spp.png')
ggplot(inc12, aes(x = dep1, y = dep2)) + geom_point(aes(colour = cpue)) + 
  facet_wrap(~ init_dist + spp, ncol = 2) + scale_colour_gradient(low = 'white', high = 'red')
dev.off()






