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

setwd("/udrive.uw.edu/udrive/")
list.files("Volumes/udrive/hlsimulator_runs")

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

ctl1 <- make_ctl(distribute = 'beta', mortality = 0, move_out_prob = .05, 
      nfish1 = 100000,
      nfish2 = 0, prob1 = .01, prob2 = .05, nyear = 2, scope = 0, seed = 1,
      location = data.frame(vessel = 1, x = 1, y = 1), numrow = 30, numcol = 30,
      shapes = c(.1, .1) , max_prob = 0, min_prob = 0, comp_coeff = .5, niters = 1)    

#--------------------------------------------------------------------------------------------
#Figure 1. Show distributions of each sceanrio
#Format this figure
inits <- lapply(1:nrow(shape_list1), FUN = function(ss){
  ctl1$shapes <- c(shape_list1[ss, 2], shape_list1[ss, 3])
  temp <- initialize_population(ctl = ctl1, nfish = ctl1$nfish1)
  return(temp)
})

#Work on this plot
hist(inits[[1]], breaks = 30)
hist(inits[[2]], breaks = 30)
hist(inits[[3]], breaks = 30)
hist(inits[[4]], breaks = 30)

#--------------------------------------------------------------------------------------------
#Figure 2
#--------------------------------------------------------------------------------------------
#RUN 1

#----------------------------------------
#Format simulation
fishes <- seq(0, 200000, by = 20000)

#Run Simulation with 1000 replicates
start_time <- Sys.time()
onespp <- run_sampled_locs(shape_list = shape_list1, ncores = nncores,
  ctl_o = ctl1, thing1 = fishes, name1 = 'nfish1', nreps = 1000, 
  nsites_vec = c(5, 10, 30, 50, 100))
onespp <- onespp %>% filter(spp == 'spp1')

run_time <- Sys.time() - start_time
send_email(body = 'lab mac run done')

onespp$dep <- factor(onespp$dep, levels = unique(onespp$dep))
onespp$nsites <- factor(onespp$nsites, levels = unique(onespp$nsites))
save(onespp, file = "onespp_1000.Rdata")
# save(onespp, file = 'onespp.Rdata')

#first run took 8 hours I think
#----------------------------------------
#Load the data if run already
load("output/onespp.Rdata")

#Calculate mean, variance, and cv of each value
# onespp <- onespp %>% group_by(nsites, init_dist, nfish1, spp, type) %>% 
#   mutate(mean_cpue = mean(cpue), sd_cpue = sd(cpue), cv_cpue = sd_cpue / mean_cpue) %>%
#   as.data.frame

# ggplot(onespp) + geom_point(aes(x = nsites, y = cv_cpue, colour = init_dist))

# onespp %>% ggplot() + 
#   geom_point(aes(dep, cpue, colour = nsites)) + 
#   facet_wrap(~ init_dist + type, ncol = 2)

# onespp %>% filter(init_dist == 'rightskew') %>% ggplot() + 
#   geom_boxplot(aes(dep, cpue, colour = nsites)) + facet_wrap(~ type)

# png(width = 11.5, height = 9, units = 'in', res = 150, file = 'figs/onespp_uniform.png')
# onespp %>% filter(init_dist == 'unif') %>% ggplot() + 
#   geom_boxplot(aes(dep, cpue, colour = type)) + facet_wrap(~ nsites)
# dev.off()


# png(width = 11.5, height = 9, units = 'in', res = 150, file = 'figs/onespp_rightskew.png')
# onespp %>% filter(init_dist == 'patchy') %>% ggplot() + 
#   geom_boxplot(aes(dep, cpue, colour = type)) + facet_wrap(~ nsites)
# dev.off()

# png(width = 11.5, height = 9, units = 'in', res = 150, file = 'figs/onespp_patchy.png')
# onespp %>% filter(init_dist == 'patchy') %>% ggplot() + 
#   geom_boxplot(aes(dep, cpue, colour = type)) + facet_wrap(~ nsites)
# dev.off()

# Increasing number of sites gets closer to true curve
#Least variability as fish distribution gets more even

#---------------------------------------------
#---------------------------------------------
#Two species with comp_coeff equal to 0.5

#Increasing number of location with two species, even comp_coeff
#Both species increasing together

fishes1 <- seq(0, 200000, by = 20000)
fishes2 <- rev(fishes1)

#Change comp_coeff sometime
ctl1$comp_coeff <- 0.5

start_time2 <- Sys.time()
twospp <- run_sampled_locs_2spp(shape_list = shape_list1, nsites_vec = c(5, 10, 30, 50, 100),
  ncores = nncores, ctl_o = ctl1, thing1 = fishes1, thing2 = fishes2, name1 = 'nfish1', 
  name2 = 'nfish2', nreps = 100)
run_time2 <- Sys.time() - start_time2
  
send_email()
  
#Save results
# paste0(results_dir, '//inc12.Rdata')
# save(twospp, file = paste0(results_dir, '//inc12.Rdata'))
save(twospp, file = 'twospp.Rdata')


load('output/twospp.Rdata')

#Took 14 hours!

#change formats
twospp$nfish1 <- as.numeric(twospp$nfish1)
twospp$nfish2 <- as.numeric(twospp$nfish2)

#Swing dep by species for plots
dep2 <- twospp %>% dcast(nfish1 + nfish2 + init_dist + nsites + 
  rep + iter + type ~ spp, value.var = 'dep')
names(dep2)[grep('spp', names(dep2))] <- c('dep1', 'dep2')

twospp <- inner_join(twospp, dep2, by = c('nfish1', 'nfish2', 'init_dist', 'nsites',
  'rep', 'iter', 'type'))


#Look only at simulations that sampled 50 sites
focus <- twospp %>% filter(nsites == 50) 
focus %>% group_by(init_dist, rep, nfish1, nfish2, type) %>% mutate(tot_cpue = sum(cpue)) %>% 
  as.data.frame -> focus

focus %>% group_by(dep1, dep2, init_dist, spp, type) %>% 

summarize(mean_cpue = mean(cpue),
  sd_cpue = sd(cpue), cv_cpue = sd_cpue / mean_cpue) %>% as.data.frame -> for_contour


#Remove duplicated values
# focus %>% group_by(init_dist, dep1, dep2, spp, type) %>% filter(row_number(cv_cpue) == 1) %>% 
#   as.data.frame -> focus

 %>% filter(init_dist == 'unif' & dep1 == 0.1 & dep2 == 1.0) %>% head


#What colors to use?
#Patchy contour plot
for_contour %>% filter(init_dist == 'unif') %>% ggplot(aes(x = dep1, y = dep2, z = mean_cpue)) + 
  stat_contour(aes(colour = ..level..), binwidth = .1) + facet_wrap(~ spp + type) + 
  scale_colour_gradient2(low = 'blue', high = 'red', limits = c(0, 1))

for_contour %>% filter(init_dist == 'unif') %>% tail



volcano3d <- melt(volcano)

v + stat_contour(geom="polygon", aes(fill=..level..))
fc2 <- for_contour %>% group_by(dep1, dep2, init_dist, type) %>% summarize(tot_mean_cpue = sum(mean_cpue))

#This makes sense
ggplot(fc2, aes(x = dep1, y = dep2, z = tot_mean_cpue)) + stat_contour(aes(colour = ..level..), 
  binwidth = .1, size = 1) + facet_wrap(~ init_dist + type, ncol = 2) + scale_colour_gradient(low = 'white',
  high = 'red')


#Try plotting this as dots? Something seems wrong


funi %>% group_by(nfish1, nfish2, type) %>% mutate(tot_cpue = sum(cpue)) %>% as.data.frame %>% head


#Patchy contour plot
focus %>% filter(init_dist == 'patchy') %>% ggplot(aes(x = dep1, y = dep2, z = mean_cpue)) + 
  geom_contour(aes(colour = ..level..), bins = 5) + facet_wrap(~ spp + type)


# with ggplot > 2.0.0 you'll need to add method="bottom.pieces" (or top.pieces) to the direct.label call
funi <- focus %>% filter(init_dist == 'unif')



focus %>% filter(init_dist == 'unif') %>% ggplot() + geom_point(aes(x = dep1, y = dep2, colour = mean_cpue)) + 
  facet_wrap(~ spp + type)



unique(paste(focus$dep1, focus$dep2))




#---------------------------------------------
#Run two species simulation with comp_coeff of .1, weak competition
#only 10 replicates to see how it goes

fishes1 <- seq(20000, 200000, by = 20000)
fishes2 <- rev(fishes1)

#Change comp_coeff sometime
ctl1 <- make_ctl(distribute = 'beta', mortality = 0, move_out_prob = .05, nfish1 = 100000,
      nfish2 = 0, prob1 = .01, prob2 = .05, nyear = 1, scope = 0, seed = 1,
      location = data.frame(vessel = 1, x = 1, y = 1), numrow = 30, numcol = 30,
      shapes = c(.1, .1) , max_prob = 0, min_prob = 0, niters = 1, 
      comp_coeff = .1)    

shape_list1 <- data.frame(scen = c('patchy','rightskew', 'normdist', 'unif'), 
                          shapes1 = c(.1, 1, 10, 10), 
                          shapes2 = c(10, 10, 10, .10))

start_time <- Sys.time()
twospp_lowcomp <- run_sampled_locs_2spp(shape_list = shape_list1, nsites_vec = c(50),
  ncores = nncores, ctl_o = ctl1, thing1 = fishes1, thing2 = fishes2, name1 = 'nfish1', 
  name2 = 'nfish2', nreps = 10)
run_time <- Sys.time() - start_time
  
send_email()

save(twospp_lowcomp, file = 'twospp_lowcomp.Rdata')






