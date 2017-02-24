setwd("/Users/peterkuriyama/School/Research/hlsimulator")

library(devtools)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(doParallel)
#----------------------------------------------------------------------------------------
# What range of catch per hooks provides a relative index of abundance?
# What range of hooks without an aggressive species provides a relative index of abundance.

#To-Do:
#1. Explore patchiness configurations
#2. Figure way to get the probabilities lower
#3. Scenarios:
  #Increasing/decreasing Trend
  #Gear saturation
  #Aggressive behavior

#Default locations, 15% of available sites

#--------------------------------------------------------------------------------------------
#May need to track depletion by drop at some points, this is in conduct_survey
#--------------------------------------------------------------------------------------------
#Options to load the package

#From github straight
install_github('peterkuriyama/hlsimulator')
library(hlsimulator)

#--------------------------------------------------------------------------------------------
#Define default locations that will be used for other scenarios
set.seed(3)
locs <- expand.grid(1:10, 1:10)
locs$vessel <- 1
names(locs)[1:2] <- c('x', 'y')
locs <- locs[, c('vessel', 'x', 'y')]

samps <- base::sample(1:100, 15)
def_locs <- locs[samps, ]

#----------------------------------------------------------------------------------------
#Test nfish in one location
one_loc <- data.frame(vessel = 1, x = 1, y = 1)
ctl <- make_ctl(distribute = 'patchy', mortality = 0, move_out_prob = .5,
      nfish1 = 10, nfish2 = 0, prob1 = .01, prob2 = 0, nyear = 15, scope = 1, seed = 4,
      location = one_loc, numrow = 1, numcol = 1)  

one_loc_test1 <- run_scenario(ncores = 6, loop_over = seq(100, 1500, by = 100),
  ctl_in = ctl, to_change = 'nfish1')

#Plot
one_loc_test1[[3]] %>% filter(variable == 'cpue1') %>% ggplot() + geom_line(aes(x = nfish, y = cpue), 
  size = 1) + theme_bw() + facet_wrap(~ nfish1)

#-------------------------------------
#With p = 0.02
ctl <- make_ctl(distribute = 'patchy', mortality = 0, move_out_prob = .5,
      nfish1 = 10, nfish2 = 0, prob1 = .02, prob2 = 0, nyear = 15, scope = 1, seed = 4,
      location = one_loc, numrow = 1, numcol = 1)  

one_loc_test2 <- run_scenario(ncores = 6, loop_over = seq(100, 1500, by = 100),
  ctl_in = ctl, to_change = 'nfish1')

#Plot
one_loc_test2[[3]] %>% filter(variable == 'cpue1') %>% ggplot() + geom_line(aes(x = nfish, y = cpue), 
  size = 1) + theme_bw() + facet_wrap(~ nfish1)


#-------------------------------------
#With p = 0.03
ctl <- make_ctl(distribute = 'patchy', mortality = 0, move_out_prob = .5,
      nfish1 = 10, nfish2 = 0, prob1 = .03, prob2 = 0, nyear = 15, scope = 1, seed = 4,
      location = one_loc, numrow = 1, numcol = 1)  

one_loc_test3 <- run_scenario(ncores = 6, loop_over = seq(100, 1500, by = 100),
  ctl_in = ctl, to_change = 'nfish1')

#Plot
one_loc_test2[[3]] %>% filter(variable == 'cpue1') %>% ggplot() + geom_line(aes(x = nfish, y = cpue), 
  size = 1) + theme_bw() + facet_wrap(~ nfish1)

#--------------------------------------------------------------------------------------------
#Plot both examples
for_plot1$prob <- .01
for_plot2$prob <- .02
for_plot3$prob <- .03

for_plot <- rbind(for_plot1, for_plot2, for_plot3)
for_plot$prob <- as.factor(for_plot$prob)

for_plot <- for_plot %>% filter(variable == 'cpue1')

png(width = 12.6, height = 8.37, units = 'in', res = 150, 
  file = 'figs/one_cell.png')
ggplot(for_plot) + geom_line(aes(x = nfish, y = cpue, colour = prob, group = prob)) + 
  facet_wrap(~ nfish1)
dev.off()

#--------------------------------------------------------------------------------------------
###Increasing number of fishing locations
#--------------------------------------------------------------------------------------------
set.seed(3)
locs <- expand.grid(1:10, 1:10)
locs$vessel <- 1
names(locs)[1:2] <- c('x', 'y')
locs <- locs[, c('vessel', 'x', 'y')]  
samps <- base::sample(1:100, 45)
locs <- locs[samps, ]  

locs_list <- vector('list', length = nrow(locs))
for(ii in 1:nrow(locs)){
  locs_list[[ii]] <- locs[1:ii, ]
}

#Use only odd numbered things in locs_list
pull_these <- which(1:length(locs_list) %% 2 == 1)
locs_list <- locs_list[pull_these]

ctl <- make_ctl(distribute = 'patchy', mortality = .1, move_out_prob = .5,
  nfish1 = 10000, nfish2 = 0, prob1 = .01, prob2 = 0, nyear = 15, scope = 1, seed = 4,
  location = def_locs)  

nlocs_out <- run_scenario(ctl_in = ctl, loop_over = locs_list, ncores = 6,
  to_change = 'location', add_index = TRUE)

#Plot these
nlocs_out[[3]] %>% filter(variable == 'cpue1') %>% ggplot() + 
  geom_line(aes(x = nfish, y = cpue, colour = location, group = location)) 

