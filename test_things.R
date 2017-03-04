setwd("/Users/peterkuriyama/School/Research/hlsimulator")

library(devtools)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(doParallel)
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
set.seed(3)
locs <- expand.grid(1:10, 1:10)
locs$vessel <- 1
names(locs)[1:2] <- c('x', 'y')
locs <- locs[, c('vessel', 'x', 'y')]

samps <- base::sample(1:100, 15)
def_locs <- locs[samps, ]

#----------------------------------------------------------------------------------------
##Number of hooks/locations vs. number of total fish
#Does patchiness affect the relationship?

#----------------------------------------
#For one species, uniformly distributed fish
  #no mortality

#Define Locations to fish in
set.seed(3)
locs <- expand.grid(1:10, 1:10)
locs$vessel <- 1
names(locs)[1:2] <- c('x', 'y')
locs <- locs[, c('vessel', 'x', 'y')]  
samps <- base::sample(1:100, 20)

locs <- locs[samps, ]  

locs_list <- vector('list', length = nrow(locs))
for(ii in 1:nrow(locs)){
  locs_list[[ii]] <- locs[1:ii, ]
}

ctl <- make_ctl(distribute = 'uniform', mortality = 0, move_out_prob = .5,
      nfish1 = 20000, nfish2 = 0, prob1 = .02, prob2 = .05, nyear = 15, scope = 1, seed = 4,
      location = def_locs, numrow = 10, numcol = 10)  

#2000 fish per cell, you shouldn't fish it down really
nlocs_onespp_out <- run_scenario(ctl_in = ctl, loop_over = locs_list, ncores = 6,
  to_change = 'location', add_index = TRUE)
 
#Plot 
nlocs_onespp_out[[3]] %>% filter(spp == 'spp1') %>% ggplot() + 
  geom_line(aes(x = nfish_total, y = cpue)) + 
  geom_point(aes(x = nfish_total, y = cpue, size = prop_of_unfished)) + 
  facet_wrap(~ index)

#Straight line
nlocs_onespp_out[[3]] %>% filter(spp == 'spp1') %>% ggplot() + 
  geom_line(aes(x = prop_of_unfished, y = cpue))  + 
  facet_wrap(~ index)

#Doesn't matter if 1000 fish are in each place, 
#Doesn't matter if 500 fish in each site
#200 fish start to see declines
#If 100 fish in each site, pattern is like connect the dots going down

#----------------------------------------
#Repeat plot above, but with patchy fish distribution

#Check initial population
initialize_population(ctl = ctl, nfish = 10000)

#Effect of patch distribution and number of fishing locations
#Keep number of fish per cell standardized
perc <- seq(0.1, 1, by = .1)
perc_outs <- vector('list', length = length(perc))

for(pp in 1:length(perc)){
  print(pp)  
  ctl <- make_ctl(distribute = 'patchy', mortality = 0, move_out_prob = .5,
      nfish1 = 20000, nfish2 = 0, prob1 = .02, prob2 = .05, nyear = 15, scope = 1, seed = 10,
      location = def_locs, numrow = 10, numcol = 10, percent = perc[pp])  
  perc_outs[[pp]] <- run_scenario(ctl_in = ctl, loop_over = locs_list, ncores = 6,
    to_change = 'location', add_index = TRUE)  
}

#Look at overlap between fish distribution and fishing locations
locs_list


for(pp in 1:length(perc)){
  print(pp)  
  ctl <- make_ctl(distribute = 'patchy', mortality = 0, move_out_prob = .5,
      nfish1 = 20000, nfish2 = 0, prob1 = .02, prob2 = .05, nyear = 15, scope = 1, seed = 10,
      location = def_locs, numrow = 10, numcol = 10, percent = perc[pp])  
  perc_outs[[pp]] <- run_scenario(ctl_in = ctl, loop_over = locs_list, ncores = 6,
    to_change = 'location', add_index = TRUE)  
}


#Pull relevant stuff for plots
perc_outs_plot <- lapply(perc_outs, FUN = function(x) x[[3]])
names(perc_outs_plot) <- as.character(perc)

perc_outs_plot <- ldply(perc_outs_plot)
names(perc_outs_plot)[1] <- "perc_distributed"

#Plot as number of fish
perc_outs_plot %>% filter(spp == 'spp1') %>% ggplot() + 
  geom_line(aes(x = nfish_total, y = cpue, colour = perc_distributed)) +
  geom_point(aes(x = nfish_total, y = cpue, colour = perc_distributed)) +
  facet_wrap(~ index)

#Plot as proportion of unfished population
perc_outs_plot %>% filter(spp == 'spp1') %>% ggplot() + 
  geom_line(aes(x = prop_of_pop, y = cpue, colour = perc_distributed)) +
  geom_point(aes(x = prop_of_pop, y = cpue, colour = perc_distributed)) +
  facet_wrap(~ index)



#----------------------------------------
#Fishing in the best areas? Fishing next to best areas? Fishing in shitty areas?
#Repeat plot above, but with patchy fish distribution


 


#----------------------------------
###Increasing number of fishing locations, uniform distribution, one species

ctl <- make_ctl(distribute = 'uniform', mortality = .1, move_out_prob = .5,
  nfish1 = 10000, nfish2 = 0, prob1 = .01, prob2 = 0, nyear = 15, scope = 1, seed = 4,
  location = def_locs)  

nlocs_out_u <- run_scenario(ctl_in = ctl, loop_over = locs_list, ncores = 6,
  to_change = 'location', add_index = TRUE)

#Plot these
nlocs_out_u[[3]] %>% filter(variable == 'cpue1') %>% ggplot() + 
  geom_line(aes(x = nfish, y = cpue, colour = location, group = location)) + 
  geom_point(aes(x = nfish, y = cpue, colour = location, group = location))












#----------------------------------------------------------------------------------------
##Competition effects 

#For one cell
one_loc <- data.frame(vessel = 1, x = 1, y = 1)
ctl <- make_ctl(distribute = 'patchy', mortality = 0, move_out_prob = .5,
      nfish1 = 500, nfish2 = 1000, prob1 = .05, prob2 = .05, nyear = 15, scope = 1, seed = 4,
      location = one_loc, numrow = 1, numcol = 1, comp_coeff = .7)  

comp_effects <- run_scenario(ncores = 6, loop_over = seq(500, 1500, by = 100),
  ctl_in = ctl, to_change = 'nfish1', add_index = FALSE)

png(width = 12, height = 8.4, units = 'in', res = 100, 
  file = 'fig1_effects_of_competition.png')
ggplot(comp_effects$for_plot) + geom_line(aes(x = year, y = value, group = variable, 
  colour = variable)) + geom_point(aes(x = year, y = value, group = variable,
  colour = variable, size = nfish)) + facet_wrap(~ nfish1)
dev.off()

#----------------------------------------
#For multiple fishing locations, fish in default locations
ctl <- make_ctl(distribute = 'patchy', mortality = 0, move_out_prob = .5,
      nfish1 = 50000, nfish2 = 100000, prob1 = .05, prob2 = .05, nyear = 15, scope = 1, seed = 4,
      location = def_locs, numrow = 10, numcol = 10, comp_coeff = .7)  

comp_effects_locs <- run_scenario(ncores = 6, loop_over = seq(50000, 150000, by = 10000),
  ctl_in = ctl, to_change = 'nfish1', add_index = FALSE)

# png(width = 12, height = 8.4, units = 'in', res = 100, 
#   file = 'fig1_effects_of_competition_multlocs.png')

ggplot(comp_effects_locs$for_plot) + geom_line(aes(x = year, y = cpue, group = variable, 
  colour = variable)) + geom_point(aes(x = year, y = cpue, group = variable,
  colour = variable, size = nfish)) + facet_wrap(~ nfish1)

# dev.off()




#----------------------------------------------------------------------------------------
#Loop over probabilities and number of fish
#Find critical numbers of fish for each individual cell

one_loc <- data.frame(vessel = 1, x = 1, y = 1)

probs <- seq(.01, .1, by = .01)
prob_fish_plot <- vector('list', length = length(probs))

for(jj in 1:length(probs)){
  ctl <- make_ctl(distribute = 'patchy', mortality = 0, move_out_prob = .5,
      nfish1 = 10, nfish2 = 0, prob1 = probs[jj], prob2 = 0, nyear = 15, scope = 1, seed = 4,
      location = one_loc, numrow = 1, numcol = 1)  
  temp <- one_loc_test1 <- run_scenario(ncores = 6, loop_over = seq(100, 1500, by = 100),
            ctl_in = ctl, to_change = 'nfish1', add_index = FALSE)
  prob_fish_plot[[jj]] <- temp[[3]]
  prob_fish_plot[[jj]]$prob <- probs[jj]
}

prob_fish_plot <- ldply(prob_fish_plot)
prob_fish_plot$prob <- as.character(prob_fish_plot$prob)

#Plot results
#Facet by number of fish
prob_fish_plot %>% filter(variable == 'cpue1') %>% ggplot() + 
  geom_line(aes(x = nfish, y = cpue, group = prob, colour = prob), size = 1) + facet_wrap(~ nfish1)

#How many years does it take to deplete the population?
prob_fish_plot <- prob_fish_plot %>% filter(variable == 'cpue1') 

prob_fish_plot %>% filter(cpue <= 0.2) %>% 
  ggplot(aes(x = year, y = cpue, group = nfish1, colour = nfish1)) + 
  geom_line() + geom_point() + facet_wrap(~ prob)

#Probs of 0.01 to 0.05 will give a little bit of spread where you don't always catch
#everything
  #How do some values go up?
  # prob_fish_plot %>% filter(nfish1 == 800 & prob == 0.1)

#-------------------------------------
#Zoom in on effects with smaller numbers of fish
prob_smallfish_plot <- vector('list', length = length(probs))

for(jj in 1:length(probs)){
  ctl <- make_ctl(distribute = 'patchy', mortality = 0, move_out_prob = .5,
      nfish1 = 10, nfish2 = 0, prob1 = probs[jj], prob2 = 0, nyear = 15, scope = 1, seed = 4,
      location = one_loc, numrow = 1, numcol = 1)  
  temp <- run_scenario(ncores = 6, loop_over = seq(100, 500, by = 25),
            ctl_in = ctl, to_change = 'nfish1', add_index = FALSE)
  prob_smallfish_plot[[jj]] <- temp[[3]]
  prob_smallfish_plot[[jj]]$prob <- probs[jj]
}

prob_smallfish_plot <- ldply(prob_smallfish_plot)
prob_smallfish_plot$prob <- as.character(prob_smallfish_plot$prob)

#Plot results
prob_smallfish_plot %>% filter(variable == 'cpue1') %>% ggplot() + 
  geom_line(aes(x = nfish, y = cpue, group = prob, colour = prob), size = 1) + 
  facet_wrap(~ nfish1) + geom_point(aes(x = nfish, y = cpue, group = prob, colour = prob))

#Goal is to find number of fish per cell that won't lead to hyperdepletion

#1000 fish per cell might be chill?

#For these cases, how many years does it take to deplete the population? 

#--------------------------------------------------------------------------------------------
#At what probabilities will you catch everything?  
#aka how much hook attraction can you get?
#This is still in one location also

ctl <- make_ctl(distribute = 'patchy', mortality = 0, move_out_prob = .5,
    nfish1 = 200, nfish2 = 0, prob1 = probs[jj], prob2 = 0, nyear = 15, scope = 1, seed = 4,
    location = one_loc, numrow = 1, numcol = 1)  

temp <- run_scenario(ncores = 6, loop_over = seq(.05, 1, by = .05),
          ctl_in = ctl, to_change = 'prob1', add_index = FALSE)

#Is there a difference between probabilities? probably not really
xx <- temp$for_plot %>% filter(variable == 'cpue1')
xx %>% filter(prob1 == 0.2)

#Look at all the plots
temp$for_plot %>% filter(variable == 'cpue1') %>% 
  ggplot(aes(x = year, y = cpue)) + 
  geom_line() + geom_point() + facet_wrap(~ prob1)

#Once probs get over like 0.1, hook attraction is the same


#--------------------------------------------------------------------------------------------
#Two species, one location
#Try with different probabilities

#fish1 numbers low, prob1 high
#fish2 numbers vary, prob2 high

# fish2s <- seq(100, 1500, by = 100)
fish2s <- seq(100, 1500, by = 100)

ctl <- make_ctl(distribute = 'patchy', mortality = 0, move_out_prob = .5,
      nfish1 = 10000, nfish2 = 0, prob1 = .05, prob2 = .01, nyear = 15, scope = 1, seed = 4,
      location = one_loc, numrow = 1, numcol = 1, comp_coeff = .9)  
fish2_out <- run_scenario(ncores = 6, loop_over = fish2s, ctl_in = ctl, to_change = 'nfish2',
  add_index = FALSE)
ggplot(fish2_out$for_plot) + geom_line(aes(x = nfish_tot, y = value, group = variable,
  colour = variable)) + facet_wrap(~ nfish2)



#Can't get a ton of spread by modifying prob1 and prob2 or numbers of fish
#Try getting a difference
#comp_coeff is competition coefficient
n1start <- 100
n2start <- 100
comp_coeff <- .9 #Favor species 1...

sample_exp1(nfish1 = n1start, nfish2 = n2start, prob1 = .05, prob2 = .05, 
  comp_coeff = .9)

sample_exp1 <- function(nfish1, nfish2, prob1, prob2, comp_coeff){
  #------------------------------------------------
  #Define probabilities based on number of fish

  #Might need to adjust the shape of this curve
  #Can adjust these to account for behavior of certain species
  p1 <- 1 - exp(-nfish1 * prob1) #use prob 1 to define probability of catching fish 1
  p2 <- 1 - exp(-nfish2 *  prob2) #use prob2 to define probability of catching fish 2

  #Probability of catching a fish
  hook_prob <- 1 - ((1 - p1) * (1 - p2))

  fish <- rbinom(n = 1, size = 1,  prob = hook_prob)  
  #------------------------------------------------
  # Which fish was caught?
  #initially declare both as 0
  fish1 <- 0
  fish2 <- 0

  #If a fish was caught determine if it was fish1 or fish2
  if(fish == 1 & is.na(comp_coeff)){
    p1a <- p1 / (p1 + p2)  
    fish1 <- rbinom(n = 1, size = 1, prob = p1a)
  }
  
  if(fish == 1 & is.na(comp_coeff) == FALSE){
    # p1a <- p1 / (p1 + p2)  
    fish1 <- rbinom(n = 1, size = 1, prob = comp_coeff)
  }

  if(fish1 == 0 & fish == 1){    
    fish2 <- 1
  } 
  
  #Return values as data frame
  return(data.frame(fish1 = fish1, fish2 = fish2))
  
}













#--------------------------------------------------------------------------------------------
#1000 of species 1 with p = 0.02
#500 of species2 with p = 0.05

ctl <- make_ctl(distribute = 'uniform', mortality = .1, move_out_prob = .5,
  nfish1 = 100000, nfish2 = 100000, prob1 = .02, prob2 = 0.05, nyear = 15, scope = 1, seed = 4,
  location = def_locs)  

two_spp <- run_scenario(ctl_in = ctl, loop_over = locs_list, ncores = 6,
  to_change = 'location', add_index = TRUE)

ggplot(two_spp[[3]]) + geom_line(aes(x = nfish, y = cpue, colour = location, group = location)) + 
  geom_point(aes(x = nfish, y = cpue, colour = location, group = location)) + facet_wrap(~ variable)




























