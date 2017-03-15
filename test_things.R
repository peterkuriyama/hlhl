setwd("/Users/peterkuriyama/School/Research/hlsimulator")

library(devtools)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(doParallel)
library(parallel)
library(foreach)
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

#--------------------------------------------------------------------------------------------
#Add option to distribute fish with beta distribution
#normal distribution, 10, 10
#Equal high and low, 1, 1

#Few sites with many fish, .1, 10, very patchy
#Many sites with many fish c(3, 1) - approaches uniform numbers of fish
xx <- seq(0, 1, length = 21)

#--------------------------------------------------------------------------------------------
#Patchy fishing with beta distribution
#patchy, .1, .1



#Define sites
sites <- list(pick_sites(ctl = ctl, nbest = 30),
              pick_sites(ctl = ctl, nbest = 10, nmed = 10, nbad = 10),
              pick_sites(ctl = ctl, nbest = 20, nbad = 10))

#Loop over fish numbers
ttest <- change_two(thing1 = seq(1000, 50000, by = 2000), name1 = 'nfish1',
                    thing2 = sites, name2 = 'location', ncores = 6, add_index = TRUE,
                    ctl = ctl)
focus <- ttest[[3]] %>% filter(spp == 'spp1' & year == 1)
focus$dep <- focus$nfish_total / max(focus$nfish_total)



#--------------------------------------------
#Figure out how to control which function runs in parallel

#easy example
ctl <- make_ctl(distribute = 'beta', mortality = 0, move_out_prob = .05,
        nfish1 = 20000, nfish2 = 0, prob1 = .01, prob2 = .05, nyear = 2, scope = 0, seed = 4,
        location = data.frame(vessel = 1, x = 1, y = 1), numrow = 10, numcol = 10, 
        shapes = c(.1, .1))  

for(tt in 1:2){
  tl_list[[tt]] <- twenty_locs[1:tt, ]
}

#I think this works
ttt <- change_two(thing1 = seq(1000, 3000, by = 1000), name1 = 'nfish1',
  thing2 = tl_list, name2 = 'location', ctl = ctl, ncores = 3, 
  par_func = 'change_two', add_index = FALSE)








#--------------------------------------------
#Only good and bad sites...
nbests <- 1:20
gb_sites <- lapply(nbests, FUN = function(x){
  pick_sites(ctl = ctl, nbest = x, nbad = 20 - x)
})

two_sites <- gb_sites[1:2]

#Try running this for multiple replicates
reps <- run_replicates(niters = 5, thing1 = seq(1000, 2000, by = 1000), name1 = 'nfish1',
  thing2 = two_sites, name2 = 'location', ncores = 2, add_index = TRUE, ctl = ctl)



gb_test <- change_two(thing1 = seq(1000, 50000, by = 2000), name1 = 'nfish1',
                    thing2 = gb_sites, name2 = 'location', ncores = 6, add_index = TRUE,
                    ctl = ctl)


focus <- gb_test[[3]] %>% filter(spp == 'spp1' & year == 1)
focus$dep <- focus$nfish_total / max(focus$nfish_total)

#round depletion leve
focus$rd_dep <- round(focus$dep, digits = 2)

ggplot(focus, aes(x = dep, y = cpue, group = index, colour = index)) + 
  geom_point() + facet_wrap(~ index)

#Look at this as a boxplot for rounded depletion levels
ggplot(focus) + geom_boxplot(aes(factor(rd_dep), cpue))


#--------------------------------------------
# Test many iterations of each gb_sites


#--------------------------------------------
#Run 50 iterations of one location and number of fish
temp_loc <- gb_sites[[20]]
seeds <- 1:50

gb_iters <- change_two(thing1 = seq(1000, 50000, by = 1000), name1 = 'nfish1',
  thing2 = seeds, name2 = "seed", ncores = 6, ctl = ctl)
focus <- gb_iters[[3]] %>% filter(spp == 'spp1' & year == 1)
focus$dep <- focus$nfish_total / max(focus$nfish_total)
focus$rd_dep <- round(focus$dep, digits = 2)

ggplot(focus, aes(x = dep, y = cpue, group = index, colour = index)) + geom_point()

ggplot(focus) + geom_violin(aes(factor(rd_dep), cpue))



seq(1000, 50000, by = 2000)








#--------------------------------------------------------------------------------------------
#Fish once for specific number of fish, loop over number of initial fish
ctl <- make_ctl(distribute = 'uniform', mortality = 0, move_out_prob = .05,
        nfish1 = 20000, nfish2 = 0, prob1 = .01, prob2 = .05, nyear = 2, scope = 0, seed = 4,
        location = def_locs, numrow = 10, numcol = 10)  

dd <- change_two(thing1 = seq(.01, .05, by = .01), thing2 = seq(1000, 50000, by = 1000),
  name1 = 'prob1', name2 = 'nfish1', ncores = 2, ctl = ctl)

focus <- dd[[3]] %>% filter(spp == 'spp1' & year == 1 & cpue <= 0.9)
focus$dep <- focus$nfish_total / max(focus$nfish_total)

#standardize the nfish_total
focus %>% ggplot(aes(x = dep, y = cpue, group = prob1, colour = prob1)) + 
  geom_point(aes(size = nfish_total)) + geom_line()


#-------------------------------------
#Two Species
 
conduct_survey(ctl)










#----------------------------------------------------------------------------------------
#Add in hotspot feature


ctl <- make_ctl(distribute = 'hs', mortality = 0, move_out_prob = .05,
      nfish1 = 20000, nfish2 = 0, prob1 = .01, prob2 = .05, nyear = 15, scope = 0, seed = 4,
      location = def_locs, numrow = 10, numcol = 10, hs_loc = data.frame(x = c(3, 4), 
        y = c(5, 5)), hs_scope = 1, delta = 2)  
dd <- conduct_survey(ctl = ctl)


#----------------------------------------------------------------------------------------
##Number of hooks/locations vs. number of total fish
#Does patchiness affect the relationship?

#----------------------------------------
#Run1
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
png(width = 13, height = 8, res = 100, file = 'figs/run1_200.png', units = 'in')
nlocs_onespp_out[[3]] %>% filter(spp == 'spp1') %>% ggplot() + 
  geom_line(aes(x = nfish_total, y = cpue)) + 
  geom_point(aes(x = nfish_total, y = cpue, size = prop_of_unfished)) + 
  facet_wrap(~ index) + ylim(c(0, 1))
dev.off()

#Now with two species
#Cpue goes up as you fish one spp down
ctl <- make_ctl(distribute = 'uniform', mortality = 0, move_out_prob = .5,
      nfish1 = 5000, nfish2 = 30000, prob1 = .01, prob2 = .01, nyear = 15, scope = 1, seed = 4,
      location = def_locs, numrow = 10, numcol = 10, comp_coeff = .7)  

#2000 fish per cell, you shouldn't fish it down really
nlocs_twospp_out <- run_scenario(ctl_in = ctl, loop_over = locs_list, ncores = 6,
  to_change = 'location', add_index = TRUE)
 
#Plot 
png(width = 13, height = 8, res = 100, file = 'figs/run1_twospp_fishdown.png', units = 'in')
nlocs_twospp_out[[3]] %>% ggplot() + 
  geom_line(aes(x = nfish_total, y = cpue, colour = spp)) + 
  geom_point(aes(x = nfish_total, y = cpue, colour = spp)) + 
  facet_wrap(~ index) + ylim(c(0, 1))
dev.off()


#Both species at relatively even levels, slight competition
ctl <- make_ctl(distribute = 'uniform', mortality = 0, move_out_prob = .5,
      nfish1 = 30000, nfish2 = 30000, prob1 = .02, prob2 = .05, nyear = 15, scope = 1, seed = 4,
      location = def_locs, numrow = 10, numcol = 10, comp_coeff = .7)  

#2000 fish per cell, you shouldn't fish it down really
nlocs_twospp_out <- run_scenario(ctl_in = ctl, loop_over = locs_list, ncores = 6,
  to_change = 'location', add_index = TRUE)
 
#Plot 
png(width = 13, height = 8, res = 100, file = 'figs/run1_twospp_even.png', units = 'in')
nlocs_twospp_out[[3]] %>% ggplot() + 
  geom_line(aes(x = nfish_total, y = cpue, colour = spp)) + 
  geom_point(aes(x = nfish_total, y = cpue, colour = spp)) + 
  facet_wrap(~ index) + ylim(c(0, 1))
dev.off()


#Straight line
nlocs_onespp_out[[3]] %>% filter(spp == 'spp1') %>% ggplot() + 
  geom_line(aes(x = prop_of_unfished, y = cpue))  + 
  facet_wrap(~ index)

#Doesn't matter if 1000 fish are in each place, 
#Doesn't matter if 500 fish in each site
#200 fish start to see declines
#If 100 fish in each site, pattern is like connect the dots going down

#----------------------------------------
#Run2
#Repeat plot above, but with patchy fish distribution

#Effect of patch distribution and number of fishing locations
#Keep number of fish per cell standardized
perc <- seq(0.1, 1, by = .1)
perc_outs <- vector('list', length = length(perc))

# Come up with way to visualize overlap

#Check initial population
ctl <- make_ctl(distribute = 'patchy', mortality = 0, move_out_prob = .5,
      nfish1 = 20000, nfish2 = 0, prob1 = .02, prob2 = .05, nyear = 15, scope = 1, seed = 10,
      location = def_locs, numrow = 10, numcol = 10, percent = perc[3])  
initialize_population(ctl = ctl, nfish = 10000)


for(pp in 1:length(perc)){
  print(pp)  
  ctl <- make_ctl(distribute = 'patchy', mortality = 0, move_out_prob = .5,
      nfish1 = 20000, nfish2 = 0, prob1 = .02, prob2 = .05, nyear = 15, scope = 1, seed = 10,
      location = def_locs, numrow = 10, numcol = 10, percent = perc[pp])  
  perc_outs[[pp]] <- run_scenario(ctl_in = ctl, loop_over = locs_list, ncores = 6,
    to_change = 'location', add_index = TRUE)  
}

po_plots <- lapply(perc_outs, FUN = function(xx) xx[[3]])
names(po_plots) <- as.character(perc)
po_plots <- ldply(po_plots)
names(po_plots)[1] <- 'perc_dist'


#Fish in more locations with different distributions of fish
png(width = 12, height = 7.5, units = 'in', res = 100, file = 'figs/run2.png')
po_plots %>% filter(spp == 'spp1') %>% ggplot(aes(x = nfish_total, y = cpue)) + 
  geom_line(aes(group = index, colour = index)) +
  geom_point(aes(group = index, colour = index)) + facet_wrap(~ perc_dist)
dev.off()


#----------------------------------------
#Run3
#What percentage of best spots do you fish in?

#####Do this but loop over nfish 1 and look at the effect of sampling a larger
#and larger proportion of the total population
ctl <- make_ctl(distribute = 'patchy', mortality = 0, move_out_prob = .5,
      nfish1 = 80000, nfish2 = 0, prob1 = .02, prob2 = .05, nyear = 15, scope = 1, seed = 10,
      location = def_locs, numrow = 10, numcol = 10, percent = .5)  

initialize_population(ctl = ctl, nfish = ctl$nfish1)

sum(melt(initialize_population(ctl = ctl, nfish = ctl$nfish1))$value != 0)

props <- seq(.1, 1, .1)
locs_props <- lapply(props, FUN = function(yy){
                out <- sample_good_locs(ctl = ctl, prop_good = yy, ngoods = 30, 
                  which_spp = 'spp1')
              })

locs_props_outs <- run_scenario(ctl_in = ctl, loop_over = locs_props, 
    ncores = 6, to_change = 'location', add_index = TRUE)[[3]]

locs_props_outs %>% filter(spp == 'spp1') %>% ggplot() + 
  geom_line(aes(x = nfish_total, y = cpue)) + 
  geom_point(aes(x = nfish_total, y = cpue)) + 
   ylim(c(0, 1)) + facet_wrap(~ index)


locs_props_outs %>% filter(spp == 'spp1') %>% ggplot() + 
  geom_line(aes(x = prop_of_pop, y = cpue)) + facet_wrap(~ index)

locs_props_outs %>% filter(index == 1 & spp == 'spp1')


#Sampling large part of population, get pretty consistent results

#15 spots
#5 spots in high areas, 5 next to some cells, 5 far from anything
#10 next to some cells, 5 far from anything
#10 in high areas, 5 next to something
#15 in places with fish

locs_good_bad <- vector('list', length = 4)

#Look at everything to find it
initialize_population(ctl = ctl, nfish = ctl$nfish1)

#Function to calculate the proportions of sampling locations that are good and bad
sample_good_locs(ctl = ctl, prop_good = .5, ngoods = 15)





find_gb <- melt(initialize_population(ctl = ctl, nfish = ctl$nfish1))

five_bad <- as.data.frame(rbind(c(4, 2), c(1, 9), c(5, 5), c(1, 10), c(8, 9)))
names(five_bad) <- c('Var1', 'Var2')

these_zero <- which(diff(find_gb$value) == 0)
next_to <- which(diff(find_gb$value) != 0) - 1

set.seed(3)
five_next <- find_gb[sample(these_zero[these_zero %in% next_to], 5), c("Var1", 'Var2')]
ten_next <- find_gb[sample(these_zero[these_zero %in% next_to], 10, replace = FALSE), c('Var1', 'Var2')]

five_good <- find_gb[sample(which(find_gb$value != 0), 5), c("Var1", "Var2")]
ten_good <- find_gb[sample(which(find_gb$value != 0), 10), c("Var1", "Var2")]
fifteen_good <- find_gb[sample(which(find_gb$value != 0), 15), c("Var1", "Var2")]

locs_good_bad[[1]] <- rbind(five_good, five_next, five_bad)
locs_good_bad[[2]] <- rbind(ten_next, five_bad)
locs_good_bad[[3]] <- rbind(ten_good, five_next)
locs_good_bad[[4]] <- fifteen_good

#Check for duplicates
sum(duplicated(locs_good_bad[[1]]))

locs_good_bad <- lapply(locs_good_bad, FUN = function(x){
  x$vessel <- rep(1, nrow(x))
  names(x)[1:2] <- c('x', 'y')
  x <- x[, c('vessel', 'x', 'y')]
  return(x)  
})

#Run the scenario
ctl <- make_ctl(distribute = 'patchy', mortality = 0, move_out_prob = .5,
      nfish1 = 10000, nfish2 = 0, prob1 = .02, prob2 = .05, nyear = 15, scope = 1, seed = 10,
      location = def_locs, numrow = 10, numcol = 10, percent = .5)  

good_bad <- run_scenario(ctl_in = ctl, loop_over = locs_good_bad, ncores = 6,
    to_change = 'location', add_index = TRUE)[[3]]

#Plots
good_bad %>% filter(spp == 'spp1') %>% ggplot() + 
  geom_line(aes(x = nfish_total, y = cpue)) + 
  geom_point(aes(x = nfish_total, y = cpue, size = prop_of_unfished)) + 
  facet_wrap(~ index)

#Straight line
good_bad %>% filter(spp == 'spp1') %>% ggplot() + 
  geom_line(aes(x = prop_of_unfished, y = cpue, colour = index))

good_bad %>% filter(spp == 'spp1') %>% group_by(index) %>%
   summarize(mean(prop_of_pop, na.rm = TRUE))



















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





#--------------------------------------------------------------------------------------------
#Debugging place

#add in recruitment
ctl <- make_ctl(distribute = 'patchy', mortality = 0, move_out_prob = .5,
      nfish1 = 80000, nfish2 = 0, prob1 = .02, prob2 = .05, nyear = 15, scope = 0, seed = 10,
      location = locs_props[[1]][1, ], numrow = 10, numcol = 10, percent = .5, 
      rec_years = 1:15,
      rec_rate = .1)  
outs <- conduct_survey(ctl = ctl)





















