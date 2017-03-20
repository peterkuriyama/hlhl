
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
}

if(Sys.info()['sysname'] == 'Windows'){
  setwd("C://Users//Peter//Desktop//hlsimulator")
}


#--------------------------------------------------------------------------------------------
#May need to track depletion by drop at some points, this is in conduct_survey
#--------------------------------------------------------------------------------------------
#Options to load the package

#From github straight
install_github('peterkuriyama/hlsimulator')
library(hlsimulator)

#--------------------------------------------------------------------------------------------


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
#RUN 1
#--------------------------------------------------------------------------------------------
#Increasing number of sites from 2-20
fishes <- seq(1000, 50000, by = 2000)

ctl1 <- make_ctl(distribute = 'beta', mortality = 0, move_out_prob = .05, nfish1 = 10000,
      nfish2 = 0, prob1 = .01, prob2 = .05, nyear = 1, scope = 0, seed = 1,
      location = data.frame(vessel = 1, x = 1, y = 1), numrow = 10, numcol = 10,
      shapes = c(.1, .1) , max_prob = 0, min_prob = 0, comp_coeff = .5, niters = 1)    

#Distribution Scenarios
#---------------------------------------------
#Patchy
ctl1$shapes <- c(.1, 10)
init1 <- initialize_population(ctl = ctl1, nfish = ctl1$nfish1)

locs <- lapply(seq(2, 20, by = 2), FUN = function(ff){
  pick_sites(nbest = ff, fish_mat = init1)
})

patchy <- change_two(thing1 = fishes, thing2 = locs, name1 = 'nfish1', 
  name2 = 'location', ctl = ctl1, index1 = FALSE, index2 = TRUE, par_func = 'change_two',
  ncores = nncores)

#Save the results
save(patchy, file = 'output/1_patchy.Rdata')

#---------------------------------------------
#2. rightskew Right skewed distribution of fish
ctl1$shapes <- c(1, 10)
init1 <- initialize_population(ctl = ctl1, nfish = ctl1$nfish1)
locs <- lapply(seq(2, 20, by = 2), FUN = function(ff){
  pick_sites(nbest = ff, fish_mat = init1)
})

rightskew <- change_two(thing1 = fishes, thing2 = locs, name1 = 'nfish1', 
  name2 = 'location', ctl = ctl1, index1 = FALSE, index2 = TRUE, par_func = 'change_two',
  ncores = nncores)

#Save the results
save(rightskew, file = 'output/1_rightskew.Rdata')

#---------------------------------------------
#3. normdist Normal dist c(10, 10)
ctl1$shapes <- c(10, 10)
init1 <- initialize_population(ctl = ctl1, nfish = ctl1$nfish1)

locs <- lapply(seq(2, 20, by = 2), FUN = function(ff){
  pick_sites(nbest = ff, fish_mat = init1)
})

normdist <- change_two(thing1 = fishes, thing2 = locs, name1 = 'nfish1', 
  name2 = 'location', ctl = ctl1, index1 = FALSE, index2 = TRUE, par_func = 'change_two',
  ncores = nncores)

#Save the results
save(normdist, file = 'output/1_normdist.Rdata')

#---------------------------------------------
#4. uniform c(10, .1)
ctl1$shapes <- c(10, .10)
init1 <- initialize_population(ctl = ctl1, nfish = ctl1$nfish1)

locs <- lapply(seq(2, 20, by = 2), FUN = function(ff){
  pick_sites(nbest = ff, fish_mat = init1)
})

unif <- change_two(thing1 = fishes, thing2 = locs, name1 = 'nfish1', 
  name2 = 'location', ctl = ctl1, index1 = FALSE, index2 = TRUE, par_func = 'change_two',
  ncores = nncores)

#Save the results
save(unif, file = 'output/1_unif.Rdata')
#Hypothesize that scenario 3 and 4 will be the same

#---------------------------------------------
#results and compare
res <- list(patchy = patchy, rightskew = rightskew, normdist = normdist,
  unif = unif)
res <- lapply(res, FUN = function(ff) ff[[3]])
res <- ldply(res)
names(res)[1] <- 'dist'
res <- res %>% filter(spp == 'spp1' & year == 1)
res <- res %>% group_by(dist) %>% mutate(dep = nfish_total / max(nfish_total)) %>%
  as.data.frame
res$location <- factor(res$location, levels = unique(res$location))

ggplot(res, aes(x = dep, y = cpue)) + geom_point(aes(colour = dist)) + facet_wrap(~ location)



#--------------------------------------------------------------------------------------------
#RUN 2
#--------------------------------------------------------------------------------------------
#Picking some number of good, med, bad sites








#--------------------------------------------------------------------------------------------
#Write function that takes beta distributions of fish, 
#picks good, medium, and bad sites
#to see the effect of configurations on 

#Run 1 - mostly good sites, 15 sites total
ctl1 <- make_ctl(distribute = 'beta', mortality = 0, move_out_prob = .05, nfish1 = 10000,
      nfish2 = 0, prob1 = .01, prob2 = .05, nyear = 1, scope = 0, seed = 1,
      location = data.frame(vessel = 1, x = 1, y = 1), numrow = 10, numcol = 10,
      shapes = c(.1, .1) , max_prob = 0, min_prob = 0, comp_coeff = .5, niters = 1)    

dd <- run_locs(nbests = 5, nmeds = 5, nbads = 5, seeds = 10, ncores = 10, nsites = 15, 

#Save the results
run1 <- dd
save(run1, file = "..//hlsimulator_runs//run1.Rdata")


#Plots
for_plot <- dd[[2]]
for_plot$location <- factor(for_plot$location, levels = unique(for_plot$location))
for_plot <- for_plot %>% filter(year == 1 & spp == 'spp1')

for_plot <- for_plot %>% group_by(spp) %>% mutate(dep = nfish_total / max(nfish_orig)) %>%
  as.data.frame

for_plot %>% ggplot(aes(x = dep, y = cpue)) + geom_point(aes(colour = spp), alpha = 3/10) + 
 facet_wrap(~ location) + xlim(c(0, 1)) + ylim(c(0, 1))


#------------------------------
#Run 2 - Same as above but with different shape parameters

#Run 1 - mostly good sites, 15 sites total
ctl2 <- make_ctl(distribute = 'beta', mortality = 0, move_out_prob = .05, nfish1 = 10000,
                 nfish2 = 0, prob1 = .01, prob2 = .05, nyear = 1, scope = 0, seed = 1,
                 location = data.frame(vessel = 1, x = 1, y = 1), numrow = 10, numcol = 10,
                 shapes = c(1, .1) , max_prob = 0, min_prob = 0, comp_coeff = .5, niters = 1)    
initialize_population(ctl = ctl2, nfish = ctl2$nfish1)

send_email(subject = "run start")
dd <- run_locs(nbests = 10, nmeds = 5, nbads = 5, seeds = 10, ncores = 10, nsites = 15, 
               thing1 = seq(1000, 50000, by = 1000), name1 = 'nfish1', ctl_o = ctl2)

send_email(body = "whitefish run 1 end")

#Save the results
run2 <- dd
save(run2, file = "..//hlsimulator_runs//run2.Rdata")





#Patchy distribution
#Single Species
#Increasing number of "good" sites
#Do 10 iterations then increase to 100?


#Make sure to start this with
ctl1 <- make_ctl(distribute = 'beta', mortality = 0, move_out_prob = .05,
        nfish1 = 10000, nfish2 = 0, prob1 = .01, prob2 = .05, nyear = 2, scope = 0, seed = 2, 
        location = data.frame(vessel = 1, x = 1, y = 1), numrow = 10, numcol = 10, 
        shapes = c(.1, .1), max_prob = 0, min_prob = 0, comp_coeff = .5, niters = 1)  

#Initialize populations
init_area1 <- initialize_population(ctl = ctl1, nfish = ctl1$nfish1)

nsites_var <- expand.grid(1:5, 1:5, 1:5)
nsites_var$tot <- rowSums(nsites_var)
names(nsites_var)[1:3] <- c('good', 'med', 'bad')
nsites_var <- subset(nsites_var, tot == 10)

#Scenario with good, medium, and bad sites
nlocs <- lapply(1:nrow(nsites_var), FUN = function(xx){
  pick_sites(nbest = nsites_var[xx, 1], nmed = nsites_var[xx, 2],
    nbad = nsites_var[xx, 3], fish_mat = init_area1)
})

send_email(body = "whitefish run 1 start")
patch_inc_nlocs <- change_two(thing1 = seq(1000, 50000, by = 2000), name1 = 'nfish1',
  thing2 = nlocs, name2 = 'location', ctl = ctl1, ncores = 6, index1 = FALSE, 
  index2 = TRUE, par_func = 'change_two')[[3]]

run1 <- patch_inc_nlocs
run1$location <- factor(run1$location, levels = unique(run1$location))

#Save the data
save(run1, file = "..//hlsimulator_runs//run1.Rdata")

# send_email
# send_email(body = "whitefish run 1 done")

#Arrange run1 by loc_case
for_plot <- run1 %>% filter(year == 1 & spp == 'spp1')
for_plot <- for_plot %>% group_by(spp) %>% mutate(dep = nfish_total / max(nfish_orig)) %>%
  as.data.frame

for_plot %>% ggplot(aes(x = dep, y = cpue)) + geom_point(aes(colour = spp), alpha = 3/10) + 
 facet_wrap(~ location) + xlim(c(0, 1)) + ylim(c(0, 1))

# for_plot_1 <- for_plot 

ff <- list(for_plot_1, for_plot)
names(ff) <- c(1, 2)
ff <- ldply(ff)
names(ff)[1] <- 'seed'

ff %>% ggplot(aes(x = dep, y = cpue)) + geom_point(aes(colour = seed), alpha = 3/10) + 
 facet_wrap(~ location) + xlim(c(0, 1)) + ylim(c(0, 1))

#add in numbers of good, med, and bad sites...

 



# send_email
send_email(body = "whitefish run 1 done")

head(patch_inc_nlocs[[3]])
patch_inc_nlocs[[3]] %>% filter(spp == 'spp1', year == 1) %>% select(cpue)

#------------------------------
#Run 2, different distribution
#Different distribution
ctl1 <- make_ctl(distribute = 'beta', mortality = 0, move_out_prob = .05,
        nfish1 = 20000, nfish2 = 0, prob1 = .01, prob2 = .05, nyear = 2, scope = 0, seed = 7, 
        location = data.frame(vessel = 1, x = 1, y = 1), numrow = 10, numcol = 10, 
        shapes = c(.1, .1), max_prob = 0, min_prob = 0, comp_coeff = .5, niters = 3)  


send_email(body = "whitefish run 2 started")

send_email(body = "whitefish run 2 done")


#Run 2
#patchy distribution run
#Effect of increasing 
ctl <- make_ctl(distribute = 'beta', mortality = 0, move_out_prob = .05,
        nfish1 = 20000, nfish2 = 0, prob1 = .01, prob2 = .05, nyear = 2, scope = 0, seed = 4,
        location = data.frame(vessel = 1, x = 1, y = 1), numrow = 10, numcol = 10, 
        shapes = c(.1, .1))  

##Specify number of iterations
seeds <- 1:10

seeds_out <- vector('list', length = length(seeds))

start_time <- Sys.time()

for(ss in 1:length(seeds)){
  
  ctl$seed <- ss

##Specify number of initial fish
  #Increase number of locations
  twenty_locs <- pick_sites(ctl = ctl, nbest = 20)

  #List with increasing number of locations
  tl_list <- vector('list', length = 20)
  
  for(tt in 1:20){
    tl_list[[tt]] <- twenty_locs[1:tt, ]
  }

##Specify number of initial fish
  seeds_out[[ss]] <- change_two(thing1 = seq(1000, 50000, by = 1000), name1 = 'nfish1',
                              thing2 = tl_list, name2 = 'location', ctl = ctl,
                              ncores = )[[3]]

}

run_time <- Sys.time() - start_time

names(seeds_out) <- as.character(seeds)
s_out <- ldply(seeds_out)
names(s_out)[1] <- 'seed'

#Save the data 
save(s_out, file = "output//run1.Rdata")

# send_email
send_email(body = paste("whitefish done", run_time))





