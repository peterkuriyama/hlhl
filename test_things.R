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

#To-Do:
#1. Explore patchiness configurations
#3. Scenarios:
  #Increasing/decreasing Trend
  #Gear saturation
  #Aggressive behavior

#Default locations, 15% of available sites

#--------------------------------------------------------------------------------------------
#RUN 1 - Increasing number of sites from 2-20
#--------------------------------------------------------------------------------------------
#Set Up Values for this run

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





fishes <- seq(10000, 50000, by = 10000)
run_locs(shape_list = shape_list1,
  loc_scenario = 'pick', loc_list = pick_locs1,
  ncores = 6, ctl_o = ctl1, thing1 = fishes,
  name1 = 'nfish1')



#Distribution Scenarios
#---------------------------------------------
#Patchy




#--------------------------------------------------------------------------------------------
#RUN 2
#--------------------------------------------------------------------------------------------

#20 sites total:
  #70% good, 20% med, 10% bad
  #70% good, 30% med
  #70% good, 30% bad
  #60% good, 30% med, 10% bad
  #60% good, 40% bad
  #80% good, 20% bad
  #80% good, 10% med, 10% bad


pick_locs2 <- data.frame(nbests = c(.7, .7, .7, .6, .6, .7, .8),
                nmeds = c(.2, .3, 0, .3, 0, 0, .1),
                nbads = c(.1, 0, .3, .1, .4, .2, .1))
(pick_locs2 <- pick_locs2 * 20)

#Picking some number of good, med, bad sites
#Increasing number of sites from 2-20
fishes <- seq(1000, 50000, by = 2000)

ctl2 <- make_ctl(distribute = 'beta', mortality = 0, move_out_prob = .05, nfish1 = 10000,
      nfish2 = 0, prob1 = .01, prob2 = .05, nyear = 1, scope = 0, seed = 1,
      location = data.frame(vessel = 1, x = 1, y = 1), numrow = 10, numcol = 10,
      shapes = c(.1, .1) , max_prob = 0, min_prob = 0, comp_coeff = .5, niters = 1)    

#Distribution Scenarios
#---------------------------------------------
#Patchy
ctl2$shapes <- c(.1, 10)
init2 <- initialize_population(ctl = ctl2, nfish = ctl2$nfish1)
hist(init2, breaks = 30)

##Med/bad locations both have 0 fish
locs2 <- lapply(1:nrow(pick_locs2), FUN = function(xx){
  pick_sites(nbest = pick_locs2[xx, 1], nmed = pick_locs2[xx, 2], 
    nbad = pick_locs2[xx, 3], fish_mat = init2)
})

patchy2 <- change_two(thing1 = fishes, thing2 = locs2, name1 = 'nfish1', 
  name2 = 'location', ctl = ctl1, index1 = FALSE, index2 = TRUE, par_func = 'change_two',
  ncores = nncores)

#Save the results
save(patchy2, file = 'output/2_patchy.Rdata')

#---------------------------------------------
#2. rightskew Right skewed distribution of fish
ctl2$shapes <- c(1, 10)
init2 <- initialize_population(ctl = ctl2, nfish = ctl2$nfish1)
locs2 <- lapply(1:nrow(pick_locs2), FUN = function(xx){
  pick_sites(nbest = pick_locs2[xx, 1], nmed = pick_locs2[xx, 2], 
    nbad = pick_locs2[xx, 3], fish_mat = init2)
})

rightskew2 <- change_two(thing1 = fishes, thing2 = locs2, name1 = 'nfish1', 
  name2 = 'location', ctl = ctl1, index1 = FALSE, index2 = TRUE, par_func = 'change_two',
  ncores = nncores)

#Save the results
save(rightskew2, file = 'output/2_rightskew.Rdata')

#---------------------------------------------
#3. normdist Normal dist c(10, 10)
ctl2$shapes <- c(10, 10)
init2 <- initialize_population(ctl = ctl2, nfish = ctl2$nfish1)

locs2 <- lapply(1:nrow(pick_locs2), FUN = function(xx){
  pick_sites(nbest = pick_locs2[xx, 1], nmed = pick_locs2[xx, 2], 
    nbad = pick_locs2[xx, 3], fish_mat = init2)
})

normdist2 <- change_two(thing1 = fishes, thing2 = locs2, name1 = 'nfish1', 
  name2 = 'location', ctl = ctl1, index1 = FALSE, index2 = TRUE, par_func = 'change_two',
  ncores = nncores)

#Save the results
save(normdist2, file = 'output/2_normdist.Rdata')

#---------------------------------------------
#4. uniform c(10, .1)
ctl2$shapes <- c(10, .10)
init2 <- initialize_population(ctl = ctl2, nfish = ctl2$nfish1)

locs2 <- lapply(1:nrow(pick_locs2), FUN = function(xx){
  pick_sites(nbest = pick_locs2[xx, 1], nmed = pick_locs2[xx, 2], 
    nbad = pick_locs2[xx, 3], fish_mat = init2)
})

unif2 <- change_two(thing1 = fishes, thing2 = locs2, name1 = 'nfish1', 
  name2 = 'location', ctl = ctl1, index1 = FALSE, index2 = TRUE, par_func = 'change_two',
  ncores = nncores)

#Save the results
save(unif2, file = 'output/2_unif.Rdata')
#Hypothesize that scenario 3 and 4 will be the same

#---------------------------------------------
#results and compare
run2 <- list.files("output")[grep("2", list.files("output")) ]

files2 <- paste0('output/', run2)
for(ff in 1:length(files2)){
  load(files2[ff])
}

res2 <- list(patchy = patchy2, rightskew = rightskew2, normdist = normdist2,
  unif = unif2)
res2 <- lapply(res2, FUN = function(ff) ff[[3]])
res2 <- ldply(res2)
names(res2)[1] <- 'dist'
res2 <- res2 %>% filter(spp == 'spp1' & year == 1)
res2 <- res2 %>% group_by(dist) %>% mutate(dep = nfish_total / max(nfish_total)) %>%
  as.data.frame
res2$location <- factor(res2$location, levels = unique(res2$location))

#Add more descriptive names
descs <- data.frame(location = as.character(unique(res2$location)), 
                    desc = c("70% good, 20% med, 10% bad",
                             "70% good, 30% med",
                             "70% good, 30% bad",
                             "60% good, 30% med, 10% bad",
                             "60% good, 40% bad",
                             "80% good, 20% bad",
                             "80% good, 10% med, 10% bad"))
descs$location <- factor(descs$location, levels = descs$location)

res2 <- inner_join(res2, descs, by = "location")

ggplot(res2, aes(x = dep, y = cpue)) + geom_point(aes(colour = dist)) + facet_wrap(~ location)

#Can get lots of spread in the data based on the sampling locations
ggplot(res2, aes(x = dep, y = cpue)) + geom_point(aes(colour = dist)) + 
  facet_wrap(~ desc) + ylim(c(0, 1))

# pull 60s only
res2[grep('60', res2$desc), ] %>% ggplot(aes(x = dep, y = cpue)) + geom_point(aes(colour = desc)) +
  facet_wrap(~ dist) + ylim(c(0, 1))

names(locs2) <- descs$desc

visualize_fishing(loc = locs2[[1]], init2)


#Visualize Fishing manually
pdf(width = 8.8, height = 5.5, file = 'figs/locs2.pdf')
for(ll in 1:length(locs2)){
  print(visualize_fishing(loc = locs2[[ll]], init2))
}
dev.off()


#--------------------------------------------------------------------------------------------
#RUN 3 - Two Species
#--------------------------------------------------------------------------------------------

fishes <- seq(1000, 50000, by = 2000)

ctl3 <- make_ctl(distribute = 'beta', mortality = 0, move_out_prob = .05, nfish1 = 10000,
      nfish2 = 10000, prob1 = .01, prob2 = .05, nyear = 1, scope = 0, seed = 1,
      location = data.frame(vessel = 1, x = 1, y = 1), numrow = 10, numcol = 10,
      shapes = c(.1, .1) , max_prob = 0, min_prob = 0, comp_coeff = .5, niters = 1)    

#Distribution Scenarios
#---------------------------------------------
#Patchy
ctl3$shapes <- c(.1, 10)

#Fish in 20 locations
init3 <- initialize_population(ctl = ctl3, nfish = ctl3$nfish1)
ctl3$location <- pick_sites(nbest = 20, fish_mat = init3)

patchy3 <- change_two(thing1 = fishes, thing2 = rev(fishes), name1 = 'nfish1', 
  name2 = 'nfish2', ctl = ctl3, index1 = FALSE, index2 = FALSE, par_func = 'change_two',
  ncores = nncores)

#Save the results
save(patchy3, file = 'output/3_patchy.Rdata')
plot3 <- patchy3[[3]] %>% group_by(spp) %>% mutate(dep = nfish_total / max(nfish_total)) %>%
  as.data.frame
plot3 <- plot3 %>% filter(year == 1)

#reformat data to plot pairs of cpue
cpue3 <- left_join(plot3 %>% filter(spp == 'spp1') %>% select(nfish1, nfish2, cpue), 
          plot3 %>% filter(spp == 'spp2') %>% select(nfish1, nfish2, cpue), 
          by = c('nfish1', 'nfish2'))
ggplot(cpue3, aes(x = cpue.x, y = cpue.y)) + geom_point()


ggplot(plot3, aes(x = dep, y = cpue)) + geom_line(aes(colour = spp, group = paste(nfish1, nfish2))) 



#Now add in more attraction with competition_coeff
ctl3$comp_coeff <- .8

patchy3.1 <- change_two(thing1 = fishes, thing2 = rev(fishes), name1 = 'nfish1', 
  name2 = 'nfish2', ctl = ctl3, index1 = FALSE, index2 = FALSE, par_func = 'change_two',
  ncores = nncores)

#Save the results
save(patchy3.1, file = 'output/3.1_patchy.Rdata')

plot3.1 <- patchy3.1[[3]] %>% group_by(spp, year) %>% mutate(dep = nfish_total / max(nfish_total)) %>%
  as.data.frame

plot3.1 <- plot3.1 %>% filter(year == 1)
plot3.1$cpue_spp <- gsub('spp', 'cpue', plot3.1$spp)
plot3.1$dep_spp <- gsub('spp', 'dep', plot3.1$spp)

#Cpues
# r1 <- plot3.1 %>% select(nfish1, nfish2, cpue_spp, cpue) %>% 
#   dcast(nfish1 + nfish2 ~ cpue_spp, value.var = "cpue")
#deps
r1 <- plot3.1 %>% select(nfish1, nfish2, dep_spp, dep) %>% 
  dcast(nfish1 + nfish2 ~ dep_spp, value.var = "dep")

r2 <- plot3.1 %>% select(nfish1, nfish2, spp, cpue)

cpue31 <- inner_join(r1, r2, by = c('nfish1', 'nfish2'))

#Plot comparison of two species with competiton and different depletion levels
ggplot(cpue31) + geom_tile(aes(x = dep1, y = dep2, colour = cpue), size = 4) + 
  facet_wrap(~ spp)


#look at the differences in cpues as depletion goes up or down...







#--------------------------------------------------------------------------------------------
#look more into the right skew results
ctl2$shapes <- c(1, 9)
init2 <- initialize_population(ctl = ctl2, nfish = ctl2$nfish1)
hist(init2, breaks = 30)

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





