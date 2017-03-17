####Whitefish Runs


##Set working directory
setwd("C://Users//Peter//Desktop//hlsimulator")

library(devtools)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(doParallel)
library(parallel)

library(sendmailR)

#--------------------------------------------------------------------------------------------
#Options to load the package

#From github straight
install_github('peterkuriyama/hlsimulator')
library(hlsimulator)

#--------------------------------------------------------------------------------------------
#Run with increasing number of locations
#--------------------------------------------------------------------------------------------

#Patchy distribution
#Single Species
#Increasing number of "good" sites
#Do 10 iterations then increase to 100?

#------------------------------
#Run 1
#Make sure to start this with
ctl1 <- make_ctl(distribute = 'beta', mortality = 0, move_out_prob = .05,
        nfish1 = 10000, nfish2 = 0, prob1 = .01, prob2 = .05, nyear = 2, scope = 0, seed = 7, 
        location = data.frame(vessel = 1, x = 1, y = 1), numrow = 10, numcol = 10, 
        shapes = c(.1, .1), max_prob = 0, min_prob = 0, comp_coeff = .5, niters = 10)  

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

# send_email(body = "whitefish run 1 start")
patch_inc_nlocs <- change_two(thing1 = seq(1000, 50000, by = 2000), name1 = 'nfish1',
  thing2 = nlocs, name2 = 'location', ctl = ctl1, ncores = 10, index1 = FALSE, 
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

