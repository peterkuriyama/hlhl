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

