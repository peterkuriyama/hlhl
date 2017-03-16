#'Fish the Population

setwd("/Users/peterkuriyama/School/Research/hlsimulator")

library(devtools)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(doParallel)
library(parallel)
library(foreach)
library(stringr)
#--------------------------------------------------------------------------------------------
#May need to track depletion by drop at some points, this is in conduct_survey
#--------------------------------------------------------------------------------------------
#Options to load the package

#From github straight
install_github('peterkuriyama/hlsimulator')
library(hlsimulator)




#Simple run, no movement or anything

#set seed
ctl1 <- make_ctl(distribute = 'beta', mortality = 0, move_out_prob = .05,
        nfish1 = 20000, nfish2 = 10000, prob1 = .01, prob2 = .05, nyear = 2, scope = 0, seed = 7, 
        location = data.frame(vessel = 1, x = 1, y = 1), numrow = 10, numcol = 10, 
        shapes = c(.1, .1), max_prob = 0, min_prob = 0, comp_coeff = .5, niters = 10)  
# Specify fishing locations before from ctl

#Initialize populations
init_area1 <- initialize_population(ctl = ctl1, nfish = ctl1$nfish1)

#specify fishing locations, will be the same for both species
locs1 <- pick_sites(nbest = 5, fish_mat = init_area1)
locs2 <- pick_sites(nbad = 2, fish_mat = init_area1)

#Update location here
ctl1$location <- locs1

# dd <- run_replicates(ctl_in = ctl1)

locs <- list(locs1, locs2)

dd <- run_scenario(ctl_start = ctl1, loop_over = locs, to_change = 'location', add_index = TRUE,
  ncores = 2, par_func = "run_scenario")

#Now with different numbers of fish
thing2 <- locs
thing1 <- 

dd <- change_two(thing1 = seq(10000, 20000, by = 10000), name1 = 'nfish1',
  thing2 = locs, name2 = 'location', ctl = ctl1, ncores = 2, index1 = FALSE, 
  index2 = TRUE)





#pass init pops to conduct_survey function
#input relevant arguments to conduct_survey
#will be treated as ... within the function

#Repeat things for XX number of iterations
test1 <- conduct_survey(init_area = list(init_area1, init_area2), nhooks = nhooks, nangs = nangs,
  prob1 = prob1, prob2 = prob2, comp_coeff = comp_coeff, numrow = numrow, numcol = numcol,
  rec_years = rec_years, rec_rate = rec_rate, nyear = nyear, ndrops = ndrops, 
  location = location, scope = scope, mortality = mortality)

test2 <- conduct_survey(init_area = list(init_area1, init_area2), nhooks = nhooks, nangs = nangs,
  prob1 = prob1, prob2 = prob2, comp_coeff = comp_coeff, numrow = numrow, numcol = numcol,
  rec_years = rec_years, rec_rate = rec_rate, nyear = nyear, ndrops = ndrops, 
  location = location, scope = scope, mortality = mortality)

test1$fished_areas$year0
test2$fished_areas$year0
#let fishing occur, maybe many times if necessary

# run_scenario

# init_area <- list(initialize_population(ctl = xx, nfish = xx$nfish1),
      #                   initialize_population(ctl = xx, nfish = xx$nfish2))
      

      # out <- conduct_survey(init_area = init_area, nhooks = xx$nhooks,
      #   nangs = xx$nangs, prob1 = xx$prob1, prob2 = xx$prob2, comp_coeff = xx$comp_coeff, 
      #   numrow = xx$numrow, numcol = xx$numcol, rec_years = xx$rec_years, 
      #   rec_rate = xx$rec_rate, nyear = xx$nyear, ndrops = xx$ndrops, 
      #   location = xx$location, scope = xx$scope, mortality = xx$mortality)






