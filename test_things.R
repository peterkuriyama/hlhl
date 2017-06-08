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
setwd("/Users/peterkuriyama/School/Research/hlsimulator")

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

if(Sys.info()['sysname'] == 'Windows'){
  setwd("C://Users//Peter//Desktop//hlsimulator")
}

library(hlsimulator)
#--------------------------------------------------------------------------------------------
#Define scenarios for all simulations
shape_list1 <- data.frame(scen = c('leftskew', 'rightskew', 'normdist', 'uniform', 'patchy'),
  shapes1 = c(10, 1, 5, 1, .1),
  shapes2 = c(1 , 10 ,5, 1, 10))
shape_list1$for_plot <- c('Left Skew', 'Right Skew', 'Normal', 'Uniform', 'Patchy')

ctl1 <- make_ctl(distribute = 'beta', mortality = 0, move_out_prob = .05, 
      nfish1 = 100000,
      nfish2 = 0, prob1 = .01, prob2 = .05, nyear = 2, scope = 0, seed = 1,
      location = data.frame(vessel = 1, x = 1, y = 1), numrow = 30, numcol = 30,
      shapes = c(.1, .1) , max_prob = 0, min_prob = 0, comp_coeff = .5, niters = 1)    

#--------------------------------------------------------------------------------------------
#Source figures

#Figure 1
source('figs/hlfig1.R')

#Figure 2
source('figs/hlfig2.R')

#Figure 3
source('figs/hlfig3.R')

#Figure 4
source('figs/hlfig4.R')

#Figure 5
source('figs/hlfig5.R')

#Figure 6
source('figs/hlfig6.R')

#Figure 7
source('figs/hlfig7.R')

#Figure 8
source('figs/hlfig8.R')

#Figure 9
source('figs/hlfig9.R')

#Figure 10
source('figs/hlfig10.R')

#-----------------------------------------------------------------------------
#Plot of ranges for uncertainties in fig. 2
to_plot$unc <- to_plot$q95 - to_plot$q5

to_plot$init_dist_fac <- factor(to_plot$init_dist, levels = c('leftskew', 'normdist',
  'uniform', 'patchy'))

png(width = 11.4, height = 9.24, file = 'figs/hlfig2_uncertainty.png', res = 100, units = 'in')
ggplot(to_plot) + geom_point(aes(x = dep, y = unc, colour = type)) + 
  facet_wrap(~ init_dist_fac + nsites) + ylab("Range of uncertainty") + xlab("Depletion")
dev.off()

#-----------------------------------------------------------------------------
#Random plots for presentation
qs <- .01
ns <- 1:1000
ps <- 1 - exp(-ns * qs)
plot(ns, ps)



#-----------------------------------------------------------------------------
#Figure S1 - Figure of competition scenarios with proportion of fish1
#-----------------------------------------------------------------------------
source('figs/hlfigS1.R')

#-----------------------------------------------------------------------------
#Figure S2 - Sensitivity to prob1 values
#-----------------------------------------------------------------------------
load('output/twospp1_50sens.Rdata')
onespp_sens <- twospp1
onespp_sens$dep <- onespp_sens$nfish1 / max(onespp_sens$nfish1)
onespp_sens$dep <- as.factor(onespp_sens$dep)

s1 <- onespp_sens %>% filter(init_dist %in% c('normdist', 'patchy'), type == 'pref')

png(width = 15, height = 5, file = 'figs/hlfigS2.png', units = 'in', res = 150)
ggplot(s1, aes(x = dep, y = cpue)) + geom_boxplot(aes()) + 
  facet_wrap(~ init_dist + prob1, ncol = 10)
dev.off()




