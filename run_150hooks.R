#Initialize mega runs for 150 hooks

#mega run in computer lab
# install.packages('devtools')
# install.packages("sendmailR")

#--------------------------------------------------------------------------------------------
#Load Packages
library(devtools)
library(plyr)
library(dplyr)
library(reshape2)
library(tidyr)
library(ggplot2)
library(doParallel)
library(parallel)
library(foreach)
library(stringr)
library(sendmailR)

#Specify results directory
results_dir <- "C://Users//Peter//Dropbox//phd//research//hlsimulator//output"
setwd("/Users/peterkuriyama/Dropbox/phd/research/hlsimulator")
#--------------------------------------------------------------------------------------------
#Update directory

#Automatically detect # of cores
nncores <- detectCores() - 2

#Big Lab Mac
if(Sys.info()['sysname'] == 'Darwin' & nncores == 22){
  #Make sure to login to 
  results_dir <- "/Volumes/udrive/hlsimulator_runs"
  sys <- 'mac'
  nncores <- 20
  ##Make sure that udrive is functional
}

#My Laptop Mac
if(Sys.info()['sysname'] == 'Darwin' & nncores < 10){
  setwd("/Users/peterkuriyama/School/Research/hlsimulator")  
  type <- 'mac'
  results_dir <- "/Volumes/udrive/hlsimulator_runs"
}

#Whitefish
if(Sys.info()['sysname'] == 'Windows' & nncores == 10){
  results_dir <- "C://Users//Peter//Desktop//hlsimulator"
}

#Smaller Lab computers, save to UDRIVE
if(Sys.info()['sysname'] == 'Windows' & nncores < 10){
  # setwd("C://Users//Peter//Desktop//hlsimulator")
  results_dir <- "Z://hlsimulator_runs"
}

#Big Lab computer, save to UDRIVE
if(Sys.info()['sysname'] == 'Windows' & nncores > 11){
  nncores <- 20
  #Specify somehing here, I think it's U
  results_dir <- "U://hlsimulator_runs"
  sys <- 'pc'
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
shape_list1$for_plot <- c('Left Skew', 'Right Skew', 'Symmetric', 'Uniform', 'Patchy')

#Only run for patchy and normal
# shape_list1 <- subset(shape_list1, scen %in% c('normdist', 'patchy'))

#Keep the same prob1 and prob2

#Specify Number of hooks
num_hooks <- 40 # num_hooks * 150
num_hooks <- 10 # num_hooks * 150
hook_run <- num_hooks * 15

##Double the number of hooks##
ctl1 <- make_ctl(distribute = 'beta', mortality = 0, move_out_prob = .05, 
      nfish1 = 100000,
      nfish2 = 0, prob1 = .01, prob2 = .01, nyear = 1, scope = 0, seed = 1,
      location = data.frame(vessel = 1, x = 1, y = 1), numrow = 30, numcol = 30,
      shapes = c(.1, .1) , max_prob = 0, min_prob = 0, comp_coeff = .5, niters = 1, 
      nhooks = num_hooks)   

#--------------------------------------------------------------------------------------------
#Functions to create to_loop values
#Function to that returns rounded numbers of fish1 at evenly spaced proportions
calc_fish1_prop <- function(nfish2, prop = seq(0, .9, .1)){
  fishes <- prop * nfish2 / (1 - prop)
  fishes <- round(fishes, digits = 0)
  return(fishes)
}

#Function to create to_loop data frame
create_to_loop <- function(fishes1, fishes2, comp_coeffs = c(.3, .5, .7),
  shape_rows = c(3, 5), nsites = 50){

  to_loop <- expand.grid(fishes1, fishes2, comp_coeffs, shape_rows, c('pref', 'rand'))
  names(to_loop) <- c('nfish1', 'nfish2', 'comp_coeff', 
    'shape_list_row', 'type')
  to_loop$nsites <- nsites
  to_loop$c1_sum <- .01
  return(to_loop)
}

#--------------------------------------------------------------------------------------------
#To loop Key
# 1 - leftskew
# 2 - rightskew
# 3 - normdist
# 4 - uniform
# 5 - patchy

#--------------------------------------------------------------------------------------------
#0 - 200,000 in increments of 20,000
fishes1 <- seq(0, 200000, by = 20000)
# fishes2 <- seq(0, 200000, by = 20000)
fishes2 <- seq(0, 0, by = 0)

to_loop <- create_to_loop(fishes1 = fishes1, fishes2 = fishes2, comp_coeffs = .5,
  shape_rows = 5, nsites = 50)
#remove the rows with 0 and 0 for numbers of fish
to_loop <- to_loop[-which(to_loop$nfish1 == 0 & to_loop$nfish2 == 0), ]

#--------------------------------------------------------------------------------------------
#Only do 
#If there is a check data file there, remove it before running this again
file.remove(paste0(results_dir, '//',  'twospp1_newcc_check_5.Rdata'))

#For testing the new comp coefficient curves
nreps <- 1000

#Adjust number of reps
to_loop$nreps <- nreps

#--------------------------------------------------------------------------------------------

#Create indices for each computer, plan is to do this on five computers
tot <- 1:nrow(to_loop)

#Specify one run for each core
tots <- split(tot, ceiling(seq_along(tot) / (nrow(to_loop) / ((nrow(to_loop) / nncores)))))

#Specify Index for each computer
#-----------------
run_this_ind <- 1:2

if(length(run_this_ind) == 1) to_run <- tots[[run_this_ind]]
if(length(run_this_ind) > 1){
  to_run <- unlist(tots[run_this_ind])
  names(to_run) <- NULL
} 

#--------------------------------------------------------------------------------------------
start_time <- Sys.time()

clusters <- parallel::makeCluster(nncores)
doParallel::registerDoParallel(clusters)

twospp <- foreach(ii = to_run,
  .packages = c('plyr', 'dplyr', 'reshape2', 'hlsimulator'), .export = c("shape_list1")) %dopar% {
    fixed_parallel(index = ii, ctl1 = ctl1, to_loop = to_loop, 
      change_these = c('nfish1', 'nfish2', 'comp_coeff'))  
}

#Close clusters
stopCluster(clusters)

#Record run time
run_time <- Sys.time() - start_time

#Format output
site_cpues <- lapply(twospp, FUN = function(x) x[[2]])
twospp <- lapply(twospp, FUN = function(x) x[[1]])

twospp <- ldply(twospp)  

if(length(run_this_ind) > 1) run_this_ind <- paste(run_this_ind, collapse = "")

# assign(paste0("onespp_150_hooks"), twospp)
onespp <- twospp

#From previous runs
# filename <- paste0("twospp", run_this_ind )

#Run now, run2
filename <- paste0("onespp", "_", hook_run, "_hooks") #for new competition coefficient

#Save output in U drive
save(onespp, file = paste0(results_dir, "//" , paste0(filename, nreps, '.Rdata')))

#--------------------------------------------------------------------------------------------
#process the data

# load('output/onespp_150_hooks500.Rdata')
# onespp <- onespp_150_hooks
load('output/onespp_150_hooks1000.Rdata')
onespp$nhooks <- 150

onespp_hooks <- onespp

load('output/onespp_600_hooks1000.Rdata')
onespp$nhooks <- 600
onespp_hooks <- rbind(onespp_hooks, onespp)

#Rename the data frame
onespp <- onespp_hooks

onespp <- onespp %>% filter(spp == 'spp1')
onespp$location <- onespp$iter
onespp$dep <- onespp$nfish_orig / 200000

#Add zeroes in for sloope calculations
temp <- onespp[1, ]
temp$dep <- 0
temp$cpue <- 0

onespp <- rbind(onespp, temp)
oo <- onespp %>% complete(dep, nesting(iter, init_dist, type, nsites, nhooks), 
  fill = list(cpue = 0) ) %>% 
  as.data.frame

#Redo the index
oo <- oo %>% group_by(iter, init_dist, type, nsites, nhooks) %>% mutate(index = row_number()) %>% 
  as.data.frame 

onespp <- oo

#Need to document this part better!
onespp_summary <- calc_mare_slopes(input = oo)
mares <- onespp_summary[[1]]
mares$init_dist <- as.character(mares$init_dist)
mares$type <- as.character(mares$type)

#--------------------------------------------------------------------------------------------
# many_hooks_figs
to_plot_hooks <- onespp

to_plot_hooks <- to_plot_hooks %>% group_by(dep, init_dist, type,
  nhooks) %>% summarize(q5 = quantile(cpue, .05), q95 = quantile(cpue, .95),
  m5 = median(cpue)) %>% as.data.frame
to_plot_hooks$init_dist <- as.character(to_plot_hooks$init_dist)
to_plot_hooks$type <- as.character(to_plot_hooks$type)

to_plot_hooks$ind <- 1
to_plot_hooks[which(to_plot_hooks$nhooks == 600), 'ind'] <- 2
to_plot_hooks <- plyr::rename(to_plot_hooks, c("m5" = 'med_cpue'))
to_plot_hooks$type <- as.character(to_plot_hooks$type)
delta <- .02
fig1_letts <- paste0(letters[1:4], ")")

#Add in MARE values
to_plot_hooks <- to_plot_hooks %>% left_join(mares, 
  by = c('init_dist', 'type', 'nhooks'))

#Add in slope values
slopes <- onespp_summary[[2]] 
slopes <- plyr::rename(slopes, c('q5' = 'q5_slope', 'm5' = 'med_slope',
  'q95' = 'q95_slope'))
slopes$init_dist <- as.character(slopes$init_dist)
slopes$type <- as.character(slopes$type)
slopes <- slopes %>% select(-nsites)
slopes <- plyr::rename(slopes, c("dep_numeric" = 'dep'))

to_plot_hooks <- to_plot_hooks %>% left_join(slopes, by = c("init_dist",
  'type', 'nhooks', 'dep'))


#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------

####PUT FIGURE HERE
# dev.new(width = 7, height = 7)
png(width = 7, height = 7, units = 'in', res = 200, file = 'figs/hlfig_many_hooks.png')

par(mfrow = c(2, 2), mar = c(0, 0, 0, 0), oma = c(3, 4, 3, 2), mgp = c(0, .5, 0))

for(ii in 1:4){
  if(ii %in% 1:2){
    temp <- subset(to_plot_hooks, ind == ii)
  }

  if(ii %in% 3:4){
    temp <- subset(to_plot_hooks, ind == ii - 2)
    temp$q5 <- temp$q5_slope
    temp$q95 <- temp$q95_slope
    temp$med_cpue <- temp$med_slope
  }

  temp$dep <- as.numeric(as.character(temp$dep))    
  temp$dep_adj <- temp$dep
    
  prefs <- subset(temp, type == 'pref')
  prefs$dep_adj <- prefs$dep_adj - delta
    
  rands <- subset(temp, type == 'rand')
  rands$dep_adj <- rands$dep_adj + delta
  
  if(ii %in% 1:2){
    plot(temp$dep_adj, temp$med_cpue, type = 'n', ylim = c(0, 1), ann = FALSE, 
      axes = FALSE, xlim = c(-delta, 1 + .05))
  }

  if(ii %in% 3:4){
   plot(temp$dep_adj, temp$med_cpue, type = 'n', ylim = c(-1, 2.2), ann = FALSE, 
      axes = FALSE, xlim = c(-delta, 1 + .05)) 
  }
  box()
  
  #Add Axes
  if(ii == 1) axis(side = 2, las = 2, cex.axis = 1.2, 
    at = c(0, .2, .4, .6, .8, 1), labels = c("0.0", .2, .4, .6, .8, "1.0") )
  
  if(ii == 3){
    axis(side = 2, las = 2, cex.axis = 1.2, at = c(-1, 0, 1, 2))
  }
  if(ii %in% 1:2) mtext(side = 3, paste0(unique(temp$nhooks), ' hooks'))
  if(ii > 2) axis(side = 1, cex.axis = 1.2)
    
  #add in 1:1 line
  if(ii %in% 1:2) abline(a = 0, b = 1, lty = 2, col = 'gray', lwd = 2)
  if(ii %in% 3:4) abline(h = 0, lty = 2, col = 'gray', lwd = 2)    
  
  #Plot points and segments 
  points(prefs$dep_adj, prefs$med_cpue, pch = 19, cex = 1.2)
  segments(x0 = prefs$dep_adj, y0 = prefs$med_cpue, y1 = prefs$q95)
  segments(x0 = prefs$dep_adj, y0 = prefs$q5, y1 = prefs$med_cpue)
    
  points(rands$dep_adj, rands$med_cpue, pch = 17, cex = 1.2)
  segments(x0 = rands$dep_adj, y0 = rands$med_cpue, y1 = rands$q95, lty = 1)
  segments(x0 = rands$dep_adj, y0 = rands$q5, y1 = rands$med_cpue, lty = 1)
  mtext(side = 3, adj = .02, fig1_letts[ii], line = -1.2, cex = 1.1)
  
  

  # #Add in median absolute relative error
  mares <- temp %>% ungroup %>% distinct(type, med_are)
  mares[, 2] <- round(mares[, 2] * 100, digits = 0)
  # #Only include the median relative error values
  mares$caption <- paste0("mare=", mares$med_are)

  if(ii == 1){
    leg1 <- c(paste0('preferential; ', subset(mares, type == 'pref')$caption),
              paste0('random; ', subset(mares, type == 'rand')$caption))
    legend(x = .02, y = 1.05, pch = c(19, 17), 
      legend = leg1, cex = .9, bty = 'n', x.intersp = .5)
  } 

  if(ii == 2){
    legend(x = .02, y = 1.05, pch = c(19, 17), 
      legend = mares$caption, cex = .9, bty = 'n', x.intersp = .5)
  }

  if(ii == 1) mtext(side = 2, "CPUE", line = 2.1, cex = 1.4)
  if(ii == 3) mtext(side = 2, "Difference in slope", line = 2.1, cex = 1.4)
}

mtext(side = 1, "Relative abundance", outer = T, line = 2, cex = 1.4)
mtext(side = 4, "Patchy; 50 sites", outer = T, line = .7, cex = 1.3)

dev.off()
