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
shape_list1$for_plot <- c('Left Skew', 'Right Skew', 'Normal', 'Uniform', 'Patchy')

ctl1 <- make_ctl(distribute = 'beta', mortality = 0, move_out_prob = .05, 
      nfish1 = 100000,
      nfish2 = 0, prob1 = .01, prob2 = .05, nyear = 2, scope = 0, seed = 1,
      location = data.frame(vessel = 1, x = 1, y = 1), numrow = 30, numcol = 30,
      shapes = c(.1, .1) , max_prob = 0, min_prob = 0, comp_coeff = .5, niters = 1)    

#--------------------------------------------------------------------------------------------
#Plot Arguments
#--------------------------------------------------------------------------------------------
#Figure 1
#REMOVE RIGHTSKEW
shape_list4 <- subset(shape_list1, scen != 'rightskew')

#Figure 1. Show distributions of each sceanrio
ctl1$nfish1 <- 60000

#Format this figure
inits <- lapply(1:nrow(shape_list4), FUN = function(ss){
  ctl1$shapes <- c(shape_list4[ss, 2], shape_list4[ss, 3])
  temp <- initialize_population(ctl = ctl1, nfish = ctl1$nfish1)
  return(temp)
})

letts <- c('a)', 'b)', 'c)', 'd)')


#----------------------------------------
#Figure 2 Stuff
fishes <- seq(0, 200000, by = 20000)

#Run Simulation with 1000 replicates
start_time <- Sys.time()
onespp <- run_sampled_locs(shape_list = shape_list1, ncores = nncores,
  ctl_o = ctl1, thing1 = fishes, name1 = 'nfish1', nreps = 1000, 
  nsites_vec = c(5, 10, 30, 50, 100))
onespp <- onespp %>% filter(spp == 'spp1')

run_time <- Sys.time() - start_time
send_email(body = 'lab mac run done')

onespp$dep <- factor(onespp$dep, levels = unique(onespp$dep))
onespp$nsites <- factor(onespp$nsites, levels = unique(onespp$nsites))
save(onespp, file = "onespp_1000.Rdata")
# save(onespp, file = 'onespp.Rdata')

#first run took 8 hours I think
#----------------------------------------
#Load the data if run already

# load("output/onespp1.Rdata") #has 5, 10, 30, 50, 100 nsites samples
# onespp1 <- onespp
# load("output/onespp20.Rdata")
load('output/onespp20_1000.Rdata')
load('output/onespp_1000.Rdata')

onespp <- rbind(onespp, onespp20)

#Calculate mean, variance, and cv of each value
# onespp <- onespp %>% group_by(nsites, init_dist, nfish1, spp, type) %>% 
#   mutate(mean_cpue = mean(cpue), sd_cpue = sd(cpue), cv_cpue = sd_cpue / mean_cpue) %>%
#   as.data.frame

onespp$nsites <- as.numeric(as.character(onespp$nsites))

to_plot <- onespp %>% group_by(nsites, dep, init_dist, spp, type) %>% summarize(med_cpue = median(cpue),
  q5 = quantile(cpue, .05), q95 = quantile(cpue, .95)) %>% as.data.frame

#Convert init_dist to a factor to order then conert back to character
to_plot$init_dist <- factor(to_plot$init_dist, levels = c('leftskew', 'normdist',
  'uniform', 'patchy', 'rightskew'))
to_plot <- to_plot %>% arrange(init_dist)
to_plot$init_dist <- as.character(to_plot$init_dist)

#Filter specific nsites and initial distributions so that the number of 
to_plot <- to_plot %>% filter(nsites != 10 & nsites != 30)
to_plot <- to_plot %>% filter(init_dist != 'rightskew')

to_plot$unq <- paste(to_plot$nsites, to_plot$init_dist, to_plot$spp)
add_int <- data.frame(unq = unique(to_plot$unq), ind = 1:length(unique(to_plot$unq)))
add_int$unq <- as.character(add_int$unq)

to_plot <- inner_join(to_plot, add_int, by = 'unq')
to_plot$unq <- NULL
nn <- data.frame(init_dist = unique(to_plot$init_dist), init_dist_plot = c('Left Skew', 'Normal', 'Uniform',
  'Patchy'), stringsAsFactors = FALSE)
to_plot <- left_join(to_plot, nn, by = 'init_dist')

#Calculate mean and 95% intervals at each level of depletion
delta <- .02
fig1_letts <- paste0(letters[1:16], ')')

#----------------------------------------
#Figure 3
#Variability from 1 in ability to detect change in cpue with change in depletion  
plot3 <- onespp %>% group_by(nsites, init_dist, type) %>% 
           do({out <- sample_change(dep_fixed = 1, dep_vec = seq(.1, .9, by = .1), input = .)
              }) %>% as.data.frame 

#hlfig3 sketch
png(width = 13, height = 9, units = 'in', res = 150, file = 'figs/hlfig3_sketch.png')           
ggplot(plot3, aes(x = delta_dep)) + geom_point(aes(y = med_cpue, colour = type)) + 
  geom_line(aes(y = cpue5, colour = type)) + geom_line(aes(y = cpue95, colour = type)) +
  facet_wrap(nsites ~ init_dist, ncol = 5)
dev.off()

#Filter plot 3 before plot
plot3 <- plot3 %>% filter(nsites != 10 & nsites != 30 & init_dist != 'rightskew')

inds <- plot3 %>% group_by(nsites, init_dist) %>% filter(row_number() == 1) %>% 
  select(nsites, init_dist) %>% as.data.frame
inds$init_dist <- factor(inds$init_dist, levels = c('leftskew', 'normdist', 'uniform', 'patchy'))
inds <- inds %>% arrange(init_dist)
inds$init_dist <- as.character(inds$init_dist)
inds$init_dist_plot <- c(rep('Left Skew', 4), rep('Normal', 4), 
  rep('Uniform', 4), rep('Patchy', 4))

inds <- inds %>% arrange(nsites)

fig2_letts <- as.vector(matrix(fig1_letts, nrow = 4, ncol = 4, byrow = TRUE))

#----------------------------------------
#Figure 4
ups <- onespp %>% group_by(nsites, init_dist, type) %>% 
           do({out <- sample_change(dep_fixed = .5, dep_vec = seq(.6, .9, by = .1), input = .)
              }) %>% as.data.frame 

ups$x_dep <- ups$start_dep + abs(ups$delta_dep)
ups$x_dep_lab <- abs(ups$delta_dep)
ups$x_dep_lab <- paste0('+', ups$x_dep_lab)

downs <- onespp %>% group_by(nsites, init_dist, type) %>% 
           do({out <- sample_change(dep_fixed = .5, dep_vec = seq(.1, .4, by = .1), input = .)
              }) %>% as.data.frame 
downs$x_dep <- downs$start_dep - downs$delta_dep

downs$x_dep_lab <- paste0('-', downs$delta_dep)

plot4 <- rbind(ups, downs)

#----------------------------------------
#Figure 5

#Two spp things run in "mega_run.R"

load("output/twospp1_50.Rdata")
load("output/twospp23_50.Rdata")
load("output/twospp45_50.Rdata")
twospp <- rbind(twospp1, twospp23, twospp45)

#Check number of iterations for each
twospp %>% group_by(init_dist) %>% summarize(niters = length(unique(iter)), 
  nindex = length(unique(index)))

load('output/twospp12_1000.Rdata')
load('output/twospp34_1000.Rdata')
twospp1000 <- rbind(twospp12, twospp34)

#Check number of iterations for each
twospp1000 %>% group_by(init_dist) %>% summarize(niters = length(unique(iter)),
  nindex = length(unique(index)))

#Add depletion calculation
twospp$dep1 <- twospp$nfish1 / 2e5
twospp$dep2 <- twospp$nfish2 / 2e5

plot5 <- twospp %>% filter(init_dist == 'patchy')
plot5$tot_fish <- plot5$nfish1 + plot5$nfish2
plot5$prop1 <- plot5$nfish1 / plot5$tot_fish

plot5 <- plot5 %>% group_by(spp, comp_coeff, init_dist, for_plot, type, nsites, prop1) %>% 
  summarize(median_cpue = median(cpue), quant5 = quantile(cpue, .05),
  quant95 = quantile(cpue, .95), nvals = length(cpue)) %>% as.data.frame

#Plot 5 Sketch
ggplot(plot5, aes(x = prop1, y = median_cpue)) + geom_point(aes(colour = spp)) + 
  facet_wrap(~ type + comp_coeff, ncol = 3)

#Add indices for subsetting
fig5_letts <- paste0(letters[1:6], ")")
inds5 <- plot5 %>% select(comp_coeff, type) %>% distinct() %>% arrange(type)
#Wait for final run to evaluate the randoms

inds5$ind <- 1:3
plot5 <- inner_join(plot5, inds5, by = c("comp_coeff", "type"))

#----------------------------------------
#Figure 6

plot6 <- twospp %>% group_by(spp, comp_coeff, init_dist, for_plot, type, nsites, dep1, dep2) %>%
  summarize(median_cpue = median(cpue), sd_cpue = sd(cpue)) %>% as.data.frame

#Filter Data for each distribution
ls6 <- plot6 %>% filter(init_dist == 'leftskew')
n6 <- plot6 %>% filter(init_dist == 'normdist')
p6 <- plot6 %>% filter(init_dist == 'patchy')
u6 <- plot6 %>% filter(init_dist == 'uniform')

the_data <- rbind(p6, n6)

inds <- rbind(p6, n6) %>% select(type, spp, comp_coeff, init_dist) %>% distinct() %>%
   arrange(init_dist, type, comp_coeff, spp)

inds$ind <- 1:24
#Define function to rotate matrix

rotate <- function(x) t(apply(x, 2, rev))




#--------------------------------------------------------------------------------------------
#Order is left skew, normal, uniform, and patchy
#--------------------------------------------------------------------------------------------
#Figure 1
#--------------------------------------------------------------------------------------------
#Should probably be a one column figure
png(width = 7, height = 7, units = 'in', res = 150, file = 'figs/hlfig1.png')

par(mfrow = c(2, 2), mar = c(0, 0, 0, 0), oma = c(5, 5, .5, .75))

for(ii in 1:length(inits)){
  temp <- inits[[ii]]
  hist(temp, breaks = seq(0, 2270, 5), main = shape_list1[ii, 'scen'], freq = FALSE, 
    xlim = c(0, 300), axes = F, ann = F, ylim = c(0, .14), yaxs = 'i', xaxs = 'i')
  box()
  mtext(letts[ii], side = 3, line = -1.7, adj = 0.01, cex = 1.5)
  mtext(shape_list4[ii, 'for_plot'], side = 3, line = -1.7, adj = .95, cex = 1.5)
  # mtext(paste0('mean = ', round(mean(temp), digits = 0)), side = 3, line = -3, adj = .95)
  mtext(paste0('median = ', round(median(temp), digits = 0)), side = 3, line = -3, adj = .95)
  mtext(paste0('range = ', range(temp)[1], ', ', range(temp)[2]), side = 3, line = -4, adj = .95)
  if(ii == 1) axis(side = 2, at = seq(0, 0.12, by = .02), labels = seq(0, 0.12, by = .02), las = 2)
  if(ii == 3){
    axis(side = 2, at = seq(0, 0.12, by = .02), labels = seq(0, 0.12, by = .02), las = 2)
    axis(side = 1, at = seq(0, 250, by = 50))
  } 
  if(ii == 4) axis(side = 1)
}
mtext(side = 1, "Number of Fish / Site", outer = T, cex = 1.75, line = 3)
mtext(side = 2, "Proportion", outer = T, cex = 1.75, line = 3)

dev.off()


#--------------------------------------------------------------------------------------------
#Figure 2
#--------------------------------------------------------------------------------------------
png(width = 10, height = 10, units = 'in', res = 150, file = 'figs/hlfig2.png')

  par(mfrow = c(4, 4), mar = c(0, 0, 0, 0), oma = c(4, 6, 3, 2), mgp = c(0, .5, 0))

  for(ii in 1:16){
    temp <- subset(to_plot, ind == ii)
    temp$dep <- as.numeric(as.character(temp$dep))
    
    temp$dep_adj <- temp$dep
    
    prefs <- subset(temp, type == 'preferential')
    prefs$dep_adj <- prefs$dep_adj - delta
    
    rands <- subset(temp, type == 'random')
    rands$dep_adj <- rands$dep_adj + delta

    plot(temp$dep_adj, temp$med_cpue, type = 'n', ylim = c(0, 1.05), ann = FALSE, 
      axes = FALSE, xlim = c(-delta, 1 + .05))
    box()

    #Add Axes
    if(ii == 1) legend('bottomright', pch = c(19, 17), legend = c('preferential', 'random' ), 
      cex = 1.1, bty = 'n')
    if(ii %% 4 == 1) axis(side = 2, las = 2)
    if(ii < 5) mtext(side = 3, unique(temp$nsites))
    if(ii > 12) axis(side = 1)
    if(ii %% 4 == 0) mtext(side = 4, unique(temp$init_dist_plot), line = .6)
    
    #Plot points and segments 
    points(prefs$dep_adj, prefs$med_cpue, pch = 19)
    segments(x0 = prefs$dep_adj, y0 = prefs$med_cpue, y1 = prefs$q95)
    segments(x0 = prefs$dep_adj, y0 = prefs$q5, y1 = prefs$med_cpue)
    
    points(rands$dep_adj, rands$med_cpue, pch = 17)
    segments(x0 = rands$dep_adj, y0 = rands$med_cpue, y1 = rands$q95, lty = 1)
    segments(x0 = rands$dep_adj, y0 = rands$q5, y1 = rands$med_cpue, lty = 1)
    mtext(side = 3, adj = .02, fig1_letts[ii], line = -1.5)
  }

  mtext(side = 1, "Relative Abundance", outer = T, line = 3, cex = 2)
  mtext(side = 2, "CPUE", outer = T, line = 3, cex = 2)

dev.off()

#-----------------------------------------------------------------------------
#Figure 3 - Probability of increase or decrease
#Power of the survey. all from depletion 1 to .1
#-----------------------------------------------------------------------------
png(width = 10, height = 10, units = 'in', res = 150, file = 'figs/hlfig3.png')
par(mfcol = c(4, 4), mar = c(0, 0, 0, 0), oma = c(4, 6, 3, 2), xpd = T, 
  mgp = c(0, .5, 0))

for(ii in 1:16){
  temp_inds <- inds[ii, ]
  temp <- plot3 %>% filter(nsites == temp_inds$nsites, init_dist == temp_inds$init_dist)

  temp$dep <- as.numeric(as.character(temp$dep))  
  temp$dep_adj <- temp$delta_dep
  
  prefs <- subset(temp, type == 'preferential')
  prefs$dep_adj <- prefs$dep_adj - delta
  
  rands <- subset(temp, type == 'random')
  rands$dep_adj <- rands$dep_adj + delta

  plot(temp$dep_adj, temp$med_cpue, type = 'n', ylim = c(-.85, .45), ann = FALSE, 
    axes = FALSE, xlim = c(-delta, 1 + delta))
  abline(h = 0, lty = 2)
  box()

  #Add Axes
  if(ii == 1) legend('bottomleft', pch = c(19, 17), legend = c('preferential', 'random' ), bty = 'n')
  if(ii < 5) axis(side = 2, las = 2)
  if(ii %% 4 == 0) axis(side = 1)
  if(ii %% 4 == 1) mtext(side = 3, unique(temp$nsites))
  if(ii > 12) mtext(side = 4, unique(temp_inds$init_dist_plot), line = .6)
  
  #Plot points and segments 
  points(prefs$dep_adj, prefs$med_cpue, pch = 19)
  segments(x0 = prefs$dep_adj, y0 = prefs$med_cpue, y1 = prefs$cpue95)
  segments(x0 = prefs$dep_adj, y0 = prefs$cpue5, y1 = prefs$med_cpue)
  
  points(rands$dep_adj, rands$med_cpue, pch = 17)
  segments(x0 = rands$dep_adj, y0 = rands$med_cpue, y1 = rands$cpue95, lty = 1)
  segments(x0 = rands$dep_adj, y0 = rands$cpue5, y1 = rands$med_cpue, lty = 1)
  mtext(side = 3, adj = .02, fig2_letts[ii], line = -1.5)

  #add anchor point
  points(0, 0, pch = 21, cex = 2)
}

mtext(side = 1, "Decrease from Unfished", outer = T, line = 3, cex = 2)
mtext(side = 2, "Change in CPUE", outer = T, line = 3, cex = 2)

dev.off()

#-----------------------------------------------------------------------------
#Figure 4 - Probability of increase or decrease
#Starting at some level and going up and down
#-----------------------------------------------------------------------------

png(width = 10, height = 10, units = 'in', res = 150, file = 'figs/hlfig4.png')

par(mfcol = c(4, 4), mar = c(0, 0, 0, 0), oma = c(4, 6, 3, 2), xpd = T, 
  mgp = c(0, .5, 0))

for(ii in 1:16){
  temp_inds <- inds[ii, ]
  temp <- plot4 %>% filter(nsites == temp_inds$nsites, init_dist == temp_inds$init_dist)

  # temp$dep <- as.numeric(as.character(temp$dep))  
  temp$dep_adj <- temp$x_dep
  
  prefs <- subset(temp, type == 'preferential')
  prefs$dep_adj <- prefs$dep_adj - delta
  
  rands <- subset(temp, type == 'random')
  rands$dep_adj <- rands$dep_adj + delta

  plot(temp$dep_adj, temp$med_cpue, type = 'n', ylim = c(-.6, .4), ann = FALSE, 
    axes = FALSE, xlim = c(-delta, 1 + delta))
  abline(h = 0, lty = 2)
  # abline(v = .5, lty = 2)
  box()

  #Add Axes
  if(ii == 1) legend('bottomright', pch = c(19, 17), legend = c('preferential', 'random' ), bty = 'n')
  if(ii < 5) axis(side = 2, las = 2)
  if(ii %% 4 == 0) axis(side = 1, at = c(.1, .3, .5, .7, .9), labels = c("-.4", "-.2", "0", "+.2", "+.4"))
  if(ii %% 4 == 1) mtext(side = 3, unique(temp$nsites))
  if(ii > 12) mtext(side = 4, unique(temp_inds$init_dist_plot), line = .6)
  
  #Plot points and segments 
  points(prefs$dep_adj, prefs$med_cpue, pch = 19)
  segments(x0 = prefs$dep_adj, y0 = prefs$med_cpue, y1 = prefs$cpue95)
  segments(x0 = prefs$dep_adj, y0 = prefs$cpue5, y1 = prefs$med_cpue)
  
  points(rands$dep_adj, rands$med_cpue, pch = 17)
  segments(x0 = rands$dep_adj, y0 = rands$med_cpue, y1 = rands$cpue95, lty = 1)
  segments(x0 = rands$dep_adj, y0 = rands$cpue5, y1 = rands$med_cpue, lty = 1)
  mtext(side = 3, adj = .02, fig2_letts[ii], line = -1.5)
  points(x = .5, y = 0, pch = 21, cex = 2) #add anchor point
}

mtext(side = 1, "Change from 0.5", outer = T, line = 3, cex = 2)
mtext(side = 2, "Change in CPUE", outer = T, line = 3, cex = 2)

dev.off()
#At what depletion levels will ability to detect a change be significant?
#Results will be what number of 


#-----------------------------------------------------------------------------
#Figure 5 - Two Species Plots
#Easy plot simply understand the interaction between two two species
#-----------------------------------------------------------------------------
#Comp_coeff of 0.3, 0.5, 0.7 for one case, and sampling in 50 sites

png(width = 7.45, height = 6, units = 'in', res = 150, file = 'figs/hlfig5.png')
par(mfrow = c(2, 3), mar = c(0, 0, 0, 0), oma = c(4, 4, 4, 2), xpd = T, 
  mgp = c(0, .5, 0))

for(ii in 1:6){
  temp <- subset(plot5, ind == ii)

  temp1 <- subset(temp, spp == 'spp1')
  temp2 <- subset(temp, spp == 'spp2')

  #Plot empty plot
  plot(temp$prop1, temp$median_cpue, type = 'n', axes = F, ann = F, ylim = c(0, 1.05),
    xlim = c(0, 1.05))
  box()
  
  #Add points
  points(temp1$prop1, temp1$median_cpue, pch = 19)
  points(temp2$prop1, temp2$median_cpue, pch = 19, col = 'gray')

  #Add Text
  mtext(side = 3, adj = 0.02, fig5_letts[ii], line = -1.5)
  if(ii < 4) mtext(side = 3, unique(temp1$comp_coeff))
  if(ii < 4) mtext(side = 3, unique(temp1$comp_coeff))
  if(ii == 3) mtext(side = 4, "Preferential", line = .3)
  if(ii == 6) mtext(side = 4, "Random", line = .3)
  
  #Add Axes
  if(ii %% 3 == 1) axis(side = 2, las = 2)
  if(ii > 3) axis(side = 1)
  if(ii == 6) legend('topright', c('Species 1', 'Species 2'), col = c('black', 'gray'), 
    pch = 19, bty = 'n')
}

mtext(side = 1, outer = T, "Proportion Species 1", line = 2, cex = 1.2)
mtext(side = 2, outer = T, "Median CPUE", line = 2, cex = 1.2)
mtext(side = 3, outer = T, "Patchy Distribution", line = 2, cex = 1.4)

dev.off()

#-----------------------------------------------------------------------------
#Figure 6 - Two Species Contour Plots
#Starting at some level and going up and down
#-----------------------------------------------------------------------------

#####Figure out which things to compare
#Look at Patchy and Normal Distribution for differences

png(width = 11.29, height = 8.15, file = 'figs/hlfig6.png', units = 'in', res = 150)                   

matlay <- matrix(c(1, 2, 0, 3, 4, 0, 5,  6,
                   7, 8, 0, 9, 10, 0, 11, 12,
                   0, 0, 0, 0,  0,  0,0, 0,
                   13, 14, 0, 15, 16, 0, 17, 18,
                   19, 20, 0, 21, 22, 0, 23, 24), ncol = 8, byrow = TRUE)
                   
layout(matlay, heights = c(1,1,0.2,1,1), widths = c(1, 1, 0.1, 1, 1, 0.1, 1, 1))
par(mar = c(0.0, 0.5, 0.7, 0.3), oma = c(4, 4, 5, 2), mgp = c(.6, .5, 0))
fig6_letts <- paste0(letters[1:24], ")")

for(jj in 1:24){
  #------------------
  #Format the data
  temp <- inds[jj, ]

  #tp for temp plot 
  tp <- rbind(p6, n6) %>% filter(type == temp$type, spp == temp$spp, comp_coeff == temp$comp_coeff,
    init_dist == temp$init_dist)

  #------------------
  #Create the matrix of median_cpue values
  #Dep1 is the columns, y
  ind1 <- data.frame(dep1 = unique(tp$dep1), col_dep1 = 1:11)
  # ind1 <- data.frame(dep1 = unique(tp$dep1), col_dep1 = 11:1)
  tp <- inner_join(tp, ind1, by = 'dep1')
  
  #Dep2 is the rows, x
  # ind2 <- data.frame(dep2 = unique(tp$dep2), row_dep2 = 1:11)
  ind2 <- data.frame(dep2 = unique(tp$dep2), row_dep2 = 11:1)
  tp <- inner_join(tp, ind2, by = 'dep2')

  #Fill in the Matrix
  mm <- matrix(NA, nrow = 11, ncol = 11)
  for(ii in 1:nrow(tp)){
    mm[tp[ii, 'row_dep2'], tp[ii, 'col_dep1']] <- tp[ii, 'median_cpue']  
    # mm[tp[ii, 'col_dep1'], tp[ii, 'row_dep2']] <- tp[ii, 'median_cpue']  
  }

  #------------------
  #plots
  mylevels <- seq(0, 1, .1)
  greys <- paste0('grey', seq(100, 0, -10))
  
  x <- 10 * (1:11)
  y <- 10 *(1:11)
  mm <- rotate(mm)
  
  filled.contour2(x, y, mm, levels = mylevels,  col = greys, ann = F, axes = F)
  box()
  if(jj %in% c(2, 3, 4, 5, 8, 10, 11, 14, 16, 17)){
    contour(x, y, mm, levels = mylevels, add = T, labcex = 1, col = 'white')
    # text(103, 103, fig6_letts[jj], cex = 1.3, col = 'white')
  } 
  if(jj %in% c(2, 3, 4, 5, 8, 10, 11, 14, 16, 17) == FALSE){
    contour(x, y, mm, levels = mylevels, add = T, labcex = 1)
  } 
  

  #------------------
  #Add axes 
  if(jj %% 6 == 1){
    axis(side = 2, las = 2, at = c(10, 30, 50, 70, 90, 110), labels = c(0, .2, .4, .6, .8, 1))
  } 

  if(jj > 18) axis(side = 1, at = c(10, 30, 50, 70, 90, 110), labels = c(0, .2, .4, .6, .8, 1))

  #Add Text
  if(jj < 7 & jj %% 2 == 1) mtext(side = 3, "Species 1", adj = 0, line = .01)
  if(jj < 7 & jj %% 2 == 0) mtext(side = 3, "Species 2", adj = 0, line = .05)
  if(jj %in% c(6, 18)) mtext(side = 4, "Preferential", line = .5)
  if(jj %in% c(12, 24)) mtext(side = 4, "Random", line = .5)
  if(jj %in% c(1, 3, 5)){
    mtext(side = 3, paste0("Comp = ", unique(tp$comp_coeff)), adj = 0, line = 1.5, cex = 1.05)
    # mtext(side = 3, paste0("Comp = ", unique(tp$comp_coeff)), adj = 1.7, line = 2, cex = .9)
  } 
  
  #Add Letters
  if(jj %in% c(2, 3, 4, 5, 8, 10, 11, 14, 16, 17)){
    text(103, 103, fig6_letts[jj], cex = 1.3, col = 'white')
  } 
  if(jj %in% c(2, 3, 4, 5, 8, 10, 11, 14, 16, 17) == FALSE){
    text(103, 103, fig6_letts[jj], cex = 1.3)
  } 
  
}
#------------------
#Add outside text
mtext(side = 1, "Species 1 Depletion", outer = T, line = 2.2, cex = 1.5)
mtext(side = 2, "Species 2 Depletion", outer = T, line = 2, cex = 1.5)
mtext(side = 3, "Normal", outer = T, line = 2.7, cex = 1.5, adj = .005)
mtext(side = 3, "Patchy", outer = T, line = -27.5, cex = 1.5, adj = .005)

#Do this as 8.5 x 7 inch png?
dev.off()


