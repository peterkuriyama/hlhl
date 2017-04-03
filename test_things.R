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
#--------------------------------------------------------------------------------------------
#Figure 1
#--------------------------------------------------------------------------------------------
#REMOVE RIGHTSKEW
shape_list4 <- subset(shape_list1, scen != 'rightskew')

#Figure 1. Show distributions of each sceanrio
ctl1$nfish1 <- 50000

#Format this figure
inits <- lapply(1:nrow(shape_list4), FUN = function(ss){
  ctl1$shapes <- c(shape_list4[ss, 2], shape_list4[ss, 3])
  temp <- initialize_population(ctl = ctl1, nfish = ctl1$nfish1)
  return(temp)
})

letts <- c('a)', 'b)', 'c)', 'd)')
# inits[[4]][which(inits[[4]] >= 325)] <- 325

#Work on this plot

#Should probably be a one column figure
png(width = 7, height = 7, units = 'in', res = 150, file = 'hlfig1.png')
par(mfcol = c(2, 2), mar = c(0, 0, 0, 0), oma = c(5, 5, .5, .75))

for(ii in 1:length(inits)){
  temp <- inits[[ii]]
  hist(temp, breaks = seq(0, 2000, 5), main = shape_list1[ii, 'scen'], freq = FALSE, 
    xlim = c(0, 300), axes = F, ann = F, ylim = c(0, .14), yaxs = 'i', xaxs = 'i')
  box()
  mtext(letts[ii], side = 3, line = -1.7, adj = 0.01, cex = 1.5)
  mtext(shape_list4[ii, 'for_plot'], side = 3, line = -1.7, adj = .95, cex = 1.5)
  mtext(paste0('mean = ', round(mean(temp), digits = 0)), side = 3, line = -3, adj = .95)
  mtext(paste0('range = ', range(temp)[1], ', ', range(temp)[2]), side = 3, line = -4, adj = .95)
  if(ii == 1) axis(side = 2, at = seq(0, 0.12, by = .02), labels = seq(0, 0.12, by = .02), las = 2)
  if(ii == 2){
    axis(side = 2, at = seq(0, 0.12, by = .02), labels = seq(0, 0.12, by = .02), las = 2)
    axis(side = 1, at = seq(0, 250, by = 50))
  } 
  if(ii == 4) axis(side = 1)
}
mtext(side = 1, "Number of Fish", outer = T, cex = 1.75, line = 3)
mtext(side = 2, "Proportion", outer = T, cex = 1.75, line = 3)
dev.off()
#--------------------------------------------------------------------------------------------
#Figure 2
#--------------------------------------------------------------------------------------------
#RUN 1

#----------------------------------------
#Format simulation
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

load("output/onespp1.Rdata") #has 5, 10, 30, 50, 100 nsites samples
# onespp1 <- onespp
load("output/onespp20.Rdata")
load('output/onespp_1000.Rdata')

onespp <- rbind(onespp, onespp20)

#Calculate mean, variance, and cv of each value
# onespp <- onespp %>% group_by(nsites, init_dist, nfish1, spp, type) %>% 
#   mutate(mean_cpue = mean(cpue), sd_cpue = sd(cpue), cv_cpue = sd_cpue / mean_cpue) %>%
#   as.data.frame

#-----------------------------------------------------------------------------
#Fig 2. Single species results with point and stick
#in ggplot
# png(width = 15, height = 9, units = 'in', res = 200, file = 'figs/hlfig2.png')
# ggplot(onespp, aes(x = dep, y = cpue)) + geom_boxplot(aes(colour = type)) + 
#   facet_wrap(init_dist ~ nsites)
# dev.off()

onespp$nsites <- as.numeric(as.character(onespp$nsites))

to_plot <- onespp %>% group_by(nsites, dep, init_dist, spp, type) %>% summarize(med_cpue = median(cpue),
  q5 = quantile(cpue, .05), q95 = quantile(cpue, .95)) %>% as.data.frame

#Filter specific nsites and initial distributions so that the number of 
to_plot <- to_plot %>% filter(nsites != 10 & nsites != 30)
to_plot <- to_plot %>% filter(init_dist != 'rightskew')

to_plot$unq <- paste(to_plot$nsites, to_plot$init_dist, to_plot$spp)
add_int <- data.frame(unq = unique(to_plot$unq), ind = 1:length(unique(to_plot$unq)))
add_int$unq <- as.character(add_int$unq)

to_plot <- inner_join(to_plot, add_int, by = 'unq')
to_plot$unq <- NULL

#Calculate mean and 95% intervals at each level of depletion
delta <- .02

png(width = 10, height = 10, units = 'in', res = 150, file = 'figs/hlfig2.png')

par(mfcol = c(4, 4), mar = c(0, 0, 0, 0), oma = c(4, 6, 3, 2))

for(ii in 1:16){
  temp <- subset(to_plot, ind == ii)
  temp$dep <- as.numeric(as.character(temp$dep))
  
  temp$dep_adj <- temp$dep
  
  prefs <- subset(temp, type == 'preferential')
  prefs$dep_adj <- prefs$dep_adj - delta
  
  rands <- subset(temp, type == 'random')
  rands$dep_adj <- rands$dep_adj + delta

  plot(temp$dep_adj, temp$med_cpue, type = 'n', ylim = c(0, 1.1), ann = FALSE, 
    axes = FALSE, xlim = c(-delta, 1 + delta))
  box()

  #Add Axes
  if(ii == 1) legend('topleft', pch = c(19, 17), legend = c('preferential', 'random' ), bty = 'n')
  if(ii < 5) axis(side = 2, las = 2)
  if(ii %% 4 == 0) axis(side = 1)
  if(ii %% 4 == 1) mtext(side = 3, unique(temp$nsites))
  if(ii > 12) mtext(side = 4, unique(temp$init_dist), line = .6)
  
  #Plot points and segments 
  points(prefs$dep_adj, prefs$med_cpue, pch = 19)
  segments(x0 = prefs$dep_adj, y0 = prefs$med_cpue, y1 = prefs$q95)
  segments(x0 = prefs$dep_adj, y0 = prefs$q5, y1 = prefs$med_cpue)
  
  points(rands$dep_adj, rands$med_cpue, pch = 17)
  segments(x0 = rands$dep_adj, y0 = rands$med_cpue, y1 = rands$q95, lty = 1)
  segments(x0 = rands$dep_adj, y0 = rands$q5, y1 = rands$med_cpue, lty = 1)
}

mtext(side = 1, "Depletion", outer = T, line = 3, cex = 2)
mtext(side = 2, "CPUE", outer = T, line = 3, cex = 2)

dev.off()

#-----------------------------------------------------------------------------
#Figure 3 - Probability of increase or decrease
#Power of the survey. all from depletion 1 to .1
#-----------------------------------------------------------------------------
#Variability from 1 in ability to detect change in cpue with change in depletion

temp <- onespp %>% filter(nsites == 5, init_dist == 'leftskew', type == 'random')
  
sample_change <- function(nsamps = 1000, dep_fixed, dep_vec, input){   
  high <- input %>% filter(dep == dep_fixed)
  
  ss <- lapply(dep_vec, FUN = function(dd){
          low <- input %>% filter(dep == dd)
          s2 <- sample(high$cpue, size = nsamps, replace = TRUE)
          s1 <- sample(low$cpue, size = nsamps, replace = TRUE)
          diffs <- s1 - s2
          outs <- c(median(diffs), as.numeric(quantile(diffs, c(.05, .95))))
      
          return(outs)
        })
  names(ss) <- dep_vec
  ss <- ldply(ss)
  names(ss) <- c('dep', 'med_cpue', 'cpue5', 'cpue95')
  ss$start_dep <- dep_fixed
  ss$dep <- as.numeric(ss$dep)
  ss$delta_dep <- ss$start_dep - ss$dep
  return(ss)
}

plot3 <- onespp %>% group_by(nsites, init_dist, type) %>% 
           do({out <- sample_change(dep_fixed = 1, dep_vec = seq(.1, .9, by = .1), input = .)
              }) %>% as.data.frame 


#hlfig3 sketch
png(width = 13, height = 9, units = 'in', res = 150, file = 'figs/hlfig3_sketch.png')           
ggplot(plot3, aes(x = delta_dep)) + geom_point(aes(y = med_cpue, colour = type)) + 
  geom_line(aes(y = cpue5, colour = type)) + geom_line(aes(y = cpue95, colour = type)) +
  facet_wrap(nsites ~ init_dist, ncol = 5)
dev.off()

ggplot(plot3, aes(x = delta_dep, y = cpue95, colour = nsites)) + geom_point() +
 facet_wrap(~ init_dist + type)

#Power is invariant among random vs preferential sampling..





sample_change(dep_fixed = 1, dep_vec = seq(.1, .9, by = .1), input = temp)





temp <- onespp %>% filter(nsites == 5, init_dist == 'leftskew', type == 'random')


onespp %>% group_by(nsites, init_dist, type, dep) %>% 
  summarize(mean = sample_change(depletion = unique(dep), input = .)[1],
    low = sample_change(depletion = unique(dep), input = .)[2],
    high = sample_change(depletion = unique(dep), input = .)[3])


%>% 
  do({
    out <- sample_change(depletion = .2, input = .)
    data.frame(., out)
  })

sample_change(depletion = .2, input = temp)


temp <- onespp %>% filter(nsites == 5, init_dist == 'leftskew', type == 'random')
temp1 <- temp %>% filter(dep == 1)
temp.1 <- temp %>% filter(dep == .1)

#resampled change in depletion vs. change in cpue
#Resample 1000 times and save difference
s1 <- sample(temp1$cpue, size = 1000, replace = TRUE)
s2 <- sample(temp.1$cpue, size = 1000, replace = TRUE)

#calculate mean, 5%, 95% intervals 


hist(s1 - s2)


#---------------------------------------------
#---------------------------------------------
#Two species with comp_coeff equal to 0.5

#Increasing number of location with two species, even comp_coeff
#Both species increasing together

fishes1 <- seq(0, 200000, by = 20000)
fishes2 <- rev(fishes1)

#Change comp_coeff sometime
ctl1$comp_coeff <- 0.5

start_time2 <- Sys.time()
twospp <- run_sampled_locs_2spp(shape_list = shape_list1, nsites_vec = c(5, 10, 30, 50, 100),
  ncores = nncores, ctl_o = ctl1, thing1 = fishes1, thing2 = fishes2, name1 = 'nfish1', 
  name2 = 'nfish2', nreps = 100)
run_time2 <- Sys.time() - start_time2
  
send_email()
  
#Save results
# paste0(results_dir, '//inc12.Rdata')
# save(twospp, file = paste0(results_dir, '//inc12.Rdata'))
save(twospp, file = 'twospp.Rdata')


load('output/twospp.Rdata')

#Took 14 hours!

#change formats
twospp$nfish1 <- as.numeric(twospp$nfish1)
twospp$nfish2 <- as.numeric(twospp$nfish2)

#Swing dep by species for plots
dep2 <- twospp %>% dcast(nfish1 + nfish2 + init_dist + nsites + 
  rep + iter + type ~ spp, value.var = 'dep')
names(dep2)[grep('spp', names(dep2))] <- c('dep1', 'dep2')

twospp <- inner_join(twospp, dep2, by = c('nfish1', 'nfish2', 'init_dist', 'nsites',
  'rep', 'iter', 'type'))


#Look only at simulations that sampled 50 sites
focus <- twospp %>% filter(nsites == 50) 
focus %>% group_by(init_dist, rep, nfish1, nfish2, type) %>% mutate(tot_cpue = sum(cpue)) %>% 
  as.data.frame -> focus

focus %>% group_by(dep1, dep2, init_dist, spp, type) %>% 

summarize(mean_cpue = mean(cpue),
  sd_cpue = sd(cpue), cv_cpue = sd_cpue / mean_cpue) %>% as.data.frame -> for_contour


#Remove duplicated values
# focus %>% group_by(init_dist, dep1, dep2, spp, type) %>% filter(row_number(cv_cpue) == 1) %>% 
#   as.data.frame -> focus

 %>% filter(init_dist == 'unif' & dep1 == 0.1 & dep2 == 1.0) %>% head


#What colors to use?
#Patchy contour plot
for_contour %>% filter(init_dist == 'unif') %>% ggplot(aes(x = dep1, y = dep2, z = mean_cpue)) + 
  stat_contour(aes(colour = ..level..), binwidth = .1) + facet_wrap(~ spp + type) + 
  scale_colour_gradient2(low = 'blue', high = 'red', limits = c(0, 1))

for_contour %>% filter(init_dist == 'unif') %>% tail



volcano3d <- melt(volcano)

v + stat_contour(geom="polygon", aes(fill=..level..))
fc2 <- for_contour %>% group_by(dep1, dep2, init_dist, type) %>% summarize(tot_mean_cpue = sum(mean_cpue))

#This makes sense
ggplot(fc2, aes(x = dep1, y = dep2, z = tot_mean_cpue)) + stat_contour(aes(colour = ..level..), 
  binwidth = .1, size = 1) + facet_wrap(~ init_dist + type, ncol = 2) + scale_colour_gradient(low = 'white',
  high = 'red')


#Try plotting this as dots? Something seems wrong


funi %>% group_by(nfish1, nfish2, type) %>% mutate(tot_cpue = sum(cpue)) %>% as.data.frame %>% head


#Patchy contour plot
focus %>% filter(init_dist == 'patchy') %>% ggplot(aes(x = dep1, y = dep2, z = mean_cpue)) + 
  geom_contour(aes(colour = ..level..), bins = 5) + facet_wrap(~ spp + type)


# with ggplot > 2.0.0 you'll need to add method="bottom.pieces" (or top.pieces) to the direct.label call
funi <- focus %>% filter(init_dist == 'unif')



focus %>% filter(init_dist == 'unif') %>% ggplot() + geom_point(aes(x = dep1, y = dep2, colour = mean_cpue)) + 
  facet_wrap(~ spp + type)



unique(paste(focus$dep1, focus$dep2))




#---------------------------------------------
#Run two species simulation with comp_coeff of .1, weak competition
#only 10 replicates to see how it goes

fishes1 <- seq(20000, 200000, by = 20000)
fishes2 <- rev(fishes1)

#Change comp_coeff sometime
ctl1 <- make_ctl(distribute = 'beta', mortality = 0, move_out_prob = .05, nfish1 = 100000,
      nfish2 = 0, prob1 = .01, prob2 = .05, nyear = 1, scope = 0, seed = 1,
      location = data.frame(vessel = 1, x = 1, y = 1), numrow = 30, numcol = 30,
      shapes = c(.1, .1) , max_prob = 0, min_prob = 0, niters = 1, 
      comp_coeff = .1)    

shape_list1 <- data.frame(scen = c('patchy','rightskew', 'normdist', 'unif'), 
                          shapes1 = c(.1, 1, 10, 10), 
                          shapes2 = c(10, 10, 10, .10))

start_time <- Sys.time()
twospp_lowcomp <- run_sampled_locs_2spp(shape_list = shape_list1, nsites_vec = c(50),
  ncores = nncores, ctl_o = ctl1, thing1 = fishes1, thing2 = fishes2, name1 = 'nfish1', 
  name2 = 'nfish2', nreps = 10)
run_time <- Sys.time() - start_time
  
send_email()

save(twospp_lowcomp, file = 'twospp_lowcomp.Rdata')






