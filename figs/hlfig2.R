#----------------------------------------
#Figure 2 Stuff
# fishes <- seq(0, 200000, by = 20000)

# #Run Simulation with 1000 replicates
# start_time <- Sys.time()
# onespp <- run_sampled_locs(shape_list = shape_list1, ncores = nncores,
#   ctl_o = ctl1, thing1 = fishes, name1 = 'nfish1', nreps = 1000, 
#   nsites_vec = c(5, 10, 30, 50, 100))
# onespp <- onespp %>% filter(spp == 'spp1')

# run_time <- Sys.time() - start_time
# send_email(body = 'lab mac run done')

# onespp$dep <- factor(onespp$dep, levels = unique(onespp$dep))
# onespp$nsites <- factor(onespp$nsites, levels = unique(onespp$nsites))
# save(onespp, file = "onespp_1000.Rdata")

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

#add zeros for 20 samps
zero20 <- onespp %>% filter(nsites == 5, nfish_orig == 0)
zero20$nsites <- 20

onespp <- rbind(onespp, zero20)


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
nn <- data.frame(init_dist = unique(to_plot$init_dist), init_dist_plot = c('Left Skew', 'Symmetric', 'Uniform',
  'Patchy'), stringsAsFactors = FALSE)
to_plot <- left_join(to_plot, nn, by = 'init_dist')

#Calculate mean and 95% intervals at each level of depletion
delta <- .02
fig1_letts <- paste0(letters[1:16], ')')

#Calculate results of interest for figure 2
#Difference in means between preferential and random sampling
to_plot$qrange <- to_plot$q95 - to_plot$q5

# ggplot(to_plot, aes(x = dep, y = qrange)) + geom_point(aes(group = type, colour = type)) + 
#   facet_wrap(~ init_dist + nsites)

to_plot %>% group_by(nsites, dep, init_dist) %>% summarize(diff = round(med_cpue[1] - med_cpue[2], digits = 3)) %>%
  as.data.frame  %>% arrange(init_dist) -> fig2_diffs

# ggplot(fig2_diffs) + geom_point(aes(x = dep, y = diff, colour = nsites)) + facet_wrap(~ init_dist)

fig2_diffs %>% filter(init_dist == 'patchy') %>% select(diff) %>% summarize(mean(diff))

to_plot %>% group_by(nsites, dep, init_dist) %>% summarize

plot2 <- to_plot

to_plot %>% filter(init_dist == 'patchy' & nsites == 100) %>% select(type, qrange) %>%
  arrange(type, desc(qrange))

#Leftskew and normal distribution
plot2 %>% dcast(nsites + dep + init_dist ~ type, value.var = 'med_cpue') %>% 
  filter(init_dist %in% c('leftskew', 'normdist')) %>% 
  mutate(diff = round(preferential - random, digits = 3)) %>% range(diff)

plot2 %>% filter(med_cpue != 0) %>% dcast(nsites + dep + init_dist ~ type, value.var = 'med_cpue') %>% 
  filter(init_dist %in% c('uniform')) %>% 
  mutate(diff = round(preferential - random, digits = 3)) %>% select(diff) %>%
  summarize(min(diff), max(diff))

#Calculate difference from 1-1 line. 
plot2$dep <- as.numeric(as.character(plot2$dep))
plot2$resid <- plot2$med_cpue - plot2$dep

plot2 %>% group_by(nsites, init_dist, type) %>% summarize(ss = sum(resid^2)) %>% as.data.frame %>%
  arrange(init_dist, type) %>% group_by(init_dist, type)  %>% summarize(mean(ss))

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
    cex = 1.3, bty = 'n')
  if(ii %% 4 == 1) axis(side = 2, las = 2, cex.axis = 1.2)
  if(ii < 5) mtext(side = 3, unique(temp$nsites))
  if(ii > 12) axis(side = 1, cex.axis = 1.2)
  if(ii %% 4 == 0) mtext(side = 4, unique(temp$init_dist_plot), line = .6)
    
  #Plot points and segments 
  points(prefs$dep_adj, prefs$med_cpue, pch = 19, cex = 1.2)
  segments(x0 = prefs$dep_adj, y0 = prefs$med_cpue, y1 = prefs$q95)
  segments(x0 = prefs$dep_adj, y0 = prefs$q5, y1 = prefs$med_cpue)
    
  points(rands$dep_adj, rands$med_cpue, pch = 17, cex = 1.2)
  segments(x0 = rands$dep_adj, y0 = rands$med_cpue, y1 = rands$q95, lty = 1)
  segments(x0 = rands$dep_adj, y0 = rands$q5, y1 = rands$med_cpue, lty = 1)
  mtext(side = 3, adj = .02, fig1_letts[ii], line = -1.5)
}

mtext(side = 1, "Relative Abundance", outer = T, line = 2.8, cex = 1.4)
mtext(side = 2, "CPUE", outer = T, line = 3, cex = 1.4)

dev.off()


#--------------------------------------------------------------------------------------------
#Slow reveal
#First with only circles
png(width = 7, height = 7, units = 'in', res = 150, file = 'figs/hlfig2_pres1.png')
par(mgp = c(0, .5, 0))
temp <- subset(to_plot, ind == 5)
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
axis(side = 2, las = 2, cex.axis = 1.2)  
mtext(side = 3, unique(temp$nsites))
axis(side = 1, cex.axis = 1.2)  
mtext(side = 4, unique(temp$init_dist_plot), line = .6)

#Plot points and segments 
points(prefs$dep_adj, prefs$med_cpue, pch = 19, cex = 1.2)
segments(x0 = prefs$dep_adj, y0 = prefs$med_cpue, y1 = prefs$q95)
segments(x0 = prefs$dep_adj, y0 = prefs$q5, y1 = prefs$med_cpue)
mtext(side = 1, "Relative Abundance", line = 2, cex = 1.4)
mtext(side = 2, "CPUE", line = 2.25, cex = 1.4)
dev.off()

#---------------------------------------------
#Second reveal, add triangles and legend
png(width = 7, height = 7, units = 'in', res = 150, file = 'figs/hlfig2_pres2.png')
par(mgp = c(0, .5, 0))
temp <- subset(to_plot, ind == 5)
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
axis(side = 2, las = 2, cex.axis = 1.2)  
mtext(side = 3, unique(temp$nsites))
axis(side = 1, cex.axis = 1.2)  
mtext(side = 4, unique(temp$init_dist_plot), line = .6)

#Plot points and segments 
points(prefs$dep_adj, prefs$med_cpue, pch = 19, cex = 1.2)
segments(x0 = prefs$dep_adj, y0 = prefs$med_cpue, y1 = prefs$q95)
segments(x0 = prefs$dep_adj, y0 = prefs$q5, y1 = prefs$med_cpue)
mtext(side = 1, "Relative Abundance", line = 2, cex = 1.4)
mtext(side = 2, "CPUE", line = 2.25, cex = 1.4)

legend('bottomright', pch = c(19, 17), legend = c('preferential', 'random'), cex = 1.3, bty = 'n')    
points(rands$dep_adj, rands$med_cpue, pch = 17, cex = 1.2)
segments(x0 = rands$dep_adj, y0 = rands$med_cpue, y1 = rands$q95, lty = 1)
segments(x0 = rands$dep_adj, y0 = rands$q5, y1 = rands$med_cpue, lty = 1)
dev.off()

#---------------------------------------------
#Third reveal, for all symmetric distribution
png(width = 12.5, height = 4.75, units = 'in', res = 150, file = 'figs/hlfig2_pres3.png')
par(mfrow = c(1, 4), mar = c(0, 0, 0, 0), oma = c(4, 6, 3, 2), mgp = c(0, .5, 0))
# par(mgp = c(0, .5, 0))
for(ii in 5:8){
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
  if(ii == 5) axis(side = 2, las = 2, cex.axis = 1.2)  
  mtext(side = 3, unique(temp$nsites))
  axis(side = 1, cex.axis = 1.2)  
  if(ii == 8) mtext(side = 4, unique(temp$init_dist_plot), line = .6)
  
  #Plot points and segments 
  points(prefs$dep_adj, prefs$med_cpue, pch = 19, cex = 1.2)
  segments(x0 = prefs$dep_adj, y0 = prefs$med_cpue, y1 = prefs$q95)
  segments(x0 = prefs$dep_adj, y0 = prefs$q5, y1 = prefs$med_cpue)
  # mtext(side = 1, "Relative Abundance", line = 2, cex = 1.4)
  
  
  if(ii == 5) legend('bottomright', pch = c(19, 17), legend = c('preferential', 'random'), cex = 1.3, bty = 'n')    
  points(rands$dep_adj, rands$med_cpue, pch = 17, cex = 1.2)
  segments(x0 = rands$dep_adj, y0 = rands$med_cpue, y1 = rands$q95, lty = 1)
  segments(x0 = rands$dep_adj, y0 = rands$q5, y1 = rands$med_cpue, lty = 1)
}

mtext(side = 1, "Relative Abundance", line = 2.5, cex = 1.4, outer = T)
mtext(side = 2, "CPUE", line = 2.55, cex = 1.4, outer = T)

dev.off()

#--------------------------------------------------------------------------------------------

png(width = 12.5, height = 6.6, units = 'in', res = 150, file = 'figs/hlfig2_pres4.png')
par(mfrow = c(2, 4), mar = c(0, 0, 0, 0), oma = c(4, 6, 3, 2), mgp = c(0, .5, 0))
for(ii in c(5:8, 13:16)){
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
  if(ii == 5) legend('bottomright', pch = c(19, 17), legend = c('preferential', 'random' ), 
    cex = 1.3, bty = 'n')
  if(ii %% 4 == 1) axis(side = 2, las = 2, cex.axis = 1.2)
  if(ii <= 8) mtext(side = 3, unique(temp$nsites))
  if(ii > 12) axis(side = 1, cex.axis = 1.2)
  if(ii %% 4 == 0) mtext(side = 4, unique(temp$init_dist_plot), line = .6)
    
  #Plot points and segments 
  points(prefs$dep_adj, prefs$med_cpue, pch = 19, cex = 1.2)
  segments(x0 = prefs$dep_adj, y0 = prefs$med_cpue, y1 = prefs$q95)
  segments(x0 = prefs$dep_adj, y0 = prefs$q5, y1 = prefs$med_cpue)
    
  points(rands$dep_adj, rands$med_cpue, pch = 17, cex = 1.2)
  segments(x0 = rands$dep_adj, y0 = rands$med_cpue, y1 = rands$q95, lty = 1)
  segments(x0 = rands$dep_adj, y0 = rands$q5, y1 = rands$med_cpue, lty = 1)
  # mtext(side = 3, adj = .02, fig1_letts[ii], line = -1.5)
}

mtext(side = 1, "Relative Abundance", outer = T, line = 2.8, cex = 1.4)
mtext(side = 2, "CPUE", outer = T, line = 3, cex = 1.4)
dev.off()


