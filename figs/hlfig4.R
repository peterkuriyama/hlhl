

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

#add colors depending on
ups$bg <- 'black'
ups[which(ups$cpue5 <= 0), 'bg'] <- 'white'

downs$bg <- 'black'
downs[which(downs$cpue95 >= 0), 'bg'] <- 'white'

plot4 <- rbind(ups, downs)

#See at what decrease does the survey not overlap with 0
ups %>% filter(cpue5 > 0) %>% group_by(nsites, init_dist, type) %>% 
  summarize(lvl = max(dep)) %>% as.data.frame %>%  
  dcast(nsites + init_dist ~ type, value.var = 'lvl') %>% arrange(init_dist) -> u1
u1$change <- 'up'

downs %>% filter(cpue95 < 0) %>% group_by(nsites, init_dist, type) %>% 
  summarize(lvl = max(dep)) %>% as.data.frame %>%  
  dcast(nsites + init_dist ~ type, value.var = 'lvl') %>% arrange(init_dist) -> d1
d1$change <- 'down'

rbind(u1, d1) %>% arrange(init_dist, change, nsites)

#for abstract
ab4 <- plot4 %>% group_by(nsites, init_dist, type) %>% summarize(sigup = max(dep[which(cpue5 > 0)]), 
  sigdown = max(dep[which(cpue95 < 0)])) %>% as.data.frame %>% 
  arrange(init_dist, type, nsites)

1 - sum(is.infinite(ab4$sigup)) / nrow(ab4)
1 - sum(is.infinite(ab4$sigdown)) / nrow(ab4)
ab4 %>% filter(init_dist == 'patchy')

plot4 %>% filter(init_dist == 'patchy', dep == .1) %>% group_by(type) %>% summarize(mmin = min(med_cpue), 
  mmax = max(med_cpue))

#--------------------------------------------------------------------------------
#resample the ups and downs
set.seed(300)
true_ups <- onespp %>% group_by(nsites, init_dist, type) %>% 
  do({out <- sample_diffs(dep_fixed = .5, dep_vec = seq(.6, .9, by = .1), input = .)
  }) %>% as.data.frame 

#Subtract 1 from the detection rate of all of the true_ups
true_ups$detection_rate <- 1 - true_ups$detection_rate

true_downs <- onespp %>% group_by(nsites, init_dist, type) %>% 
  do({out <- sample_diffs(dep_fixed = .5, dep_vec = seq(.1, .4, by = .1), input = .)
  }) %>% as.data.frame 

#Combine the two
true_mids <- rbind(true_ups, true_downs)
true_mids <- true_mids %>% filter(nsites %in% c(5, 20, 50, 100), init_dist != 'rightskew')

true_mids %>% filter(detection_rate > .1, nsites == 100, init_dist != 'rightskew')
true_mids$init_dist <- factor(true_mids$init_dist, levels = c("leftskew", 'normdist',
                                                                 'uniform', 'patchy'))

true_mids %>% filter(init_dist == 'patchy', detection_rate > .1)

ggplot(true_mids) + geom_point(aes(x = dep, y = detection_rate, colour = type)) +
  facet_grid(init_dist ~ nsites)
 
#-----------------------------------------------------------------------------
#Figure 4 - Probability of increase or decrease
#Starting at some level and going up and down
#-----------------------------------------------------------------------------
plot4$point <- 21
plot4[which(plot4$type == "random"), 'point'] <- 24

plot4[which(plot4$cpue95 < 0 & plot4$type == 'preferential'), 'point'] <- 19
plot4[which(plot4$cpue95 < 0 & plot4$type == 'random'), 'point'] <- 17

png(width = 10, height = 10, units = 'in', res = 150, file = 'figs/hlfig4.png')

par(mfcol = c(4, 4), mar = c(0, 0, 0, 0), oma = c(4.5, 6, 3, 2), xpd = T, 
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
  abline(h = 0, lty = 2, col = 'black')
  abline(a = -.5, b = 1, lty = 2, col = 'gray')
  # lines(x = seq(0, 1, by = .1), y = seq(-.5, .5, by = .1), lty = 2, col = 'grey')
  # abline(v = .5, lty = 2)
  box()

  #Add Axes
  if(ii == 1) legend('bottomright', pch = c(19, 17), legend = c('density-based', 'random' ), bty = 'n', cex = 1.3)
  if(ii < 5) axis(side = 2, las = 2, cex.axis = 1.2, at = c(-.4, -.2, 0, .2, .4), 
    labels = c("-0.4", "-0.2", "0", "0.2", "0.4"))
  if(ii %% 4 == 0) axis(side = 1, at = c(.1, .3, .5, .7, .9), labels = c("-0.4", "-0.2", "0", "+0.2", "+0.4"),
    cex.axis = 1.2)
  if(ii %% 4 == 1) mtext(side = 3, paste0(unique(temp$nsites), " sites"))
  if(ii > 12) mtext(side = 4, unique(temp_inds$init_dist_plot), line = .6)
  
  #Plot points and segments 
  segments(x0 = prefs$dep_adj, y0 = prefs$med_cpue, y1 = prefs$cpue95)
  segments(x0 = prefs$dep_adj, y0 = prefs$cpue5, y1 = prefs$med_cpue)
  points(prefs$dep_adj, prefs$med_cpue, pch = prefs$point, bg = prefs$bg)
  
  segments(x0 = rands$dep_adj, y0 = rands$med_cpue, y1 = rands$cpue95, lty = 1)
  segments(x0 = rands$dep_adj, y0 = rands$cpue5, y1 = rands$med_cpue, lty = 1)
  points(rands$dep_adj, rands$med_cpue, pch = rands$point, bg = rands$bg)
  
  mtext(side = 3, adj = .02, fig2_letts[ii], line = -1.5)
  
  #Add anchor point
  points(x = .5, y = 0, pch = 23, cex = 2, bg = 'gray50', col = 'gray50') #add anchor point
}

mtext(side = 1, "Change from 0.5", outer = T, line = 3, cex = 1.4)
mtext(side = 2, "Change in CPUE", outer = T, line = 3, cex = 1.4)

dev.off()
#At what depletion levels will ability to detect a change be significant?
#Results will be what number of 




#-----------------------------------------------------------------------------
#Figures for presentation
#just one plot
png(width = 7, height = 7, units = 'in', res = 150, file = 'figs/hlfig4_pres1.png')  
  temp_inds <- inds[4, ]
  temp <- plot4 %>% filter(nsites == temp_inds$nsites, init_dist == temp_inds$init_dist)

  # temp$dep <- as.numeric(as.character(temp$dep))  
  temp$dep_adj <- temp$x_dep
  
  prefs <- subset(temp, type == 'preferential')
  prefs$dep_adj <- prefs$dep_adj + delta
  # prefs$dep_adj_flipped <- 1 - prefs$dep_adj  
  
  rands <- subset(temp, type == 'random')
  rands$dep_adj <- rands$dep_adj - delta

  plot(temp$dep_adj, temp$med_cpue, type = 'n', ylim = c(-.6, .5), ann = FALSE, 
    axes = FALSE, xlim = c(-delta, 1 + delta))
  
  abline(h = 0, lty = 2)
  # abline(v = .5, lty = 2)
  box()

  #Add Axes
  axis(side = 1, at = c(.1, .3, .5, .7, .9), labels = c("-0.4", "-0.2", "0", "0.2", "0.4"),
    cex.axis = 1.2)
  if(ii == 1) legend('bottomright', pch = c(19, 17), legend = c('preferential', 'random' ), bty = 'n',
    cex = 1.3)
  axis(side = 2, cex.axis = 1.2, las = 2)

  # #Plot points and segments 
  # points(prefs$med_cpue, prefs$dep_adj, pch = 19, cex = 1.2)
  # segments(y0 = prefs$dep_adj, x0 = prefs$med_cpue, x1 = prefs$cpue95)
  # segments(y0 = prefs$dep_adj, x0 = prefs$cpue5, x1 = prefs$med_cpue)
  
  # points(rands$med_cpue, rands$dep_adj, pch = 17, cex = 1.2)
  # segments(y0 = rands$dep_adj, x0 = rands$med_cpue, x1 = rands$cpue95, lty = 1)
  # segments(y0 = rands$dep_adj, x0 = rands$cpue5, x1 = rands$med_cpue, lty = 1)
  # mtext(side = 3, adj = .02, fig2_letts[ii], line = -1.5)
  points(y = 0, x = .5, pch = 23, cex = 2, bg = 'white') #add anchor point

mtext(side = 1, "Change in CPUE", line = 3, cex = 1.4)
mtext(side = 2, "Change from 0.5", line = 2.75, cex = 1.4)
dev.off()


png(width = 7, height = 7, units = 'in', res = 150, file = 'figs/hlfig4_pres2.png')  
  temp_inds <- inds[4, ]
  temp <- plot4 %>% filter(nsites == temp_inds$nsites, init_dist == temp_inds$init_dist)

  # temp$dep <- as.numeric(as.character(temp$dep))  
  temp$dep_adj <- temp$x_dep
  
  prefs <- subset(temp, type == 'preferential')
  prefs$dep_adj <- prefs$dep_adj + delta
  # prefs$dep_adj_flipped <- 1 - prefs$dep_adj  
  
  rands <- subset(temp, type == 'random')
  rands$dep_adj <- rands$dep_adj - delta

  plot(temp$dep_adj, temp$med_cpue, type = 'n', ylim = c(-.6, .5), ann = FALSE, 
    axes = FALSE, xlim = c(-delta, 1 + delta))
  abline(h = 0, lty = 2)
  
  # plot(temp$med_cpue, temp$dep_adj, type = 'n', xlim = c(-.6, .5), ann = FALSE, 
  #   axes = FALSE, ylim = c(.05, .95))
  # abline(v = 0, lty = 2)
  # abline(v = .5, lty = 2)
  box()

  #Add Axes
  axis(side = 1, at = c(.1, .3, .5, .7, .9), labels = c("-0.4", "-0.2", "0", "0.2", "0.4"),
    cex.axis = 1.2)
  if(ii == 1) legend('bottomright', pch = c(19, 17), legend = c('preferential', 'random' ), bty = 'n',
    cex = 1.3)
  axis(side = 2, cex.axis = 1.2, las = 2)

  #Add Axes
  # axis(side = 2, at = c(.1, .3, .5, .7, .9), labels = c("-0.4", "-0.2", "0", "0.2", "0.4"), las = 2,
  #   cex.axis = 1.2)
  # if(ii == 1) legend('bottomright', pch = c(19, 17), legend = c('preferential', 'random' ), bty = 'n',
  #   cex = 1.3)
  # axis(side = 1, cex.axis = 1.2)

  # #Plot points and segments 
  segments(x0 = prefs$dep_adj, y0 = prefs$med_cpue, y1 = prefs$cpue95)
  segments(x0 = prefs$dep_adj, y0 = prefs$cpue5, y1 = prefs$med_cpue)
  points(prefs$dep_adj, prefs$med_cpue, pch = prefs$point, bg = prefs$bg)
  
  segments(x0 = rands$dep_adj, y0 = rands$med_cpue, y1 = rands$cpue95, lty = 1)
  segments(x0 = rands$dep_adj, y0 = rands$cpue5, y1 = rands$med_cpue, lty = 1)
  points(rands$dep_adj, rands$med_cpue, pch = rands$point, bg = rands$bg)
  
  points(x = .5, y = 0, pch = 23, cex = 2, bg = 'white') #add anchor point

mtext(side = 1, "Change in CPUE", line = 3, cex = 1.4)
mtext(side = 2, "Change from 0.5", line = 2.75, cex = 1.4)
dev.off()



#Figure for presentation
png(width = 14, height = 5, units = 'in', res = 150, file = 'figs/hlfig4_pres3.png')

par(mfcol = c(1, 4), mar = c(0, 0, 0, 0), oma = c(4.5, 6, 3, 2), xpd = T, 
  mgp = c(0, .5, 0))

for(ii in c(4, 8, 12, 16)){
  temp_inds <- inds[ii, ]
  temp <- plot4 %>% filter(nsites == temp_inds$nsites, init_dist == temp_inds$init_dist)

  # temp$dep <- as.numeric(as.character(temp$dep))  
  temp$dep_adj <- temp$x_dep
  
  prefs <- subset(temp, type == 'preferential')
  prefs$dep_adj <- prefs$dep_adj + delta
  # prefs$dep_adj_flipped <- 1 - prefs$dep_adj  
  
  rands <- subset(temp, type == 'random')
  rands$dep_adj <- rands$dep_adj - delta

  
  plot(temp$dep_adj, temp$med_cpue, type = 'n', ylim = c(-.6, .5), ann = FALSE, 
    axes = FALSE, xlim = c(-delta, 1 + delta))
  abline(h = 0, lty = 2)
  
  # plot(temp$med_cpue, temp$dep_adj, type = 'n', xlim = c(-.6, .5), ann = FALSE, 
  #   axes = FALSE, ylim = c(.05, .95))
  # abline(v = 0, lty = 2)
  # abline(v = .5, lty = 2)
  box()

  #Add Axes
  axis(side = 1, at = c(.1, .3, .5, .7, .9), labels = c("-0.4", "-0.2", "0", "0.2", "0.4"),
    cex.axis = 1.2)
  if(ii == 1) legend('bottomright', pch = c(19, 17), legend = c('preferential', 'random' ), bty = 'n',
    cex = 1.3)
  if(ii == 4) axis(side = 2, cex.axis = 1.2, las = 2)

  #Add Axes
  # axis(side = 2, at = c(.1, .3, .5, .7, .9), labels = c("-0.4", "-0.2", "0", "0.2", "0.4"), las = 2,
  #   cex.axis = 1.2)
  # if(ii == 1) legend('bottomright', pch = c(19, 17), legend = c('preferential', 'random' ), bty = 'n',
  #   cex = 1.3)
  # axis(side = 1, cex.axis = 1.2)

  # #Plot points and segments 
  segments(x0 = prefs$dep_adj, y0 = prefs$med_cpue, y1 = prefs$cpue95)
  segments(x0 = prefs$dep_adj, y0 = prefs$cpue5, y1 = prefs$med_cpue)
  points(prefs$dep_adj, prefs$med_cpue, pch = prefs$point, bg = prefs$bg)
  

  segments(x0 = rands$dep_adj, y0 = rands$med_cpue, y1 = rands$cpue95, lty = 1)
  segments(x0 = rands$dep_adj, y0 = rands$cpue5, y1 = rands$med_cpue, lty = 1)
  points(rands$dep_adj, rands$med_cpue, pch = rands$point, bg = rands$bg)
  points(x = .5, y = 0, pch = 23, cex = 2, bg = 'white') #add anchor point

}

mtext(side = 1, "Change in CPUE", outer = T, line = 3, cex = 1.4)
mtext(side = 2, "Change from 0.5", outer = T, line = 3, cex = 1.4)
dev.off()


