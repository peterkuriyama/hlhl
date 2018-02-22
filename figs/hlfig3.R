#----------------------------------------
#Figure 3
#Variability from 1 in ability to detect change in cpue with change in depletion  
plot3 <- onespp %>% group_by(nsites, init_dist, type) %>% 
           do({out <- sample_change(dep_fixed = 1, dep_vec = seq(.1, .9, by = .1), input = .)
              }) %>% as.data.frame 

#hlfig3 sketch
# png(width = 13, height = 9, units = 'in', res = 150, file = 'figs/hlfig3_sketch.png')           
# ggplot(plot3, aes(x = delta_dep)) + geom_point(aes(y = med_cpue, colour = type)) + 
#   geom_line(aes(y = cpue5, colour = type)) + geom_line(aes(y = cpue95, colour = type)) +
#   facet_wrap(nsites ~ init_dist, ncol = 5)
# dev.off()

#Filter plot 3 before plot
plot3 <- plot3 %>% filter(nsites != 10 & nsites != 30 & init_dist != 'rightskew')

inds <- plot3 %>% group_by(nsites, init_dist) %>% filter(row_number() == 1) %>% 
  select(nsites, init_dist) %>% as.data.frame
inds$init_dist <- factor(inds$init_dist, levels = c('leftskew', 'normdist', 'uniform', 'patchy'))
inds <- inds %>% arrange(init_dist)
inds$init_dist <- as.character(inds$init_dist)
inds$init_dist_plot <- c(rep('Left Skew', 4), rep('Symmetric', 4), 
  rep('Uniform', 4), rep('Patchy', 4))

inds <- inds %>% arrange(nsites)

fig2_letts <- as.vector(matrix(fig1_letts, nrow = 4, ncol = 4, byrow = TRUE))

##results
#See at what decrease does the survey not overlap with 0
plot3 %>% filter(cpue95 < 0) %>% group_by(nsites, init_dist, type) %>% 
  summarize(lvl = max(dep)) %>% as.data.frame %>%  
  dcast(nsites + init_dist ~ type, value.var = 'lvl') %>% arrange(init_dist) 

#-----------------------------------------------------------------------------
#Figure 3 - Probability of increase or decrease
#Power of the survey. all from depletion 1 to .1
#-----------------------------------------------------------------------------
#Add color to indicate significance
plot3$color <- 'black'

#Add open points to nonsignificant values
#Open circle is 21, filled 19
#Open triangle is 24, filled 17
plot3$point <- 21
plot3[which(plot3$type == "random"), 'point'] <- 24

plot3[which(plot3$cpue95 < 0 & plot3$type == 'preferential'), 'point'] <- 19
plot3[which(plot3$cpue95 < 0 & plot3$type == 'random'), 'point'] <- 17

png(width = 10, height = 10, units = 'in', res = 150, file = 'figs/hlfig3.png')

par(mfcol = c(4, 4), mar = c(0, 0, 0, 0), oma = c(4, 6, 3, 2), xpd = T, 
  mgp = c(0, .5, 0))

for(ii in 1:16){
  temp_inds <- inds[ii, ]
  temp <- plot3 %>% filter(nsites == temp_inds$nsites, init_dist == temp_inds$init_dist)

  #Color non-significant values gray
  temp$dep <- as.numeric(as.character(temp$dep))  
  temp$dep_adj <- temp$delta_dep
  
  prefs <- subset(temp, type == 'preferential')
  # prefs$dep_adj <- prefs$dep_adj - delta
  prefs$dep_adj <- prefs$dep - delta
  
  rands <- subset(temp, type == 'random')
  # rands$dep_adj <- rands$dep_adj + delta
  rands$dep_adj <- rands$dep + delta

  plot(temp$dep_adj, temp$med_cpue, type = 'n', ylim = c(-.85, .45), ann = FALSE, 
    axes = FALSE, xlim = c(-delta, 1 + delta))
  abline(h = 0, lty = 2)
  box()

  #Add Axes
  if(ii == 1) legend('bottomright', pch = c(19, 17), legend = c('density-based', 'random' ), bty = 'n',
    cex = 1.3)
  if(ii < 5) axis(side = 2, las = 2, cex.axis = 1.2)
  if(ii %% 4 == 0) axis(side = 1, cex.axis = 1.2, at = c(0, .2, .4, .6, .8, 1),
    labels = rev(c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0')))
  if(ii %% 4 == 1) mtext(side = 3, paste0(unique(temp$nsites), " sites"))
  if(ii > 12) mtext(side = 4, unique(temp_inds$init_dist_plot), line = .6)
  
  #Plot points and segments 
  segments(x0 = prefs$dep_adj, y0 = prefs$med_cpue, y1 = prefs$cpue95, col = prefs$color)
  segments(x0 = prefs$dep_adj, y0 = prefs$cpue5, y1 = prefs$med_cpue, col = prefs$color)
  points(prefs$dep_adj, prefs$med_cpue, pch = prefs$point, cex = 1.2, col = prefs$color, bg = 'white')
  
  segments(x0 = rands$dep_adj, y0 = rands$med_cpue, y1 = rands$cpue95, lty = 1, col = rands$color)
  segments(x0 = rands$dep_adj, y0 = rands$cpue5, y1 = rands$med_cpue, lty = 1, col = rands$color)
  points(rands$dep_adj, rands$med_cpue, pch = rands$point, cex = 1.2, col = rands$color, bg = 'white')
  
  mtext(side = 3, adj = .02, fig2_letts[ii], line = -1.5)

  #add anchor point
  points(1, 0, pch = 23, cex = 2, bg = 'gray50', col = 'gray50')
  # points(0, 0, pch = 21, cex = 2, bg = 'gray')
}

mtext(side = 1, "Decrease from unfished", outer = T, line = 2.7, cex = 1.4)
mtext(side = 2, "Change in CPUE", outer = T, line = 3, cex = 1.4)

dev.off()
