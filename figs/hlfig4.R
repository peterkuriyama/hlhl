

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



#-----------------------------------------------------------------------------
#Figure 4 - Probability of increase or decrease
#Starting at some level and going up and down
#-----------------------------------------------------------------------------

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
  points(x = .5, y = 0, pch = 21, cex = 2, bg = 'white') #add anchor point
}

mtext(side = 1, "Change from 0.5", outer = T, line = 3, cex = 2)
mtext(side = 2, "Change in CPUE", outer = T, line = 3, cex = 2)

dev.off()
#At what depletion levels will ability to detect a change be significant?
#Results will be what number of 
