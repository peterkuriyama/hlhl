
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

library(tidyr)

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
  if(ii %in% 1:2) abline(a = 0, b = 1, lty = 2, col = 'gray', lwd = 3)
  if(ii %in% 3:4) abline(h = 0, lty = 2, col = 'black', lwd = 3)    
  
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
  mares$caption <- paste0("MARE=", mares$med_are)
  
  if(ii == 1){
    leg1 <- c(paste0('density-based; ', subset(mares, type == 'pref')$caption),
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
