#----------------------------------------------------------------------
#Plot the 
#load depletion runs

# load("output/onespp_depletion.Rdata")

#Load the data with a lower coefficient but say that it is from 
#random site selection 
load("output/onespp_depletion_0.3.Rdata")
onespp_depletion$type <- 'random'
onespp_depletion1 <- onespp_depletion

load("output/onespp_depletion_0.5.Rdata")

#Combine the 0.3 and 0.5 data sets
onespp_depletion <- rbind(onespp_depletion1, onespp_depletion)

#----------------------------------------------------------------------

onespp1 <- onespp_depletion %>% filter(spp == 'spp1')

#Give dep_type a more informative label
labz <- data_frame(dep_type = unique(onespp1$dep_type)) %>% as.data.frame
labz$label <- c('dd habitat selection', "local depletion")

# onespp_depletion %>% filter(spp == "spp1") %>% 
#   ggplot(aes(x = dep, y = cpue, colour = type)) + geom_point() + 
#   facet_grid(init_dist ~ nsites + dep_type) + ylim(c(0, 1)) +
#   geom_abline(slope = 1, intercept = 0, lty = 2)

#add zeroes and nhooks columns to onespp1
onespp1$nhooks <- 75

zeros <- onespp1 %>% filter(dep == .1) %>% distinct(.keep_all = T) 
zeros$cpue <- 0
zeros$dep <- 0 

onespp1 <- rbind(onespp1, zeros)
onespp1$location <- onespp1$iter

mares1 <- calc_mare_slopes(input = onespp1 %>% filter(dep_type == 'increasing'))
mares1[[1]]$dep_type <- 'increasing'

mares2 <- calc_mare_slopes(input = onespp1 %>% filter(dep_type == 'decreasing'))
mares2[[1]]$dep_type <- 'decreasing'

mares <- rbind(mares1[[1]], mares2[[1]])

#----------------------------------------------------------------------
#Plot the figuere
to_plot_dep <- onespp1

to_plot_dep <- to_plot_dep %>% group_by(dep, init_dist, type, dep_type,
                                            nhooks) %>% summarize(q5 = quantile(cpue, .05), q95 = quantile(cpue, .95),
                                                                  m5 = median(cpue)) %>% as.data.frame
to_plot_dep$init_dist <- as.character(to_plot_dep$init_dist)
to_plot_dep$type <- as.character(to_plot_dep$type)

to_plot_dep$ind <- 1
to_plot_dep[which(to_plot_dep$dep_type == 'increasing'), 'ind'] <- 2
to_plot_dep <- plyr::rename(to_plot_dep, c("m5" = 'med_cpue'))
to_plot_dep$type <- as.character(to_plot_dep$type)
delta <- .02
fig1_letts <- paste0(letters[1:4], ")")

#Add in MARE values
to_plot_dep <- to_plot_dep %>% left_join(mares, by = c('init_dist', 
  'type', 'nhooks', 'dep_type'))

#Add in slope values
mares1[[2]]$dep_type <- 'increasing'
mares2[[2]]$dep_type <- 'decreasing'
slopes <- rbind(mares1[[2]], mares2[[2]])
slopes <- plyr::rename(slopes, c('q5' = 'q5_slope', 'm5' = 'med_slope',
                                 'q95' = 'q95_slope'))
slopes$init_dist <- as.character(slopes$init_dist)
slopes$type <- as.character(slopes$type)
slopes <- slopes %>% select(-nsites)
slopes <- plyr::rename(slopes, c("dep_numeric" = 'dep'))

to_plot_dep <- to_plot_dep %>% left_join(slopes, by = c("init_dist", "dep_type",
                                                            'type', 'nhooks', 'dep'))
to_plot_dep <- to_plot_dep %>% filter(dep != 0)
to_plot_dep <- to_plot_dep %>% left_join(labz, by = 'dep_type')


#----------------------------------------------------------------------
if(length(unique(to_plot_dep$type)) == 1) png(width = 7, 
  height = 7, units = 'in', res = 150, file = 'figs/hlfig2_depletion.png')
if(length(unique(to_plot_dep$type)) == 2) png(width = 7, 
  height = 7, units = 'in', res = 150, file = 'figs/hlfigS1_depletion.png')

par(mfrow = c(2, 2), mar = c(0, 0, 0, 0), oma = c(3, 4, 3, 2), mgp = c(0, .5, 0))

for(ii in 1:4){
  if(ii %in% 1:2){
    temp <- subset(to_plot_dep, ind == ii)
  }
  
  if(ii %in% 3:4){
    temp <- subset(to_plot_dep, ind == ii - 2)
    temp$q5 <- temp$q5_slope
    temp$q95 <- temp$q95_slope
    temp$med_cpue <- temp$med_slope
  }
  
  temp$dep <- as.numeric(as.character(temp$dep))    
  temp$dep_adj <- temp$dep
  
  prefs <- subset(temp, type == 'pref')
  prefs$dep_adj <- prefs$dep_adj - delta
  
  rands <- subset(temp, type == 'random')
  rands$dep_adj <- rands$dep_adj + delta
  
  if(ii %in% 1:2){
    plot(temp$dep_adj, temp$med_cpue, type = 'n', ylim = c(0, 1), ann = FALSE, 
         axes = FALSE, xlim = c(-delta, 1 + .05))
  }
  
  if(ii %in% 3:4){
    if(nrow(rands) == 0){
      plot(temp$dep_adj, temp$med_cpue, type = 'n', ylim = c(-1.1, 3.55), ann = FALSE, 
           axes = FALSE, xlim = c(-delta, 1 + .05))   
    }
    
    if(nrow(rands) != 0){
      plot(temp$dep_adj, temp$med_cpue, type = 'n', ylim = c(-1.3, 4), 
        ann = FALSE, 
        axes = FALSE, xlim = c(-delta, 1 + .05))    
    }
  }
  box()

  #Add Axes
  if(ii == 1) axis(side = 2, las = 2, cex.axis = 1.2, 
                   at = c(0, .2, .4, .6, .8, 1), labels = c("0.0", .2, .4, .6, .8, "1.0") )
  
  if(ii == 3){
    axis(side = 2, las = 2, cex.axis = 1.2, at = c(-1, 0, 1, 2, 3))
  }
  if(ii == 2) mtext(side = 3, "Density-dependent habitat")
  if(ii == 1) mtext(side = 3, "Local depletion")
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
    if(nrow(rands) != 0){
      leg1 <- c(paste0('high; ', subset(mares, type == 'pref')$caption),
                paste0('medium; ', subset(mares, type == 'random')$caption))
      # legend(x = .7, y = .1, pch = c(19, 17), 
      #        legend = leg1, cex = .9, bty = 'n', x.intersp = .5)  
      legend('bottomright', pch = c(19, 17), 
             legend = leg1, cex = .9, bty = 'n', x.intersp = .5)  
    }
    
    if(nrow(rands) == 0){
      leg1 <- subset(mares, type == 'pref')$caption
      leg1 <- paste0("density-based; ", leg1)
      legend(x = .02, y = 1.05, pch = c(19, 17), 
             legend = leg1, cex = .9, bty = 'n', x.intersp = .5)  
    }
  } 
  
  if(ii == 2){
    if(nrow(rands) != 0){
      legend("bottomright", pch = c(19, 17), 
           legend = mares$caption, cex = .9, bty = 'n', x.intersp = .5)
      # legend(x = .7, y = .1, pch = c(19, 17), 
      #      legend = mares$caption, cex = .9, bty = 'n', x.intersp = .5)
    }    
    
    if(nrow(rands) == 0){
      legend(x = .02, y = 1.05, pch = c(19, 17), 
             legend = mares$caption, cex = .9, bty = 'n', x.intersp = .5)  
    }
    
    # leg1 <- paste0("Density based; ", leg1)
  }
  
  if(ii == 1) mtext(side = 2, "CPUE", line = 2.1, cex = 1.4)
  if(ii == 3) mtext(side = 2, "Difference in slope", line = 2.1, cex = 1.4)
}

mtext(side = 1, "Relative abundance", outer = T, line = 2, cex = 1.4)
mtext(side = 4, "Patchy; 50 sites", outer = T, line = .7, cex = 1.3)


dev.off()


