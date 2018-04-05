#--------------------------------------------------------------------------------------------
#Source hlfig2.R, up to plot part

#Calculate slopes of indices
#Add a column for index
onespp$ind <- onespp$dep_numeric * 10
onespp <- onespp %>% mutate(ind_lag = ind - 1, ind_lead = ind + 1) 

#-----------------------------------------
#Create data frames with lags
lags <- onespp %>% select(nsites, init_dist, type, location, cpue, ind_lag) 
names(lags)[5] <- 'lag_cpue'
names(lags)[6] <- 'ind'

leads <- onespp %>% select(nsites, init_dist, type, location, cpue, ind_lead) 
names(leads)[5] <- 'lead_cpue'
names(leads)[6] <- 'ind'

#Add in the lags 
onespp1 <- left_join(onespp, lags, by = c("nsites", 'init_dist', 'type',
  'location', 'ind'))

#Add in the leads
onespp1 <- left_join(onespp1, leads, by = c("nsites", 'init_dist', 'type',
  'location', 'ind'))

#Overwrite onespp with the one that works
onespp <- onespp1
names(onespp)[23] <- 'lead_cpue'
names(onespp)[24] <- 'lag_cpue'

onespp <- onespp %>% arrange(nsites, init_dist, type, location)

#Calculate slopes
onespp$slope <- (onespp$lead_cpue - onespp$lag_cpue) / .2

onespp$diff_from_one <- onespp$slope - 1

#Plot histograms
# onespp %>% filter(nsites == 100, init_dist == 'patchy', 
#   dep_numeric != 0, dep_numeric != 1) %>%
#   ggplot() + geom_histogram(aes(x = diff_from_one), binwdith = .5) + 
#   facet_wrap(~ dep_numeric + type, ncol = 2) +
#   geom_vline(xintercept = 0, lty = 2)
  

#Find 5%, median, and 95% difference in slope at each depletion level
#for each scenario
#Remove the zeroes
onespp_nozero <- onespp %>% filter(nfish1 != 0)

slope_summ <- onespp_nozero %>% group_by(nsites, init_dist, type, dep_numeric) %>%
  summarize(q5 = quantile(diff_from_one, 0.05, na.rm = T), 
            m5 = median(diff_from_one, na.rm = T),
            q95 = quantile(diff_from_one, .95, na.rm = T)) %>%
  as.data.frame

# #Summarioze the slopes and plot
# slope_summ %>% filter(nsites %in% c(5, 20, 50, 100)) %>% ggplot() + 
#   geom_point(aes(x = dep_numeric, y = m5, colour = type)) + 
#   geom_hline(yintercept = 0, col = 'red') + 
#   facet_grid(init_dist ~ nsites)

#------------------------------------------------------------
#Format to_plot and slope_summ
to_plot_slopes <- to_plot %>% distinct(nsites, dep, init_dist, spp, type, init_dist_plot)
slope_summ <- plyr::rename(slope_summ, c("dep_numeric" = 'dep'))

to_plot_slopes <- to_plot_slopes %>% left_join(slope_summ, 
  by = c('nsites', 'init_dist', 'type', 'dep'))  

#Fill NAs with zeroes
to_plot_slopes[which(is.na(to_plot_slopes$m5)), c('q5', 'm5', 'q95') ] <- 0

#Add plot indices into to_plot_slopes
to_plot_slopes <- to_plot %>% distinct(nsites, init_dist, ind) %>% 
  right_join(to_plot_slopes, by = c("nsites", 'init_dist'))

range(c(to_plot_slopes$q95, to_plot_slopes$q5, to_plot_slopes$m5))

#Remove values at dep = 0 and 1
to_plot_slopes <- to_plot_slopes %>% filter(dep != 0 & dep!= 1) 

#Convert the slopes into proportions above or below 1

#Do this plot with layout also

#------------------------------------------------------------
#Figure
png(width = 10, height = 10, units = 'in', res = 150, file = 'figs/hlfig2_slopes.png')

par(mfrow = c(4, 4), mar = c(0, 0, 0, 0), oma = c(4, 6, 3, 2), mgp = c(0, .5, 0))

for(ii in 1:16){
  
  temp <- subset(to_plot_slopes, ind == ii)
  temp$dep <- as.numeric(as.character(temp$dep))
    
  temp$dep_adj <- temp$dep
    
  prefs <- subset(temp, type == 'preferential')
  prefs$dep_adj <- prefs$dep_adj - delta
    
  rands <- subset(temp, type == 'random')
  rands$dep_adj <- rands$dep_adj + delta
  
  #Add in hyperstability coefficients
  # pref_beta <- temp %>% filter(type == 'preferential') %>% select(beta) %>%
  #   unique
  # pref_beta <- round(pref_beta, digits = 2)
  # rand_beta <- temp %>% filter(type == 'random') %>% select(beta) %>%
  #   unique
  # rand_beta <- round(rand_beta, digits = 2)
  
  plot(temp$dep_adj, temp$med_cpue, type = 'n', ylim = c(-1, 4), ann = FALSE, 
    axes = FALSE, xlim = c(-delta, 1 + .05))
  box()
  # lines(temp$dep_adj, temp$dep_adj, lty = 2, col = 'grey')

  #Add Axes
  # if(ii == 1) legend('bottomright', pch = c(19, 17), 
  #   legend = c('preferential', 'random'), cex = 1.3, bty = 'n')
  # if(ii == 1) legend('bottomright', pch = c(19, 17), 
  #   legend = c(paste0('preferential; ', pref_beta), 
  #     paste0('random; ',rand_beta)), cex = 1.3, bty = 'n')
  if(ii %% 4 == 1) axis(side = 2, las = 2, cex.axis = 1.2)
  if(ii < 5) mtext(side = 3, paste0(unique(temp$nsites), " sites"))
  if(ii > 12) axis(side = 1, cex.axis = 1.2)
  if(ii %% 4 == 0) mtext(side = 4, unique(temp$init_dist_plot), line = .6)

  #add in 1:1 line
  abline(a = 0, b = 0, lty = 2, col = 'black', lwd = 3)

  #Plot points and segments 
  points(prefs$dep_adj, prefs$m5, pch = 19, cex = 1.2)
  segments(x0 = prefs$dep_adj, y0 = prefs$m5, y1 = prefs$q95)
  segments(x0 = prefs$dep_adj, y0 = prefs$q5, y1 = prefs$m5)
    
  points(rands$dep_adj, rands$m5, pch = 17, cex = 1.2)
  segments(x0 = rands$dep_adj, y0 = rands$m5, y1 = rands$q95, lty = 1)
  segments(x0 = rands$dep_adj, y0 = rands$q5, y1 = rands$m5, lty = 1)
  mtext(side = 3, adj = .02, fig1_letts[ii], line = -1.5)
  


  #Add in median absolute relative error
  # mares <- temp %>% distinct(type, med_are)
  # mares[, 2] <- round(mares[, 2] * 100, digits = 0)
  # #Only include the median relative error values
  # # mares$caption <- paste0("mare=", mares$med_are)
  # mares$caption <- ""

  # mares$caption <- paste0("med=", mares$med_are, "; ", "min=", mares$min_are,  ", ",
  #   "max=",mares$max_are)
  
  if(ii == 1){
# browser()    
    leg1 <- c(('density-based'),
              ('random'))
    # leg1 <- c(paste0('density-based', subset(mares, type == 'preferential')$caption),
    #           paste0('random', subset(mares, type == 'random')$caption))
    legend(x = .02, y = 4.4, pch = c(19, 17), 
      legend = leg1, cex = 1.3, bty = 'n', x.intersp = .5)
  } 

  # if(ii != 1){
  #   legend(x = .02, y = 1.3, pch = c(19, 17), 
  #     legend = mares$caption, cex = 1.3, bty = 'n', x.intersp = .5)
  # }
  
  #Preferential first
  # mtext(side = 3, adj = .1, line = -1.5, subset(mares, type == 'preferential')$caption)
  # mtext(side = 3, adj = .1, line = -2.5, subset(mares, type == 'random')$caption)

  #add in coefficient text
  # if(ii != 1){
  #   legend('bottomright', pch = c(19, 17), legend = c(pref_beta,
  #     rand_beta), bty = 'n', cex = 1.3) 
  # }
}

mtext(side = 1, "Relative abundance", outer = T, line = 2.8, cex = 1.4)
mtext(side = 2, "Difference in slope", outer = T, line = 2.7, cex = 1.4)

dev.off()