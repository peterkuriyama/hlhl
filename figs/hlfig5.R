
#----------------------------------------
#Figure 5

# load("output/twospp1_50sens.Rdata")
#Two spp things run in "mega_run.R"

#These are with .01 and .05
# load("output/twospp1_50.Rdata")
# load("output/twospp23_50.Rdata")
# load("output/twospp45_50.Rdata")
# twospp <- rbind(twospp1, twospp23, twospp45)

#Old versions
# load('output/twospp.1.1_100.Rdata')

# load('output/twospp1_newcc_50_6.Rdata')
# load('output/twospp2_newcc_50_6.Rdata')

# load('output/twospp12_newcc_50.Rdata')
# twospp12$c1_sum <- .2
# save(twospp12, file = 'output/twospp12_newcc_50.Rdata' )
load("output/twospp1_newcc_1000_001.Rdata")
load("output/twospp2_newcc_1000_001.Rdata")
# load('output/twospp12_newcc_50_0.01.Rdata')
# load('output/twospp12_newcc_50_0.015.Rdata')
# twospp12$c1_sum <- .1
# save(twospp12, file = 'output/twospp12_newcc_50_0.01.Rdata' )

twospp <- rbind(twospp1, twospp2 )

#Check number of iterations for each
twospp %>% group_by(init_dist) %>% summarize(niters = length(unique(iter)), 
  nindex = length(unique(index)))

# load('output/twospp12_1000.Rdata')
# load('output/twospp34_1000.Rdata')
# twospp1000 <- rbind(twospp12, twospp34)

# #Check number of iterations for each
# twospp1000 %>% group_by(init_dist) %>% summarize(niters = length(unique(iter)),
#   nindex = length(unique(index)))

#----------------------------------------
#Modify data frames
#Add depletion calculation
twospp$dep1 <- twospp$nfish1 / 2e5
twospp$dep2 <- twospp$nfish2 / 2e5

#----------------------------------------
#Filter to have only one case
one_case <- twospp %>% filter(nfish2 == 60000)
one_case <- one_case %>% filter(init_dist == 'patchy')
one_case$prop1 <- one_case$nfish1 / (one_case$nfish1 + one_case$nfish2)
one_case$dep_end <- one_case$nfish_total / 2e5

spp1 <- one_case %>% filter(spp == "spp1")
spp1$dep_start <- spp1$nfish1 / 2e5

spp2 <- one_case %>% filter(spp == "spp2")
spp2$dep_start <- spp2$nfish2 / 2e5

one_case <- rbind(spp1, spp2)

#Add in error
one_case$re <- (one_case$cpue - one_case$dep_start) / one_case$dep_start

#Calculate medians and quantiles
one_case1 <- one_case %>% group_by(spp, init_dist, type, comp_coeff, prop1) %>% mutate(
  med = median(cpue), q5 = quantile(cpue, .05), q95 = quantile(cpue, .95), nvals = n(),
  mean_dep = mean(dep_start)) %>% group_by(spp, init_dist, type, comp_coeff) %>%
  mutate(mare = median(abs(re), na.rm = T)) %>% distinct(prop1,
    med, q5, q95, nvals, mean_dep, mare) %>% 
  as.data.frame


one_case1 %>%
  ggplot() + geom_point(aes(x = prop1, y = med, group = spp, colour = spp)) +
  geom_line(aes(x = prop1, y = mean_dep, colour = spp)) + 
  # geom_line(aes(x = prop1, y = mean_dep2)) + 
  geom_segment(aes(x = prop1, xend = prop1, y = med, yend = q95, colour = spp)) +
  geom_segment(aes(x = prop1, xend = prop1, y = q5, yend = med, colour = spp)) +
  facet_wrap(~ type + init_dist + comp_coeff)

fig5_letts <- paste0(letters[1:6], ")")
inds1 <- one_case1 %>% select(comp_coeff, type) %>% distinct() %>% arrange(type)
#Wait for final run to evaluate the randoms

inds1$ind <- 1:6
one_case1 <- inner_join(one_case1, inds1, by = c("comp_coeff", "type"))

#Add new figure 

png(width = 7.45, height = 6, units = 'in', res = 150, file = 'figs/hlfig5.png')

par(mfrow = c(2, 3), mar = c(0, 0, 0, 0), oma = c(4, 4.5, 2, 2), xpd = T, 
  mgp = c(0, .5, 0))

for(ii in 1:6){
  
  temp <- subset(one_case1, ind == ii)

  temp1 <- subset(temp, spp == 'spp1')
  temp1$prop1 <- temp1$prop1 + .03
  temp2 <- subset(temp, spp == 'spp2')

  #Plot empty plot
  plot(temp$prop1, temp$median_cpue, type = 'n', axes = F, ann = F, ylim = c(0, 1),
    xlim = c(0, .85))
  box()

  #Add the truth
  lines(temp1$prop1, temp1$mean_dep, lty = 2, lwd = 1)
  lines(temp2$prop1, temp2$mean_dep, lty = 2, col = 'gray', lwd = 1)
  
  #Add points
  points(temp1$prop1, temp1$med, pch = 19, cex = 1.2)
  segments(x0 = temp1$prop1, y0 = temp1$med, y1 = temp1$q95)
  segments(x0 = temp1$prop1, y0 = temp1$q5, y1 = temp1$med)
  
  points(temp2$prop1, temp2$med, pch = 19, cex = 1.2, col = 'gray')
  segments(x0 = temp2$prop1, y0 = temp2$med, y1 = temp2$q95, col = 'gray')
  segments(x0 = temp2$prop1, y0 = temp2$q5, y1 = temp2$med, col = 'gray')

  #Add Text
  mtext(side = 3, adj = 0.02, fig5_letts[ii], line = -1.5, cex = 1)
  # if(ii < 4) mtext(side = 3, unique(temp1$comp_coeff))
  # if(ii < 4) mtext(side = 3, paste0('comp. = ', unique(temp1$comp_coeff)))
  if(ii < 4) mtext(side = 3, comp_captions[ii])
  if(ii == 3) mtext(side = 4, "Preferential", line = .3)
  if(ii == 6) mtext(side = 4, "Random", line = .3)
  
  #Add Axes
  if(ii %% 3 == 1) axis(side = 2, las = 2, cex.axis = 1)
  if(ii > 3) axis(side = 1, cex.axis = 1)
  
  if(ii == 1) legend(x = 0.05, y = 1.05, 
    c(paste0("Species1; ", round(unique(temp1$mare), digits = 2)),
      paste0("Species2; ", round(unique(temp2$mare), digits = 2))), col = c('black', 'gray'), 
    pch = 19, bty = 'n', cex = 1)
  if(ii != 1) legend(x = 0.05, y = 1.05, 
    c(paste0(round(unique(temp1$mare), digits = 2)),
      paste0(round(unique(temp2$mare), digits = 2))), col = c('black', 'gray'), 
    pch = 19, bty = 'n', cex = 1)
}

mtext(side = 1, outer = T, "Proportion of species 1", line = 2.2, cex = 1)
mtext(side = 2, outer = T, "CPUE / relative abundance", line = 2.2, cex = 1)

dev.off()




#----------------------------------------
plot5 <- twospp %>% filter(init_dist == 'patchy')
plot5$tot_fish <- plot5$nfish1 + plot5$nfish2
plot5$prop1 <- plot5$nfish1 / plot5$tot_fish

#Remove points with no fish
plot5 <- plot5 %>% filter(tot_fish != 0)

#Remove end points also because those are obvious
plot5 <- plot5 %>% filter(prop1 != 0, prop1 != 1)

#----------------------------------------
#Plot as proportion of nfish1 to nfish2 changes
plot5$prop1_prop2 <- plot5$nfish1 / plot5$nfish2
diff_thing <- plot5 %>% group_by(spp, comp_coeff, init_dist,
  for_plot, type, nsites, nfish1, nfish2, dep1, dep2)

#----------------------------------------
plot5 %>% arrange(desc(prop1)) %>% distinct(prop1) 

plot5$prop1 <- round(plot5$prop1, digits = 2)
plot5 <- plot5 %>% arrange(desc(prop1))
plot5$int <- findInterval(plot5$prop1, seq(0, 1, by = .1))


thing <- plot5 %>% group_by(spp, comp_coeff, init_dist, for_plot, type, nsites, int) %>% 
  summarize(median_cpue = median(cpue), mean_cpue = mean(cpue), quant5 = quantile(cpue, .05),
  quant95 = quantile(cpue, .95), nvals = length(cpue)) %>% as.data.frame

#Add in alphas, 
thing$nvals <- thing$nvals / max(thing$nvals) 
greys <-  paste0('grey', 100 - (thing$nvals * 100))
thing$grey <- rgb(t(col2rgb(greys)), maxColorValue = 255)

inds5 <- thing %>% select(comp_coeff, type) %>% distinct() %>% arrange(type)
inds5$ind <- 1:6
thing <- inner_join(thing, inds5, by = c("comp_coeff", "type"))


#Include uncertainty

#Comp captions
comp_captions <- c('Spp2 more aggressive', 'Equally aggresive', "Spp1 more aggresive")

# png(width = 7.45, height = 6, units = 'in', res = 150, file = 'figs/hlfig5.png')


# par(mfrow = c(2, 3), mar = c(0, 0, 0, 0), oma = c(4, 4.5, 2, 2), xpd = T, 
#   mgp = c(0, .5, 0))

# for(ii in 1:6){
  
#   temp <- subset(thing, ind == ii)

#   temp1 <- subset(temp, spp == 'spp1')
#   temp1$int1 <- temp1$int - .1
#   temp2 <- subset(temp, spp == 'spp2')
#   temp2$int1 <- temp2$int + .1
  
#   #Plot empty plot
#   plot(temp$int, temp$median_cpue, type = 'n', axes = F, ann = F, ylim = c(0, 1),
#     xlim = c(0, 10))
#   box()
  
#   #Add points
#   points(temp1$int1, temp1$median_cpue, pch = 19, cex = 1.2, col = temp1$grey)
#   segments(x0 = temp1$int1, y0 = temp1$median_cpue, y1 = temp1$quant95, col = temp1$grey)
#   segments(x0 = temp1$int1, y0 = temp1$quant5, y1 = temp1$median_cpue, col = temp1$grey)
  
#   points(temp2$int1, temp2$median_cpue, pch = 17, cex = 1.2, col = temp2$grey)
#   segments(x0 = temp2$int1, y0 = temp2$median_cpue, y1 = temp2$quant95, col = temp2$grey)
#   segments(x0 = temp2$int1, y0 = temp2$quant5, y1 = temp2$median_cpue, col = temp2$grey)
#   # segments(x0 = temp2$prop1, y0 = temp2$median_cpue, y1 = temp2$quant95, col = 'gray')
#   # segments(x0 = temp2$prop1, y0 = temp2$quant5, y1 = temp2$median_cpue, col = 'gray')

#   #Add Text
#   mtext(side = 3, adj = 0.02, fig5_letts[ii], line = -1.5, cex = 1.1)
#   # if(ii < 4) mtext(side = 3, unique(temp1$comp_coeff))
#   # if(ii < 4) mtext(side = 3, paste0('comp. = ', unique(temp1$comp_coeff)))
#   if(ii < 4) mtext(side = 3, comp_captions[ii])
#   if(ii == 3) mtext(side = 4, "Preferential", line = .3)
#   if(ii == 6) mtext(side = 4, "Random", line = .3)
  
#   #Add Axes
#   if(ii %% 3 == 1) axis(side = 2, las = 2, cex.axis = 1.2)
#   if(ii > 3) axis(side = 1, cex.axis = 1.2)
#   if(ii == 1) legend('bottomright', c('Species 1', 'Species 2'), col = c('black', 'gray'), 
#     pch = 19, bty = 'n', cex = 1.3)
# }

# mtext(side = 1, outer = T, "Proportion of species 1", line = 2.5, cex = 1.2)
# mtext(side = 2, outer = T, "Median CPUE", line = 2.5, cex = 1.2)
# # mtext(side = 3, outer = T, "Patchy Distribution", line = 2, cex = 1.4)

# dev.off()



# thing$tot_fish <- thing$nfish1 + thing$nfish2
# thing$prop1 <- thing$nfish1 / thing$tot_fish
# thing$prop1_prop2 <- thing$nfish1 / thing$nfish2

# # thing$int <- findInterval(thing$prop1, seq(0, 1, by = .1))

# ggplot(thing) + geom_point(aes(x = int, y = median_cpue)) + 
#   geom_segment(aes(x = int, xend = int, y = median_cpue, yend = quant95)) + 
#   geom_segment(aes(x = int, xend = int, y = quant5, yend = median_cpue)) +
#   facet_wrap(~ spp + type + comp_coeff, ncol = 3)

#Can't really change this

#Pare down this plot
evens <- plot5 %>% filter(prop1 %in% seq(.1, .9, .1))

# evens %>% group_by(prop1, spp) %>% summarize(nvals = n())

evens1 <- evens %>% group_by(spp, comp_coeff, init_dist, for_plot, type, nsites, prop1) %>% 
  summarize(median_cpue = median(cpue), quant5 = quantile(cpue, .05),
    quant95 = quantile(cpue, .95), nvals = length(cpue)) %>% as.data.frame

# evens1 %>% filter(comp_coeff == 0.3, init_dist == 'patchy')

ggplot(evens1) + geom_point(aes(x = prop1, y = median_cpue)) + 
  geom_segment(aes(x = prop1, xend = prop1, y = median_cpue, yend = quant95)) + 
  geom_segment(aes(x = prop1, xend = prop1, y = quant5, yend = median_cpue)) + 
  facet_wrap(~ spp + type + comp_coeff, ncol = 3)


#----------------------------------------
plot5 <- plot5 %>% group_by(spp, comp_coeff, init_dist, for_plot, type, nsites, prop1) %>% 
  summarize(median_cpue = median(cpue), quant5 = quantile(cpue, .05),
  quant95 = quantile(cpue, .95), nvals = length(cpue)) %>% as.data.frame

#Plot 5 Sketch
# ggplot(plot5, aes(x = prop1, y = median_cpue)) + geom_point(aes(colour = spp)) + 
#   facet_wrap(~ type + comp_coeff, ncol = 3)

#Add indices for subsetting
fig5_letts <- paste0(letters[1:6], ")")
inds5 <- plot5 %>% select(comp_coeff, type) %>% distinct() %>% arrange(type)
#Wait for final run to evaluate the randoms

inds5$ind <- 1:6
plot5 <- inner_join(plot5, inds5, by = c("comp_coeff", "type"))

#Remove outliers
plot5 %>% filter(median_cpue != 0) %>% group_by(comp_coeff, type, spp) %>% 
  summarize(min_med = min(median_cpue),
    max_med = max(median_cpue)) %>% arrange(type) %>% filter(comp_coeff == 0.3)

# xx <- plot5 %>% filter(comp_coeff == 0.3, type == 'pref', spp == 'spp1') 
# hist(xx$median_cpue)

twospp %>% filter(comp_coeff == 0.3, spp == 'spp1', type == 'pref', 
  init_dist == 'normdist', nfish1 > 180000) %>% ggplot(aes(x = cpue)) + geom_histogram()

#----------------------------------------
#Summary results
#Range of median CPUE values
plot5 %>% filter(init_dist == 'patchy') %>% group_by(comp_coeff, type, spp) %>% 
  summarize(min_cpue = min(median_cpue), max_cpue = max(median_cpue)) %>% 
  arrange(type, comp_coeff)


#----------------------------------------
#Check twospp results, to see if differences in fig 6 are due to 
# #weird smoothing algorithm differences

# cc <- the_data %>% filter(comp_coeff == .5, init_dist == 'normdist', type == 'pref')

# ggplot(cc, aes(x = dep1, y = dep2, z = median_cpue)) + geom_tile() + facet_wrap(~ spp + init_dist)

# ggplot(cc, aes(x = dep1, y = dep2)) + geom_tile(aes(fill = median_cpue)) + 
#   facet_wrap(~ spp + type + init_dist)

# ggplot(cc, aes(x = dep1, y = dep2, z = median_cpue)) + geom_contour() + facet_wrap(~ spp + init_dist)


#-----------------------------------------------------------------------------
#Figure 5 - Two Species Plots
#Easy plot simply understand the interaction between two two species
#-----------------------------------------------------------------------------
#Comp_coeff of 0.3, 0.5, 0.7 for one case, and sampling in 50 sites

#Comp captions
comp_captions <- c('Spp2 more aggressive', 'Equally aggresive', "Spp1 more aggresive")

png(width = 7.45, height = 6, units = 'in', res = 150, file = 'figs/hlfig5_old.png')

par(mfrow = c(2, 3), mar = c(0, 0, 0, 0), oma = c(4, 4.5, 2, 2), xpd = T, 
  mgp = c(0, .5, 0))

for(ii in 1:6){
  
  temp <- subset(plot5, ind == ii)

  temp1 <- subset(temp, spp == 'spp1')
  temp2 <- subset(temp, spp == 'spp2')

  #Plot empty plot
  plot(temp$prop1, temp$median_cpue, type = 'n', axes = F, ann = F, ylim = c(0, 1),
    xlim = c(0, 1.05))
  box()
  
  #Add points
  points(temp1$prop1, temp1$median_cpue, pch = 19, cex = 1.2)
  # segments(x0 = temp1$prop1, y0 = temp1$median_cpue, y1 = temp1$quant95)
  # segments(x0 = temp1$prop1, y0 = temp1$quant5, y1 = temp1$median_cpue)
  
  points(temp2$prop1, temp2$median_cpue, pch = 19, col = 'gray', cex = 1.2)
  # segments(x0 = temp2$prop1, y0 = temp2$median_cpue, y1 = temp2$quant95, col = 'gray')
  # segments(x0 = temp2$prop1, y0 = temp2$quant5, y1 = temp2$median_cpue, col = 'gray')

  #Add Text
  mtext(side = 3, adj = 0.02, fig5_letts[ii], line = -1.5, cex = 1.1)
  # if(ii < 4) mtext(side = 3, unique(temp1$comp_coeff))
  # if(ii < 4) mtext(side = 3, paste0('comp. = ', unique(temp1$comp_coeff)))
  if(ii < 4) mtext(side = 3, comp_captions[ii])
  if(ii == 3) mtext(side = 4, "Preferential", line = .3)
  if(ii == 6) mtext(side = 4, "Random", line = .3)
  
  #Add Axes
  if(ii %% 3 == 1) axis(side = 2, las = 2, cex.axis = 1.2)
  if(ii > 3) axis(side = 1, cex.axis = 1.2)
  if(ii == 1) legend('bottomright', c('Species 1', 'Species 2'), col = c('black', 'gray'), 
    pch = 19, bty = 'n', cex = 1.3)
}

mtext(side = 1, outer = T, "Proportion of species 1", line = 2.5, cex = 1.2)
mtext(side = 2, outer = T, "Median CPUE", line = 2.5, cex = 1.2)
# mtext(side = 3, outer = T, "Patchy Distribution", line = 2, cex = 1.4)

dev.off()


#-----------------------------------------------------------------------------
#With uncertainty
png(width = 7.45, height = 6, units = 'in', res = 150, file = 'figs/hlfig5_old_uncertainty.png')

par(mfrow = c(2, 3), mar = c(0, 0, 0, 0), oma = c(4, 4.5, 2, 2), xpd = T, 
  mgp = c(0, .5, 0))

for(ii in 1:6){
  
  temp <- subset(plot5, ind == ii)

  temp1 <- subset(temp, spp == 'spp1')
  temp2 <- subset(temp, spp == 'spp2')

  #Plot empty plot
  plot(temp$prop1, temp$median_cpue, type = 'n', axes = F, ann = F, ylim = c(0, 1),
    xlim = c(0, 1.05))
  box()
  
  #Add points
  points(temp1$prop1, temp1$median_cpue, pch = 19, cex = 1.2)
  segments(x0 = temp1$prop1, y0 = temp1$median_cpue, y1 = temp1$quant95)
  segments(x0 = temp1$prop1, y0 = temp1$quant5, y1 = temp1$median_cpue)
  
  points(temp2$prop1, temp2$median_cpue, pch = 19, col = 'gray', cex = 1.2)
  segments(x0 = temp2$prop1, y0 = temp2$median_cpue, y1 = temp2$quant95, col = 'gray')
  segments(x0 = temp2$prop1, y0 = temp2$quant5, y1 = temp2$median_cpue, col = 'gray')

  #Add Text
  mtext(side = 3, adj = 0.02, fig5_letts[ii], line = -1.5, cex = 1.1)
  # if(ii < 4) mtext(side = 3, unique(temp1$comp_coeff))
  # if(ii < 4) mtext(side = 3, paste0('comp. = ', unique(temp1$comp_coeff)))
  if(ii < 4) mtext(side = 3, comp_captions[ii])
  if(ii == 3) mtext(side = 4, "Preferential", line = .3)
  if(ii == 6) mtext(side = 4, "Random", line = .3)
  
  #Add Axes
  if(ii %% 3 == 1) axis(side = 2, las = 2, cex.axis = 1.2)
  if(ii > 3) axis(side = 1, cex.axis = 1.2)
  if(ii == 1) legend('bottomright', c('Species 1', 'Species 2'), col = c('black', 'gray'), 
    pch = 19, bty = 'n', cex = 1.3)
}

mtext(side = 1, outer = T, "Proportion of species 1", line = 2.5, cex = 1.2)
mtext(side = 2, outer = T, "Median CPUE", line = 2.5, cex = 1.2)
# mtext(side = 3, outer = T, "Patchy Distribution", line = 2, cex = 1.4)

dev.off()

#-----------------------------------------------------------------------------
# one <- subset(plot5, ind == 1)
# one1 <- subset(one, prop1 %in% seq(.1, .9, .1))



# ggplot(one1, aes(x = prop1, y = median_cpue, colour = spp)) + geom_point()








