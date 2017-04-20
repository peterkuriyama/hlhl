



#----------------------------------------
#Figure 5

# load("output/twospp1_50sens.Rdata")
#Two spp things run in "mega_run.R"

#These are with .01 and .05
# load("output/twospp1_50.Rdata")
# load("output/twospp23_50.Rdata")
# load("output/twospp45_50.Rdata")
# twospp <- rbind(twospp1, twospp23, twospp45)

load('output/twospp.1.1_100.Rdata')
twospp <- twospp1

#Check number of iterations for each
twospp %>% group_by(init_dist) %>% summarize(niters = length(unique(iter)), 
  nindex = length(unique(index)))

# load('output/twospp12_1000.Rdata')
# load('output/twospp34_1000.Rdata')
# twospp1000 <- rbind(twospp12, twospp34)

# #Check number of iterations for each
# twospp1000 %>% group_by(init_dist) %>% summarize(niters = length(unique(iter)),
#   nindex = length(unique(index)))

#Add depletion calculation
twospp$dep1 <- twospp$nfish1 / 2e5
twospp$dep2 <- twospp$nfish2 / 2e5

plot5 <- twospp %>% filter(init_dist == 'patchy')
plot5$tot_fish <- plot5$nfish1 + plot5$nfish2
plot5$prop1 <- plot5$nfish1 / plot5$tot_fish

#Remove points with no fish
plot5 <- plot5 %>% filter(tot_fish != 0)

#Remove end points also because those are obvious
plot5 <- plot5 %>% filter(prop1 != 0, prop1 != 1)

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

inds5$ind <- 1:6
plot5 <- inner_join(plot5, inds5, by = c("comp_coeff", "type"))

#Remove outliers
plot5 %>% filter(median_cpue != 0) %>% group_by(comp_coeff, type, spp) %>% 
  summarize(min_med = min(median_cpue),
    max_med = max(median_cpue)) %>% arrange(type) %>% filter(comp_coeff == 0.3)

xx <- plot5 %>% filter(comp_coeff == 0.3, type == 'pref', spp == 'spp1') 

hist(xx$median_cpue)



#----------------------------------------
#Check twospp results, to see if differences in fig 6 are due to 
#weird smoothing algorithm differences

cc <- the_data %>% filter(comp_coeff == .5, init_dist == 'normdist', type == 'pref')

ggplot(cc, aes(x = dep1, y = dep2, z = median_cpue)) + geom_tile() + facet_wrap(~ spp + init_dist)

ggplot(cc, aes(x = dep1, y = dep2)) + geom_tile(aes(fill = median_cpue)) + 
  facet_wrap(~ spp + type + init_dist)

ggplot(cc, aes(x = dep1, y = dep2, z = median_cpue)) + geom_contour() + facet_wrap(~ spp + init_dist)



#-----------------------------------------------------------------------------
#Figure 5 - Two Species Plots
#Easy plot simply understand the interaction between two two species
#-----------------------------------------------------------------------------
#Comp_coeff of 0.3, 0.5, 0.7 for one case, and sampling in 50 sites

png(width = 7.45, height = 6, units = 'in', res = 150, file = 'figs/hlfig5.png')

par(mfrow = c(2, 3), mar = c(0, 0, 0, 0), oma = c(4, 4.5, 2, 2), xpd = T, 
  mgp = c(0, .5, 0))

for(ii in 1:6){
  temp <- subset(plot5, ind == ii)

  temp1 <- subset(temp, spp == 'spp1')
  temp2 <- subset(temp, spp == 'spp2')

  #Plot empty plot
  plot(temp$prop1, temp$median_cpue, type = 'n', axes = F, ann = F, ylim = c(0, .9),
    xlim = c(0, 1.05))
  box()
  
  #Add points
  points(temp1$prop1, temp1$median_cpue, pch = 19, cex = 1.2)
  points(temp2$prop1, temp2$median_cpue, pch = 19, col = 'gray', cex = 1.2)

  #Add Text
  mtext(side = 3, adj = 0.02, fig5_letts[ii], line = -1.5, cex = 1.1)
  # if(ii < 4) mtext(side = 3, unique(temp1$comp_coeff))
  if(ii < 4) mtext(side = 3, paste0('comp. = ', unique(temp1$comp_coeff)))
  if(ii == 3) mtext(side = 4, "Preferential", line = .3)
  if(ii == 6) mtext(side = 4, "Random", line = .3)
  
  #Add Axes
  if(ii %% 3 == 1) axis(side = 2, las = 2, cex.axis = 1.2)
  if(ii > 3) axis(side = 1, cex.axis = 1.2)
  if(ii == 1) legend('bottomright', c('Species 1', 'Species 2'), col = c('black', 'gray'), 
    pch = 19, bty = 'n', cex = 1.3)
}

mtext(side = 1, outer = T, "Proportion species 1", line = 2.5, cex = 1.2)
mtext(side = 2, outer = T, "Median CPUE", line = 2.5, cex = 1.2)
# mtext(side = 3, outer = T, "Patchy Distribution", line = 2, cex = 1.4)

dev.off()
