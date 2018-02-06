
#--------------------------------------------------------------
load("output/twospp1_newcc_1000_001.Rdata")
load("output/twospp2_newcc_1000_001.Rdata")

#Plot fig 5 with fig 6 data
twospp <- rbind(twospp1, twospp2)
twospp$dep1 <- twospp$nfish1 / 2e5
twospp$dep2 <- twospp$nfish2 / 2e5
twospp$init_dist <- as.character(twospp$init_dist)

#Add in the 5/95 percentile and median cpue values
# hold nfish2 at 60000
twospp_key <- expand.grid(c(0.3, 0.5, 0.7), c('pref', 'rand'))
names(twospp_key) <- c("comp_coeff", 'init_dist')
twospp_key$ind <- 1:6
twospp_key$init_dist <- as.character(twospp_key$init_dist)
delta_fish <- 2000

fig5_letts <- paste0(letters[1:6], ")")
comp_captions <- c('Spp2 more aggressive', 'Equally aggressive', "Spp1 more aggressive")

#Figure out the CPUE when only one species present
#No species 2
nospp2 <- twospp %>% filter(init_dist == "patchy", nfish2 == 0) %>% 
  group_by(spp, type, comp_coeff, nfish1) %>%
  mutate(q5 = quantile(cpue, .05), med_cpue = median(cpue),
    q95 = quantile(cpue, .95)) %>% as.data.frame 
nospp2 <- nospp2 %>% filter(spp == 'spp1')

#No species 1
twospp %>% filter(nfish1 == 0) %>% head

nospp1 <- twospp %>% filter(init_dist == "patchy", nfish1 == 0) %>% 
  group_by(spp, type, comp_coeff, nfish1) %>%
  mutate(q5 = quantile(cpue, .05), med_cpue = median(cpue),
    q95 = quantile(cpue, .95)) %>% as.data.frame 

nospp1 <- nospp1 %>% filter(spp == 'spp2', nfish2 == 60000)

ggplot(nospp1, aes(x = nfish1, y = med_cpue)) + geom_point(aes(colour = type)) 


#--------------------------------------------------------------
#--------------------------------------------------------------
png(width = 7.45, height = 6, units = 'in', res = 150, file = 'figs/hlfig5.png')

par(mfrow = c(2, 3), mar = c(0, 0, 0, 0), oma = c(4, 4.5, 2, 2), xpd = T, 
  mgp = c(0, .5, 0))

for(ii in 1:6){
  #----------------------------------------
  #Process numbers
  #Filter the nospp data frames also
  nospp1_temp <- nospp1 %>% filter(comp_coeff == twospp_key[ii, 'comp_coeff'],
    type == twospp_key[ii, 'init_dist']) %>% distinct(med_cpue)
  nospp2_temp <- nospp2 %>% filter(comp_coeff == twospp_key[ii, 'comp_coeff'],
    type == twospp_key[ii, 'init_dist']) %>% distinct(nfish1, med_cpue)

  # data.frame(nfish1 = 0, med_cpue = 0)
  nospp2_temp <- rbind(data.frame(nfish1 = 0, med_cpue = 0), nospp2_temp)

  nn <- twospp %>% filter(nfish2 == 60000, comp_coeff == twospp_key[ii, 'comp_coeff'],
    init_dist == 'patchy')
  
  nn <- nn %>% group_by(spp, type, comp_coeff, nfish1, init_dist) %>% 
    mutate(q5 = quantile(cpue, .05), med_cpue = median(cpue),
      q95 = quantile(cpue, .95)) %>% as.data.frame
  
  #For only 0.3 competition coefficient
  temp <- nn
  temp <- temp %>% distinct(spp, type, comp_coeff, nfish1, nfish2, index,
    q5, med_cpue, q95)
  
  temp$nfish_adj <- 0
  
  spp1 <- temp %>% filter(spp == 'spp1')
  spp1$nfish_adj <- spp1$nfish1 - delta_fish
  
  spp2 <- temp %>% filter(spp == 'spp2')
  spp2$nfish_adj <- spp2$nfish1 + delta_fish
  
  dd <- rbind(spp1, spp2)
  
  twospp_key[ii, 'init_dist']
  prefs <- dd %>% filter(type == twospp_key[ii, 'init_dist'])
  # rands <- dd %>% filter(type == 'rand')
  
  prefs_spp1 <- prefs %>% filter(spp == "spp1")
  prefs_spp1$dep1 <- prefs_spp1$nfish1 / 200000
  prefs_spp2 <- prefs %>% filter(spp == "spp2")
  
  #----------------------------------------
  #Plot
  plot(1:10, type = 'n', ylim = c(0, 1), ann = FALSE, 
      axes = FALSE, xlim = c(-delta_fish, 200000 + delta_fish))
  box()
  
  #Add True CPUE lines
  lines(nospp2_temp$nfish1, nospp2_temp$med_cpue, lwd = 2, lty = 2)
  # lines(prefs_spp1$nfish_adj, prefs_spp1$dep1, lwd = 2, lty = 2)
  abline(h = nospp1_temp$med_cpue, col = 'gray', lwd = 2, lty = 2)

  #add points
  points(prefs_spp1$nfish1 - delta_fish, prefs_spp1$med_cpue, pch = 19, cex = 1.2,
    col = 'black')    
  points(prefs_spp2$nfish1 + delta_fish, prefs_spp2$med_cpue, pch = 19, cex = 1.2,
    col = 'gray')
  
 
  #Add axis captions
  if(ii %% 3 == 1) axis(side = 2, las = 2, cex.axis = 1.1)
  if(ii > 3) axis(side = 1, cex.axis = 1.1, 
    at = c(0, 50000, 100000, 150000, 200000), labels = c(0, 50, 100,
      150, 200))

  mtext(side = 3, adj = 0.02, fig5_letts[ii], line = -1.5, cex = 1.1)
  #Add Legend
  if(ii == 1) legend(5000, 1.05,
    c(paste0("Species1"),
      paste0("Species2")), col = c('black', 'gray'), 
    pch = 19, bty = 'n', cex = 1)

  #Add captions and 
  if(ii < 4) mtext(side = 3, comp_captions[ii])
  if(ii == 3) mtext(side = 4, "Size-based", line = .3)
  if(ii == 6) mtext(side = 4, "Random", line = .3)
}
mtext(side = 1, "Number of species 1 fish (1000s)", outer = T, line = 2.8, cex = 1.4)
mtext(side = 2, "CPUE", outer = T, line = 2.2, cex = 1.4)
  
dev.off()
