
#--------------------------------------------------------------------------------------------
#Figure 8
##Look at CPUE trends? btw verm and boca
#--------------------------------------------------------------------------------------------
#Calculate total cpue, bocaccio cpue, vermilion cpue
dat[grep("Vermilion", dat$ComName), 'ComName'] <- "Vermilion"

dat %>% group_by(SiteName, Year) %>% mutate(nhooks = sum(SurvHook), totfish = sum(SurvFish)) %>%
  group_by(SiteName, Year, ComName) %>% mutate(nfish = sum(SurvFish)) %>% 
  select(nhooks, totfish, nfish) %>% as.data.frame %>% distinct() -> plot8

#Change vermilion comname to vermilion
plot8 <- plot8 %>% filter(ComName %in% c('Bocaccio', 'Vermilion')) %>% 
  dcast(SiteName + Year + nhooks + totfish ~ ComName, value.var = 'nfish')

#change NAs to 0s
plot8 <- plyr::rename(plot8, c('Bocaccio' = 'boca', "Vermilion" = 'verm'))
plot8[is.na(plot8$boca), 'boca'] <- 0
plot8[is.na(plot8$verm), 'verm'] <- 0

plot8$cpue_tot <- plot8$totfish / plot8$nhooks
plot8$cpue_boca <- plot8$boca / plot8$nhooks
plot8$cpue_verm <- plot8$verm / plot8$nhooks

ggplot(plot8, aes(x = cpue_boca, y = cpue_verm)) + 
  geom_point(aes(size = cpue_tot), alpha = .2) + 
  facet_wrap(~ Year) + theme_bw()

png(width = 8, height = 7, units = 'in', res = 150, file = 'figs/hlfig8.png')
par(mfrow = c(3, 4),mar = c(0, 0, 0, 0), oma = c(3.5, 5, 2, 1), mgp = c(1, .5, 0),
  xpd = T)
for(ii in 1:length(yrz)){
  tp8 <- plot8 %>% filter(Year == yrz[[ii]])

  plot(1:10, xlim = c(0, .85), ylim = c(0, .95), type = 'n', ann = F, axes = F)
  # plot(1:10, xlim = c(-.05, .8), ylim = c(0, .8), type = 'n', ann = F, axes = F, xaxs = 'i', 
  #   yaxs = 'i')
  box()
  
  points(tp8$cpue_boca, tp8$cpue_verm, pch = 21, cex = tp8$cpue_tot * 3, 
    bg = rgb(t(col2rgb('black')), maxColorValue = 255, alpha = 75), 
    col = NA)

  mtext(letts7[ii], side = 3, line = -1.5, adj = .02, cex = .9)
  
  if(ii == 1) legend('topright', legend = rev(c(.25, .50, .75, 1)), 
    pt.cex = rev(c(.75, 1.5, 2.25, 3)), pch = 19, 
    col = rgb(t(col2rgb('black')), maxColorValue = 255, alpha = 75), 
    bty = 'n', y.intersp = 1.2, cex = 1)

  if(ii %in% c(1, 5, 9)) axis(side = 2, at = c(0, .2, .4, .6, .8), las = 2, 
    cex.axis = 1.2)
  if(ii >= 8) axis(side = 1, at = c(0, .2, .4, .6, .8), cex.axis = 1.2)
  #Add Axes
}

mtext("Bocaccio CPUE", side = 1, outer = T, cex = 1.2, line = 2.2)
mtext("Vermilion CPUE", side = 2, outer = T, cex = 1.2, line = 2.2)

dev.off()



# cpues <- dat %>% group_by(SiteName, Year) %>% summarize(cpue = sum(SurvFish) / length(NumBoc))
# temp <- cpues %>% filter(Year == 2004)
# hist(temp$cpue, breaks = 20)


# histsd <- lapply(2004:2014, FUN = function(x){
#   temp <- cpues %>% filter(Year == x)
#   hist(temp$cpue, breaks = 20, plot = F)
# })



# props <- lapply(1:11, FUN = function(x){
#   histsd[[x]]$counts / sum(histsd[[x]]$counts)
# })


# par(mfcol = c(11, 1), mar = c(0, 0, 0, 0), oma = c(3.5, 5, 2, 1), mgp = c(1, .5, 0))
# for(ii in 1:length(props)){
#   barplot(props[[ii]], xlim = c(0, 20), ylim = c(0, .25), 
#     axes = F, xaxs = 'i', yaxs = 'i', space = 0)
#   axis(side = 2, las = 2, at = c(0, .1, .2), cex.axis = 1.2)  

#   mtext(yrz[ii], side = 3, line = -2.5, adj = .98, cex = .9)
#   # mtext(paste0('n = ', sum(histsd[[ii]]$counts)), line = -3.5, adj = .98, cex = .8)
#   if(ii == length(props)) axis(side = 1, cex.axis = 1.2, 
#     at = c(0, 5, 10, 15, 20), labels = c(0, .25, .5, .75, 1))
# }
# mtext("CPUE", side = 1, line = 2, cex = 1.2, outer = T)
# mtext("Proportion", side = 2, line = 3.2, cex = 1.2, outer = T)