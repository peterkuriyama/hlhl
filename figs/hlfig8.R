
#--------------------------------------------------------------------------------------------
#Figure 8
##Look at CPUE trends? btw verm and boca
#--------------------------------------------------------------------------------------------
#Calculate total cpue, bocaccio cpue, vermilion cpue
dat[grep("Vermilion", dat$ComName), 'ComName'] <- "Vermilion"

plot8 <- dat %>% group_by(SiteName, Year) %>% mutate(nhooks = sum(SurvHook), 
           totfish = sum(SurvFish)) %>% 
           group_by(SiteName, Year, ComName) %>% mutate(nfish = sum(SurvFish)) %>% 
           select(nhooks, totfish, nfish) %>% as.data.frame %>% distinct() 

#--------------------------------------------------------------------------------------------
#Add in cpue bins to group sites with plots
plot8$cpue <- plot8$totfish / plot8$nhooks

sd_bins <- plot8 %>% distinct(SiteName, Year, cpue) %>% group_by(SiteName) %>%
  summarize(sd_cpue = sd(cpue), mean_cpue = mean(cpue), cv_cpue = sd_cpue / mean_cpue) %>% 
  arrange(desc(sd_cpue)) 
sd_bins <- sd_bins %>% mutate(cpue_bin = cut(sd_cpue, breaks = c(0, .103, .151, .203,
  .321)))

plot8 <- plot8 %>% left_join(sd_bins, by = "SiteName")   

plot8 %>% ggplot(aes(x = Year, y = cpue)) + geom_line(aes(group = SiteName)) + 
  facet_wrap(~ cpue_bin)

plot8 %>% distinct(SiteName, .keep_all = T) %>% 
  ggplot(aes(x = cv_cpue)) + geom_histogram()


plot8 %>% distinct(SiteName, Year, cpue) %>% group_by(SiteName) %>% 
  summarize(min_cpue = min(cpue), max_cpue = max(cpue),
    range = max_cpue - min_cpue) %>% arrange(desc(range))

plot8 %>% distinct(SiteName, Year, cpue) %>% ggplot(aes(x = cpue)) +
  geom_histogram() + facet_wrap(~ Year)


ggplot(plot8, aes(x = Year, y = ))

#--------------------------------------------------------------------------------------------
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

#CPUE Trend
plot8_cpue <- plot8 %>% group_by(Year) %>% 
  summarize(mean_boca = mean(cpue_boca), mean_verm = mean(cpue_verm),
    sd_boca =  sd(cpue_boca, na.rm = T), sd_verm = sd(cpue_verm),
    cv_boca = sd_boca / mean_boca, cv_verm = sd_verm / mean_verm) %>% as.data.frame()
hist(plot8_cpue$cv_boca)
hist(plot8_cpue$cv_verm)
ggplot(plot8_cpue, aes(x = Year)) + geom_line(aes(y = cpue_boca))


# ggplot(plot8, aes(x = cpue_boca, y = cpue_verm)) + 
#   geom_point(aes(size = cpue_tot), alpha = .2) + 
#   facet_wrap(~ Year) + theme_bw()
#--------------------------------------------------------------------------------------------
png(width = 7, height = 7, units = 'in', res = 150, file = 'figs/hlfig8.png')

par(mgp = c(1, .5, 0))
plot(plot8$Year, plot8$cpue_boca, xlim = c(2003.5, 2014.5), type = 'n', axes = F,
  ylim = c(0, .2), ann = F)
points(plot8_cpue$Year, plot8_cpue$mean_boca, type = 'o', pch = 17, lty = 2)
points(plot8_cpue$Year, plot8_cpue$mean_verm, type = 'o', pch = 19)
# mtext("l) Unstandardized CPUE*", side = 3, line = -1.5, adj = .15, cex = .9)
box()
axis(side = 1, at = c(2004, 2008, 2012, 2016), labels = c("'04", "'08", "'12", "'16"),
  cex.axis = 1)
axis(side = 2, las = 2)
legend('bottomright', c('bocaccio', 'vermilion'), pch = c(17, 19), bty = 'n' )
# legend(x = 2012, y = .03, c('bocaccio', 'vermilion'), pch = c(19, 17), bty = 'n' )
mtext("Year", side = 1, line = 2, cex = 1.2)
mtext("Unstandardized CPUE", side = 2, line = 2.5, cex = 1.2)

dev.off()

round(range(plot8_cpue$mean_boca), digits = 2)
round(range(plot8_cpue$mean_verm), digits = 2)
range(diff(plot8_cpue$mean_boca))
range(diff(plot8_cpue$mean_verm))
# mtext("Bocaccio CPUE", side = 1, outer = T, cex = 1.2, line = 2.2)
# mtext("Vermilion CPUE", side = 2, outer = T, cex = 1.2, line = 2.2)

#--------------------------------------------------------------------------------------------
#multiplot with 
png(width = 8, height = 7, units = 'in', res = 150, file = 'figs/hlfig8_multi.png')

par(mfrow = c(3, 4),mar = c(0, 0, 0, 0), oma = c(3.5, 5, 2, 2.5), mgp = c(1, .5, 0),
  xpd = T)
for(ii in 1:length(yrz)){
  tp8 <- plot8 %>% filter(Year == yrz[[ii]])

  plot(1:10, xlim = c(0, .85), ylim = c(0, .95), type = 'n', ann = F, axes = F)
  # gray_color <- rgb(t(col2rgb('black')), maxColorValue = 255, alpha = 75)
  gray_color <- 'gray80'
  #Add polygon in impossible areas

  lines(x = seq(0, .9, .1), y = 1 - seq(0, .9, .1), lty = 2, col = 'gray')
  # polygon(x = c(0, .9, .9, 0), y = c(1, 1, .1, 1), col = gray_color, 
  #   border = gray_color)
  box()

  #Point size corresponds to   
  # points(tp8$cpue_boca, tp8$cpue_verm, pch = 21, cex = tp8$cpue_tot * 3, 
  #   bg = rgb(t(col2rgb('black')), maxColorValue = 255, alpha = 75), 
  #   col = NA)

  points(tp8$cpue_boca, tp8$cpue_verm, pch = 21,
    bg = rgb(t(col2rgb('black')), maxColorValue = 255, alpha = 75), 
    col = NA, cex = 1)

  mtext(letts7[ii], side = 3, line = -1.5, adj = .02, cex = .9)
  
  # if(ii == 1) legend('topright', legend = rev(c(.25, .50, .75, 1)), 
  #   pt.cex = rev(c(.75, 1.5, 2.25, 3)), pch = 19, 
  #   col = rgb(t(col2rgb('black')), maxColorValue = 255, alpha = 75), 
  #   bty = 'n', y.intersp = 1.2, cex = 1)

  #Add Axes
  if(ii %in% c(1, 5, 9)) axis(side = 2, at = c(0, .2, .4, .6, .8), las = 2, 
    cex.axis = 1.2, labels = c("0", "0.2", "0.4", "0.6", "0.8"))
  if(ii == 8) axis(side = 1, at = c(0, .2, .4, .6, .8), cex.axis = 1.2, tck = .02,
    labels = F)
  if(ii >= 9) axis(side = 1, at = c(0, .2, .4, .6, .8), cex.axis = 1.2, 
    labels = c("0", "0.2", "0.4", "0.6", "0.8"))
}

#Add in cpue plot8
plot(plot8$Year, plot8$cpue_boca, xlim = c(2003.5, 2014.5), type = 'n', axes = F,
  ylim = c(0, .2))
points(plot8_cpue$Year, plot8_cpue$mean_boca, type = 'o', pch = 19, lty = 2)
points(plot8_cpue$Year, plot8_cpue$mean_verm, type = 'o', pch = 17)
mtext("l) Unstandardized CPUE*", side = 3, line = -1.5, adj = .15, cex = .9)
box()
axis(side = 1, at = c(2004, 2008, 2012, 2016), labels = c("'04", "'08", "'12", "'16"),
  cex.axis = 1.2)
axis(side = 4, las = 2)
legend('bottomright', c('bocaccio', 'vermilion'), pch = c(19, 17), bty = 'n' )
# legend(x = 2012, y = .03, c('bocaccio', 'vermilion'), pch = c(19, 17), bty = 'n' )
mtext("Year", side = 1, line = 2, cex = .9)

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