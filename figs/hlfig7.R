#Histograms of CPUE from the CA h&l survey

#--------------------------------------------------------------------------------------------
#For all data
hists <- dat %>% group_by(SiteName, Year) %>% summarize(nhooks = sum(SurvHook), nfish = sum(SurvFish), 
  cpue = nfish / nhooks, lat = mean(Lat.DD), 
  long = mean(Lon.DD))  

for_hist_plot <- lapply(seq(2004, 2014), FUN = function(x){
  temp <- hists %>% filter(Year == x)
  out <- hist(temp$cpue)
})

#--------------------------------------------------------------------------------------------
png(width = 7, height = 7, units = 'in', res = 150, file = 'figs/cpue_hists.png')
par(mfrow = c(3, 4),mar = c(0, 0, 0, 0), oma = c(3.5, 5, 2, 2.5), mgp = c(1, .5, 0),
  xpd = T)
for(ii in 1:length(yrz)){
  tt <- for_hist_plot[[ii]]
  tt$freq <- tt$counts / sum(tt$counts)

  barplot(tt$freq, space = 0, xaxs = 'i', yaxs = 'i', ylim = c(0, .4), axes = F, ann = F)
  box()
  
  mtext(letts7[ii], side = 3, line = -1.6, adj = .02, cex = .9)
  
  #Add Axes
  if(ii %in% c(1, 5, 9)) axis(side = 2, at = c(0, .1, .2, .3), las = 2, 
    cex.axis = 1.2, labels = c("0", "0.1", "0.2" ,"0.3"))
  if(ii == 8) axis(side = 1, at = c(2, 4, 6, 8), cex.axis = 1.2,
    labels = c("0.2", "0.4", "0.6", "0.8"))  
  if(ii >= 9) axis(side = 1, at = c(0, 2, 4, 6, 8), cex.axis = 1.2,
    labels = c("0", "0.2", "0.4", "0.6", "0.8"))
}
mtext(side = 1, outer = T, "CPUE", line = 2)
mtext(side = 2, outer = T, "Proportion", line = 2.5)
dev.off()
