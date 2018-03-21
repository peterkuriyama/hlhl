#Histograms of CPUE from the CA h&l survey

load("/Users/peterkuriyama/Dropbox/phd/research/hook_and_line/data/Grand.2014.JF.dmp")
# dat <- Grand.2014.JF
# yrz <- unique(dat$Year)
dat <- Grand.2014.JF



#--------------------------------------------------------------------------------------------
#Figure 7?
##ability to detect age classes of bocaccio
#--------------------------------------------------------------------------------------------
verm <- dat[grep("Vermilion", dat$ComName), ]
boc <- dat %>% filter(ComName == 'Bocaccio')

(sum(verm$SurvFish) + sum(boc$SurvFish)) / sum(dat$SurvFish)

temp <- boc %>% filter(Year == 2013)

#Bocaccio cohort figure
yrz <- 2004:2014

#--------------------------------------------------------------------------------------------
#For all data
hists <- dat %>% group_by(SiteName, Year) %>% summarize(nhooks = sum(SurvHook), nfish = sum(SurvFish), 
  cpue = nfish / nhooks, lat = mean(Lat.DD), 
  long = mean(Lon.DD))  

for_hist_plot <- lapply(seq(2004, 2014), FUN = function(x){
  temp <- hists %>% filter(Year == x)
  out <- hist(temp$cpue, plot = F)
})

# letts7 <- paste0(letters[1:22], ")")
# letts7 <- paste(letts7, yrz)
letts7 <- yrz
#--------------------------------------------------------------------------------------------
png(width = 7, height = 7, units = 'in', res = 150, file = 'figs/hlfig7.png')
par(mfrow = c(3, 4),mar = c(0, 0, 0, 0), oma = c(3.5, 5, 2, 2.5), mgp = c(1, .5, 0),
  xpd = T)
for(ii in 1:length(yrz)){
  tt <- for_hist_plot[[ii]]
  tt$freq <- tt$counts / sum(tt$counts)

  nsites <- hists %>% filter(Year == yrz[ii]) %>% distinct(SiteName) %>% nrow

  barplot(tt$freq, space = 0, xaxs = 'i', yaxs = 'i', ylim = c(0, .4), axes = F, ann = F)
  box()

  mtext(paste0(letts7[ii], "; ", nsites, ' sites'), 
    side = 3, line = -1.6, adj = .02, cex = .9)
  
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
