load('../hook_and_line/data/Grand.2014.JF.dmp')
dat <- Grand.2014.JF

#--------------------------------------------------------------------------------------------
#Figure 7?
##ability to detect age classes of bocaccio
#--------------------------------------------------------------------------------------------
verm <- dat[grep("Vermilion", dat$ComName), ]
boc <- dat %>% filter(ComName == 'Bocaccio')
temp <- boc %>% filter(Year == 2013)

#Bocaccio cohort figure
yrz <- 2004:2014

hists <- lapply(2004:2014, FUN = function(x){
  temp <- boc %>% filter(Year == x)
  out <- hist(temp$Length.cm, breaks = seq(15, 85, by = 2), plot = F)
  out$props <- out$counts / sum(out$counts)
  return(out)
})

#For Vermilion
histsv <- lapply(2004:2014, FUN = function(x){
  temp <- verm %>% filter(Year == x)
  out <- hist(temp$Length.cm, breaks = seq(15, 85, by = 2), plot = F)
  out$props <- out$counts / sum(out$counts)
  return(out)
})

xranges <- lapply(1:11, FUN = function(x){
  c(min(hists[[x]]$breaks), max(hists[[x]]$breaks))
})

xranges <- unlist(xranges) 
range(xranges)

yranges <- lapply(1:11, FUN = function(x){
  c(min(hists[[x]]$density), max(hists[[x]]$density))
})

yranges <- unlist(yranges) 
range(yranges)

# letts7 <- paste0(letters[1:22], ")")
# letts7 <- paste(letts7, yrz)
letts7 <- yrz

#------------------------------------------------
png(file = 'figs/hlfig7.png', width = 6.5, height = 8.5, units = 'in', res = 150)

par(mfcol = c(11, 2), mar = c(0, 0, 0, 2), oma = c(3.5, 5, 1, 2), mgp = c(1, .5, 0))
for(ii in 1:length(hists)){
  barplot(hists[[ii]]$props, ylim = c(0, .25), 
    axes = F, xaxs = 'i', yaxs = 'i', space = 0)
  if(ii == 1) mtext("Bocaccio", side = 3, line = -.5, adj = .5)
  # box()
  axis(side = 2, las = 2, at = c(0, .1, .2), cex.axis = 1.2)  
  mtext(letts7[ii], side = 3, line = -2, adj = .02, cex = .9)
  mtext(paste0('n = ', sum(hists[[ii]]$counts)), line = -2, adj = .98, cex = .8)
  if(ii == length(hists)) axis(side = 1, cex.axis = 1.2, at = seq(0, 35, by = 5), labels = seq(15, 85, by = 10))
}

for(ii in 1:length(histsv)){
  barplot(histsv[[ii]]$props, ylim = c(0, .25), 
    axes = F, xaxs = 'i', yaxs = 'i', space = 0)
  if(ii == 1) mtext("Vermilion rockfish", side = 3, line = -.5, adj = .5)
  # box()
  axis(side = 2, las = 2, at = c(0, .1, .2), cex.axis = 1.2, labels = c('', '', ''))  
  mtext(letts7[ii + 11], side = 3, line = -2, adj = .02, cex = .9)
  mtext(paste0('n = ', sum(histsv[[ii]]$counts)), line = -2, adj = .98, cex = .8)
  if(ii == length(histsv)) axis(side = 1, cex.axis = 1.2, at = seq(0, 35, by = 5), labels = seq(15, 85, by = 10))
}

mtext("Length (cm)", side = 1, line = 2, cex = 1.2, outer = T)
mtext("Proportion", side = 2, line = 2.5, cex = 1.2, outer = T)

dev.off()