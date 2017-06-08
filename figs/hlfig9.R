#--------------------------------------------------------------------------------------------
#Add figure
#Evidence that distributions are patchy
#Annual CPUE plot, stand-alone
#Relative aggression - suggestion



#Add in cpue plot8
# plot(plot8$Year, plot8$cpue_boca, xlim = c(2003.5, 2014.5), type = 'n', axes = F,
#   ylim = c(0, .2))
# points(plot8_cpue$Year, plot8_cpue$mean_boca, type = 'o', pch = 19, lty = 2)
# points(plot8_cpue$Year, plot8_cpue$mean_verm, type = 'o', pch = 17)
# mtext("l) Unstandardized CPUE*", side = 3, line = -1.5, adj = .15, cex = .9)
# box()
# axis(side = 1, at = c(2004, 2008, 2012, 2016), labels = c("'04", "'08", "'12", "'16"),
#   cex.axis = 1.2)
# axis(side = 4, las = 2)
# legend('bottomright', c('bocaccio', 'vermilion'), pch = c(19, 17), bty = 'n' )
# # legend(x = 2012, y = .03, c('bocaccio', 'vermilion'), pch = c(19, 17), bty = 'n' )
# mtext("Year", side = 1, line = 2, cex = .9)

# mtext("Bocaccio CPUE", side = 1, outer = T, cex = 1.2, line = 2.2)
# mtext("Vermilion CPUE", side = 2, outer = T, cex = 1.2, line = 2.2)




#Histogram of the caught fish, does it look patchy?

#--------------------------------------------------------------------------------------------
#Aggression Figure

#Filter values to have only caught bocaccio
#Change vermilion complex to be vermilion
unique(dat$ComName)[grep("Vermilion", unique(dat$ComName)) ]
dat[grep("Vermilion", dat$ComName), 'ComName'] <- "Vermilion"

#Look ag unique Gangions
dat %>% filter(Year == 2004, SiteName == 205, DropNum == 2, AngNum == 3)

#Filter data to include gangions that caught only bocaccio or vermilion, 

hl_comp <- dat %>% group_by(Year, SiteName, DropNum, AngNum) %>% mutate(nfish = sum(SurvFish), nspecies = length(unique(ComName))) %>%
  group_by(Year, SiteName, DropNum, AngNum, ComName) %>% mutate(nfish_species = sum(SurvFish)) %>%
  filter(ComName == "Bocaccio" | ComName == "Vermilion") %>% group_by(Year, SiteName, DropNum, AngNum) %>%
  mutate(nspp = length(unique(ComName))) %>% 
#Filter out gangions that caught both bocaccio and vermilion
  filter(nspp != 2) %>%
#Now look at the bite time values
  group_by(ComName, nfish_species) %>%
  summarize(mean_fb = mean(TimeToFB, na.rm = T), 
    fb5 = quantile(TimeToFB, .05, na.rm = T), fb95 = quantile(TimeToFB, .95, na.rm = T), nvals = n()) %>% 
  filter(nfish_species != 0 & nfish_species < 6) %>% as.data.frame

bocas <- hl_comp %>% filter(ComName == "Bocaccio")
bocas$xx <- bocas$nfish_species - .1
verms <- hl_comp %>% filter(ComName == 'Vermilion')
verms$xx <- verms$nfish_species + .1


#Plot 7
png(width = 7, height = 7, units = 'in', res = 150, file = 'figs/hlfig_bitetime.png')
par(mgp = c(1, .5, 0))
plot(hl_comp$nfish_species, hl_comp$fb95, xlim = c(.5, 5.5), ylim = c(0, 220), type = 'n',
  ann = F, axes = F, xaxs = 'i', yaxs = 'i')
points(bocas$xx, bocas$mean_fb, pch = 17)
segments(x0 = bocas$xx, x1 = bocas$xx, y0 = bocas$fb5, bocas$mean_fb)
segments(x0 = bocas$xx, x1 = bocas$xx, y0 = bocas$mean_fb, bocas$fb95)
points(verms$xx, verms$mean_fb, pch = 19)
segments(x0 = verms$xx, x1 = verms$xx, y0 = verms$fb5, verms$mean_fb)
segments(x0 = verms$xx, x1 = verms$xx, y0 = verms$mean_fb, verms$fb95)
axis(side = 1, at = seq(1, 5, 1))
axis(side = 2, las = 2)
legend("topright", legend = c('bocaccio', 'vermilion'), bty = 'n', pch = c(17, 19))

mtext(side = 1, "Number of fish", line = 1.5)  
mtext(side = 2, "Time to first bite (sec)", line = 2)  
dev.off()
