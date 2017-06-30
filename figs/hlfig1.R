
shape_list4 <- subset(shape_list1, scen != 'rightskew')
shape_list4$for_plot[2] <- 'Symmetric'
#--------------------------------------------------------------------------------------------
#Table of values
fish1s <- seq(20000, 200000, 20000)
inits_list <- vector('list', length(fish1s))

for(ff in 1:length(fish1s)){
  #Make Table of values also 
  #Format this figure
  inits1 <- lapply(1:nrow(shape_list4), FUN = function(ss){
    ctl1$shapes <- c(shape_list4[ss, 2], shape_list4[ss, 3])
    temp <- initialize_population(ctl = ctl1, nfish = fish1s[ff])
    return(temp)
  })
  
  inits2 <- lapply(inits1, FUN = function(x){
    c(median(x), min(x), max(x))
  })
  
  inits2 <- ldply(inits2)
  names(inits2) <- c('meds', 'mins', 'maxs')
  inits2$nfish <- fish1s[ff]
  inits2$scen <- shape_list4$scen

  inits_list[[ff]] <- inits2
}

inits_list <- ldply(inits_list)

inits_list$summ <- paste(inits_list$mins, round(inits_list$meds, digits = 0), inits_list$maxs, sep = " - ")
inits_list$abundance <- inits_list$nfish / max(inits_list$nfish)
inits_list$meds <- round(inits_list$meds, digits = 0)
inits_list$rng <- paste(inits_list$mins, inits_list$maxs, sep = " - ")
inits_list$rng <- paste0("(", inits_list$rng, ")")
inits_list$rng <- paste(inits_list$meds, inits_list$rng)

table1 <- inits_list %>% select(abundance, nfish, scen, meds, rng)


table11 <- table1 %>% dcast(abundance + nfish ~ scen, value.var = 'meds')
# table1 <- table1 %>% select(abundance, nfish, leftskew, normdist, uniform, patchy)
names(table11) <- paste0(toupper(substr(names(table11), 1, 1)), 
  substr(names(table11), 2, nchar(names(table11))))
write.csv(table11, 'output/table1_1.csv', row.names = FALSE)



table12 <- table1 %>% dcast(abundance + nfish ~ scen, value.var = 'rng')
names(table12) <- paste0(toupper(substr(names(table12), 1, 1)), 
  substr(names(table12), 2, nchar(names(table12))))
write.csv(table12, 'output/table1_2.csv', row.names = FALSE)
#--------------------------------------------------------------------------------------------
#Figure 1

#--------------------------------------------------------------------------------------------
#Plot Arguments
#REMOVE RIGHTSKEW


#Figure 1. Show distributions of each sceanrio
ctl1$nfish1 <- 60000

#Make table of values for initial distributions
letts <- c('a)', 'b)', 'c)', 'd)')

inits <- lapply(1:nrow(shape_list4), FUN = function(ss){
  ctl1$shapes <- c(shape_list4[ss, 2], shape_list4[ss, 3])
  temp <- initialize_population(ctl = ctl1, nfish = ctl1$nfish1)
  return(temp)
})

#--------------------------------------------------------------------------------------------
#Figure
#Should probably be a one column figure
#Add in matrices to visualize
inn <- lapply(1:4, FUN = function(x) melt(inits[[x]]))
names(inn) <- seq(1:4)
inn <- ldply(inn)
inn <- plyr::rename(inn, c(".id" = "ind", "Var1" = "x", 'Var2' = "y"))

#Add a column for values that are scaled lower
inn$low_value <- inn$value

max_value <- 200
inn[which(inn$low_value > max_value), 'low_value'] <- max_value

#Scale colors to provide more contrast rather than white and black
inn$scaled_value <- round(inn$low_value / max(inn$low_value) * 100, digits = 0)

greys <- paste0('grey', 100 - inn$scaled_value)
inn$greys <- rgb(t(col2rgb(greys)), maxColorValue = 255)

letts <- letts[c(1, 1, 2, 2, 3, 3, 4, 4)]
shape_list4 <- shape_list4[c(1, 1, 2, 2, 3, 3, 4, 4), ]

# greys <- paste0('grey', 100 - (test5$tot_fish_prop * 100))

fig1_list <- list(two = inits[[1]],
                  one = subset(inn, ind == 1),                   
                  four = inits[[2]],
                  three = subset(inn, ind == 2),                  
                  six = inits[[3]],
                  five = subset(inn, ind == 3),                   
                  eight = inits[[4]],
                  seven = subset(inn, ind == 4))

#Function for Color Bar on Plot
color_bar <- function(lut, min, max=-min, nticks=11, ticks=seq(min, max, len=nticks), 
  tick_labs, title='') {
    scale = (length(lut)-1)/(max-min)

    # dev.new(width=1.75, height=5)
    plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab=title, main='', 
      cex.lab=1.5, mgp = c(0, .5, 0))
    axis(2, at = ticks, labels = tick_labs, las=1)
    for (i in 1:(length(lut)-1)) {
     y = (i-1)/scale + min
     rect(0,y,10,y+1/scale, col=lut[i], border=NA)
    }
}

color_bar(lut = gg$greys, nticks = 5 , min = 0, max = 1, tick_labs = c("0", "50", 
  "100", "150", ">=200"))

gg <- inn %>% distinct(scaled_value, .keep_all = T) %>% arrange(scaled_value) %>% select(greys)
inn %>% filter(scaled_value == 0 | scaled_value == 100) %>% group_by(scaled_value) %>%
  summarize(dist_color = unique(greys))
inn$scaled_value %>% f
which(inn$scaled_value )

png(width = 5, height = 9.2, units = 'in', res = 150, file = 'figs/hlfig1.png')
pdf(width = 5, height = 9.2, file = 'figs/hlfig1.pdf')
par(mfrow = c(4, 2), mar = c(0, 0, 0, 0), oma = c(4, 5, .5, 1), mgp = c(0, .7, 0))

for(ii in 1:8){
  temp <- fig1_list[[ii]]

  if(ii %% 2 == 0){
    plot(temp$x, temp$y, pch = 15, cex = 2.35, col = temp$greys, ann = FALSE, axes = FALSE)  
    box()
  }

  if(ii %% 2 == 1){
    hist(temp, breaks = seq(0, 2270, 5), main = shape_list1[ii, 'scen'], freq = FALSE, 
      xlim = c(0, 300), axes = F, ann = F, ylim = c(0, .14), yaxs = 'i', xaxs = 'i', col = 'gray')
    box()
    mtext(letts[ii], side = 3, line = -1.7, adj = 0.01, cex = 1)
    mtext(shape_list4[ii, 'for_plot'], side = 3, line = -1.7, adj = .95, cex = 1)
    
    mtext(paste0('median = ', round(median(temp), digits = 0)), side = 3, line = -3, adj = .95)
    mtext(paste0('range = ', range(temp)[1], ', ', range(temp)[2]), side = 3, line = -4.2, adj = .95)
    
    axis(side = 2, at = seq(0, 0.12, by = .02), labels = seq(0, 0.12, by = .02), las = 2, 
      cex.axis = 1.2)
    # if(ii == 3){
    #   axis(side = 2, at = seq(0, 0.12, by = .02), labels = seq(0, 0.12, by = .02), las = 2, cex.axis = 1.2)
    #   axis(side = 1, at = seq(0, 250, by = 50), cex.axis = 1.2)
    # } 
    if(ii == 7){
      axis(side = 1, cex.axis = 1.2)
      mtext(side = 1, "Number of fish", outer = F, cex = 1.2, line = 2.5)
      #Need to add color bar to bottom right of this figure
    } 
  }
}

#Add scale bar for the plots on the right
par(fig = c(.45, 0.49, 0.03, .15), new = T)  
      color_bar(lut = gg$greys, nticks = 5 , min = 0, max = 1, tick_labs = c("0", "50", 
        "100", "150", ">=200"))

mtext(side = 2, "Proportion of sites", outer = T, cex = 1.2, line = 3)

# png(width = 7, height = 7, units = 'in', res = 150, file = 'figs/hlfig1_old.png')

dev.off()

#--------------------------------------------------------------------------------------------
#Figure for presentation
twos <- c(2, 4)

png(width = 8, height = 4.5, units = 'in', res = 150, file = 'figs/hlfig1_pres.png')
par(mfrow = c(1, 2), mar = c(0, 0, 0, 0), oma = c(4, 5, .5, 1), mgp = c(0, .7, 0))

for(ii in twos){
  temp <- inits[[ii]]
  hist(temp, breaks = seq(0, 2270, 5), main = shape_list1[ii, 'scen'], freq = FALSE, 
    xlim = c(0, 300), axes = F, ann = F, ylim = c(0, .14), yaxs = 'i', xaxs = 'i', col = 'gray')
  box()
  # mtext(letts[ii], side = 3, line = -1.7, adj = 0.01, cex = 1.25)
  mtext(shape_list4[ii, 'for_plot'], side = 3, line = -1.7, adj = .95, cex = 1.25)
  # mtext(paste0('mean = ', round(mean(temp), digits = 0)), side = 3, line = -3, adj = .95)
  mtext(paste0('median = ', round(median(temp), digits = 0)), side = 3, line = -3, adj = .95)
  mtext(paste0('range = ', range(temp)[1], ', ', range(temp)[2]), side = 3, line = -4, adj = .95)
  # if(ii == 2) axis(side = 2, at = seq(0, 0.12, by = .02), labels = seq(0, 0.12, by = .02), las = 2, 
  #   cex.axis = 1.2)
  if(ii == 2){
    axis(side = 2, at = seq(0, 0.12, by = .02), labels = seq(0, 0.12, by = .02), las = 2, cex.axis = 1.2)
    axis(side = 1, at = seq(0, 250, by = 50), cex.axis = 1.2)
  } 
  if(ii == 4) axis(side = 1, cex.axis = 1.2)
}
mtext(side = 1, "Number of fish", outer = T, cex = 1.5, line = 2.5)
mtext(side = 2, "Proportion of sites", outer = T, cex = 1.5, line = 3)
dev.off()

