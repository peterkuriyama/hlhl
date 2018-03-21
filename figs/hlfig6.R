#----------------------------------------
#Figure 6
load("output/twospp1_newcc_1000_001.Rdata")
load("output/twospp2_newcc_1000_001.Rdata")

twospp <- rbind(twospp1, twospp2)
twospp$dep1 <- twospp$nfish1 / 2e5
twospp$dep2 <- twospp$nfish2 / 2e5

plot6 <- twospp %>% group_by(spp, comp_coeff, init_dist, for_plot, type, nsites, dep1, dep2) %>%
  summarize(median_cpue = median(cpue), sd_cpue = sd(cpue)) %>% as.data.frame

#Filter Data for each distribution
ls6 <- plot6 %>% filter(init_dist == 'leftskew')
n6 <- plot6 %>% filter(init_dist == 'normdist')
p6 <- plot6 %>% filter(init_dist == 'patchy')
u6 <- plot6 %>% filter(init_dist == 'uniform')

#----------------------------------------
#Plot fig 5 with fig 6 data
twospp <- rbind(twospp1, twospp2)
twospp$dep1 <- twospp$nfish1 / 2e5
twospp$dep2 <- twospp$nfish2 / 2e5

#Add in the 5/95 percentile and median cpue values
twospp <- twospp %>% group_by(spp, type, comp_coeff) %>% 
  mutate(q5 = quantile(cpue, .05), med_cpue = median(cpue),
    q95 = quantile(cpue, .95)) %>% as.data.frame

#For only 0.3 competition coefficient
temp <- twospp %>% filter(nfish2 == 60000, comp_coeff == .3)

spp1 <- temp %>% filter(spp == 'spp1')
spp2 <- temp %>% filter(spp == 'spp2')





#----------------------------------------
#Abstract numbers for patchy only
head(p6)
p6 %>% filter(type == 'pref', comp_coeff == 0.3, dep2 %in% c(.2, .7), dep1 == .9, spp == 'spp1') -> temp
range(temp$median_cpue)
range(subset(temp, spp == "spp1")$median_cpue)
#----------------------------------------
the_data <- rbind(p6, n6)

# add in 0, 0 for scenario in the data
zeroes <- the_data %>% select(spp, comp_coeff, init_dist, for_plot, type, nsites) %>% distinct
zeroes$dep1 <- 0
zeroes$dep2 <- 0
zeroes$median_cpue <- 0
zeroes$sd_cpue <- 0
  
zeroes <- zeroes[, names(the_data)]

the_data <- rbind(the_data, zeroes)

#Look at one of the matrices of median_cpue values to see how 
the_data %>% filter(spp == 'spp1', init_dist == 'normdist', type == 'pref',
  comp_coeff == 0.3) %>% select(dep1, dep2, median_cpue) %>% arrange(dep1, desc(dep2)) -> p1

#ggplot
#contours
# ggplot(the_data, aes(x = dep1, y = dep2, z = median_cpue)) + geom_contour() + 
#   facet_wrap(~ type + init_dist + spp + comp_coeff, ncol = 6)
# #tiles
# ggplot(the_data, aes(x = dep1, y = dep2, fill = median_cpue)) + geom_tile() + 
#   facet_wrap(~ type + init_dist + spp + comp_coeff, ncol = 6)

p1 <- p1 %>% order(dep1, dep2)
p1$tot <- p1$dep1 + p1$dep2
p1 <- p1 %>% arrange(tot, dep1, dep2)

# med_mat <- matrix(p1$median_cpue, nrow = 11, ncol = 11)
# med_mat <- rotate(med_mat)
# filled.contour2(x = seq(0, 100, 10), y = seq(0, 100, 10), z = med_mat)
# filled.contour2(x = seq(0, 1, 0.1), y = seq(0, 1, 0.1), z = med_mat)
inds <- rbind(p6, n6) %>% select(type, spp, comp_coeff, init_dist) %>% distinct() %>%
   arrange(init_dist, type, comp_coeff, spp)

#Rearrange so species 1, then species 2
inds <- inds %>% arrange(init_dist, type, spp, comp_coeff)
inds$ind <- 1:24

#Define function to rotate matrix
rotate <- function(x) t(apply(x, 2, rev))

#-----------------------------------------------------------------------------
#Normal, spp 1
the_data %>% filter(type == 'pref', init_dist == 'normdist', comp_coeff == 0.3) %>%
  filter(spp == 'spp1', dep2 == .1)

the_data %>% filter(type == 'pref', init_dist == 'normdist', median_cpue != 0) %>%
    group_by(spp, comp_coeff) %>% summarize(min_cpue = min(median_cpue),
    max_cpue = max(median_cpue), prop5 = length(which(median_cpue >= 0.5)) / length(median_cpue)) %>%
    arrange(comp_coeff)

#Ranges for patchy surveys
the_data %>% filter(init_dist == 'patchy', median_cpue != 0) %>%
   group_by(type, comp_coeff) %>% summarize(mincpue = min(median_cpue),
    maxcpue = max(median_cpue)) %>% arrange(comp_coeff, type)

#-----------------------------------------------------------------------------
#Values for abstract
the_data %>% filter(comp_coeff == .3, dep1 == .9, dep2 %in% c(.2, .7), spp == 'spp1',
  init_dist == 'patchy')


#-----------------------------------------------------------------------------
#Figure 6 - Two Species Contour Plots
#Starting at some level and going up and down
#-----------------------------------------------------------------------------

#####Figure out which things to compare
#Look at Patchy and Normal Distribution for differences
png(width = 11.29, height = 8.15, file = 'figs/hlfig6.png', units = 'in', res = 150)                   

matlay <- matrix(c( 1,  2,  3, 0,  4,  5, 6,
                    7,  8,  9, 0, 10, 11, 12,
                    0,  0,  0, 0,  0,  0,  0,
                   13, 14, 15, 0, 16, 17, 18,
                   19, 20, 21, 0, 22, 23, 24), ncol = 7, byrow = TRUE)

# whites <- c(2, 3, 4, 5, 8, 9, 10, 11, 14, 15, 16, 17)
whites <- 0
# blacks <- which(1:24 %in% whites == FALSE)
blacks <- 1:24
                   
layout(matlay, heights = c(1, 1, 0.2, 1, 1), widths = c(1, 1, 1, 0.1, 1, 1, 1))
par(mar = c(0.0, 0.5, 0, 0.3), oma = c(4, 4, 5.5, 2), mgp = c(.6, .5, 0))
fig6_letts <- paste0(letters[1:24], ")")

for(jj in 1:24){
  #------------------
  #Format the data
  temp <- inds[jj, ]

  #tp for temp plot 
  tp <- the_data %>% filter(type == temp$type, spp == temp$spp, comp_coeff == temp$comp_coeff,
    init_dist == temp$init_dist)

  # add_in <- tp[1, ]
  # add_in$dep1 <- 0
  # add_in$dep2 <- 0

  # tp <- rbind(tp, add_in)

  tp <- tp %>% arrange(dep1, dep2)  
  # tp$tot <- tp$dep1 + tp$dep2
  # tp <- tp %>% arrange(tot, dep1, dep2)
  mm <- matrix(tp$median_cpue, nrow = 11, ncol = 11)
  mm1 <- apply(mm, 2, rev)
  mm1 <- rotate(mm1)

  #------------------
  #plots
  mylevels <- seq(0, 1, .1)
  greys <- paste0('grey', seq(100, 0, -10))
  
  x <- seq(0, 100, by = 10)
  y <- seq(0, 100, by = 10)
  # mm <- rotate(mm)

  
  filled.contour2(x, y, mm1, levels = mylevels,  col = greys, ann = F, axes = F)
  box()
  if(jj %in% whites){
    contour(x, y, mm1, levels = mylevels, add = T, labcex = 1, col = 'white')
    # text(103, 103, fig6_letts[jj], cex = 1.3, col = 'white')
  } 
  if(jj %in% blacks){
    contour(x, y, mm1, levels = mylevels, add = T, labcex = 1, lwd = .5)
  } 
  
  #------------------
  #Add axes 
  if(jj %% 6 == 1){
    axis(side = 2, las = 2, at = c(0, 40, 80), labels = c(0, .4, .8), cex.axis = 1.4)
    # axis(side = 2, las = 2, at = c(0, 20, 40, 60, 80, 100), labels = c(0, .2, .4, .6, .8, 1), cex.axis = 1.2)
  } 

  # if(jj > 18) axis(side = 1, at = c(0, 20, 40, 60, 80, 100), labels = c(0, .2, .4, .6, .8, 1), cex.axis = 1.2)
    if(jj > 18) axis(side = 1, at = c(0, 40, 80), labels = c(0, .4, .8), cex.axis = 1.4)


  #Add Text
  # if(jj < 7 & jj %% 2 == 1) mtext(side = 3, "Species 1", adj = 0, line = .01)
  # if(jj < 7 & jj %% 2 == 0) mtext(side = 3, "Species 2", adj = 0, line = .05)
  if(jj %in% c(6, 18)) mtext(side = 4, "Size-based", line = .5)
  if(jj %in% c(12, 24)) mtext(side = 4, "Random", line = .5)
  if(jj == 1) mtext(side = 3, "Species 1", adj = 0, line = 1.3, cex = 1.2)
  if(jj == 4) mtext(side = 3, "Species 2", adj = 0, line = 1.3, cex = 1.2)

  if(jj %in% c(1, 4)) mtext(side = 3, comp_captions[1], adj = 0, cex = .95)
  if(jj %in% c(2, 5)) mtext(side = 3, comp_captions[2], adj = 0, cex = .95)
  if(jj %in% c(3, 6)) mtext(side = 3, comp_captions[3], adj = 0, cex = .95)
  # if(jj < 7) mtext(side = 3, paste0('comp. = ', unique(tp$comp_coeff)), adj = 0, cex = 1.05)

  #Add Letters
  #Things in white
  if(jj %in% whites){
    text(94, 94, fig6_letts[jj], cex = 1.3, col = 'white')
  } 
  if(jj %in% blacks){
    text(94, 94, fig6_letts[jj], cex = 1.3)
  } 
  
}
#------------------
#Add outside text
mtext(side = 1, "Species 1 relative abundance", outer = T, line = 2.3, cex = 1.4)
mtext(side = 2, "Species 2 relative abundance", outer = T, line = 2, cex = 1.4)
mtext(side = 3, "Symmetric", outer = T, line = 3, cex = 1.4, adj = .005)
mtext(side = 3, "Patchy", outer = T, line = -27.1, cex = 1.4, adj = .005)

#Do this as 8.5 x 7 inch png?
dev.off()