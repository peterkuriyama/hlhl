

#----------------------------------------
#Figure 6

plot6 <- twospp %>% group_by(spp, comp_coeff, init_dist, for_plot, type, nsites, dep1, dep2) %>%
  summarize(median_cpue = median(cpue), sd_cpue = sd(cpue)) %>% as.data.frame

#Filter Data for each distribution
ls6 <- plot6 %>% filter(init_dist == 'leftskew')
n6 <- plot6 %>% filter(init_dist == 'normdist')
p6 <- plot6 %>% filter(init_dist == 'patchy')
u6 <- plot6 %>% filter(init_dist == 'uniform')

the_data <- rbind(p6, n6)

inds <- rbind(p6, n6) %>% select(type, spp, comp_coeff, init_dist) %>% distinct() %>%
   arrange(init_dist, type, comp_coeff, spp)

inds$ind <- 1:24

#Rearrange so species 1, then species 2
inds <- inds %>% arrange(init_dist, type, spp, comp_coeff)
inds$ind <- 1:24

#Define function to rotate matrix
rotate <- function(x) t(apply(x, 2, rev))

#Normal, spp 1
the_data %>% filter(type == 'pref', init_dist == 'normdist', comp_coeff == 0.3) %>%
  filter(spp == 'spp1', dep2 == .1)

the_data %>% filter(type == 'pref', init_dist == 'normdist', median_cpue != 0) %>%
    group_by(spp, comp_coeff) %>% summarize(min_cpue = min(median_cpue),
    max_cpue = max(median_cpue), prop5 = length(which(median_cpue >= 0.5)) / length(median_cpue)) %>%
    arrange(comp_coeff)

#Ranges for patchy surveys
the_data %>% filter(init_dist == 'patchy', median_cpue != 0, comp_coeff == 0.5) %>%
   group_by(spp, type, comp_coeff) %>% summarize(mincpue = min(median_cpue),
    maxcpue = max(median_cpue)) %>% arrange(comp_coeff)


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
                   
layout(matlay, heights = c(1, 1, 0.2, 1, 1), widths = c(1, 1, 1, 0.1, 1, 1, 1))
par(mar = c(0.0, 0.5, 0.9, 0.3), oma = c(4, 4, 5, 2), mgp = c(.6, .5, 0))
fig6_letts <- paste0(letters[1:24], ")")

for(jj in 1:24){
  #------------------
  #Format the data
  temp <- inds[jj, ]

  #tp for temp plot 
  tp <- rbind(p6, n6) %>% filter(type == temp$type, spp == temp$spp, comp_coeff == temp$comp_coeff,
    init_dist == temp$init_dist)

  #------------------
  #Create the matrix of median_cpue values
  #Dep1 is the columns, y
  ind1 <- data.frame(dep1 = unique(tp$dep1), col_dep1 = 1:11)
  # ind1 <- data.frame(dep1 = unique(tp$dep1), col_dep1 = 11:1)
  tp <- inner_join(tp, ind1, by = 'dep1')
  
  #Dep2 is the rows, x
  # ind2 <- data.frame(dep2 = unique(tp$dep2), row_dep2 = 1:11)
  ind2 <- data.frame(dep2 = unique(tp$dep2), row_dep2 = 11:1)
  tp <- inner_join(tp, ind2, by = 'dep2')

  #Fill in the Matrix
  mm <- matrix(NA, nrow = 11, ncol = 11)
  for(ii in 1:nrow(tp)){
    mm[tp[ii, 'row_dep2'], tp[ii, 'col_dep1']] <- tp[ii, 'median_cpue']  
    # mm[tp[ii, 'col_dep1'], tp[ii, 'row_dep2']] <- tp[ii, 'median_cpue']  
  }

  #------------------
  #plots
  mylevels <- seq(0, 1, .1)
  greys <- paste0('grey', seq(100, 0, -10))
  
  x <- 10 * (1:11)
  y <- 10 *(1:11)
  mm <- rotate(mm)
  
  filled.contour2(x, y, mm, levels = mylevels,  col = greys, ann = F, axes = F)
  box()
  if(jj %in% c(2, 3, 4, 5, 8, 9, 10, 11, 14, 15, 16, 17)){
    contour(x, y, mm, levels = mylevels, add = T, labcex = 1, col = 'white')
    # text(103, 103, fig6_letts[jj], cex = 1.3, col = 'white')
  } 
  if(jj %in% c(2, 3, 4, 5, 8, 9, 10, 11, 14, 15, 16, 17) == FALSE){
    contour(x, y, mm, levels = mylevels, add = T, labcex = 1)
  } 
  

  #------------------
  #Add axes 
  if(jj %% 6 == 1){
    axis(side = 2, las = 2, at = c(10, 30, 50, 70, 90, 110), labels = c(0, .2, .4, .6, .8, 1), cex.axis = 1.2)
  } 

  if(jj > 18) axis(side = 1, at = c(10, 30, 50, 70, 90, 110), labels = c(0, .2, .4, .6, .8, 1), cex.axis = 1.2)

  #Add Text
  # if(jj < 7 & jj %% 2 == 1) mtext(side = 3, "Species 1", adj = 0, line = .01)
  # if(jj < 7 & jj %% 2 == 0) mtext(side = 3, "Species 2", adj = 0, line = .05)
  if(jj %in% c(6, 18)) mtext(side = 4, "Preferential", line = .5)
  if(jj %in% c(12, 24)) mtext(side = 4, "Random", line = .5)
  if(jj == 1) mtext(side = 3, "Species 1", adj = 0, line = 1.5, cex = 1.05)
  if(jj == 4) mtext(side = 3, "Species 2", adj = 0, line = 1.5, cex = 1.05)

  if(jj < 7) mtext(side = 3, paste0('comp. = ', unique(tp$comp_coeff)), adj = 0, cex = 1.05)


  #Add Letters
  if(jj %in% c(2, 3, 4, 5, 8, 9, 10, 11, 14, 15, 16, 17)){
    text(103, 103, fig6_letts[jj], cex = 1.3, col = 'white')
  } 
  if(jj %in% c(2, 3, 4, 5, 8, 9, 10, 11, 14, 15, 16, 17) == FALSE){
    text(103, 103, fig6_letts[jj], cex = 1.3)
  } 
  
}
#------------------
#Add outside text
mtext(side = 1, "Species 1 relative abundance", outer = T, line = 2.3, cex = 1.4)
mtext(side = 2, "Species 2 relative abundance", outer = T, line = 2, cex = 1.4)
mtext(side = 3, "Symmetric", outer = T, line = 2.7, cex = 1.4, adj = .005)
mtext(side = 3, "Patchy", outer = T, line = -27.5, cex = 1.4, adj = .005)

#Do this as 8.5 x 7 inch png?
dev.off()