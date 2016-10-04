setwd("/Users/peterkuriyama/School/Research/hlsimulator")

library(devtools)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
#--------------------------------------------------------------------------------------------
#Options to load the package

#From github straight
install_github('peterkuriyama/hlsimulator')
library(hlsimulator)



#Locally
# load_all()

#-------------------------------------------------------------------------------------------
#work on conducting survey for number of years

#Check Number of fish
ctl <- make_ctl(nhooks = 15, seed = 200, nfish = 10000, nyear = 50)
out <- conduct_survey(ctl)
cpue <- calc_cpue(out, ctl = ctl)


plot(annual_catch$nfish, annual_catch$avg_cpue, type = 'l', xlab = )


melt(out$fished_areas$year0)

sapply(out$fished_areas, FUN = sum)





check1 <- sum(test1$fished_areas$year50) + sum(test1$samples[, 5:9])
check2 <- sum(test1$fished_areas$year0)



#


plot(sum(abs(hook_probs(nfish = 4, p0 = .4))))

xx <- melt(test1$samples, id.vars = c('year', 'vessel', 'x', 'y'))


xx %>% group_by(year) %>% summarize(avg_fish = mean(value))

test1$samples %>% group_by(year) %>% summarize


#-------------------------------------------------------------------------------------------
#See how p0 affects cpue
p0_vec <- seq(.1, 1, by = 0.1)
cpue_out <- vector('list', length = length(p0_vec))


for(ii in 1:length(p0_vec)){
  temp_ctl <- make_ctl(p0 = p0_vec[ii], nfish = 1000, seed = 500)
  temp_init <- initialize_population(ctl = temp_ctl)

  cpue_out[[ii]] <- fish_population(fish_area = temp_init, ctl = temp_ctl)$samples

}

names(cpue_out) <- p0_vec
cpue_out <- ldply(cpue_out)

#-------------------------------------------------------------------------------------------
#Things to work on
xx <- make_ctl(p0 = .2)

#Things for initialize population
control <- list(
  #arguments for initializing the fish population in a matrix
  numrow = 10,
  numcol = 10,
  nfish = 10000, #initial number of fish
  distribute = 'uniform', #distribution of fish, could be uniform, or patchy
  maxfish = 10,
  percent = .3, #percentage of area to sample, only necessary if distribute = 'patchy'
  seed = 300,

  #Arguments for function that fishes population
  location = data.frame(vessel = c(1, 1, 2),
                        x = c(3, 3, 8),
                        y = c(3, 5, 8)),
  scope = 2,
  nhooks = 5,
  ndrops = 3, 
  process = 'equal_prob',
  p0 = .4 #probability that fish are attracted to gear
)     

xx <- initialize_population(ctl = control)
fish_population(xx, ctl = control)

conduct_survey(xx, ctl = control)



#-------------------------------------------------------------------------------------------
#Check that conduct_survey works

init <- initialize_population(numrow = 10, numcol = 10, nfish = 10000, distribute = 'uniform',
                                percent = .3, seed = 301)



data.frame(vessel = c(1, 1, 2), x = c(3, 3, 8), y = c(3, 5, 8))

xx <- conduct_survey(fish_area = init, location = data.frame(vessel = c(1, 1, 2), x = c(3, 3, 8), 
  y = c(3, 5, 8)), scope = 1, nhooks = 15, ndrops = 5, process = 'equal_prob')

#Make sure that process is included in conduct_survey example. 


xx <- conduct_survey(fish_area = tt, location_list = list)
#--------------------------------------------------------------------------------------------
##TO DO

##Put in no movement function
## Depends on how much of population we sample
##Recruitment functions?


#Things that might affect cpue relationship
#number of locations sampled
#distribution of fish
#number of fish
#percentage of distributed fish (if patchy distribution)





#-------------------------------------------------------------------------------------------
#NO recruitment and strong local depletion effects with year after year sampling

#-------------------------------------------------------------------------------------------
##Compare the effect of fish movement on fixed location sampling
#Specify 10 locations
location_list1 <- list(c(2, 2), 
                       c(3, 1),
                       c(3, 4),
                       c(3, 7),
                       c(4, 9),
                       c(2, 6),
                       c(7, 1),
                       c(9, 3),
                       c(8, 5),
                       c(8, 7),
                       c(9, 10),
                       c(10, 6)
                       )

pdf(width = 13, height = 5.65, 
  file = 'figs/fixed_location_different_movement.pdf')
par(mfcol = c(1, 3), oma = c(2, 2, 0, 0))

#no fish movement and uniform initial distribution
pp1 <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 5000,
                   distribute = 'uniform',
                   seed = 302,
                   nyears = 15, 
                   random_locations = FALSE,
                   location_list = location_list1, 
                   nlocs = 12, 
                   move_func = 'move_fish_none', 
                   nhooks = 15, ndrops = 3, scope = 0, print_text = TRUE)

#clockwise fish movement
pp2 <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 5000,
                   distribute = 'uniform',
                   seed = 302,
                   nyears = 15, 
                   random_locations = FALSE,
                   location_list = location_list1, 
                   nlocs = 12, 
                   move_func = 'move_fish_cw', move_prob = .7,
                   nhooks = 15, ndrops = 3, scope = 0, print_text = TRUE)

#left fish movement
pp3 <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 5000,
                   distribute = 'uniform',
                   seed = 302,
                   nyears = 15, 
                   random_locations = FALSE,
                   location_list = location_list1, 
                   nlocs = 12, 
                   move_func = 'move_fish_left', max_prob = .7, min_prob = 0,
                   nhooks = 15, ndrops = 3, scope = 0, print_text = TRUE)
dev.off()


#-------------------------------------------------------------------------------------------
#random locations and various movement patterns

pdf(width = 13, height = 5.65, 
  file = 'figs/random_location_different_movement.pdf')

par(mfcol = c(1, 3), oma = c(2, 2, 0, 0))

#no fish movement and uniform initial distribution
pp1 <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 5000,
                   distribute = 'uniform',
                   seed = 302,
                   nyears = 15, 
                   random_locations = TRUE,
                   location_list = location_list1, 
                   nlocs = 12, 
                   move_func = 'move_fish_none', 
                   nhooks = 15, ndrops = 3, scope = 0, print_text = TRUE)

#clockwise fish movement
pp2 <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 5000,
                   distribute = 'uniform',
                   seed = 302,
                   nyears = 15, 
                   random_locations = TRUE,
                   location_list = location_list1, 
                   nlocs = 12, 
                   move_func = 'move_fish_cw', move_prob = .7,
                   nhooks = 15, ndrops = 3, scope = 0, print_text = TRUE)

#left fish movement
pp3 <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 5000,
                   distribute = 'uniform',
                   seed = 302,
                   nyears = 15, 
                   random_locations = TRUE,
                   location_list = location_list1, 
                   nlocs = 12, 
                   move_func = 'move_fish_left', max_prob = .7, min_prob = 0,
                   nhooks = 15, ndrops = 3, scope = 0, print_text = TRUE)
dev.off()

#-------------------------------------------------------------------------------------------
#Fixed locations and moving fish
pdf(width = 13, height = 5.65, 
  file = 'figs/fixed_location_clockwise_movement_different_distributions.pdf')
par(mfcol = c(1, 3), oma = c(2, 2, 0, 0))

#no fish movement and uniform initial distribution
pp1 <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 5000,
                   distribute = 'uniform',
                   seed = 302,
                   nyears = 15, 
                   random_locations = FALSE,
                   location_list = location_list1, 
                   nlocs = 12, 
                   move_func = 'move_fish_cw', move_prob = .7,
                   nhooks = 15, ndrops = 3, scope = 0, print_text = TRUE)

#clockwise fish movement
pp2 <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 5000,
                   distribute = 'patchy',
                   seed = 302,
                   nyears = 15, 
                   random_locations = FALSE,
                   location_list = location_list1, 
                   nlocs = 12, 
                   move_func = 'move_fish_cw', move_prob = .7,
                   nhooks = 15, ndrops = 3, scope = 0, print_text = TRUE)

#left fish movement
pp3 <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 5000,
                   distribute = 'area',
                   area = 'upperright',
                   seed = 302,
                   nyears = 15, 
                   random_locations = FALSE,
                   location_list = location_list1, 
                   nlocs = 12, 
                   move_func = 'move_fish_cw', move_prob = .7, 
                   nhooks = 15, ndrops = 3, scope = 0, print_text = TRUE)
dev.off()


#-------------------------------------------------------------------------------------------
#Random locations and moving fish
pdf(width = 13, height = 5.65, 
  file = 'figs/random_location_clockwise_movement_different_distributions.pdf')
par(mfcol = c(1, 3), oma = c(2, 2, 0, 0))

#no fish movement and uniform initial distribution
pp1 <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 5000,
                   distribute = 'uniform',
                   seed = 302,
                   nyears = 15, 
                   random_locations = TRUE,
                   location_list = location_list1, 
                   nlocs = 12, 
                   move_func = 'move_fish_cw', move_prob = .7,
                   nhooks = 15, ndrops = 3, scope = 0, print_text = TRUE)

#clockwise fish movement
pp2 <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 5000,
                   distribute = 'patchy',
                   seed = 302,
                   nyears = 15, 
                   random_locations = TRUE,
                   location_list = location_list1, 
                   nlocs = 12, 
                   move_func = 'move_fish_cw', move_prob = .7,
                   nhooks = 15, ndrops = 3, scope = 0, print_text = TRUE)

#left fish movement
pp3 <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 5000,
                   distribute = 'area',
                   area = 'upperright',
                   seed = 302,
                   nyears = 15, 
                   random_locations = TRUE,
                   location_list = location_list1, 
                   nlocs = 12, 
                   move_func = 'move_fish_cw', move_prob = .7, 
                   nhooks = 15, ndrops = 3, scope = 0, print_text = TRUE)
dev.off()


#-------------------------------------------------------------------------------------------
par(mfcol = c(1, 3))
pp1 <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 1000,
                   distribute = 'uniform',
                   area = 'upperleft', 
                   seed = 302,
                   nyears = 30, 
                   random_locations = TRUE,
                   location_list = list(c(2, 2), c(8, 8)), 
                   nlocs = 10, 
                   move_func = 'move_fish_none', 
                   nhooks = 15, ndrops = 3, scope = 0, print_text = TRUE,
                   xlim_s = c(0, 10000))

pp2 <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 5000,
                   distribute = 'uniform',
                   area = 'upperleft', 
                   seed = 302, 
                   nyears = 30, 
                   random_locations = TRUE,
                   location_list = list(c(2, 2), c(8, 8)), 
                   nlocs = 10, 
                   move_func = 'move_fish_none', 
                   nhooks = 15, ndrops = 3, scope = 0, 
                   xlim_s = c(0, 10000))

pp3 <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 10000,
                   distribute = 'uniform',
                   area = 'upperleft', 
                   seed = 302, 
                   nyears = 30, 
                   random_locations = TRUE,
                   location_list = list(c(2, 2), c(8, 8)), 
                   nlocs = 10, 
                   move_func = 'move_fish_none', 
                   nhooks = 15, ndrops = 3, scope = 0)







#high level wrapper to run and plot
pp <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 5000,
                   distribute = 'area',
                   area = 'upperleft', 
                   seed = 302, 
                   nyears = 15, 
                   random_locations = FALSE,
                   location_list = list(c(2, 2), c(8, 8)), 
                   nlocs = 50, 
                   move_func = 'move_fish_none', 
                   nhooks = 15, ndrops = 3, scope = 0)

pp <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 1000,
                   distribute = 'uniform', 
                   seed = 302, 
                   nyears = 15, 
                   random_locations = TRUE,
                   # location_list = list(c(2, 2), c(8, 8)), 
                   nlocs = 10, 
                   move_func = 'move_fish_cw', 
                   move_prob = .8, 
                   nhooks = 15, ndrops = 3, scope = 0)




xx301[[4]] == xx300.2[[4]]


xx300.1$end_nfish == xx300.2$end_nfish
xx301$end_nfish == xx300.2$end_nfish

run_and_plot(numrow = 10, numcol = 10, nfish = 10000,
  distribute = 'uniform', seed = 300, nyears = 15, random_locations = TRUE, 
  nlocs = 10, move_func = 'move_fish_left', max_prob = .2, min_prob = .01)















#-------------------------------------------------------------------------------------------
#develop function to move fish around ontogenetically or 
xx <- survey_over_years(numrow = 10, numcol = 10, nfish = 100000, 
  distribute = 'uniform',
  seed = 300, nyears = 15, location_list, 
  random_locations = TRUE, nlocs = 100, move_func = move_fish_cw, move_prob = .8)



thing <- parse_master_list(xx)

nfish_cpue <- merge(thing$end_nfish %>% group_by(year) %>% summarise(nfish = sum(value)),
      cpue %>% group_by(year) %>% summarise(cpue = mean(value)) %>% as.data.frame,
      by = 'year')


plot(nfish_cpue$nfish, nfish_cpue$cpue, ylim = c(0, 1), pch = 19,
  xaxs = 'i', yaxs = 'i', xlim = c(0, max(pretty(nfish_cpue$nfish))))


ggplot(cpue, aes(x = variable, y = value, colour = year, group = year)) + geom_line() + 
  facet_wrap(~ location) + scale_color_gradientn(colors = gray.colors(length(cpue$year)))








#separate elements from each iteration
#everything a list for now
fish_list <- lapply(xx, FUN = function(x) x$sampled_area)
fish_melt <- melt(fish_list)
names(fish_melt) <- c('row', 'column', 'nfish', 'year')
fish_melt$density <- fish_melt$nfish / 100


ggplot(fish_melt, aes(x = row, y = column)) + geom_raster(aes(fill = density)) + 
  facet_wrap(~ year)

ggplot(fish_melt)

melt(fish_list)
melt(fish_list[[1]])

ggplot(fish_list[[1]] aes(x))

cpue_list <- lapply(xx, FUN = function(x) x$cpue)

#extract number of fish in each year
  nfish <- sapply(xx, FUN = function(x) {
    sum(unlist(x$sampled_area))
  })
  cpue <- sapply(xx, FUN = function(x) {
    mean(unlist(x$cpue[, 2:4]))
  })

plot(nfish, cpue, pch = 19, type = 'o')

to.plot <- lapply(xx, FUN = function(x) x$cpue)
sapply(to.plot, FUN = function(x) mean(unlist(x[, 2:4])))


survey_over_years(nfish = 10000, distribute = 'uniform', seed = 300, nyears = 15,
  random_locations = TRUE, nlocs = 10)


#-------------------------------------------------------------------------------------------
#Look at number of locations

cpues <- explore_nlocs_cpue(numrow = 10, numcol = 10, nfish = 2000, seed = 200, 
  numlocs = 10, distribute = 'patchy',
  percent = .5, scope = 0)

to.plot <- melt(cpues)
names(to.plot)[1] <- 'nlocs'

xx <- sapply(to.plot$location, FUN = function(x) eval(parse(text = x)))

to.plot$x <- as.vector(xx[1, ])
to.plot$y <- as.vector(xx[2, ])


#-------------------------------------------------------------------------------------------
#Look at different numbers of fish
nfish.vec <- seq(100, 10000, by = 100)
avg.cpue <- nfish.vec

for(ii in 1:length(nfish.vec)){
  init <- initialize_population(numrow = 10, numcol = 10, nfish = nfish.vec[ii], distribute = 'uniform',
                                percent = .3, seed = 301)

  temp <- conduct_survey(fish_area = init, location_list = list(c(4, 10),
                                                        c(8, 2),
                                                        c(3, 3)), scope = 1, nhooks = 15, ndrops = 5)
  avg.cpue[ii] <- mean(unlist(temp$cpue))

}


plot(nfish.vec, avg.cpue, type = 'o', pch = 19, ylim = c(0, 1), xaxs = 'i',
     yaxs = 'i', xlim = c(0, max(nfish.vec)))

png(file = '/Users/peterkuriyama/Desktop/hl_cpue_check.png')
plot(nfish, avg.cpue, type = 'o', pch = 19, yli = c(0, 1), xaxs = 'i',
     yaxs = 'i', xlim = c(0, max(nfish)))
dev.off()


conduct_survey(fish_area = init, location_list = list(c(4, 10),
                                                      c(8, 2),
                                                      c(3, 3)),
               scope = 1, nhooks = 15, ndrops = 5)



#------------------------------------------------------
#------------------------------------------------------
#Scraps
# fish.range * move.prob
# #Define movement probabilities
# 1 - (fish.range / nfish.range)


# #Each fish has a probability of catching a hook
# #Probability depends on nuber


# #conusmption by one fish
# check_cons <- function(max.prob = .9, nhooks){
#   zz <- (max.prob * nhooks) / (.6 + nhooks)
#   return(zz)
# }


# tt <- data.frame(hook = 1:75, prob = sapply(1:75,
#   FUN = function(x) check_cons(nhooks = x)))
# tt$prob.many <- 1 - exp(-(tt$prob * 50))

# plot(tt$hook, tt$prob.many, pch = 19, ylim = c(0, 1),
#   main = '70 fish and 1:75 hooks')




# #Highest probability of hook saturation:
#  #Five fish in location and more than five fish in nfish.range
# if(fish.loc >= nhooks & (nfish.range - fish.loc) >= nhooks){
#   hook.prob <- seq(from = max.prob, to = 0, by = -delta.prob)
# }

# #High probability of hook saturation:
# #if more than five fish in location and fewer than nhooks fish surrounding
# if(fish.loc >= nhooks & (nfish.range - fish.loc) < nhooks){
#   hook.prob <- seq(from = max.prob - (2 * delta.prob), to = 0, by = -delta.prob)
# }

# #Medium prob of hook saturation
# #fewer than five fish in specific location, more than five surrounding
# if(fish.loc < nhooks & (nfish.range - fish.loc) >= nhooks){
#  hook.prob <- seq(from = max.prob - (4 * delta.prob), to = 0, by = -delta.prob)
# }

# #Low prob of hook saturation
# #fewer than five in speicifc location, fewer than five surrounding
# if(fish.loc < nhooks & (nfish.range - fish.loc) < nhooks & nfish.range >= nhooks){
#   hook.prob <- seq(from = max.prob - (6 * delta.prob), to = 0, by = -delta.prob)
# }

# #Lowest prob of hook saturation
# #fewer than nhooks fish in entire range
# if(fish.loc < nhooks & (nfish.range - fish.loc) < nhooks & nfish.range < nhooks){
#   hook.prob <- seq(from = max.prob - (8 * delta.prob), to = 0, by = -delta.prob)
# }

# #Subset hook probabilites based on number of hooks
# hook.prob <- hook.prob[1:nhooks]

# #Sample fish
# fish <- rbinom(n = nhooks, size = 1, prob = hook.prob)

# #Subtract sampled fish from number of fish in fish.range matrix
# xx <- melt(fish.range)
# xx[which(xx$value != 0), 'prob'] <- 1 / sum(xx$value != 0) #equal probabilities for now
# # xx$prob <- xx$value / sum(xx$value)

# # use a multinomial sample to find which cells to subtract sampled fish from
# fish.caught <- rmultinom(prob = xx$prob, n = 1, size = sum(fish))
# xx$value <- xx$value - fish.caught

# #update whole matrix
# return(xx)

# #Keep
# # while(sum(fish) > nfish.range){
# #   fish <- rbinom(n = nhooks, size = 1, prob = hook.prob)
# # }

#-------------------------------------------------------------------------------------------
#Move fish cw
# fish_area1 <- initialize_population(distribute = 'area', area = 'upperright', numrow = 10, numcol = 10,
#   nfish = 1000)

# tt <- fish_area1
# ttp <- vector('list', length = 8)
# for(ll in 1:8){
#   temp <- move_fish_cw(fish_area = tt, move_prob = .8)
#   tt <- temp$final
#   ttp[[ll]] <- tt
# }

# ttp <- melt(ttp)

# ggplot(ttp, aes(x = Var1, y = Var2)) + geom_raster(aes(fill = value)) + 
#   theme_bw() + facet_wrap(~ L1)

# ggplot(ttp[1], )

# move_fish_cw(fish_area = fish_area1, move_prob = .8)





# #Update numbers of fish based on catches
# #Record catches
# prob.catch <- fish.loc


# #check this is working
# #------------------------------------------------------------------------------------------
# # check.this <- vector('list', length = 1000000)
# # for(ii in 1:length(check.this)){
# #   check.this[[ii]] <- rbinom(n = nhooks, size = 1, prob = hook.prob)
# # }

# # ct <- ldply(check.this)
# # colSums(ct) / length(check.this)


# #------------------------------------------------------------------------------------------





# fishArea, location, prob.max, prob.delta
