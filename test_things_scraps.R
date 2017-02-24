#--------------------------------------------------------------------------------------------
####Plot 2

#What effect does fish passing through certain parts is
#Write this as a for loop
{
  y_vals <- 1:10
  y_outs <- vector('list', length = length(y_vals))
  
  #Run this thing in for loop
  for(yy in y_vals){
    print(yy)
    locs <- expand.grid(1:10, 1:10)
    locs$vessel <- 1
    names(locs)[1:2] <- c('x', 'y')
  
    locs <- locs[, c('vessel', 'x', 'y')]
    locs %>% filter(y == yy & x %in% c(1, 5, 7, 9)) -> locs
  
    ctl <- make_ctl(distribute = 'uniform', mortality = .1, move_out_prob = .5,
    nfish1 = 10000, nfish2 = 10000, prob1 = .01, prob2 = .99, nyear = 15, scope = 1, seed = 4,
    location = locs, movement_function = move_fish_left)  
    out <- conduct_survey(ctl = ctl)
    inp <- format_plot_input(out = out)
    y_outs[[yy]] <- inp
  }

  names(y_outs) <- as.character(y_vals)
  y_outs <- ldply(y_outs)
  names(y_outs)[1] <- 'y_value'

  for_plot <- y_outs %>% group_by(y_value, year, variable) %>% summarize(cpue = mean(value),
    nfish = unique(nfish)) %>% as.data.frame
  for_plot$y_value <- as.numeric(for_plot$y_value)

  png(width = 7, height = 7, units = 'in', res = 200, file = 'figs/plot2_fish_move_left_uniform.png')
  ggplot(for_plot) + geom_line(aes(x = nfish, y = cpue, group = y_value, colour = y_value), 
    size = 1) + facet_wrap(~ variable) + theme_bw()
  dev.off()
}

#--------------------------------------------------------------------------------------------
####Plot 3

#Look at effect of nfish and probabilities
{
  nfish1s <- seq(1000, 10000, by = 1000)

#Uniform Distribution
  nfish1s_outs <- vector('list', length = 10)

  for(nnn in 1:length(nfish1s)){
    #Only do fish 1 with .5 and .5 probabilities
    ctl <- make_ctl(distribute = 'uniform', mortality = .1, move_out_prob = .5,
      nfish1 = nfish1s[nnn], nfish2 = 10000, prob1 = .5, prob2 = .5, nyear = 15, scope = 1, seed = 4,
      location = def_locs, movement_function = move_fish_left)  
    out <- conduct_survey(ctl = ctl)
    inp <- format_plot_input(out = out)  
    nfish1s_outs[[nnn]] <- inp
    print(nnn)
  }
  
  names(nfish1s_outs) <- as.character(nfish1s)
  nfish1s_outs <- ldply(nfish1s_outs)
  names(nfish1s_outs)[1] <- 'nfish1'

  for_plot <- nfish1s_outs %>% group_by(nfish1, year, variable) %>% summarize(cpue = mean(value),
    nfish = unique(nfish)) %>% as.data.frame

  for_plot$nfish1 <- as.numeric(for_plot$nfish1)
  png(width = 11.5, height = 9.29, units = 'in', res = 200, file = 'figs/plot3_nfish1_uniform.png')
  ggplot(for_plot) + geom_line(aes(x = nfish, y = cpue, group = variable, colour = variable), 
    size = 1) + facet_wrap(~ nfish1) + theme_bw()
  dev.off()

#Patchy Distribution
  nfish1s_outs <- vector('list', length = 10)

  for(nnn in 1:length(nfish1s)){
    #Only do fish 1 with .5 and .5 probabilities
    ctl <- make_ctl(distribute = 'patchy', mortality = .1, move_out_prob = .5,
      nfish1 = nfish1s[nnn], nfish2 = 10000, prob1 = .5, prob2 = .5, nyear = 15, scope = 1, seed = 4,
      location = def_locs, movement_function = move_fish_left)  
    out <- conduct_survey(ctl = ctl)
    inp <- format_plot_input(out = out)  
    nfish1s_outs[[nnn]] <- inp
    print(nnn)
  }
  
  names(nfish1s_outs) <- as.character(nfish1s)
  nfish1s_outs <- ldply(nfish1s_outs)
  names(nfish1s_outs)[1] <- 'nfish1'

  for_plot <- nfish1s_outs %>% group_by(nfish1, year, variable) %>% summarize(cpue = mean(value),
    nfish = unique(nfish)) %>% as.data.frame
  for_plot$nfish1 <- as.numeric(for_plot$nfish1)
  
  png(width = 11.5, height = 9.29, units = 'in', res = 200, file = 'figs/plot3_nfish1_patchy.png')
  ggplot(for_plot) + geom_line(aes(x = nfish, y = cpue, group = variable, colour = variable), 
    size = 1) + facet_wrap(~ nfish1) + theme_bw()
  dev.off()
}


#What effect does fish movement left
#Write this as a for loop
{
  y_vals <- 1:10
  y_outs <- vector('list', length = length(y_vals))
  
  #Run this thing in for loop
  for(yy in y_vals){
    print(yy)
    locs <- expand.grid(1:10, 1:10)
    locs$vessel <- 1
    names(locs)[1:2] <- c('x', 'y')
  
    locs <- locs[, c('vessel', 'x', 'y')]
    locs %>% filter(y == yy & x %in% c(1, 5, 7, 9)) -> locs
  
    ctl <- make_ctl(distribute = 'uniform', mortality = .1, move_out_prob = .5,
      nfish1 = 10000, nfish2 = 10000, prob1 = .01, prob2 = .99, nyear = 15, scope = 1, seed = 4,
      location = locs, movement_function = move_fish_left)  
    out <- conduct_survey(ctl = ctl)
    inp <- format_plot_input(out = out)
    y_outs[[yy]] <- inp
  }


  names(y_outs) <- as.character(y_vals)
  y_outs <- ldply(y_outs)
  names(y_outs)[1] <- 'y_value'

  

  for_plot <- y_outs %>% group_by(y_value, year, variable) %>% summarize(cpue = mean(value),
    nfish = unique(nfish)) %>% as.data.frame
  for_plot$y_value <- as.numeric(for_plot$y_value)


  png(width = 7, height = 7, units = 'in', res = 200, file = 'figs/fish_move_left.png')
  ggplot(for_plot) + geom_line(aes(x = nfish, y = cpue, group = y_value, colour = y_value), 
    size = 1) + facet_wrap(~ variable) + theme_bw()
  dev.off()
}
#--------------------------------------------------------------------------------------------
####Plot 4

#Test Aggressive fish for one species,
#Both species should have the same response
{

#Uniform Distribution
  p1s <- seq(.1, 1, by = .1)
  p_outs <- vector('list', length = length(p1s))
  
  #Figure out the location configurations, use default locations
  for(pp in 1:length(p1s)){
    print(pp)
    ctl <- make_ctl(distribute = 'uniform', mortality = .1, move_out_prob = .5,
      nfish1 = 10000, nfish2 = 0, prob1 = p1s[pp], prob2 = 0, nyear = 15, scope = 1, seed = 4,
      location = def_locs, movement_function = move_fish_none)  
    out <- conduct_survey(ctl = ctl)
    inp <- format_plot_input(out = out)
  
    p_outs[[pp]] <- inp
  }


  names(p_outs) <- as.character(p1s)
  p_outs <- ldply(p_outs)
  names(p_outs)[1] <- 'probs'

  p_plot_10000 <- p_outs %>% group_by(probs, year, variable) %>% summarize(cpue = mean(value),
    nfish = unique(nfish)) %>% as.data.frame
  p_plot_10000$probs <- as.numeric(p_plot_10000$probs)

  # png(width = 7, height = 7, units = 'in', res = 200, 
  #   file = 'figs/plot4_10000fish_uniform.png')
  p_plot_10000 %>% filter(variable == 'cpue1') %>% ggplot() + 
    geom_line(aes(x = nfish, y = cpue, group = probs, colour = probs), 
    size = 1.5) + facet_wrap(~ variable) + theme_bw() +
    xlim(0, 1000)
  # dev.off()

#Patchy Distribution
  p1s <- seq(.1, 1, by = .1)
  p_outs <- vector('list', length = length(p1s))
  
  #Figure out the location configurations, use default locations
  for(pp in 1:length(p1s)){
    print(pp)
    ctl <- make_ctl(distribute = 'patchy', mortality = .1, move_out_prob = .5,
      nfish1 = 10000, nfish2 = 0, prob1 = p1s[pp], prob2 = 0, nyear = 15, scope = 1, seed = 4,
      location = def_locs, movement_function = move_fish_none)  
    out <- conduct_survey(ctl = ctl)
    inp <- format_plot_input(out = out)
  
    p_outs[[pp]] <- inp
  }

  names(p_outs) <- as.character(p1s)
  p_outs <- ldply(p_outs)
  names(p_outs)[1] <- 'probs'

  p_plot_10000 <- p_outs %>% group_by(probs, year, variable) %>% summarize(cpue = mean(value),
    nfish = unique(nfish)) %>% as.data.frame
  p_plot_10000$probs <- as.numeric(p_plot_10000$probs)

  png(width = 7, height = 7, units = 'in', res = 200, 
    file = 'figs/plot4_10000fish_patchy.png')
  p_plot_10000 %>% filter(variable == 'cpue1') %>% ggplot() + 
    geom_line(aes(x = nfish, y = cpue, group = probs, colour = probs), 
    size = 1.5) + facet_wrap(~ variable) + theme_bw()
  dev.off()


#These are for 5000 fish initially, 50 in each cell
#Uniform
  p1s <- seq(.1, 1, by = .1)
  p_outs <- vector('list', length = length(p1s))
  
  #Figure out the location configurations, use default locations
  for(pp in 1:length(p1s)){
    print(pp)
    ctl <- make_ctl(distribute = 'uniform', mortality = .1, move_out_prob = .5,
      nfish1 = 5000, nfish2 = 0, prob1 = p1s[pp], prob2 = 0, nyear = 15, scope = 1, seed = 4,
      location = def_locs, movement_function = move_fish_none)  
    out <- conduct_survey(ctl = ctl)
    inp <- format_plot_input(out = out)
  
    p_outs[[pp]] <- inp
  }

  names(p_outs) <- as.character(p1s)
  p_outs <- ldply(p_outs)
  names(p_outs)[1] <- 'probs'

  p_plot_5000 <- p_outs %>% group_by(probs, year, variable) %>% summarize(cpue = mean(value),
    nfish = unique(nfish)) %>% as.data.frame
  p_plot_5000$probs <- as.numeric(p_plot_5000$probs)

  png(width = 7, height = 7, units = 'in', res = 200, 
    file = 'figs/plot4_5000fish_uniform.png')
  ggplot(p_plot_5000) + geom_line(aes(x = nfish, y = cpue, group = probs, colour = probs), 
    size = 1.5) + facet_wrap(~ variable) + theme_bw()
  dev.off()

#Patchy
  p1s <- seq(.1, 1, by = .1)
  p_outs <- vector('list', length = length(p1s))
  
  #Figure out the location configurations, use default locations
  for(pp in 1:length(p1s)){
    print(pp)
    ctl <- make_ctl(distribute = 'patchy', mortality = .1, move_out_prob = .5,
      nfish1 = 5000, nfish2 = 0, prob1 = p1s[pp], prob2 = 0, nyear = 15, scope = 1, seed = 4,
      location = def_locs, movement_function = move_fish_none)  
    out <- conduct_survey(ctl = ctl)
    inp <- format_plot_input(out = out)
  
    p_outs[[pp]] <- inp
  }

  names(p_outs) <- as.character(p1s)
  p_outs <- ldply(p_outs)
  names(p_outs)[1] <- 'probs'

  p_plot_5000 <- p_outs %>% group_by(probs, year, variable) %>% summarize(cpue = mean(value),
    nfish = unique(nfish)) %>% as.data.frame
  p_plot_5000$probs <- as.numeric(p_plot_5000$probs)

  png(width = 7, height = 7, units = 'in', res = 200, 
    file = 'figs/plot4_5000fish_patchy.png')
  ggplot(p_plot_5000) + geom_line(aes(x = nfish, y = cpue, group = probs, colour = probs), 
    size = 1.5) + facet_wrap(~ variable) + theme_bw()
  dev.off()


#These are for 1000 fish initially, 10 in each cell
#Uniform
  p1s <- seq(.1, 1, by = .1)
  p_outs <- vector('list', length = length(p1s))
  
  #Figure out the location configurations, use default locations
  for(pp in 1:length(p1s)){
    print(pp)
    ctl <- make_ctl(distribute = 'uniform', mortality = .1, move_out_prob = .5,
      nfish1 = 1000, nfish2 = 0, prob1 = p1s[pp], prob2 = 0, nyear = 15, scope = 1, seed = 4,
      location = def_locs, movement_function = move_fish_none)  
    out <- conduct_survey(ctl = ctl)
    inp <- format_plot_input(out = out)
  
    p_outs[[pp]] <- inp
  }

  names(p_outs) <- as.character(p1s)
  p_outs <- ldply(p_outs)
  names(p_outs)[1] <- 'probs'

  p_plot_1000 <- p_outs %>% group_by(probs, year, variable) %>% summarize(cpue = mean(value),
    nfish = unique(nfish)) %>% as.data.frame
  p_plot_1000$probs <- as.numeric(p_plot_1000$probs)

  # ggplot(p_plot_1000) + geom_line(aes(x = nfish, y = cpue, group = probs, colour = probs), 
  #   size = 1.5) + facet_wrap(~ variable) + theme_bw() + ylim(c(0, 1))

p_plot_10000$start_fish <- 10000
p_plot_5000$start_fish <- 5000
p_plot_1000$start_fish <- 1000

nfish_results <- rbind(p_plot_10000, p_plot_5000, p_plot_1000)
nfish_results %>% filter(variable == 'cpue1')

cpue1 <- nfish_results %>% filter(variable == 'cpue1')

ggplot(cpue1) + geom_line(aes(x = nfish, y = cpue, group = probs, colour = probs),
  size = 1) + facet_wrap(~ start_fish)


#--------------------------------------------------------------------------------------------
#Plot 5
###Patchily distributed Fish
#Say there are 15 really good sites,
#Fish in the 10 best sites?
#5 best sites
#1 bet site


ctl <- make_ctl(distribute = 'patchy', mortality = .1, move_out_prob = .5,
      nfish1 = 5000, nfish2 = 10000, prob1 = .3, prob2 = .9, nyear = 15, scope = 1, seed = 4,
      location = def_locs, movement_function = move_fish_none)  

#Identify best fishing locations
best <- initialize_population(ctl = ctl, nfish = 5000)
best <- melt(best)
best1 <- (best[best$value != 0, ])

best1 <- best1[c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19,
  21, 23, 25, 27, 29), c('Var1', 'Var2')]

fifteen_best <- data.frame(vessel = 1, x = best1$Var1, y = best1$Var2)
ten_best <- fifteen_best[sample(1:15, 10), ]
five_best <- fifteen_best[sample(1:15, 5), ]

bests <- list(fifteen_best, ten_best, five_best)
bests_outs <- vector('list', length = 3)

#Loop over bests
for(bb in 1:length(bests)){
    ctl <- make_ctl(distribute = 'patchy', mortality = .1, move_out_prob = .5,
        nfish1 = 5000, nfish2 = 10000, prob1 = .3, prob2 = .9, nyear = 15, scope = 1, seed = 4,
        location = bests[[bb]], movement_function = move_fish_none)  
  out <- conduct_survey(ctl = ctl)
  inp <- format_plot_input(out = out)
  
  bests_outs[[bb]] <- inp
}

names(bests_outs) <- c('15', '10', '5')
bests_outs1 <- ldply(bests_outs)
names(bests_outs1)[1] <- 'nsites'

bo <- bests_outs1 %>% group_by(nsites, year, variable) %>% summarize(cpue = mean(value),
    nfish = unique(nfish)) %>% as.data.frame

png(width = 7, height = 7, units = 'in', res = 200, 
  file = 'figs/plot5_nsites.png')
ggplot(bo) + geom
s_line(aes(x = nfish, y = cpue, group = nsites, colour = nsites),
  size = 1) + facet_wrap(~ variable)
dev.off()

#Visualize the initial conditions?


  
  





#--------------------------------------------------------------------------------------------
#Number of fish,


#--------------------------------------------------------------------------------------------
#Patchy Distribution


#--------------------------------------------------------------------------------------------
#See what happens with decreasing population trend

#Run this to see the effect of sample size
#Turn fishing off by passing an empty locs
locs <- data.frame(vessel = 1, x = 0, y = 0)

ctl <- make_ctl(distribute = 'uniform', mortality = 0, move_out_prob = .5,
  nfish1 = 10000, nfish2 = 10000, prob1 = .01, prob2 = .99, nyear = 15, scope = 1, seed = 4,
  location = locs, movement_function = move_fish_left)

out <- conduct_survey(ctl = ctl) 
plot_average_cpue(out = out )













#Come up with functions to plot the data


out$samples$fish1samp + out$samples$fish2samp

#Things to test:
#If scope is 0 (no movement), there should be pretty quick local depletion
# I don't think the two species probabilities are strong enough
# Also, still catching too many fish, this might be related to the movement though
#Need to implement tracking by drop also maybe


#Test this with two fish species
ctl <- make_ctl(distribute = 'patchy', mortality = .9, nfish1 = 10000, nfish2 = 1000)
conduct_survey(ctl = ctl)

ctl <- make_ctl(distribute = 'patchy', mortality = matrix(rep(c(.1, .2), 100), nrow = 10, ncol = 10, byrow = TRUE))



#distance from port, rec fishing impacts
#incorporated in pop dy 


#when to hit saturation point, when on slope
#Depths of hook, can be informed by actual survey data
#bocaccio agression,
  #number of bocaccio vs other species caught on 
  #each line

#Strong local depletion effect over 15 years and 10000 fish uniformly distributed

#-------------------------------------------------------------------------------------------
#Add in two species

#Need to break fish_population into smaller functions
  #Modular approach will help with effect of order on processes
#Maybe with competition coefficient in ctl 
#sampling will have to occur for both species simultaneously








#-------------------------------------------------------------------------------------------
#Evaluate effect of number of fishing locations with uniform distribution
#Random Sampling Locations
rand_locs <- get_rand_locs()
# rand_locs <- rand_locs[1:10]

res <- lapply(rand_locs, FUN = function(x) run_sim(nhooks = 15, seed = 200, nfish = 10000,
  nyear = 15, distribute = 'uniform', percent = .5, cpue_method = '75hooks', location = x))
names(res) <- 1:length(rand_locs)
for_plot <- ldply(lapply(res, FUN = function(x) return(x$cpue)))
for_plot[, 1] <- as.numeric(for_plot[, 1])

# for_plot[, 1] <- as.factor(for_plot[, 1])
# levels(for_plot[, 1]) <- 1:20

png(width = 7, height = 7, units = 'in', res = 200, file = 'figs/random_fishing.png')
ggplot(for_plot) + geom_point(aes(x = nfish, y = avg_cpue), size = 2) + theme_bw() + 
  scale_color_hue(h = c(0, 1)) + facet_wrap(~ .id) + ggtitle("Random fishing locations")
dev.off()
#-------------------------------------------------------------------------------------------
#Uniform, but with quadrant based location sampling
same_locs <- expand.grid(x = 1:5, y = 1:5)
same_locs <- data.frame(vessel = 1, same_locs)

locs <- vector('list', length = 20)

for(ii in 1:20){
  locs[[ii]] <- same_locs[1:ii, ]
}

same_res <- lapply(locs, FUN = function(x) run_sim(nhooks = 15, seed = 200, nfish = 10000,
  nyear = 15, distribute = 'uniform', percent = .5, cpue_method = '75hooks', location = x))

names(same_res) <- 1:20
for_plot <- ldply(lapply(same_res, FUN = function(x) return(x$cpue)))
for_plot[, 1] <- as.numeric(for_plot[, 1])

png(width = 7, height = 7, units = 'in', res = 200, file = 'figs/quadrant_fishing.png')
ggplot(for_plot) + geom_line(aes(x = nfish, y = avg_cpue), colour = 'gray') + 
  theme_bw() + geom_point(aes(x = nfish, y = avg_cpue), size = 2) + facet_wrap(~ .id) +
  labs(ggtitle("Fishing in same area"))
dev.off()

#Strong local depletion with only 1000 fish
#A little less with 5000 fish

#-------------------------------------------------------------------------------------------
#Specify Locations to fish in and see effect of number of fish
rand_locs <- get_rand_locs()
rand_locs_df <- rand_locs
names(rand_locs_df) <- 1:20
rand_locs_df <- ldply(rand_locs_df)
rand_locs_df[, 1] <- as.numeric(rand_locs_df[, 1])
ggplot(rand_locs_df) + geom_point(aes(x = x, y = y)) + facet_wrap(~ .id)

loc <- rand_locs[[12]]

nfish <- seq(1000, 30000, by = 1000)
fish_res <- lapply(nfish, FUN = function(x) run_sim(nhooks = 15, seed = 200, nfish = x,
  nyear = 15, distribute = 'uniform', cpue_method = '75hooks', location = loc))
names(fish_res) <- nfish
for_plot <- ldply(lapply(fish_res, FUN = function(x) return(x$cpue)))
for_plot[, 1] <- as.numeric(for_plot[, 1])

png(width = 7, height = 7, units = 'in', res = 200, file = 'figs/nfish_uniform.png')
ggplot(for_plot) + geom_line(aes(x = nfish, y = avg_cpue), colour = 'gray') + 
  theme_bw() + geom_point(aes(x = nfish, y = avg_cpue), size = 2) + facet_wrap(~ .id) +
  ggtitle("Number of fish increasing, uniformly distributed")
dev.off()

#Number of fish when it's patchy, fish only in loc
nfish <- seq(1000, 30000, by = 1000)
patch_res <- lapply(nfish, FUN = function(x) run_sim(nhooks = 15, seed = 200, nfish = x,
  nyear = 15, distribute = 'patchy', percent = .3 , cpue_method = '75hooks', location = loc))
names(patch_res) <- nfish
patch_plot <- ldply(lapply(patch_res, FUN = function(x) return(x$cpue)))
patch_plot[, 1] <- as.numeric(patch_plot[, 1])

png(width = 7, height = 7, units = 'in', res = 200, file = 'figs/nfish_patchy.png')
ggplot(patch_plot) + geom_line(aes(x = nfish, y = avg_cpue), colour = 'gray') + 
  theme_bw() + geom_point(aes(x = nfish, y = avg_cpue), size = 2) + facet_wrap(~ .id) +
  ggtitle("Number of fish increasing, patchily distributed")
dev.off()

#Look at how the patches change
patch_res[[1]]$out$fished_areas$year0

init_patches <- lapply(patch_res, FUN = function(x) return(x$out$fished_areas$year0))






#-------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------
#Look at effect of different probabilities of being attracted to hook
p0s <- seq(.1, 1, .1)

#15,000 fish uniformly distributed
p0_res <- lapply(p0s, FUN = function(x) run_sim(nhooks = 15, seed = 200, nfish = 15000,
  nyear = 15, distribute = 'uniform', cpue_method = '75hooks', location = loc, p0 = x))
names(p0_res) <- p0s
p0_plots <- ldply(lapply(p0_res, FUN = function(x) return(x$cpue)))
p0_plots[, 1] <- as.numeric(p0_plots[, 1])

png(width = 7, height = 7, units = 'in', res = 200, file = 'figs/p0_15000.png')
ggplot(p0_plots) + geom_line(aes(x = nfish, y = avg_cpue, group = .id, colour = .id)) + 
 labs(ggtitle('Vary p0; 15,000 fish '))
dev.off()

###############
#10,000 fish uniformly distributed
p0_res <- lapply(p0s, FUN = function(x) run_sim(nhooks = 15, seed = 200, nfish = 10000,
  nyear = 15, distribute = 'uniform', cpue_method = '75hooks', location = loc, p0 = x))
names(p0_res) <- p0s
p0_plots <- ldply(lapply(p0_res, FUN = function(x) return(x$cpue)))
p0_plots[, 1] <- as.numeric(p0_plots[, 1])

png(width = 7, height = 7, units = 'in', res = 200, file = 'figs/p0_10000.png')
ggplot(p0_plots) + geom_line(aes(x = nfish, y = avg_cpue, group = .id, colour = .id)) + 
  labs(ggtitle("Vary p0; 10,000 fish"))
dev.off()

###############
#5,000 fish uniformly distributed
p0_res <- lapply(p0s, FUN = function(x) run_sim(nhooks = 15, seed = 200, nfish = 5000,
  nyear = 15, distribute = 'uniform', cpue_method = '75hooks', location = loc, p0 = x))
names(p0_res) <- p0s
p0_plots <- ldply(lapply(p0_res, FUN = function(x) return(x$cpue)))
p0_plots[, 1] <- as.numeric(p0_plots[, 1])

png(width = 7, height = 7, units = 'in', res = 200, file = 'figs/p0_5000.png')
ggplot(p0_plots) + geom_line(aes(x = nfish, y = avg_cpue, group = .id, colour = .id)) + 
  labs(ggtitle("Vary p0; 5,000 fish"))
dev.off()



#-------------------------------------------------------------------------------------------
#specify patchy distribution
one_patch <- run_sim(nhooks = 15, seed = 200, nfish = 15000, nyear = 15, distribute = 'patchy', 
  location = loc, percent = .5)
init <- one_patch$out$fished_areas$year0
init <- melt(init)
names(init)[c(1, 2)] <- c('x', 'y')

#Compare fish distributions and fishing site locations

ggplot() + geom_tile(data = init, aes(x = x, y = y, fill = value)) + 
  scale_fill_gradient(low = 'white', high = 'red') + theme_bw() + 
  geom_point(data = loc, aes(x = x, y = y), size = 4)

ggplot(one_patch$cpue) + geom_point(aes(x = nfish, y = avg_cpue), size = 3)

#Are they fishing in the areas with the most fish?

#-------------------------------------------------------------------------------------------
#Number of fish with some patch distribution
#change number of fish
outs <- as.list(seq(1000, 10000, 1000))

nfish_res <- lapply(outs, FUN = function(x) run_sim(nhooks = 15, seed = 200, nfish = x, 
  nyear = 15, distribute = 'patchy', percent = .5, cpue_method = '75hooks'))
names(nfish_res) <- outs

for_plot <- ldply(lapply(nfish_res, FUN = function(x) return(x$cpue)))
ggplot(for_plot) + geom_line(aes(x = nfish, y = avg_cpue, colour = .id))


#Weigh averages by number of fish in each location

#-------------------------------------------------------------------------------------------
#Include all the arguments that go into ctl




#-------------------------------------------------------------------------------------------
#Uniformly distributed fish
p0_vec <- seq(.1, 1, .1)
outs <- vector('list', length = length(p0_vec))

for(dd in 1:length(p0_vec)){
  ctl <- make_ctl(nhooks = 15, seed = 200, nfish = 10000, nyear = 50, distribute = 'uniform', 
    p0 = p0_vec[dd])
  outs[[dd]] <- conduct_survey(ctl)
}

cpue_avg_list <- lapply(outs, FUN = function(x) calc_cpue(x, ctl = ctl))
names(cpue_avg_list) <- as.character(p0_vec)
cpue_avg <- ldply(cpue_avg_list)
names(cpue_avg)[1] <- 'p0'

png(width = 11, height = 9, units = 'in', res = 150, file = 'figs/p0_vec.png')
ggplot(cpue_avg) + geom_line(aes(x = nfish, y = avg_cpue, colour = p0), size = 1.5) + theme(text = element_text(size = 24)) + 
  labs(x = 'True Number of Fish', y = "CPUE (nfish / nhooks)")
dev.off()

#-------------------------------------------------------------------------------------------
#Fish in the best spots, 
#50% of fish are distributed, fish in all those spots
ctl <- make_ctl(nhooks = 15, seed = 200, nfish = 10000, nyear = 50, distribute = 'patchy', 
  percent = .5)
find_spots <- initialize_population(ctl = ctl)
find_spots <- melt(find_spots) %>% arrange(desc(value))

site_percent <- seq(.1, 1, .1)

outs <- vector('list', length = length(site_percent))

for(dd in 1:length(site_percent)){
  max_row <- site_percent[dd] * nrow(find_spots)
  locs <- data.frame(vessel = rep(1, max_row), x = find_spots[1:max_row, 'Var1'],
    y = find_spots[1:max_row, 'Var2'])
  ctl <- make_ctl(nhooks = 15, seed = 200, nfish = 10000, nyear = 15, distribute = 'patchy',
    percent = .5, location = locs)

  outs[[dd]] <- conduct_survey(ctl)
  # out <- conduct_survey(ctl)
  # site_cpue[[dd]] <- calc_cpue(out, ctl = ctl)
  print(dd)
}

#Check that this is working right
outs[[10]]$fished_areas$year2
# str(outs[[1]])

#Calculate CPUE it with all average
ctl <- make_ctl(nhooks = 15, seed = 200, nfish = 10000, nyear = 15, distribute = 'patchy',
    percent = .5, location = locs) 
cpue_avg_list <- lapply(outs, FUN = function(x) calc_cpue(x, ctl = ctl))
names(cpue_avg_list) <- as.character(site_percent)
cpue_avg <- ldply(cpue_avg_list)
names(cpue_avg)[1] <- 'percent'

#Calculate CPUE with 75 hooks instead
ctl <- make_ctl(nhooks = 15, seed = 200, nfish = 10000, nyear = 15, distribute = 'patchy',
    percent = .5, location = locs, cpue_method = '75hooks') 
cpue_75_list <- lapply(outs, FUN = function(x) calc_cpue(x, ctl = ctl))
names(cpue_75_list) <- as.character(site_percent)
cpue_75 <- ldply(cpue_75_list)
names(cpue_75)[1] <- 'percent'


#Compare the two in plots
cpue_avg$method <- 'average'
cpue_avg$avg_catch <- NULL
cpue_75$method <- '75hooks'

#Add in my own label
cpue_avg[cpue_avg$percent == 0.1, 'percent'] <- '20%'
cpue_avg[cpue_avg$percent == 0.2, 'percent'] <- '40%'
cpue_avg[cpue_avg$percent == 0.3, 'percent'] <- '60%'
cpue_avg[cpue_avg$percent == 0.4, 'percent'] <- '80%'
cpue_avg[cpue_avg$percent == 0.5, 'percent'] <- '100%'

cpue_avg[cpue_avg$percent == 0.6, 'percent'] <- '50% good, 10% bad'
cpue_avg[cpue_avg$percent == 0.7, 'percent'] <- '50% good, 20% bad'
cpue_avg[cpue_avg$percent == 0.8, 'percent'] <- '50% good, 30% bad'
cpue_avg[cpue_avg$percent == 0.9, 'percent'] <- '50% good, 40% bad'
cpue_avg[cpue_avg$percent == 1.0, 'percent'] <- '50% good, 50% bad'

cpue_avg$percent <- factor(cpue_avg$percent, levels = c('20%', '40%', '60%', '80%', '100%'))

png(width = 9, height = 8, units = 'in', res = 150, file = 'figs/best_sites.png')
cpue_avg[grep('%', cpue_avg$percent), ] %>% ggplot(aes(x = nfish, y = avg_cpue,
  colour = percent)) + geom_line(size = 1.5) + 
  theme(text = element_text(size = 15)) + labs(x = 'True Number of Fish', y = "Average CPUE (nfish / nhooks)")
dev.off()


png(width = 9, height = 8, units = 'in', res = 150, file = 'figs/good_bad_sites.png')
cpue_avg[grep('good', cpue_avg$percent), ] %>% ggplot(aes(x = nfish, y = avg_cpue)) + geom_line(size = 1.5) + 
  theme(text = element_text(size = 15)) + labs(x = 'True Number of Fish', y = "Average CPUE (nfish / nhooks)") + 
  facet_wrap(~ percent)
dev.off()




pdf(width = 12, height = 9, file = "cpue_best_sites.pdf")
cpue_avg %>% filter(percent <= 0.5) %>% ggplot(aes(x = nfish, y = avg_cpue)) + geom_line(size = 1.5) +
  theme_bw() + facet_wrap(~ percent) + theme(text = element_text(size = 15)) + xlab("True Number of Fish") + 
  ylab('Average CPUE (nfish/nhooks)')
dev.off()

pdf(width = 12, height = 9, file = "cpue_all_sites.pdf")
cpue_avg %>% filter(percent > 0.5) %>% ggplot(aes(x = nfish, y = avg_cpue)) + geom_line(size = 1.5) +
  theme_bw() + facet_wrap(~ percent) + theme(text = element_text(size = 15)) + xlab("True Number of Fish") + 
  ylab('Average CPUE (nfish/nhooks)')
dev.off()

pdf(width = 12, height = 9, file = "cpue_example.pdf")
ggplot(aes(x = nfish, y = avg_cpue)) + geom_line(size = 1.5) +
  theme_bw() + facet_wrap(~ percent) + theme(text = element_text(size = 15)) + xlab("True Number of Fish") + 
  ylab('Average CPUE (nfish/nhooks)')
dev.off()

comp_cpue <- rbind(cpue_avg, cpue_75)

cpue_75$avg_cpue 


cpue_avg$avg_cpue



names(site_cpue) <- site_percent

site_cpue <- ldply(site_cpue)
names(site_cpue)[1] <- 'percent'

#add column based on percentage of sites samples
site_cpue$group <- 'all'
site_cpue[site_cpue$percent <= 0.5, 'group'] <- 'best'

#Average number of fish in each year / 15 hooks
ggplot(site_cpue) + geom_line(aes(x = nfish, y = avg_cpue, colour = percent), size = 1.5) + theme_bw() + 
  scale_colour_manual(values = gray.colors(n = 10)) + facet_grid(~ group) + 
  theme(text = element_text(size = 15)) + xlab("True Number of Fish") + 
  ylab('Average CPUE (nfish/nhooks)')

#Average number of fish per 75 hooks at each site




location <- 



#-------------------------------------------------------------------------------------------
#work on conducting survey for number of years
ctl <- make_ctl(nhooks = 15, seed = 201, nfish = 10010, nyear = 50, distribute = 'patchy',
  percent = .01)
out <- conduct_survey(ctl)
out$fished_areas$year0


cpue <- calc_cpue(out, ctl = ctl)
plot(cpue$nfish, cpue$avg_cpue, type = 'l')



initialize_population(ctl)


#Check Number of fish
ctl <- make_ctl(nhooks = 15, seed = 200, nfish = 10000, nyear = 50)
out <- conduct_survey(ctl)
cpue <- calc_cpue(out, ctl = ctl)

plot(cpue$nfish, cpue$avg_cpue, type = 'l')
#strata size weighted average

plot(annual_catch$nfish, annual_catch$avg_cpue, type = 'l')


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
