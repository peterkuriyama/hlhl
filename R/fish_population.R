#'Fish the Population
#'
#'Function to fish the population
# #'@param fish_area Matrix with the distribution of fish
# #'@param location Data frame of locations with column for vessel, rows, and columns of fish are to fish in. 
# # '@param location list of locations specifying rows and columns of fish_area to fish in. The length of the list will correspond to the number of vessels
# #'@param scope the scope of fishing movement, default to 1 so fish in surrounding 1 cells can move in
# #'@param nhooks number of hooks at the smallest sampling size
# #'@param ndrops number of drops, default is 5 following hook and line protocol
# #'@param process specify process by which fish are sampled, options are 'multinomial', 'hypergeometric', and 'equal_prob'
#'@examples

#' control <- make_ctl()
#' init <- initialize_population(control)
#' fish_population(fish_area = init, ctl = control)
#'

#'@export
#may need to add angler specifications in at each time
#currently it's just 15 hooks per drop, without the ability to specify angler
#location on boat

#also play with sampling probabilities and movements

# fish_population <- function(fish_area, location, scope = 1, nhooks, ndrops,
#   ...){

fish_population <- function(fish_area, ctl){
  #Should have Four steps or so
  #Move Fish
  #Catch Fish
  #Fish Die
  #Move Fish again

  ##---------------------------------------------------------------------------------------
  #Unpack Ctl File
  ##---------------------------------------------------------------------------------------
  location <- ctl$location
  scope <- ctl$scope
  nhooks <- ctl$nhooks
  ndrops <- ctl$ndrops
  process <- ctl$process
  p0 <- ctl$p0
  browser <- ctl$browser
  mortality <- ctl$mortality

  if(class(location) != "data.frame") stop("location must be a data frame")
  
  #Add on samples for each drop into location data frame  
  add_ons <- as.data.frame(matrix(999, nrow = nrow(location), ncol = ndrops))
  names(add_ons) <- paste0('drop', 1:ndrops)

  location <- cbind(location, add_ons)
  location_angler <- vector('list', length = nrow(location))

  #convert to list
  fish_area_list <- lapply(fish_area, FUN = function(x) melt(x))

  ##---------------------------------------------------------------------------------------
  #Fish in Locations
  ##---------------------------------------------------------------------------------------
  
  ##---------------------------------------------------------------------------------------
  #Move Fish into locations
  #Call this fish_temp because to keep the original, and
  fish_temp <- lapply(fish_area, FUN = function(z){
      move_fish_loop(location = location, ff = z)
  })

  #convert fish_area into matrices
  to_fish <- lapply(fish_temp, FUN = function(x){
    matrix(x$fish_area$value, nrow = ctl$numrow, ncol = ctl$numcol, byrow = TRUE)
  })

  nfish_moved <- lapply(fish_temp, FUN = function(x){
    yy <- x$nfish_moved
    yy$moved <- yy$check
    return(yy)
  })
  
  ##---------------------------------------------------------------------------------------
  #Fish with sample_exp function
  #Do this in a for loop first, then maybe switch to an apply statement

  #Store the samps stuff
  samps_out <- as.data.frame(matrix(nrow = nrow(location), ncol = 5))
  names(samps_out) <- c('vessel', 'x', 'y', 'fish1samp', 'fish2samp')

  #declare this initially
  temp_fish_area <- to_fish

  for(ll in 1:nrow(location)){
    temp <- fish_pop_loop(fish_area = temp_fish_area, loc_row = location[ll, ],
      ctl = ctl)  
    temp_fish_area <- temp$fish_area
    samps_out[ll, ] <- temp$samps
  }
 
 #temp_fish_area is kind of a temporary

 ##---------------------------------------------------------------------------------------
 # Move Fish Back
 # Need to remove this eventually
 #Need to make sure that the move back uses the same info
 nfish_back <- move_back(nfish_moved = nfish_moved, samps_out = samps_out)
 
 #Add these into the overall fish_area
  fish_out <- vector('list', length = 2)

  for(uu in 1:length(fish_out)){
    ttry <- fish_temp[[uu]]$fish_area  

    fish_out1 <- left_join( ttry[, c('x', 'y', 'value')], nfish_back[[uu]][, c('x', 'y', 'final')], 
      by = c('x', 'y'))

    na_ind <- is.na(fish_out1$final)
    fish_out1[na_ind, 'final'] <- fish_out1[na_ind, 'value']
    fish_out[[uu]] <- matrix(fish_out1$final, nrow = ctl$numrow, ncol = ctl$numcol, 
      byrow = TRUE)
  }
 
 ##---------------------------------------------------------------------------------------
 #Add in Mortality
 
  #Add in rounded mortality numbers
  inst_mort <- exp(mortality) / 100 #convert continuous to instantaneous

  fish_out <- lapply(fish_out, FUN = function(x){
    x - round(x * inst_mort)
  })
  
  ##---------------------------------------------------------------------------------------
  #Return the fish areas

browser()  
  return(list(updated_area = fish_out, angler_samples = samps_out, ))
  #Work out some drop stuff later

  return(list(updated_area = fish_area, angler_samples = location_angler, samples = location))
}















#Old Code that fished within a for loop
 # ##---------------------------------------------------------------------------------------
 #    #Fish in Specific Areas
 #    ##---------------------------------------------------------------------------------------

 #    #Now fish in specified cell, called zero.index
 #    fish_to_catch <- fish_df[zero_index, 'moved']

 #    #specify number of anglers
 #    if(nhooks %% 5 != 0) stop('number of hooks must be multiple of 5')
 #    nang <- nhooks / 5 #number of anglers

 #    #format angler samples
 #    angler_samples <- as.data.frame(matrix(nrow = ndrops, ncol = 1 + nang))
 #    names(angler_samples) <- c(paste0('angler', 1:nang), 'drop')
 #    angler_samples$drop <- 1:ndrops

 #    #--------Equal hook probabilities
 #    if(process == 'equal_prob'){
 #      samples <- vector(length = ndrops)

 #      #If phook doesn't sum to 1, throw error
 #    #       if(sum(phook) != 1){
 #      #take absolute value of phook due to rounding errors letting it get negative
    
 #      #For loop for number of drops and anglers
 #      for(qq in 1:ndrops){

 #        #Have to redefine probabilities and check that catches don't exceed number of fish
 #        #available
        
 #        #Redefine probabilities
 #        phook <- hook_probs(nfish = fish_to_catch, p0 = p0) #probability of catching number of fish
 #        phook <- round(phook, digits = 10) #Rounding errors with the probabilities, round to 10 decimal places

 #        #Loop through anglers here,
 #        #maybe add in ability to modify the probabilities of certain anglers
 #        # samples_ang <- rep('999', length = nang)
        
 #        #Sample fish for each angler
 #        samp_temp <- rmultinom(nang, 1, phook) #probabilities defined in phook
        
 #        samp <- data.frame(nfish = 0:5, pick = samp_temp)    
 #        samp <- melt(samp, id.vars = 'nfish') #melt into columns
        
 #        samples_ang <- samp[which(samp$value == 1), 'nfish'] #need to track these also
        
 #        #Loop through samp temp to make sure only 
 #        #fish_to_catch can be caught
 #        if(sum(samples_ang) > fish_to_catch) {
 #          temp_samp <- cumsum(samples_ang)
 #          samples_ang[which(temp_samp > fish_to_catch)] <- 0          
 #        }
      
 #        #format and store angler samples
 #        # angang <- as.data.frame(t(as.data.frame(samples_ang)))
 #        # row.names(angang) <- NULL
 #        # names(angang) <- paste0('angler', 1:nang)
 #        # angang$drop <- qq

 #        #Store Samples
 #        angler_samples[qq, 1:nang] <- samples_ang
 #        samples[qq] <- sum(samples_ang)

 #        # samples[qq] <- sample_equal_prob(nfish = fish_to_catch, nhooks = nhooks, p0 = dots$p0)  
 #        fish_to_catch <- fish_to_catch - samples[qq]
      
 #        #add if statement so that samples[qq] cannot exceed fish_to_catch
 #        # if(fish_to_catch < 0) fish_to_catch <- 0
 #        if(fish_to_catch < 0) stop('negative fish')
 #      }
 #    }

 #    #--------Hypergeometric
 #    if(process == 'hypergeometric'){
 #      ###in rhyper
 #      #n is number of failures
 #      #m is number of successes (fish)
 #      #k is number of samples, both n = k = nhooks
 #      #nn is number of sampling events, maybe equal to ndrops

 #      samples <- vector(length = ndrops)

 #      for(qq in 1:ndrops){
 #        samples[qq] <- rhyper(n = nhooks, m = fish_to_catch, k = nhooks, nn = 1)
 #        fish_to_catch <- fish_to_catch - samples[qq] #remove caught fish
 #      }
 #    }

 #    #--------Multinomial
 #    #multinomial process is still really in development
 #    if(process == 'multinomial'){
 #      hookProbs <- rep(1 / (nhooks + 1), (nhooks + 1)) #All Hooks have equal probability
 #      catches <- matrix(nrow = (nhooks + 1), ncol = nhooks)

 #      for(zz in 1:nhooks){
 #      catches[, zz] <- rmultinom(1, size = 1, prob = hookProbs)

 #      rmultinom(1, size = fish_to_catch, prob = hookProbs)

 #      #update hook probabilties if fish are caught
 #      if (sum(catches[, zz]) == 1 & which(catches[, zz] == 1) != 1) {
 #        hookProbs[1] <- hookProbs[1] + hookProbs[which(catches[, zz] == 1)]
 #        hookProbs[which(catches[, zz] == 1)] <- 0
 #      }
 #     }
 #    }
