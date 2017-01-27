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
  
# pp <- nfish_moved[[1]]
# pp$check - pp$moved


# sum(pp$moving)
# sum(pp$moved)
# sum(pp$check)
# nfish_moved$check - nfish_moved$moved

  ##---------------------------------------------------------------------------------------
  #Fish with sample_exp function
  #Do this in a for loop first, then maybe switch to an apply statement

  #Declare fish area to update
  # temp_fish_area <- fish_temp

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
browser()







 #Now format the output so that everything's looking ok
zz <- 1
finals <- vector('list', length = 2)
 
 for(zz in 1:length(nfish_back)){
  moved <- nfish_back[[zz]]
  orig <- temp_fish_area[[zz]]

  orig <- melt(orig)
  names(orig) <- c('x', 'y', 'orig_value')

  temp_final <- left_join(orig, moved[, c('value', 'x', 'y', 'final')],
    by = c('x', 'y'))

  temp_final$unq <- paste(temp_final$x, temp_final$y)


 temp_final[temp_final$unq %in% temp_final[duplicated(temp_final$unq), 'unq'], ]
  temp_final[duplicated(temp_final$unq), ]

temp_final[which(duplicated(temp_final[, c('x', 'y')])), ]



sum(temp_final$orig_value)
sum(temp_final$value, na.rm = TRUE)
sum(temp_final$final, na.rm = TRUE)

 }

 tt <- nfish_back[[1]]
 tt1 <- melt(temp_fish_area[[1]])
 names(tt1)[1:2] <- c('x', 'y')


 


 tt2 <- left_join(tt1, tt[, c('x', 'y', 'value', 'final')])

 tt3 <- tt2[is.na(tt2$final) == FALSE, ]
 sum(tt3$value) - 45
 sum(tt3$final)

tt5 <- inner_join(tt1, tt[, c('x', 'y', 'final')])
sum(tt5$value) + 45
sum(tt5$final)

 tt2[which(is.na(tt2$final)), 'final'] <- tt2[which(is.na(tt2$final)), 'value']
 sum(tt2$value) + 45
 sum(tt2$final)



 #Check the number of fish in nfish_back
 lapply(nfish_back, FUN = function(x){
  matrix(x$final, nrow = ctl$numrow, ncol = ctl$numcol)
 })

 sum(nfish_back[[1]]$final)
 sum(nfish_back[[1]]$after_fishing)



 #update the temp fish 
 aa <- melt(temp_fish_area[[1]])
 names(aa)[1:2] <- c('x', 'y')

 qq <- left_join(aa, nfish_back[[1]][, c('x', 'y', 'final')], 
  by = c('x', 'y'))
 
 qq[which(is.na(qq$final)), 'final'] <- qq[which(is.na(qq$final)), 'value']

 sum(qq$value)
 sum(qq$final, na.rm = TRUE)





   

  temp <- nfish_moved[[1]]
  #Use differences to see determine numbers moving back

  #Find cells that had negative and positive changes in numbers
  

  
  

  
  

  lapply(ranges, FUN = function(x){
    
    
  })
# browser()


 

######
######I'm HERE TRYING TO FIGURE THIS OUT
######

  (froms$x - ctl$scope): (froms$x + ctl$scope)

  temp[move_to_ind, c('x', 'y')]
  
  temp[which(temp$diff >= 0), 'diff'] <- 0
  temp$diff[which(temp$diff >= 0)]
  
  temp$diff < 0


  temp$move_back_prob
  temp[, c('value', 'moving', 'moved', 'after_fishing', 'move_back_prob')]
  


  temp$value / temp$after_fishing

  rmultinom(1, size = sum(temp$after_fishing), prob = temp$move_back_prob)

  nfish_moved[[1]]$move_back_prob <- nfish_moved[[1]]$moving / sum(nfish_moved[[1]]$moving)

  rmultsum(temp$after_fishing) 

  temp_prob <- temp$value / sum(temp$value)

  

  temp$moved_back <- temp$after_fishing * temp_prob

  rmultinom(1, size = 300,
    prob = nfish_moved[[1]]$move_back_prob)





  #update nfish_moved  
# browser()
 {
    ##---------------------------------------------------------------------------------------
    #Update numbers in each cell after fishing 
    ##---------------------------------------------------------------------------------------
    #Update number of fish in each cell
    # fish_df$fished <- fish_df$moved
    # fish_df[zero_index, 'fished'] <- fish_to_catch

    #Two conditions:
    #No fish left, return empty cells

    # if(fish_to_catch == 0) {
    #   fish_df$final <- fish_df$fished
    # }

    #if there are fish that can move back, move them
    if(fish_to_catch != 0){

      #movement back to cells is based on proportions that moved in
      move_back_probs <- fish_df$moving
      move_back_probs[zero_index] <- fish_df[zero_index, 'value']

      mult_prob <- move_back_probs / sum(move_back_probs)

      # if(is.na(sum(mult_prob))) mult_prob <- rep(0, length(move_back_probs))
      # Sample from multinomial distribution
      moved_back <- as.vector(rmultinom(1, size = fish_df[zero_index, 'fished'],
                                        prob = mult_prob))

      fish_df$delta <- moved_back

      #update fish counts
      fish_df$final <- fish_df$fished + fish_df$delta
      fish_df[zero_index, 'final'] <- fish_df[zero_index, 'delta']
    }

    ##---------------------------------------------------------------------------------------
    #Add mortality
    ##---------------------------------------------------------------------------------------
    
    #Update fish_area matrix
    fish_area[row_range, col_range] <- matrix(fish_df$final,
      nrow = nrow(fish_range), ncol = ncol(fish_range))
  
    #Add in rounded mortality numbers
    inst_mort <- exp(mortality) / 100 #convert continuous to instantaneous

    fish_area <- fish_area - round(fish_area * inst_morg)

    first_drop <- which(names(location) == 'drop1')
    location[ii, first_drop:ncol(location)] <- samples #Store Samples

    #Add locations to angler samples, then store in a list
    angler_samples$vessel <- location[ii, 'vessel']
    angler_samples$x <- location[ii, 'x']
    angler_samples$y <- location[ii, 'y']
  
    angler_samples$drop <- 1:ndrops
  
    angler_samples <- angler_samples[, c('vessel', 'x', 'y', 'drop',
                        names(angler_samples)[grep('angler', names(angler_samples))])]
    location_angler[[ii]] <- angler_samples

  }

  # print(angler_samples)
  location_angler <- plyr::ldply(location_angler)

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
