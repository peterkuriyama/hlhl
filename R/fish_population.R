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

  location <- ctl$location
  scope <- ctl$scope
  nhooks <- ctl$nhooks
  ndrops <- ctl$ndrops
  process <- ctl$process
  p0 <- ctl$p0
  browser <- ctl$browser

  if(class(location) != "data.frame") stop("location must be a data frame")
  
  #Add on samples for each drop into location data frame  
  add_ons <- as.data.frame(matrix(999, nrow = nrow(location), ncol = ndrops))
  names(add_ons) <- paste0('drop', 1:ndrops)

  location <- cbind(location, add_ons)
  location_angler <- vector('list', length = nrow(location))

  for(ii in 1:nrow(location)){
# if(browser == TRUE & ii == 2) browser()            
    
    row_range <- (location[ii, 'x'] - scope):(location[ii, 'x'] + scope)
   
    row_range <- row_range[row_range %in% 1:nrow(fish_area)] #If there's a border case maybe?

    col_range <- (location[ii, 'y'] - scope):(location[ii, 'y'] + scope)
    col_range <- col_range[col_range %in% 1:ncol(fish_area)]

    #Define range to fish
    fish_area_melted <- reshape2::melt(fish_area)

    #Use melted matrix because fish_area is easier to subset
    fish_range_melted <- subset(fish_area_melted, Var1 %in% row_range & Var2 %in% col_range)
    fish_location_melted <-subset(fish_area_melted, Var1 == location[ii, 'x'] & Var2 == location[ii, 'y'])

    #define zero index
    zero_index <-  which(fish_range_melted$Var1 == location[ii, 'x'] & fish_range_melted$Var2 == location[ii, 'y'])

    fish_range <- matrix(fish_range_melted$value, nrow = length(row_range), ncol = length(col_range))
    fish_in_loc <- fish_location_melted$value

    #define number of fish outside
    nfish_outside <- sum(fish_range) - fish_in_loc

    #------------------------------------------------------------------------------------------------------------
    ##Fish Movement

    #Move fish to specified location

    #Calculate movement probabilities
    #currently nothing goes out of specified location
    #Movement probabilites depend on number of fish outside relative to
    #total fish outside
    probs <- fish_range / nfish_outside

    #create data frame of fish_range [fish to catch] because
      #easier to manipulate
    #Can streamline this whole process here
    fish_df <- melt(fish_range)
    fish_df$prob <- melt(probs)$value

    #If there are no fish within scope, set movement probabilities to 0
    if(nfish_outside == 0){
      fish_df$prob <- 0
    } 

    #Stop fishing if there are no fish (return 0s for samples)
    # if(sum(fish_df$value) == 0){
    #   return(list(updated_area = fish_area, samples = rep(0, ndrops)))
    # }

    fish_df[zero_index, 'prob'] <- 0

    #Calculate number of moving fish with binomial distribution
    fish_df$moving <- apply(fish_df, MAR = 1, FUN = function(x) rbinom(n = 1, size = x['value'],
      prob = x['prob']))

    #Now update fish
    fish_df$moved <- fish_df$value - fish_df$moving #moved column indicates nfish after movement
    fish_df[zero_index, 'moved'] <- fish_df[zero_index, 'value'] + sum(fish_df$moving)

    #------------------------------------------------------
    #Now fish in specified cell, called zero.index
    fish_to_catch <- fish_df[zero_index, 'moved']

    #specify number of anglers
    if(nhooks %% 5 != 0) stop('number of hooks must be multiple of 5')
    nang <- nhooks / 5 #number of anglers

    #format angler samples
    angler_samples <- as.data.frame(matrix(nrow = ndrops, ncol = 1 + nang))
    names(angler_samples) <- c(paste0('angler', 1:nang), 'drop')
    angler_samples$drop <- 1:ndrops

    #--------Equal hook probabilities
    if(process == 'equal_prob'){
      samples <- vector(length = ndrops)

      #If phook doesn't sum to 1, throw error
    #       if(sum(phook) != 1){
      #take absolute value of phook due to rounding errors letting it get negative
    
      #For loop for number of drops and anglers
      for(qq in 1:ndrops){

        #Have to redefine probabilities and check that catches don't exceed number of fish
        #available
        
        #Redefine probabilities
        phook <- hook_probs(nfish = fish_to_catch, p0 = p0) #probability of catching number of fish
        phook <- round(phook, digits = 10) #Rounding errors with the probabilities, round to 10 decimal places

        #Loop through anglers here,
        #maybe add in ability to modify the probabilities of certain anglers
        # samples_ang <- rep('999', length = nang)
        
        #Sample fish for each angler
        samp_temp <- rmultinom(nang, 1, phook) #probabilities defined in phook
        
        samp <- data.frame(nfish = 0:5, pick = samp_temp)    
        samp <- melt(samp, id.vars = 'nfish') #melt into columns
        
        samples_ang <- samp[which(samp$value == 1), 'nfish'] #need to track these also
        
        #Loop through samp temp to make sure only 
        #fish_to_catch can be caught
        if(sum(samples_ang) > fish_to_catch) {
          temp_samp <- cumsum(samples_ang)
          samples_ang[which(temp_samp > fish_to_catch)] <- 0          
        }
      
        #format and store angler samples
        # angang <- as.data.frame(t(as.data.frame(samples_ang)))
        # row.names(angang) <- NULL
        # names(angang) <- paste0('angler', 1:nang)
        # angang$drop <- qq

        #Store Samples
        angler_samples[qq, 1:nang] <- samples_ang
        samples[qq] <- sum(samples_ang)

        # samples[qq] <- sample_equal_prob(nfish = fish_to_catch, nhooks = nhooks, p0 = dots$p0)  
        fish_to_catch <- fish_to_catch - samples[qq]
      
        #add if statement so that samples[qq] cannot exceed fish_to_catch
        # if(fish_to_catch < 0) fish_to_catch <- 0
        if(fish_to_catch < 0) stop('negative fish')
      }
    }

    #--------Hypergeometric
    if(process == 'hypergeometric'){
      ###in rhyper
      #n is number of failures
      #m is number of successes (fish)
      #k is number of samples, both n = k = nhooks
      #nn is number of sampling events, maybe equal to ndrops

      samples <- vector(length = ndrops)

      for(qq in 1:ndrops){
        samples[qq] <- rhyper(n = nhooks, m = fish_to_catch, k = nhooks, nn = 1)
        fish_to_catch <- fish_to_catch - samples[qq] #remove caught fish
      }
    }

    #--------Multinomial
    #multinomial process is still really in development
    if(process == 'multinomial'){
      hookProbs <- rep(1 / (nhooks + 1), (nhooks + 1)) #All Hooks have equal probability
      catches <- matrix(nrow = (nhooks + 1), ncol = nhooks)

      for(zz in 1:nhooks){
      catches[, zz] <- rmultinom(1, size = 1, prob = hookProbs)

      rmultinom(1, size = fish_to_catch, prob = hookProbs)

      #update hook probabilties if fish are caught
      if (sum(catches[, zz]) == 1 & which(catches[, zz] == 1) != 1) {
        hookProbs[1] <- hookProbs[1] + hookProbs[which(catches[, zz] == 1)]
        hookProbs[which(catches[, zz] == 1)] <- 0
      }
     }
    }

    #------------------------------------------------------
    #Update number of fish in each cell
    fish_df$fished <- fish_df$moved
    fish_df[zero_index, 'fished'] <- fish_to_catch

    #Two conditions:
    #No fish left, return empty cells

    if(fish_to_catch == 0) {
# cat(ii, 'inside fish=', fish_to_catch, '\n' )      
      fish_df$final <- fish_df$fished
    }

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



    #Update fish_area matrix
    fish_area[row_range, col_range] <- matrix(fish_df$final,
      nrow = nrow(fish_range), ncol = ncol(fish_range))
 
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
