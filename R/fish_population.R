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
  location <- control$location
  scope <- control$scope
  nhooks <- control$nhooks
  ndrops <- control$ndrops
  process <- control$process
  p0 <- control$p0

  if(class(location) != "data.frame") stop("location must be a data frame")
  

  #Add on samples for each drop into location data frame  
  add_ons <- as.data.frame(matrix(999, nrow = nrow(location), ncol = ndrops))
  names(add_ons) <- paste0('drop', 1:ndrops)

  location <- cbind(location, add_ons)

  for(ii in 1:nrow(location)){

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

    # if(sum(is.na(fish_df$prob)) > 0) browser()

    #If there are no fish within scope, set movement probabilities to 0
    if(nfish_outside == 0){
      fish_df$prob <- 0
    }

    #Stop fishing if there are no fish (return 0s for samples)
    if(sum(fish_df$value) == 0){
      # browser()
      return(list(updated_area = fish_area, samples = rep(0, ndrops)))
    }

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

    #--------Equal hook probabilities
    if(process == 'equal_prob'){
      samples <- vector(length = ndrops)
      
      phook <- hook_probs(nfish = fish_to_catch, p0 = p0) #probability of catching number of fish
      #For loop for number of 

      samples <- vector(length = ndrops)

      for(qq in 1:ndrops){
        samp_temp <- rmultinom(1, 1, phook) #probabilities defined in phook
        samp <- data.frame(nfish = 0:5, pick = samp_temp)

        samples[qq] <- samp[which(samp$pick == 1), 'nfish']

        # samples[qq] <- sample_equal_prob(nfish = fish_to_catch, nhooks = nhooks, p0 = dots$p0)  
        fish_to_catch <- fish_to_catch - samples[qq]
      
        #add if statement so that samples[qq] cannot exceed fish_to_catch
        if(fish_to_catch < 0) fish_to_catch <- 0
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
    if(fish_to_catch == 0) fish_df$final <- fish_df$fished

    #if there are fish that can move back, move them
    if(fish_to_catch != 0){

      #movement back to cells is based on proportions that moved in
      move_back_probs <- fish_df$moving
      move_back_probs[zero_index] <- fish_df[zero_index, 'value']

      mult_prob <- move_back_probs / sum(move_back_probs)

      # if(is.na(sum(mult_prob))) mult_prob <- rep(0, length(move_back_probs))
      # Sample from multinomial distribution
      # if(fish_df[zero_index, 'fished'] < 0) browser()

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
    
  }

  return(list(updated_area = fish_area, samples = location))

}
