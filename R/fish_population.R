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
  #write this in apply
  #Define 
  
  #Apply over fishing locations
  #But also apply function to each location
  
  ff <- fish_area[[1]]

   <- move_fish_loop(location = location, ff = fish_area[[1]])
  








  ll <- location[1, ]


  
# browser()
  for(ii in 1:nrow(location)){    
    #Define range of fish movement
    # row_range <- (location[ii, 'x'] - scope):(location[ii, 'x'] + scope)
    # row_range <- row_range[row_range %in% 1:nrow(fish_area)] #If there's a border case maybe?

    # col_range <- (location[ii, 'y'] - scope):(location[ii, 'y'] + scope)
    # col_range <- col_range[col_range %in% 1:ncol(fish_area)]

    # #Define range to fish
    # fish_area_melted <- reshape2::melt(fish_area)

    # #Use melted matrix because fish_area is easier to subset
    # fish_range_melted <- subset(fish_area_melted, Var1 %in% row_range & Var2 %in% col_range)
    # #Find the location of fishing
    # fish_location_melted <- subset(fish_area_melted, Var1 == location[ii, 'x'] & Var2 == location[ii, 'y'])

    # #define zero index, this is the initial location of movement
    # zero_index <-  which(fish_range_melted$Var1 == location[ii, 'x'] & fish_range_melted$Var2 == location[ii, 'y'])

    # fish_range <- matrix(fish_range_melted$value, nrow = length(row_range), ncol = length(col_range))
    # fish_in_loc <- fish_location_melted$value

    #define number of fish outside
    # nfish_outside <- sum(fish_range) - fish_in_loc

    ##---------------------------------------------------------------------------------------
    #Move fish with function move_fish
    fish_df <- move_fish(fish_range1 = fish_range, nfish_outside1 = nfish_outside,
      zero_index1 = zero_index)

   
    ##---------------------------------------------------------------------------------------
    #Update numbers in each cell after fishing 
    ##---------------------------------------------------------------------------------------
    #Update number of fish in each cell
    fish_df$fished <- fish_df$moved
    fish_df[zero_index, 'fished'] <- fish_to_catch

    #Two conditions:
    #No fish left, return empty cells

    if(fish_to_catch == 0) {
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
