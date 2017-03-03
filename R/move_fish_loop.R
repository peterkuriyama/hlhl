#' Move fish in for loop

#' Function that moves fish by looping through locations. Do this rather than apply statement
#' to avoid weird fish movement if nfish in a certain cell isn't enough. 

#' @param location Data frame of locations
#' @param ff Fish area, called ff because I couldnt' think of a better name for it
#' @export

move_fish_loop <- function(location, ff){

  #Define the scope based on ctl file
  scope <- ctl$scope
  
  #Store the original data frame to compare
  ff_orig <- ff

  #Store the number of fish that moved
  nfish_moved <- vector('list', length = nrow(location))

  #melt ff to keep track of fish moving in and out
  ff_melt <- melt(ff)
  names(ff_melt)[1:2] <- c('x', 'y')

  #I need to add in updates to each fishing area so a crazy number of fish aren't moving
  #at once
  #Now calculate the number of fish that move in each location

# ff <- ff_orig
# fish_area <- ff
# x <- location[zz, 'x']
# y <- location[zz, 'y']
# scope <- scope

  for(zz in 1:nrow(location)){
    moves <- define_movement(fish_area = ff, x = location[zz, 'x'],
      y = location[zz, 'y'], scope = scope)
    
    moved <- move_fish(fish_range1 = moves$fish_range, nfish_outside1 = moves$nfish_outside,
      zero_index1 = moves$zero_index)

    orig_inds <- expand.grid(moves$row_range, moves$col_range)

    moved$x <- orig_inds$Var1
    moved$y <- orig_inds$Var2

    update_these <- which(ff_melt$x %in% moved$x & ff_melt$y %in% moved$y)

    #Subtract moving fish
    ff_melt[update_these, 'value'] <- ff_melt[update_these, 'value'] - moved$moving

    #Define location to add all the moved fish
    # zero_index <- which(moved$moving == 0)
    ff_melt[update_these[moves$zero_index], 'value'] <- moved[moves$zero_index, 'moved']

    #update ff that goes into movement functions
    ff <- matrix(ff_melt$value, nrow = max(ff_melt$x), ncol = max(ff_melt$y))

    nfish_moved[[zz]] <- moved
  }

  #Calculate the number of fish moving in each fishing location
  names(nfish_moved) <- paste0("loc", 1:length(nfish_moved))

  #Pull out location and nfish moved
  location$unq <- paste(location$x, location$y)
  location$loc <- paste0("loc", 1:length(nfish_moved))

  #Convert the list to a data frame
  nfish_moved <- ldply(nfish_moved)
  names(nfish_moved)[1] <- 'loc'
  
  #Here's the number of fish moving into each location
  moved_in <- nfish_moved %>% group_by(loc) %>% summarize(nmoving = sum(moving)) %>% as.data.frame
  #Add on the location
  moved_in <- left_join(moved_in, location[, c('unq', 'loc')], by = 'loc')
    
  #Add in unq columns for joining  
  nfish_moved$unq <- paste(nfish_moved$x, nfish_moved$y)
  
  #Join based on each unique location
  nfish_moved1 <- nfish_moved %>% group_by(unq) %>% mutate(nmoving = sum(moving)) %>%
    filter(row_number(unq) == 1) %>% as.data.frame
  nfish_moved1$moving <- nfish_moved1$nmoving
  
  #Update the number of fish  
  nfish_moved1$moved <- nfish_moved1$value - nfish_moved1$moving
  nfish_moved1$nmoving <- NULL

  #Now add in the fish that moved in 
  # nfish_moved1[nfish_moved1$unq %in% moved_in$unq, ]

  t1 <- nfish_moved1[nfish_moved1$unq %in% moved_in$unq, ]
  tf <- left_join(t1, moved_in[, c('nmoving', 'unq')], by = 'unq')
  tf$moved <- tf$moved + tf$nmoving  
  
  #Now replace in nfisH_moved1
  nfish_moved1[nfish_moved1$unq %in% moved_in$unq, 'moved'] <- tf$moved
  
  #Clean up the data frames
  nfish_moved1$loc <- NULL
  nfish_moved1$unq <- NULL

  #replace with 
  ff_melt$unq <- paste(ff_melt$x, ff_melt$y)
  nfish_moved1$unq <- paste(nfish_moved1$x, nfish_moved1$y)

  #Check
  check <- inner_join(nfish_moved1[, c('moved', 'unq')], ff_melt[, c('value', 'unq')], by = 'unq')

  if(sum(check$moved == check$value) != nrow(check)){
    print('error in number of fish')
    browser()
  }

  nfish_moved1$check <- nfish_moved1$moved
  
  return(list(fish_area = ff_melt, nfish_moved = nfish_moved1))

}

  