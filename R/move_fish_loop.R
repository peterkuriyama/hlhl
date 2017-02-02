#' Move fish in for loop

#' Function that moves fish by looping through locations. Do this rather than apply statement
#' to avoid weird fish movement if nfish in a certain cell isn't enough. 

#' @param location Data frame of locations
#' @param ff Fish area, called ff because I couldnt' think of a better name for it
#' @export

move_fish_loop <- function(location, ff){
# browser()
  #Define the scope based on ctl file
  scope <- ctl$scope
  
  #Store the original data frame to compare
  ff_orig <- ff

  #Store the number of fish that moved
  nfish_moved <- vector('list', length = 3)

  #melt ff to keep track of fish moving in and out
  ff_melt <- melt(ff)
  names(ff_melt)[1:2] <- c('x', 'y')

  #Now calculate the number of fish that move in each location
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
    zero_index <- which(moved$moving == 0)
    ff_melt[update_these[zero_index], 'value'] <- moved[zero_index, 'moved']

    nfish_moved[[zz]] <- moved
  }

  #Calculate the number of fish moving in each fishing location
  names(nfish_moved) <- paste0("loc", 1:length(nfish_moved))
  nfish_moved <- ldply(nfish_moved)
  names(nfish_moved)[1] <- 'loc'
  
  #Here's the number of fish moving into each location
  moved_in <- nfish_moved %>% group_by(loc) %>% summarize(nmoving = sum(moving)) %>% as.data.frame
  
  #Now look at the number of moving fish in each unique location,
  #Need to do this because there are duplicates
  no_dupes <- nfish_moved %>% group_by(x, y) %>% summarize(nmoving = sum(moving)) %>% 
                as.data.frame 
  
  #Add in unq columns for joining
  no_dupes$unq <- paste(no_dupes$x, no_dupes$y)
  nfish_moved$unq <- paste(nfish_moved$x, nfish_moved$y)
  
  #Join based on each unique location
  nfish_moved1 <- left_join(nfish_moved[-which(duplicated(nfish_moved$unq)), ], 
            no_dupes[, c('nmoving', 'unq')], by = 'unq')
  nfish_moved1$moving <- nfish_moved1$nmoving
  nfish_moved1$moved <- nfish_moved1$value - nfish_moved1$moving
  nfish_moved1$nmoving <- NULL
  
  #Update moved column with moved_in
  # fish_locs <- which(nfish_moved1$moving == 0)
  fish_locs <- paste(location$x, location$y)
  fish_locs <- which(nfish_moved1$unq %in% fish_locs)

# if((length(nfish_moved1[fish_locs, 'value'] ) == length(moved_in$nmoving)) == FALSE) browser()

  nfish_moved1[fish_locs, 'moved'] <- nfish_moved1[fish_locs, 'value'] + moved_in$nmoving
  
  #Clean up the data frames
  nfish_moved1$loc <- NULL
  nfish_moved1$unq <- NULL

  #replace with 
  ff_melt$unq <- paste(ff_melt$x, ff_melt$y)
  nfish_moved1$unq <- paste(nfish_moved1$x, nfish_moved1$y)

  nfish_moved1$check <- ff_melt[ff_melt$unq %in% nfish_moved1$unq, 'value']

  return(list(fish_area = ff_melt, nfish_moved = nfish_moved1))

}

  