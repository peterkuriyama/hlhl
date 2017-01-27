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
  nfish_moved <- vector('list', length = 3)

  #melt ff to keep track of fish moving in and out
  ff_melt <- melt(ff)
  names(ff_melt)[1:2] <- c('x', 'y')

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

  #Remove duplicated rows
  nfish_moved <- ldply(nfish_moved)

  #Check duplicated
  duped <- which(duplicated(nfish_moved[, c('x', 'y')]))
  nfish_moved <- nfish_moved[-duped, ]

  #replace with 
  ff_melt$unq <- paste(ff_melt$x, ff_melt$y)
  nfish_moved$unq <- paste(nfish_moved$x, nfish_moved$y)

  nfish_moved$check <- ff_melt[ff_melt$unq %in% nfish_moved$unq, 'value']

  return(list(fish_area = ff_melt, nfish_moved = nfish_moved))

}

  