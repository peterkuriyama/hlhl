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

  for(zz in 1:nrow(location)){
    moves <- define_movement(fish_area = ff, x = location[zz, 'x'],
      y = location[zz, 'y'], scope = scope)
    
    moved <- move_fish(fish_range1 = moves$fish_range, nfish_outside1 = moves$nfish_outside,
      zero_index1 = moves$zero_index)
    orig_inds <- expand.grid(moves$row_range, moves$col_range)
    
    moved$x <- orig_inds$Var1
    moved$y <- orig_inds$Var2

    #Update the original fish area data frames
    ff <- melt(ff)
    update_these <- which(ff$Var1 %in% moved$x & ff$Var2 %in% moved$y)

    #Subtract moving fish
    ff[update_these, 'value'] <- ff[update_these, 'value'] - moved$moving
    ff[update_these, 'value'] <- moved$moved

    #convert ff to a matrix
    ff <- matrix(ff$value, nrow = ctl$numrow, ncol = ctl$numcol)

    nfish_moved[[zz]] <- moved
  }

  #use this to check stuff
  #Things that will check
  # ff_check <- melt(ff)
  # ff_orig <- melt(ff_orig)

  # ff2 <- inner_join(ff_check, ff_orig,  by = c("Var1", "Var2"))
  # ff2$diff <- ff2$value.y - ff2$value.x
  return(list(fish_area = ff, nfish_moved = nfish_moved))

}

  