#' Define Movement range of each location

#' Function to define movement range at specified locations
#' @param fish_area Area of fish
#' @param x X location of fishing
#' @param y Y location of fishing
#' @param scope Scope of fish movement

#' @export

define_movement <- function(fish_area, x, y, scope){
  row_range <- (x - scope): (x + scope)
  row_range <- row_range[row_range %in% 1:nrow(fish_area)] #If there's a border case maybe?
  
  col_range <- (y - scope):(y + scope)
  col_range <- col_range[col_range %in% 1:ncol(fish_area)]
  
  #Define range to fish
  fish_area_melted <- reshape2::melt(fish_area)
  
  #Use melted matrix because fish_area is easier to subset
  fish_range_melted <- subset(fish_area_melted, Var1 %in% row_range & Var2 %in% col_range)
  
  #Find the location of fishing
  fish_location_melted <- subset(fish_area_melted, Var1 == x & Var2 == y)
  
  #define zero index, this is the initial location of movement
  # zero_index <-  which(fish_range_melted$Var1 == location[ii, 'x'] & fish_range_melted$Var2 == location[ii, 'y'])
  zero_index <-  which(fish_range_melted$Var1 == x & fish_range_melted$Var2 == y)
  
  fish_range <- matrix(fish_range_melted$value, nrow = length(row_range), ncol = length(col_range))
  fish_in_loc <- fish_location_melted$value
  nfish_outside <- sum(fish_range) - fish_in_loc
  
  return(list(fish_range = fish_range, nfish_outside = nfish_outside, 
    zero_index = zero_index, row_range = row_range, col_range = col_range))
}