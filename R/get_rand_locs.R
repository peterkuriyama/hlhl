#' Get Random Fishing Locations
#'
#' Function to generate random fishing locations. 
#' @param seed Set seed
#' @param numrow Number of rows in fishing area
#' @param numcol Number of columns in fishing area
#' @param nlocs Maximum number of fishing locations

#'@examples
#' rand_locs <- get_rand_locs()
#'@export

get_rand_locs <- function(seed = 3, numrow = 10, numcol = 10,
  nlocs = 20){
  set.seed(seed)
  x <- sample(1:numrow, nlocs, replace = T)
  y <- sample(1:numrow, nlocs, replace = T)

  locs <- vector('list', length = nlocs)
  locs_df <- data.frame(vessel = 1, x = x, y = y)
  
  #Check to make sure there are no duplicate sites
  while(sum(duplicated(locs_df)) != 0){
    x <- sample(1:numrow, nlocs, replace = T)
    y <- sample(1:numrow, nlocs, replace = T)
  
    #Increase this up to 10 locations
    locs_df <- data.frame(vessel = 1, x = x, y = y)
    # print('still going')
  }

  for(ii in 1:nlocs){
    locs[[ii]] <- locs_df[1:ii, ]
  }

  return(locs)
}