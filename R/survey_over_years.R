#' Initialize Population
#'
#' This function initializes the spatial distribution of the fish population
#' @param numrow Number of rows in matrix
#' @param numcol Number of columns in matrix
#' @param nfish Number of fish to allocate among matrix
#' @param seed Set seed if distribute == random, defaults to 300
#' @param distribute Specify fish distribution to be 'uniform', 'patchy', or 'area' specific
#' @param maxfish Maximum number of fish that can be sampled at a time
#' @param percent percentage of area to sample. Only necessary if distribute == 'patchy'
#' @param area Specify area to distribute fish, options are 'upperleft', 'upperright', 'lowerleft', 'lowerright',
#' 'upperhalf', 'lowerhalf', 'righthalf', 'lefthalf' 
#'   Only necessary if distribute == 'area'
#' @keywords initialize
#' @export
#' @examples 
#' 
#' Uniformly distribute fish
#' initialize_population(numrow = 10, numcol = 10, nfish = 1000, distribute = 'uniform')
#' 
#' Distribute fish in upper right quadrant only
#' initialize_population(numrow = 10, numcol = 10, nfish = 1000, distribute = 'area',
#'                             area = 'upperright')
#'
#' Distribute fish in upper half
#' initialize_population(numrow = 10, numcol = 10, nfish = 1000, distribute = 'area',
#'                             area = 'upperhalf')
#'
#' Patchily distribute fish
#' initialize_population(numrow = 10, numcol = 10, nfish = 100, distribute = 'patchy',
#'   percent = .2)
#'
#' 

survey_over_years <- function(numrow = 10, numcol = 10, nfish = 1000, distribute,
  seed = 300, nyears = 15, location_list, random_locations = FALSE, nlocs = 10,
  move_func, nhooks, ndrops, scope, ...){
# browser()  
  init <- initialize_population(numrow = numrow, numcol = numcol, nfish = nfish, 
        distribute = distribute, seed = seed, ...)

# browser()  

  #Specify locations to fish if random locations is TRUE
  if(random_locations == TRUE){
    potential_location <- expand.grid(1:numrow, 1:numcol)
    
    location_list <- vector('list', length = nyears)

    for(pp in 1:nyears){
      sampled <- base::sample(1:nrow(potential_location), replace = FALSE, size = nlocs)
      temp_list <- potential_location[sampled, ]
      temp_list <- split(temp_list, f = seq(nrow(temp_list)))
      temp_list <- lapply(temp_list, FUN = function(x) c(as.numeric(x[1]), 
        as.numeric(x[2])))
      names(temp_list) <- NULL

      location_list[[pp]] <- temp_list
    }
    
  }

  #conduct nyears iterations
  temp_area <- init
  master_list <- vector('list', length = nyears)
  cpue_list <- vector('list', length = nyears)

  for(ii in 1:nyears){
    if(random_locations == TRUE){
      temp <- conduct_survey(fish_area = temp_area, location = location_list[[ii]], 
            scope = scope, nhooks = nhooks, ndrops = ndrops, ...)  
    }

    if(random_locations == FALSE){
     temp <- conduct_survey(fish_area = temp_area, location = location_list, 
            scope = scope, nhooks = nhooks, ndrops = ndrops, ...)   
    }
    
    master_list[[ii]] <- temp

    cpue <- temp$cpue
    cpue$year <- ii
    cpue_list[[ii]] <- cpue

    temp_area <- temp$sampled_area

    temp <- move_func(temp_area, ...)
    temp_area <- temp$final

  }
  
  #return master_list
  return(master_list)
}
