#'Fish the Population
#'
#'Function to fish the population
#' @param fish_area Matrix with the distribution of fish
#' @param ... Arguments specified in previous function

# '@examples

##' control <- make_ctl()
##' init <- initialize_population(control)
##' fish_population(fish_area = init, ctl = control)
##'

#'@export

fish_population <- function(fish_area, ...){
  #Should have Four steps or so
  #Move Fish
  #Catch Fish
  #Fish Die
  #Move Fish again
  
  #define things early
  location <- list(...)$location
  ndrops <- list(...)$ndrops
  scope <- list(...)$scope
  numrow <- list(...)$numrow
  numcol <- list(...)$numcol
  nhooks <- list(...)$nhooks
  nangs <- list(...)$nangs
  prob1 <- list(...)$prob1
  prob2 <- list(...)$prob2
  comp_coeff <- list(...)$comp_coeff
  mortality <- list(...)$mortality

  if(class(location) != "data.frame") stop("location must be a data frame")
  
  #Add on samples for each drop into location data frame  
  add_ons <- as.data.frame(matrix(999, nrow = nrow(location), ncol = ndrops, byrow = FALSE))
  names(add_ons) <- paste0('drop', 1:ndrops)
  location <- cbind(location, add_ons)
  location_angler <- vector('list', length = nrow(location))

  #convert to list
  fish_area_list <- lapply(fish_area, FUN = function(x) melt(x))

  ##---------------------------------------------------------------------------------------
  #Do all this if there are fishing locations specified

  if(sum(location$x %in% 0) == 0){
    ##---------------------------------------------------------------------------------------    
    #Add Recruitment if in a recruitment year
    #year 1 is when kk = 0 here
    # if(kk == 0) kk <- 1

    # if(sum(kk %in% rec_years)){
    #   fish_area <- lapply(fish_area, FUN = function(xx){
    #                         xx + round(xx * rec_rate)
    #                       }) 
    # } 

    ##---------------------------------------------------------------------------------------
    #Move Fish into locations
    #Call this fish_temp because to keep the original, and
    fish_temp <- lapply(fish_area, FUN = function(z){
        move_fish_loop(location = location, ff = z, scope = scope)
    })
  
    #convert fish_area into matrices
    to_fish <- lapply(fish_temp, FUN = function(x){
      matrix(x$fish_area$value, nrow = numrow, ncol = numcol, byrow = FALSE)
    })

    nfish_moved <- lapply(fish_temp, FUN = function(x){
      yy <- x$nfish_moved
      yy$moved <- yy$check
      return(yy)
    })

    ##---------------------------------------------------------------------------------------
    #Fish with sample_exp function
    #Do this in a for loop first, then maybe switch to an apply statement
    #Declare objects to track stuff
    samps_out <- as.data.frame(matrix(nrow = nrow(location), ncol = 5))
    names(samps_out) <- c('vessel', 'x', 'y', 'fish1samp', 'fish2samp')
    
    temp_fish_area <- to_fish
    temp_fish_area_orig <- temp_fish_area
  
    samps_out_drop <- vector('list', length = ndrops)
    fish_area_drop <- samps_out_drop



    #Loop through drops and store catch  
    for(dd in 1:ndrops){
      for(ll in 1:nrow(location)){
        temp <- fish_pop_loop(fish_area = temp_fish_area, loc_row = location[ll, ],
          nhooks = nhooks, nangs = nangs, prob1 = prob1, prob2 = prob2, comp_coeff = comp_coeff)   
        temp_fish_area <- temp$fish_area  
        samps_out[ll, ] <- temp$samps
      }
      samps_out_drop[[dd]] <- samps_out
      fish_area_drop[[dd]] <- temp_fish_area
    }

    names(samps_out_drop) <- paste0('drop', 1:ndrops)
    samps_out_drop <- ldply(samps_out_drop)
    names(samps_out_drop)[1] <- 'drop'
  
    #Compress samps_out for nfish_back function
    samps_out <- samps_out_drop %>% group_by(x, y) %>% summarize(fish1samp = sum(fish1samp), 
      fish2samp = sum(fish2samp)) %>% as.data.frame  

   ##---------------------------------------------------------------------------------------
   # Move Fish Back
    nfish_back <- move_back(nfish_moved = nfish_moved, samps_out = samps_out, scope = scope,
      fish_area = temp_fish_area, fish_area_orig = temp_fish_area_orig) 
   
   #Add these into the overall fish_area
   fish_out <- vector('list', length = 2)
  
   for(uu in 1:length(fish_out)){
     ttry <- fish_temp[[uu]]$fish_area  
  
     fish_out1 <- invisible(left_join( ttry[, c('x', 'y', 'value')], nfish_back[[uu]][, c('x', 'y', 'final')], 
           by = c('x', 'y')))
  
     na_ind <- is.na(fish_out1$final)
     fish_out1[na_ind, 'final'] <- fish_out1[na_ind, 'value']
     fish_out[[uu]] <- matrix(fish_out1$final, nrow = numrow, ncol = numcol, 
        byrow = FALSE)
    }    
  }

 ##---------------------------------------------------------------------------------------
 #If there's no fishing, make sure that the output has the same
 if(sum(location$x %in% 0) != 0){
  fish_out <- fish_area
  samps_out <- data.frame(x = location$x, y = location$y, fish1samp = 0, fish2samp = 0)
 }

  ##---------------------------------------------------------------------------------------
  #Add in Mortality
  #Add in rounded mortality numbers
  fish_out <- lapply(fish_out, FUN = function(x){
    x - round(x * mortality)    
  })

  #Convert all negative numbers to 0
  fish_out[[1]][which(fish_out[[1]] < 0)] <- 0
  fish_out[[2]][which(fish_out[[2]] < 0)] <- 0
  ##---------------------------------------------------------------------------------------
  #Return the fish areas  
  return(list(updated_area = fish_out, angler_samples = samps_out))
  
}







