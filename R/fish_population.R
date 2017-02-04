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

fish_population <- function(fish_area, ctl, kk = 0){
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
  #Move Fish into locations
  #Call this fish_temp because to keep the original, and

  fish_temp <- lapply(fish_area, FUN = function(z){
      move_fish_loop(location = location, ff = z)
  })

  #convert fish_area into matrices
  to_fish <- lapply(fish_temp, FUN = function(x){
    matrix(x$fish_area$value, nrow = ctl$numrow, ncol = ctl$numcol)
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
        ctl = ctl, kk = kk)   

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
 # Need to remove this eventually
 #Need to make sure that the move back uses the same info
 if(ctl$scope != 0){
   nfish_back <- move_back(nfish_moved = nfish_moved, samps_out = samps_out, kk = kk, ctl = ctl,
    fish_area = temp_fish_area, fish_area_orig = temp_fish_area_orig) 
 }
 
 #Add these into the overall fish_area
 fish_out <- vector('list', length = 2)

 for(uu in 1:length(fish_out)){
   ttry <- fish_temp[[uu]]$fish_area  

   fish_out1 <- invisible(left_join( ttry[, c('x', 'y', 'value')], nfish_back[[uu]][, c('x', 'y', 'final')], 
         by = c('x', 'y')))

   na_ind <- is.na(fish_out1$final)
   fish_out1[na_ind, 'final'] <- fish_out1[na_ind, 'value']
   fish_out[[uu]] <- matrix(fish_out1$final, nrow = ctl$numrow, ncol = ctl$numcol, 
      byrow = TRUE)
  }

 ##---------------------------------------------------------------------------------------
 #Add in Mortality
 
  #Add in rounded mortality numbers
  inst_mort <- exp(mortality) / 100 #convert continuous to instantaneous
  if(mortality == 0) inst_mort <- 0

  fish_out <- lapply(fish_out, FUN = function(x){
    x - round(x * inst_mort)
  })

  ##---------------------------------------------------------------------------------------
  #Return the fish areas  
  return(list(updated_area = fish_out, angler_samples = samps_out))
  
}







