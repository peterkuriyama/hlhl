#' Conduct Survey
#' Wrapper to repeatedly run fish_population function
#' Give function arguments for
#' This function initializes the spatial distribution of the fish population

#' @keywords survey
#' @param init_area list of intialized areas
#' @param ... arguments from other thing
#' @export
#' @examples
#'
#' ctl <- make_ctl()
#' conduct_survey(ctl)

conduct_survey <- function(init_area, ...){    
  nnyear <- list(...)$nyear
  nnangs <- list(...)$nangs
  nnhooks <- list(...)$nhook
  nndrops <- list(...)$ndrops

  drop_samples <- vector('list', length = nnyear)
  fished_areas <- vector('list', length = nnyear)
  
  names(drop_samples) <- 1:nnyear
  names(fished_areas) <- 1:nnyear  

  #Fish Area Once
  after_first <- fish_population(fish_area = init_area,...)

  drop_samples[[1]] <- after_first$angler_samples
  fished_areas[[1]] <- after_first$updated_area

  temp_area <- after_first$updated_area #Define temporary area for use in the for loop

  #Specify movement function if there is one
  # movement_function <- ctl$movement_function

  #Get into this loop if nnyear is > 1  
  if(nnyear > 1){
    #Loop over years of survey, specified in ctl
    for(kk in 2:nnyear){
    
      #Leave this in case I want to add movement in 
      # temp_area[[1]] <- movement_function(temp_area[[1]], max_prob = ctl$max_prob, min_prob = ctl$min_prob)$final
      # temp_area[[2]] <- movement_function(temp_area[[2]], max_prob = ctl$max_prob, min_prob = ctl$min_prob)$final    
      temp <- fish_population(fish_area = temp_area, ...)
  
      # survey_samples[[kk]] <- temp$samples
      drop_samples[[kk]] <- temp$angler_samples
      fished_areas[[kk]] <- temp$updated_area
  
      #Redefine the fishing area
      temp_area <- temp$updated_area    
    }    
  }


  #manipulate samples  
  samples <- ldply(drop_samples)
  names(samples)[1] <- 'year'

  #calculate CPUE based on number of hooks
  effort <- nnhooks * nnangs * nndrops
  samples$cpue1 <- samples$fish1samp / effort
  samples$cpue2 <- samples$fish2samp / effort

  fished_areas[[nnyear + 1]] <- init_area
  names(fished_areas) <- c(paste0('year', 1:nnyear), 'year0')
  
  #define output list
  out <- list(fished_areas = fished_areas, samples = samples)
   
  return(out)
}
