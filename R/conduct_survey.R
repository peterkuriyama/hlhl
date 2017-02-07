#' Conduct Survey
#' Wrapper to repeatedly run fish_population function
#' Give function arguments for
#' This function initializes the spatial distribution of the fish population

#' @keywords survey
#' @param ctl control list defined in make_ctl function
#' @export
#' @examples
#'
#' ctl <- make_ctl()
#' conduct_survey(ctl)

conduct_survey <- function(ctl){
  # location, scope, nhooks, ndrops, 
  # ...){
  nyear <- ctl$nyear
  # survey_samples <- vector('list', length = nyear)
  drop_samples <- vector('list', length = nyear)
  fished_areas <- vector('list', length = nyear)
  
  # names(survey_samples) <- 1:nyear
  names(drop_samples) <- 1:nyear
  names(fished_areas) <- 1:nyear

  #initialize population  
  init_area1 <- initialize_population(ctl = ctl, nfish = ctl$nfish1)
  init_area2 <- initialize_population(ctl = ctl, nfish = ctl$nfish2)
  
  #Fish Area Once
  init_area <- list(init_area1, init_area2)
  after_first <- fish_population(init_area, ctl = ctl)

  drop_samples[[1]] <- after_first$angler_samples
  fished_areas[[1]] <- after_first$updated_area

  temp_area <- after_first$updated_area #Define temporary area for use in the for loop

  #Loop over years of survey, specified in ctl
  for(kk in 2:nyear){
# print(kk)    
    temp <- fish_population(fish_area = temp_area, ctl = ctl, kk = kk)

    # survey_samples[[kk]] <- temp$samples
    drop_samples[[kk]] <- temp$angler_samples
    fished_areas[[kk]] <- temp$updated_area

    #Redefine the fishing area
    temp_area <- temp$updated_area    

  }

  #manipulate samples  
  samples <- ldply(drop_samples)
  names(samples)[1] <- 'year'

  #calculate CPUE based on number of hooks
  effort <- ctl$nhooks * ctl$nangs * ctl$ndrops
  samples$cpue1 <- samples$fish1samp / effort
  samples$cpue2 <- samples$fish2samp / effort

  #Find columns that don't have 'drop' in the name
  # to_bind <- samples[, 1:ncol(samples) %in% grep('drop',  names(samples)) == FALSE]
  
  # cpue <- cbind(to_bind, cpue)

  fished_areas[[nyear + 1]] <- init_area
  names(fished_areas) <- c(paste0('year', 1:nyear), 'year0')
  
  #define output list
  out <- list(fished_areas = fished_areas, samples = samples)
   
  return(out)
}
