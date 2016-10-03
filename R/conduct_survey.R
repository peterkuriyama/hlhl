#' Conduct Survey
#' Wrapper to repeatedly run fish_population function
#' Give function arguments for
#' This function initializes the spatial distribution of the fish population

# #'@param fish_area Matrix with the distribution of fish
# # '@param location_list list specifying rows and columns to survey in
# #'@param location Data frame of locations with column for vessel, rows, and columns of fish are to fish in. 
# #'@param scope the scope of fishing movement, default to 1 so fish in surrounding 1 cells can move in
# #'@param nhooks number of hooks at the smallest sampling size
# #'@param ndrops number of drops, default is 5 following hook and line protocol
# #'@param process specify process by which fish are sampled, options are 'multinomial' and 'hypergeometric'
#' @keywords survey
#' @export
#' @examples
#' put example here dude
#'

#' init <- initialize_population(numrow = 10, numcol = 10, nfish = 10000, distribute = 'uniform',
#'                                percent = .3, seed = 301)

#' xx <- conduct_survey(fish_area = init, location_list = list(c(4, 10),
#'                                                        c(8, 2),
#'                                                        c(3, 3)), 
#'                      scope = 1, nhooks = 15, ndrops = 5, process = 'equal_prob')

conduct_survey <- function(ctl){
  # location, scope, nhooks, ndrops, 
  # ...){

  nyear <- ctl$nyear
  survey_samples <- vector('list', length = nyear)
  names(survey_samples) <- 1:nyear

  #initialize population
  init_area <- initialize_population(ctl = ctl)

  #Fish Area Once
  after_first <-fish_population(init_area, ctl = ctl)
  survey_samples[[1]] <- after_first$samples

  temp_area <- after_first$updated_area #Define temporary area for use in the for loop

  #Loop over years of survey, specified in ctl
  for(kk in 2:nyear){
    temp <- fish_population(fish_area = temp_area, ctl = ctl)
    survey_samples[[kk]] <- after_first$samples

    #Redefine the fishing area
    temp_area <- temp$updated_area

  }

  samples <- ldply(survey_samples)
  names(samples)[1] <- 'year'

  
  #calculate CPUE based on number of hooks
  nhooks <- ctl$nhooks
  cpue <- samples[, grep('drop',  names(samples))] / nhooks
  
  #Find columns that don't have 'drop' in the name
  to_bind <- samples[, 1:ncol(samples) %in% grep('drop',  names(samples)) == FALSE]
  
  cpue <- cbind(to_bind, cpue)

  #keep initial population matrix
  # init_area <- fish_area

  ####Make sure to set seed#####

  # #Convert all the list structure into data frame structure
  # temp <- fish_population(fish_area = fish_area, ctl = ctl)

  # nhooks <- ctl$nhooks
  
  # #calculate cpue based on number of hooks
  # cpue <- temp$samples[, grep('drop', names(temp$samples))] / nhooks
  # cpue <- cbind(temp$samples[, 1:3], cpue)

  out <- list(init_area = init_area, end_area = temp_area, 
    samples = samples, cpue = cpue)

  return(out)
}
