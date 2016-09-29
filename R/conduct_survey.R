#' Conduct Survey
#' Wrapper to repeatedly run fish_population function
#' Give function arguments for
#' This function initializes the spatial distribution of the fish population

#'@param fish_area Matrix with the distribution of fish
# '@param location_list list specifying rows and columns to survey in
#'@param location Data frame of locations with column for vessel, rows, and columns of fish are to fish in. 
#'@param scope the scope of fishing movement, default to 1 so fish in surrounding 1 cells can move in
#'@param nhooks number of hooks at the smallest sampling size
#'@param ndrops number of drops, default is 5 following hook and line protocol
#'@param process specify process by which fish are sampled, options are 'multinomial' and 'hypergeometric'
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

conduct_survey <- function(fish_area, location_list, scope, nhooks, ndrops, 
  ...){
# browser()
  #keep initial population matrix
  init_area <- fish_area

#Browser to see how thigns work out


location <- data.frame(vessel = c(1, 1, 2), x = c(3, 3, 8), 
      y = c(3, 5, 8))


# browser()
  #Convert all the list structure into data frame structure
  temp <- fish_population(fish_area = fish_area, location = location, scope = scope, 
    nhooks = nhooks, ndrops = ndrops, ...)

  ####Make sure to set seed#####

  #calculate cpue based on number of hooks
  cpue <- temp$samples[, grep('drop', names(temp$samples))] / nhooks
  cpue <- cbind(temp$samples[, 1:3], cpue)

  out <- list(init_area = init_area, sampled_area = temp$updated_area, 
    samples = temp$samples, cpue = cpue)

  #Name sample list
  # sample_list <- vector('list', length = length(location_list))
  # names(sample_list) <- paste(location_list)

  #Sample at locations with for loop
# browser()
  # for(zz in 1:length(sample_list)){
  #   # if(zz == 5 & get('pp', parent.frame()) == 2) browser()
  #   # temp <- fish_population(fish_area = fish_area, location = location_list[[zz]], scope = scope,
  #   #                 nhooks = nhooks, ndrops = ndrops, process = process, ...)
  #   temp <- fish_population(fish_area = fish_area, location = location_list[[zz]], scope = scope,
  #                   nhooks = nhooks, ndrops = ndrops, ...)
  #   fish_area <- temp[[1]]
  #   sample_list[[zz]] <- temp[[2]]
  # }

  # samples <- ldply(sample_list)
  # names(samples) <- c('location', paste0('drop', 1:ndrops))
  # cpue <- samples
  # cpue[2:(ndrops + 1)] <- cpue[2:(ndrops + 1)] / nhooks

  # out <- list(init_area = init_area, sampled_area = fish_area, samples = samples,
  #      cpue = cpue)

  return(out)
}
