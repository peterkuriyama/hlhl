#' parse_master_list
#'
#' This function initializes the spatial distribution of the fish population
#' @param master_list output from survey_over_years
#' @keywords initialize
#' @export
#' @examples 
#' xx <- survey_over_years(numrow = 10, numcol = 10, nfish = 10000, 
#'   distribute = 'uniform',
#'   seed = 300, nyears = 15, location_list, 
#'   random_locations = TRUE, nlocs = 10, move_func = move_fish_cw, move_prob = .8)

#'thing <- parse_master_list(xx)

parse_master_list <- function(master_list){
  temp <- suppressMessages(melt(master_list))
  names(temp) <- c('row', 'column', 'value', 'location', 'variable',
    'output_type', 'year')

  #number of fish available at beginning of year
  init_nfish <- subset(temp, output_type == 'init_area')
  init_nfish$location <- NULL
  init_nfish$variable <- NULL
  init_nfish$output_type <- NULL
  
  #number of samples
  nsamples <- subset(temp, output_type == 'samples')
  nsamples <- nsamples[, which(names(nsamples) %in% c('row', 'column', 'output_type') == FALSE)]
  nsamples$row <- sapply(nsamples$location, function(x) eval(parse(text = x))[1])
  nsamples$column <- sapply(nsamples$location, function(x) eval(parse(text = x))[2])

  #number of fish after sampling
  end_nfish <- subset(temp, output_type == 'sampled_area')
  end_nfish <- end_nfish[, which(names(end_nfish) %in% 
    c('location', 'variable', 'output_type') == FALSE)]  

  #cpue
  cpue <- subset(temp, output_type == 'cpue')
  cpue <- cpue[, which(names(cpue) %in% 
    c('row', 'column', 'output_type') == FALSE)]  
  cpue$row <- sapply(cpue$location, function(x) eval(parse(text = x))[1])
  cpue$column <- sapply(cpue$location, function(x) eval(parse(text = x))[2])

  return(list(init_nfish = init_nfish, end_nfish = end_nfish, samples = nsamples, cpue = cpue))

}
