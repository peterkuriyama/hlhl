#'Make Control
#'
#'Define arguments for hook and line simulations
#' Used in initialize_population
#' @param numrow Number of rows in matrix
#' @param numcol Number of columns in matrix
#' @param nfish1 Number of fish1 to allocate among matrix
#' @param nfish2 Number of fish2 to allocate among matrix
#' @param prob1 Probability of catching species 1
#' @param prob2 Probability of catching species 2
#' @param seed Set seed if distribute == random, defaults to 300
#' @param distribute Specify fish distribution to be 'uniform', 'patchy', or 'area' specific
#' @param maxfish Maximum number of fish that can be sampled at a time; default is 10
#' @param percent percentage of area to sample. Only necessary if distribute == 'patchy'; default is 0.3
#' @param area Specify area to distribute fish, options are 'upperleft', 'upperright', 'lowerleft', 
#' 'lowerright', 'upperhalf', 'lowerhalf', 'righthalf', 'lefthalf'  Only necessary if distribute == 'area'
#'@param location Data frame of locations with column for vessel, rows, and columns of fish are to fish in. 
#'@param scope the scope of fishing movement, default to 1 so fish in surrounding 1 cells can move in; default is 1
#'@param nhooks number of hooks at the smallest sampling size; default is 5
#'@param ndrops number of drops, default is 5 following hook and line protocol; default is 5
#'@param process specify process by which fish are sampled, options are 'multinomial', 'hypergeometric', and 'equal_prob'. 
#' 'equal_prob' is based on CIE comments from 2012. 
#'@param p0 Probability that fish detects gear, used in the hook_prob function; default is 0.4  
#' @param nyear Number of years in survey; default is 10
#' @param browser Switch to turn on browser for debugging
#' @param cpue_method Method of aggregating cpue. For use in calc_cpue function. Currently
#' @param mortality Natural mortality values, can be input as single value or matrix
#' options are 'average', '75hooks' to aggregate by hooks, and 'weighted_average' is in development

#' @examples
#' make_ctl(p0 = .2)

#' @export

make_ctl <- function(numrow = 10, numcol = 10, nfish1 = 10000, #initial number of fish
  nfish2 = 0,
  prob1 = .1, #Probability of catching species1
  prob2 = 0, #Probability of catching species2
  distribute = 'uniform', #distribution of fish, could be uniform, or patchy
  maxfish = 10,
  percent = .3, #percentage of area to sample, only necessary if distribute = 'patchy'
  area = NULL,
  seed = 300,

  #Arguments for function that fishes population
  location = data.frame(vessel = c(1, 1, 2),
                        x = c(3, 3, 8),
                        y = c(3, 5, 8)),
  scope = 1,
  nhooks = 5,
  ndrops = 5, 
  process = 'equal_prob',
  p0 = .4, #probability that fish are attracted to gear 
  nyear = 10, #number of times to repeat fish_population
  browser = FALSE,
  cpue_method = 'average',
  mortality
  ){
# browser()
  control <- list(numrow = numrow, numcol = numcol, nfish1 = nfish1, nfish2 = nfish2, prob1 = prob1, 
    prob2 = prob2,distribute = distribute,
    maxfish = maxfish, percent = percent, area = area, seed = seed, location = location, 
    scope = scope, nhooks = nhooks, ndrops = ndrops, process = process, p0 = p0, 
    nyear = nyear, browser = browser,
    cpue_method = cpue_method, mortality = mortality)

  return(control)

}