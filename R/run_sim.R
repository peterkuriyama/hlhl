#'Run Simulation
#'
#'Define arguments for hook and line simulations; arguments the same as make_ctl function
#' Used in initialize_population
#' @param numrow Number of rows in matrix
#' @param numcol Number of columns in matrix
#' @param nfish Number of fish to allocate among matrix
#' @param seed Set seed if distribute == random, defaults to 300
#' @param distribute Specify fish distribution to be 'uniform', 'patchy', or 'area' specific
#' @param maxfish Maximum number of fish that can be sampled at a time
#' @param percent percentage of area to sample. Only necessary if distribute == 'patchy'
#' @param area Specify area to distribute fish, options are 'upperleft', 'upperright', 'lowerleft', 
#' 'lowerright', 'upperhalf', 'lowerhalf', 'righthalf', 'lefthalf'  Only necessary if distribute == 'area'
#'@param location Data frame of locations with column for vessel, rows, and columns of fish are to fish in. 
#'@param scope the scope of fishing movement, default to 1 so fish in surrounding 1 cells can move in
#'@param nhooks number of hooks at the smallest sampling size
#'@param ndrops number of drops, default is 5 following hook and line protocol
#'@param process specify process by which fish are sampled, options are 'multinomial', 'hypergeometric', and 'equal_prob'. 
#' 'equal_prob' is based on CIE comments from 2012. 
#'@param p0 Probability that fish detects gear, used in the hook_prob function
#' @param nyear Number of years in survey
#' @param browser Switch to turn on browser for debugging
#' @param cpue_method Method of aggregating cpue. For use in calc_cpue function. Currently
#' options are 'average', '75hooks' to aggregate by hooks, and 'weighted_average' is in development

#' @examples
#' run_sim(nhooks = 15, seed = 200, nfish = 1000, nyear = 15, distribute = 'patchy', 
#'        percent = .5, cpue_method = '75hooks')

#' @export

run_sim <- function(numrow = 10, numcol = 10, nfish = 10000, distribute = "uniform", 
  maxfish = 10, percent = 0.3, area = NULL, seed = 300, location = data.frame(vessel = c(1, 
      1, 2), x = c(3, 3, 8), y = c(3, 5, 8)), scope = 1, nhooks = 5, 
  ndrops = 5, process = "equal_prob", p0 = 0.4, nyear = 10, 
  browser = FALSE, cpue_method = "average"){
  
  #make ctl file
  ctl <- make_ctl(numrow = numrow, numcol = numcol, nfish = nfish, distribute = distribute,
    maxfish = maxfish, percent = percent, area = area, seed = seed, location = location, 
    scope = scope, nhooks = nhooks, ndrops = ndrops, process = process, p0 = p0, nyear = nyear,
    browser = browser, cpue_method = cpue_method)

  out <- conduct_survey(ctl = ctl)
  cpues <- calc_cpue(out, ctl = ctl)

  return(list(out = out, cpue = cpues))
}
