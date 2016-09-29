#' Conduct survey at nlocaitons and save cpue for each drop
#'
#' 
#' Exploratory function more than anything
#'@param numrow number of rows to include in fishing space
#'@param numcol number of columns to include in fishing space
#'@param nfish number of fish to populate matrix
#'@param seed set seed for sampling
#'@param numlocs number of locations to fish in. default to 100
#'@param distribute specify fish distribution, 'patchy', 'uniform', or 'area'
#'@param percent percentage of fish to populate
#'@param location_list list specifying rows and columns to survey in
#'@param scope the scope of fishing movement, default to 1 so fish in surrounding 1 cells can move in
#'@param nhooks number of hooks at the smallest sampling size
#'@param ndrops number of drops, default is 5 following hook and line protocol
#' @keywords exploratory analysis
#' @export
#' @examples
#' put example here 
#'

#Look at number of locations
explore_nlocs_cpue <- function(numrow, numcol, nfish, seed = 300, numlocs = 100, distribute,
  percent, scope = 1, nhooks = 15, ndrops = 5,...){
  
  #define locations vectors and objects
  locations <- expand.grid(1:numrow, 1:numcol)
  locations <- data.frame(x = locations[, 1], y = locations[, 2])
  names(locations) <- c("", "")

  set.seed(seed)
  locations.list <- sapply(1:numlocs, FUN = function(x) sample(1:nrow(locations), x, 
    replace = FALSE))
  avg.cpue <- vector(length = length(locations.list))
  cpue.list <- vector('list', length = length(locations.list))

  #simulate fishing
  for(ii in 1:length(locations.list)){
    if(ii %% 10 == 0) print(ii)
    
    temp.locs <- vector('list', length = length(locations.list[[ii]]))
    
    for(jj in 1:length(temp.locs)){
      temp.locs[[jj]] <- c(locations[locations.list[[ii]][jj], 1], locations[locations.list[[ii]][jj], 2])
    }

    init <- initialize_population(numrow = numrow, numcol = numcol, nfish = nfish, 
      distribute = distribute, seed = seed, percent = percent)

    temp <- conduct_survey(fish_area = init, location_list = temp.locs, scope = scope, 
      nhooks = nhooks, ndrops = ndrops)

    cpue.list[[ii]] <- temp$cpue
  }

  #Convert list into data frame
  names(cpue.list) <- 1:numlocs
  cpues <- ldply(cpue.list)
  #Just average all things
  # avg.cpue <- sapply(cpue.list, FUN = function(x) mean(unlist(x)))
  
  # plot(1:numlocs, avg.cpue, type = 'o', pch = 19, ylim = c(0, 1), xlim = c(0, numlocs + 1))

  return(cpues)
}






