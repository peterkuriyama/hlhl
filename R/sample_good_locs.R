#' Sample Good Locations

#' Conduct survey in a proportion of locations with fish. 

#' @param ctl Control file
#' @param prop_good Proportion of good site to sample
#' @param ngoods Number of total good sites
#' @param which_spp Specify spp1 or spp2

#' @export

sample_good_locs <- function(ctl, prop_good, ngoods, which_spp){
  #Function to calculate the proportions of sampling locations that are good and bad
  set.seed(ctl$seed)
  
  nfish <- paste0("nfish", substr(which_spp, 4, 4))
  pick_from <- suppressWarnings(melt(initialize_population(ctl = ctl, nfish = as.numeric(ctl[nfish]))))
  
  goods <- sample(which(pick_from$value != 0), round(ngoods * prop_good))
  bads <- sample(which(pick_from$value == 0), ngoods - length(goods))
  picked <- pick_from[c(goods, bads), c('Var1', 'Var2')]
  names(picked) <- c('x', 'y')
  
  picked$vessel <- rep(1, nrow(picked))
  picked <- picked[, c('vessel', 'x', 'y')]
  
  return(picked)

}


