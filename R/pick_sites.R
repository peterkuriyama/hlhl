#' Pick Sites

#' Function to pick number of good, medium, and bad sites
#' @param ctl Control file
#' @param nbest Number of best/good sites to samle
#' @param nmed Number of medium sites to samle
#' @param nbest Number of bad sites to samle
#' @export

pick_sites <- function(ctl = ctl, nbest = 0, nmed = 0, nbad = 0){
  #Set Seed
  set.seed(ctl$seed)
  
  #Initialize the population
  fishes <- initialize_population(ctl = ctl, nfish = ctl$nfish1)
  fishes <- melt(fishes)
  names(fishes) <- c('x', 'y', 'value')

  #Sample sites based on quantiles
  best <- sample(which(fishes$int >= 3), size = nbest, replace = FALSE)
  med <- sample(which(fishes$int == 2), size = nmed, replace = FALSE)
  bad <- sample(which(fishes$int == 1), size = nbad, replace = FALSE)


  locs_out <- fishes[c(best, med, bad), ]
  locs_out$vessel <- 1
  locs_out <- locs_out[, c('vessel', 'x', 'y')]
  return(locs_out)
}