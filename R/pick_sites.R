#' Pick Sites

#' Function to pick number of good, medium, and bad sites
#' @param ctl Control file
#' @param nbest Number of best/good sites to samle
#' @param nmed Number of medium sites to samle
#' @param nbest Number of bad sites to samle
#' @export

pick_sites <- function(nbest = 0, nmed = 0, nbad = 0, fish_mat){
  fishes <- fish_mat
  fishes <- melt(fishes)
  names(fishes) <- c('x', 'y', 'value')

  fishes$int <- findInterval(fishes$value, quantile(fishes$value))

  #Modify the values if things are the same
  if(quantile(fishes$value)[1] == quantile(fishes$value)[2]){
    fishes[which(fishes$value == quantile(fishes$value)[2]), 'int'] <- 1
  }
  
  #if three of these things
  if(sum(which(quantile(fishes$value) == 0) %in% 1:3) == 3){
    print('med and bad are the same')
    best <- sample(which(fishes$int >= 4), size = nbest, replace = FALSE)
    med <- sample(which(fishes$int == 3), size = nmed, replace = FALSE)
    bad <- sample(which(fishes$int == 1 | fishes$int == 2), size = nbad, replace = FALSE)
  }

  if(sum(which(quantile(fishes$value) == 0) %in% 1:3) != 3){
  #Sample sites based on quantiles
    best <- sample(which(fishes$int >= 3), size = nbest, replace = FALSE)
    med <- sample(which(fishes$int == 2), size = nmed, replace = FALSE)
    bad <- sample(which(fishes$int == 1), size = nbad, replace = FALSE)
  }

  locs_out <- fishes[c(best, med, bad), ]
  locs_out$vessel <- 1
  locs_out <- locs_out[, c('vessel', 'x', 'y')]
  return(locs_out)
}