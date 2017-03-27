#' Pick Sites based on probability

#' Function to pick number of good, medium, and bad sites
#' @param nsites Number of sites to select
#' @param fish_mat Matrix of fish
#' @param samp_option Sample option, either random or for best locations
#' @export

pick_sites_prob <- function(nsites, fish_mat, samp_option){
  fishes <- fish_mat
  fishes <- melt(fishes)
  names(fishes) <- c('x', 'y', 'value')

  #Option1
  #option to sample all sites, sample randomly
  if(samp_option == 'random'){
    samp_ind <- sample(1:nrow(fishes), size = nsites, replace = FALSE)  
  }
  
  if(samp_option == 'best'){
    
    #option to sample only sites with fish
    fish_sites <- subset(fishes, value != 0)
    
    #define probabilities based on number of locations
    fish_sites$prob <- fish_sites$value / sum(fish_sites$value)
    
    samp_ind <- sample(rownames(fish_sites), size = nsites, 
      prob = fish_sites$prob, replace = FALSE)
    samp_ind <- as.integer(samp_ind)  
  }
  
  locs_out <- fishes[samp_ind, ]
  locs_out$vessel <- 1
  locs_out <- locs_out[, c('vessel', 'x', 'y')]
  return(locs_out)
}