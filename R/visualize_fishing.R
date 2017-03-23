#' Visualize Fishing location
#' Function to visualize fishing location

#' @param loc Fishing location
#' @param fish_mat Matrix of initial fish distribution
#' @export

visualize_fishing <- function(loc, fish_mat){
  
  #Pull out fishing locs
  fishes <- fish_mat
  fishes <- melt(fishes)
  names(fishes) <- c('x', 'y', 'value')
  
  fishes$int <- findInterval(fishes$value, quantile(fishes$value))
  
  #Modify the values if things are the same
  if(quantile(fishes$value)[1] == quantile(fishes$value)[2]){
    fishes[which(fishes$value == quantile(fishes$value)[2]), 'int'] <- 1
  }
    
  #What to do if there is no middle quantile?
  fishes$status <- 999  
  
  #if three of these things
  if(sum(which(quantile(fishes$value) == 0) %in% 1:3) == 3){
    print('med and bad are the same')
    fishes[which(fishes$int >= 4), 'status'] <- 'best'
    fishes[which(fishes$int == 3), 'status'] <- 'med'
    fishes[which(fishes$int == 1), 'status'] <- 'bad'
  
  }
  
  if(sum(which(quantile(fishes$value) == 0) %in% 1:3) != 3){
  #Sample sites based on quantiles
    fishes[which(fishes$int >= 3), 'status'] <- 'best'
    fishes[which(fishes$int == 2), 'status'] <- 'med'
    fishes[which(fishes$int == 1), 'status'] <- 'bad'
  }
  
  fishes <- full_join(fishes, loc, by = c('x', 'y'))
  names(fishes)[6] <- 'fished'
  
  fishes[which(is.na(fishes$fished)), 'fished'] <- "no"
  fishes[which(fishes$fished == 1), 'fished'] <- 'yes'
  
  fishes$status <- factor(fishes$status, levels = c('bad', 'med', 'best'))

browser()  

  ggplot() + geom_histogram(data = fishes, aes(x = value), alpha = .3, colour = 'black') +  
    geom_histogram(data = subset(fishes, fished == 'yes'), aes(x = value),
      alpha = .3, fill = 'red') + facet_wrap(~ status) + theme_bw()
  
}
