#' Move Fish Left
#'
#' This function is meant to simulate fishing moving offshore. One function call results in 
#' fish moving one column to the left with some probability (specified in the arguments). 
#' Probabilities decline evenly by column (highest on furthest left column).
#' @param fish_area Input matrix of distributed fish
#' @param max_prob maximum movement probability in the furthest left location
#' @param min_prob minimum movement probability in furthest right location
#' @keywords movement
#' @export
#' @examples 
#' 

move_fish_left <- function(fish_area, max_prob = 0.5, min_prob = 0.05, ...){
  numrow <- nrow(fish_area)
  numcol <- ncol(fish_area)

  #Convert fish_area into a data frame to facilitate calculations
  fish_area_df <- melt(fish_area)
  names(fish_area_df) <- c('row', 'column', 'nfish')

  #Define movement probabilities based on specified max and min probs
  col_fish <- colSums(fish_area) #summed fish by columns
  prob_fish <- col_fish
  probs <- seq(max_prob, min_prob, length.out = length(which(col_fish != 0)))
  prob_fish[prob_fish != 0] <- probs
  
  #Sample number of moving fish based on column
  fish_area_df$sampled <- apply(fish_area_df, MAR = 1, function(x) rbinom(n = 1, size = x['nfish'], 
    prob = prob_fish[x[2]]))
  fish_area_df$updated <- fish_area_df$nfish - fish_area_df$sampled

  #Split fish_area_df into two data columns
  #subtract 1 from columns in moving df
  #merge the two and update nfish  
  orig <- fish_area_df[, c('row', 'column', 'updated')]
  moving <- fish_area_df[, c('row', 'column', 'sampled')]
  moving$column <- moving$column - 1
  moving[which(moving$column == 0), 'column'] <- 1
  moving$column <- as.integer(moving$column)
  
  moving %>% group_by(row, column) %>% mutate(sampled = sum(sampled))

  moving %>% 
              group_by(row, column) %>% 
              summarise(sampled = sum(sampled)) %>% 
              as.data.frame


  moving <- moving %>% 
              group_by(row, column) %>% 
              summarise(sampled = sum(sampled)) %>% 
              as.data.frame

  out <- merge(orig, moving, by = c('row', 'column'), all = TRUE)
  out[is.na(out$sampled), 'sampled'] <- 0
  out$moved <- out$updated + out$sampled

  final <- matrix(out$moved, nrow = numrow, ncol = numcol, byrow = T)
  
  return(list(init = fish_area, final = final))
}


