#' Move Fish Clockwise
#'
#' This function is meant to simulate fishing moving offshore. One function call results in 
#' fish moving one column to the left with some probability (specified in the arguments). 
#' Probabilities decline evenly by column (highest on furthest left column).
#' @param fish_area Input matrix of distributed fish
#' @param move_prob probability of moving from one quadrant to the next
#' @keywords movement
#' @export
#' @examples 
#' 

move_fish_cw <- function(fish_area, move_prob, seed = 300, ...){
  numrow <- nrow(fish_area)
  numcol <- ncol(fish_area)
  
  #Convert fish_area into a data frame to facilitate calculations
  fish_area_df <- melt(fish_area)
  names(fish_area_df) <- c('row', 'column', 'nfish')

  #add quadrants to fish_area_df
  #upper left
  fish_area_df$quadrant <-   
  #upper left
  fish_area_df[which(fish_area_df$row %in% 1:(numrow / 2) & 
                     fish_area_df$column %in% 1:(numcol / 2)), 'quadrant'] <- 'ul'
  #upper right
  fish_area_df[which(fish_area_df$row %in% 1:(numrow / 2) & 
                     fish_area_df$column %in% ((numcol / 2) + 1):numcol), 'quadrant'] <- 'ur'
  
  #lower left
  fish_area_df[which(fish_area_df$row %in% ((numrow / 2) + 1):numrow & 
                     fish_area_df$column %in% 1:(numcol / 2)), 'quadrant'] <- 'll'
  #lower right
  fish_area_df[which(fish_area_df$row %in% ((numrow / 2) + 1):numrow & 
                     fish_area_df$column %in% ((numcol / 2) + 1):numcol), 'quadrant'] <- 'lr'

  #matrix(fish_area_df$quadrant, nrow = numrow, ncol = numcol, byrow = FALSE)
  #sample fish in each cell then move to corresponding place in cw region
  set.seed(seed)
  fish_area_df$sample <- rbinom(n = length(fish_area_df$nfish),
    size = fish_area_df$nfish, prob = move_prob)
  fish_area_df$nfish_left <- fish_area_df$nfish - fish_area_df$sample

  moving_fish <- fish_area_df
  moving_fish$nfish <- NULL
  moving_fish$nfish_left <- NULL
  
  #move fish based on what quadrant they're in  
  #upper left move to lower left
  moving_fish[which(moving_fish$quadrant == 'ul'), 'row'] <- 
    moving_fish[which(moving_fish$quadrant == 'ul'), 'row'] + numrow / 2

  #lower left move to lower right
  moving_fish[which(moving_fish$quadrant == 'll'), 'column'] <- 
    moving_fish[which(moving_fish$quadrant == 'll'), 'column'] + numcol / 2

  #lower right move to upper right
  moving_fish[which(moving_fish$quadrant == 'lr'), 'row'] <- 
    moving_fish[which(moving_fish$quadrant == 'lr'), 'row'] - numrow / 2

  #upper right move to upper left
  moving_fish[which(moving_fish$quadrant == 'ur'), 'column'] <- 
    moving_fish[which(moving_fish$quadrant == 'ur'), 'column'] - numcol / 2


  merged_fish <- merge(fish_area_df[, c('row', 'column', 'nfish_left')],
                   moving_fish[, c('row', 'column', 'sample')], 
                   by = c('row', 'column'), all = TRUE)


  merged_fish$final_fish <- merged_fish$nfish_left + merged_fish$sample


  out <- matrix(0, nrow = numrow, ncol = numcol)
  for(rr in 1:nrow(merged_fish)){
    temp <- merged_fish[rr, ]
    out[temp$row, temp$col] <- temp$final_fish
  }

  return(list(init = fish_area, final = out))
}

  
  

