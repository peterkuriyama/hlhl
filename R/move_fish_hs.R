#' Move Fish based on Hotspots
#'
#' This function is meant to simulate fish moving towards hotspots. The idea is that rockfish
#' tend to aggregate around rocky structure. The user specifies hotspots and the
#' fish then move towards the hotspots with some probability max_prob.
#'
#' @param fish_area Input matrix of distributed fish. Use initialize population to specify this
#' @param hotspots Specify the location of hotspots as a data frame with the names 
#' @param max_prob Maximum probability of moving toward the hotspots. Cells adjacent to 
#' the hotspots have the highest probability, cells two cells from the hotspots have a probability
#' of moving that is half max_prob.

#' @keywords movement
#' @export
#' @examples 
#' move_fish_hs(fish_area = initialize_population(distribute = 'patchy', numrow = 10, numcol = 10, seed = 5,
#'                          nfish = 1000, percent = .50), 
#'              hotspots = data.frame('row' = c(3, 2), 'column' = c(3, 5)),
#'              max_prob = 0.2)

move_fish_hs <- function(fish_area, hotspots, max_prob = 0.5, ...){
  numrow <- nrow(fish_area)
  numcol <- ncol(fish_area)

  #Convert fish_area into a data frame to facilitate calculations
  fish_area_df <- melt(fish_area)
  names(fish_area_df) <- c('row', 'column', 'nfish')
  fish_area_df$unq <- paste(fish_area_df$row, fish_area_df$column)
  hotspots$unq <- paste(hotspots$row, hotspots$column)

  #Identify hotspots, 0 for not hotspot, 1 for hotspots
  # fish_area_df$hotspot <- 0
  # fish_area_df[which(fish_area_df$unq %in% hotspots$unq), 'hotspot'] <- 1
  
  
  #------------------------------------------------------------------------
  #loop through hotspots to assign movement probabilities
  
  #Assign movement probabilities basd on proximity to hotspots
  #specify object to store probabilities
  hotspots_prob <- as.data.frame(matrix(0, ncol = nrow(hotspots), nrow = nrow(fish_area_df)))
  hotspots_prob <- data.frame(row = fish_area_df$row, column = fish_area_df$column, hotspots_prob)
  
  #program in 
  for(ii in 1:nrow(hotspots)){
    temp_hs <- hotspots[ii, ] #temporary hotspots
    
    #for cells adjacent to hotspot
    closest <- which(fish_area_df$row <= temp_hs$row + 1 & 
                     fish_area_df$row >= temp_hs$row - 1 & 
                     fish_area_df$column <= temp_hs$column + 1 & 
                     fish_area_df$column >= temp_hs$column - 1)
    hotspots_prob[closest, ii + 2] <- max_prob

    #for cells two cells away from hotspot
    further <- which(fish_area_df$row <= temp_hs$row + 2 & 
                     fish_area_df$row >= temp_hs$row - 2 & 
                     fish_area_df$column <= temp_hs$column + 2 & 
                     fish_area_df$column >= temp_hs$column - 2)
    further <- further[further %in% closest == FALSE]
    hotspots_prob[further, ii + 2] <- max_prob / 2

    #make sure that movement probability for hotspots is zero
    hotspots_prob[which(hotspots_prob$row == temp_hs$row & 
      hotspots_prob$column == temp_hs$column), ii + 2] <- 0
  }
  
  #------------------------------------------------------------------------
  #Sample Fish to move

  #create data frame to sample from
  fish_df <- fish_area_df[, c('row', 'column', 'nfish')]
  
  for(ii in 1:nrow(hotspots)){
    fish_df$draw <- rbinom(length(fish_df$nfish), size = fish_df$nfish, 
      prob = hotspots_prob[, ii + 2])
    names(fish_df)[which(names(fish_df) == 'draw')] <- paste0('hotspot', ii)
  }

  #check to make sure that the drawn fish doesn't exceed the
  #number of fish in the cell
  drawn_fish <- rowSums(fish_df[, 4:ncol(fish_df)])
  
  #stop the function if the number of drawn fish is greater than the number of actual
  #fish
  if(sum(drawn_fish > fish_df$nfish) > 0) browser()

  #Move the drawn fish to the specified hotspots
  fish_area_df$moving_out <- drawn_fish
  fish_area_df$moving_in <- 0

  for(ii in 1:nrow(hotspots)){
    hs <- which(fish_area_df$unq %in% hotspots$unq[ii])
    fish_area_df[hs, 'moving_in'] <- sum(fish_df[ii + 3])
  }
  
  #------------------------------------------------------------------------  
  #Update number of fish in each cell
  fish_area_df$nfish_after <- fish_area_df$nfish - fish_area_df$moving_out + 
    fish_area_df$moving_in

  final <- matrix(fish_area_df$nfish_after, nrow = numrow, ncol = numcol, byrow = FALSE)
  return(list(init = fish_area, final = final))

}


