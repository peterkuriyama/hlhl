#' Move fish back

#' Function that moves fish back to their starting place. This occurs after fishing

#' @param nfish_moved List of fish that moved from previous function
#' @param samps_out Fish samples from fishing step

#' @export

#Figure out inputs
move_back <- function(nfish_moved, samps_out){
  nfish_moved[[1]] <- left_join(nfish_moved[[1]], samps_out[, c('x', 'y', 'fish1samp')])
  nfish_moved[[2]] <- left_join(nfish_moved[[2]], samps_out[, c('x', 'y', 'fish2samp')])

  nfish_moved_update <- nfish_moved

  #convert NAs to 0
  nfish_moved <- lapply(nfish_moved, FUN = function(x){
                          x[which(is.na(x[, 11])), 11] <- 0
                          x$after_fishing <- x$moved - x[, 11]
                          return(x)
  })

  #Loop through the nfish_moved list
  for(nn in 1:length(nfish_moved)){
    temp_df <- nfish_moved[[nn]]
    temp_df$Var1 <- NULL
    temp_df$Var2 <- NULL
    temp_df$diff <- temp_df$after_fishing - temp_df$value
    
    #Fish should go from values with positive diff to negative diff
    # Can use the positive ones to scope the range of the multinomial sampling that
    # moves things back to original cells  
    
    #Find direction of movement, goal is to move fish back to original locations
    move_from_ind <- which(temp_df$diff > 0)
    move_to_ind <- which(temp_df$diff < 0)
    
    froms <- temp_df[move_from_ind, c('x', 'y')] 
   
    #----------------------------------------------------
    #If there are fish, do this
    if(sum(temp_df$value) != 0){
          #Find all the rows nearby
        ranges <- apply(froms, MAR = 1, FUN = function(x){
                    expand.grid(((x[1]) - ctl$scope) : (x[1] + ctl$scope), 
                      (x[2] - ctl$scope) : (x[2] + ctl$scope))
        })

        #Set up zero column to store the move samples
        temp_df$moved_in_samp <- 0
        temp_df$caught_samp <- 0
        
        nfish_move_out <- ranges

        #Loop over ranges now -----
        for(dd in 1:length(ranges)){
          common_inds <- which(temp_df$x %in% ranges[[dd]]$Var1 & 
                  temp_df$y %in% ranges[[dd]]$Var2)

          move_from <- which(temp_df[common_inds, 'diff'] > 0)
          move_to <- which(temp_df[common_inds, 'diff'] < 0)

          #Define probabilities of moving based on distribution of fish
          #Currently based on the original distribution of the fish, although sampled 
          #from multinomial draw
          prob <- temp_df[common_inds, 'value'] #This used to be based on the difference between everything
          # prob[which(prob >= 0)] <- 0
          # prob <- abs(prob)
          prob <- prob / sum(prob) 
          # prob <- prob * ctl$move_out_prob #Control the proportion of fish that move out of fishing areas

          #Number of fish that moved in
          moved_in <- temp_df[common_inds[move_from], 'moved'] - temp_df[common_inds[move_from], 'value']
          caught <- temp_df[common_inds[move_from], 9]

          #Store the numbers of fish that moved in
          nfish_move_out[[dd]] <- moved_in

          moved_in_samp <- rmultinom(1, size = moved_in, prob = prob)
          caught_samp <- rmultinom(1, size = caught, prob = prob)

          temp_df[common_inds, 'moved_in_samp'] <- temp_df[common_inds, 'moved_in_samp'] + moved_in_samp
          temp_df[common_inds, 'caught_samp'] <- temp_df[common_inds, 'caught_samp'] + caught_samp

          #Check the numbers here
          # sum(temp_df[, 'moving'])
          # sum(temp_df[, 'moved_in_samp'])
          # sum(temp_df[, 'caught_samp'])
        }   

      #Subtract all the fish that moved
      fish_locs <- which(temp_df$diff > 0)
      no_fish_locs <- which(temp_df$diff < 0)

      #update the final column
      temp_df$final <- temp_df$moved
      
      #This step could be fucked
      #Remove catches
      temp_df[fish_locs, 'final'] <- temp_df[fish_locs, 'final'] - ldply(nfish_move_out)$V1
      
      #add the fish that move or stay
      temp_df$final <- temp_df$final + temp_df$moved_in_samp
      #Subtract fish that were caught
      temp_df$final <- temp_df$final - temp_df$caught_samp

    }   

    #----------------------------------------------------
    #If there are no fish, do this
    if(sum(temp_df$value) == 0){
      temp_df$final <- 0
    }
     
    nfish_moved_update[[nn]] <- temp_df
  }   

  #Format stuff for output 
  return(nfish_moved_update)

}
 
  