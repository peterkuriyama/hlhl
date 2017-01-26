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
                          x[which(is.na(x[, 9])), 9] <- 0
                          x$after_fishing <- x$moved - x[, 9]
                          return(x)
      })

  for(nn in 1:length(nfish_moved)){
    temp_df <- nfish_moved[[nn]]
    temp_df$Var1 <- NULL
    temp_df$Var2 <- NULL
    temp_df$diff <- temp_df$after_fishing - temp_df$value
    
    #Fish should go from values with positive diff to negative diff
    # Can use the positive ones to scope the range of the multinomial sampling that
    # moves things back to original cells  
    
    temp_df$diff[temp_df$diff > 0]
    prob <- temp_df$diff
    prob[which(prob >= 0)] <- 0
    prob <- abs(prob)
    prob <- prob / sum(prob)

    if(sum(temp_df$diff) == 0) prob <- rep(0, nrow(temp_df))

    #Maybe need to make sure seed is set?
    #if there are no fish, move_back_sample is 0
    temp_df$move_back_sample <- rep(0, length(prob))
    
    #If Probabilities are nonzero, do this
    if(sum(prob) != 0){
        temp_df$move_back_sample <- rmultinom(1, size = sum(temp_df$diff[temp_df$diff > 0]), prob = prob)  
        
      #Find direction of movement, goal is to move fish back to original locations
      move_from_ind <- which(temp_df$diff > 0)
      move_to_ind <- which(temp_df$diff < 0)
    
      froms <- temp_df[move_from_ind, c('x', 'y')] 
      
      #Find all the rows nearby
      ranges <- apply(froms, MAR = 1, FUN = function(x){
                  expand.grid(((x[1]) - ctl$scope) : (x[1] + ctl$scope), 
                    (x[2] - ctl$scope) : (x[2] + ctl$scope))
            })
      #Determine where fish ultimately move by looping through ranges
      temp_df$final <- 999
      
      #Loop over ranges now -----
      for(dd in 1:length(ranges)){
        common_inds <- which(temp_df$x %in% ranges[[dd]]$Var1 & 
                temp_df$y %in% ranges[[dd]]$Var2)
        
        move_from <- which(temp_df[common_inds, 'diff'] > 0)
        move_to <- which(temp_df[common_inds, 'diff'] < 0)

        #Subtract some fish
        temp_df[common_inds[move_from], 'final'] <- temp_df[common_inds[move_from], 'after_fishing'] - 
         sum(temp_df[common_inds[move_to], 'move_back_sample'])
        
        #Now add the fish that are moving
        temp_df[common_inds[move_to], 'final'] <- temp_df[common_inds[move_to], 'move_back_sample'] + 
          temp_df[common_inds[move_to], 'after_fishing']
      }      

      #Change 999 to 0
      temp_df[which(temp_df$final == 999), 'final'] <- 0
    }

    #If Probabilities are zero, do this
    if(sum(prob) == 0){
      temp_df$final <- 0
    }

    nfish_moved_update[[nn]] <- temp_df
  }   

  #Format stuff for output 
  return(nfish_moved_update)

}
 
  