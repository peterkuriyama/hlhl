#' Move fish back

#' Function that moves fish back to their starting place. This occurs after fishing

#' @param nfish_moved List of fish that moved from previous function
#' @param samps_out Fish samples from fishing step

#' @export

#Figure out inputs
move_back <- function(nfish_moved, samps_out, ctl, kk = 0, fish_area, fish_area_orig){
  nfish_moved[[1]] <- suppressMessages(left_join(nfish_moved[[1]], samps_out[, c('x', 'y', 'fish1samp')], 
    by = c('x', 'y')))
  nfish_moved[[2]] <- suppressMessages(left_join(nfish_moved[[2]], samps_out[, c('x', 'y', 'fish2samp')],
    by = c('x', 'y')))

  #melt the fish_area update
  fam <- melt(fish_area)
  fam$unq <- paste(fam$Var1, fam$Var2)

  nfish_moved_update <- nfish_moved

  after1 <- fam %>% filter(L1 == 1)  
  nfish_moved[[1]]$after_fishing <- after1[after1$unq %in% nfish_moved[[1]]$unq, 'value']

  after2 <- fam %>% filter(L1 == 2)
  nfish_moved[[2]]$after_fishing <- after2[after2$unq %in% nfish_moved[[2]]$unq, 'value']

  #Loop through the nfish_moved list
  for(nn in 1:length(nfish_moved)){    
    temp_df <- nfish_moved[[nn]]
    temp_df$Var1 <- NULL
    temp_df$Var2 <- NULL
    temp_df$diff <- temp_df$after_fishing - temp_df$value

    #Find direction of movement, goal is to move fish back to original locations
    #Switched so that this doesn't use the diffs columns
    pasted_fish_locs <- paste(samps_out$x, samps_out$y)

    move_from_ind <- which(temp_df$unq %in% pasted_fish_locs)
    move_to_ind <- which(temp_df$unq %in% pasted_fish_locs == FALSE)
    
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
  
      #Try to do this without a loop
      each_range <- vector('list', length = 3)
      move_from <- which(temp_df$unq %in% pasted_fish_locs)
      move_to <- which(temp_df$unq %in% pasted_fish_locs == FALSE)
      
      #Try to maintain original distribution of fish
      prob <- temp_df$value
      prob <- prob / sum(prob)
      
      if(sum(is.na(prob)) != 0) prob <- rep(0, length(common_inds))
      
      #-----------------------------------------------------------------
      #Start moving fish around    

      #Define the number of fish that moved/caught in a particular area
      moved_in <- temp_df[move_from, 'moved'] - temp_df[move_from, 'value']
      caught <- temp_df[move_from, 9]
      
      #sample if the probabilities are non-zero
      if(sum(prob) != 0){
        temp_df$moved_in_samp <- rmultinom(1, size = sum(moved_in), prob = prob)
        temp_df$caught_samp <- rmultinom(1, size = sum(caught), prob = prob)
      }
      
      #Define the change in number of fish
      temp_df$change <- temp_df$moved_in_samp - temp_df$caught_samp #ch for change
      
      #Create final fish count column
      temp_df$final <- temp_df$after_fishing + temp_df$change
  
      # sum(temp_df[move_to, 'moved_in_samp']) + sum(temp_df[move_to, 'caught_samp'])
      
      fish_in <- temp_df[move_from, 'check'] - temp_df[move_from, 'value']
  
      #Update number of fish in fishing locations
      temp_df[move_from, 'final'] <- temp_df[move_from, 'final'] - fish_in + temp_df[move_from, 9]

      #If there are negatives, just put them randomly somewhere
      if(sum(temp_df$final < 0) != 0){
        
        non_negs <- which(temp_df$final > 0)  
        negs <- which(temp_df$final < 0)  
        # cat(abs(sum(temp_df[negs, "final"])), "negative fish", '\n')
        n_neg_fish <- abs(sum(temp_df[negs, "final"]))

        #Expand the non_negative indices based on the number of fish present
        expanded_indices <- rep(non_negs, temp_df[non_negs, "final"])
        rm_fish <- sample(expanded_indices, n_neg_fish, replace = FALSE)
        rm_fish1 <- table(rm_fish) #Remove fish from random locations
                
        #loop through the table to add negative fish to temp_df
        for(tt in 1:dim(rm_fish1)){
          temp_df[as.numeric(names(rm_fish1)[tt]), 'final'] <- temp_df[as.numeric(names(rm_fish1)[tt]), 
            'final'] - rm_fish1[tt]
        }
        
        #Now change the negative values in final column to zeroes
        temp_df[which(temp_df$final < 0), 'final'] <- 0                
      }

      #Condition of everything caught in after_fishing
      if(sum(temp_df$after_fishing) == 0){
        temp_df$final <- 0    
      } 

      if(sum(temp_df$final) != sum(temp_df$after_fishing)){
        print("numbers don't add up")
        browser()
      } 
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

