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

  
  #Update the after_fishing columns
  # nfish_moved <- lapply(nfish_moved, FUN = function(x){
  #                         x[which(is.na(x[, 11])), 11] <- 0
  #                         x$after_fishing <- x$moved - x[, 11]
  #                         return(x)
  # })
  
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
          
      moved_in <- temp_df[move_from, 'moved'] - temp_df[move_from, 'value']
      caught <- temp_df[move_from, 9]
      
      # nfish_move_out <- caught
      
      if(sum(prob != 0)){
        temp_df$moved_in_samp <- rmultinom(1, size = sum(moved_in), prob = prob)
        temp_df$caught_samp <- rmultinom(1, size = sum(caught), prob = prob)
      }
      
      temp_df$change <- temp_df$moved_in_samp - temp_df$caught_samp #ch for change
      
      #Create final fish count column
      temp_df$final <- temp_df$after_fishing + temp_df$change
  
      sum(temp_df[move_to, 'moved_in_samp']) + sum(temp_df[move_to, 'caught_samp'])
      
      fish_in <- temp_df[move_from, 'check'] - temp_df[move_from, 'value']
  
      #Update number of fish in fishing locations
      temp_df[move_from, 'final'] <- temp_df[move_from, 'final'] - fish_in + temp_df[move_from, 9]
      
      nfish_moved_update[[nn]] <- temp_df
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
 
  
#Debuggins scraps
  # if(kk == 5) browser()
  #convert NAs to 0
# ccc <- nfish_moved[[2]]
# ccc


# lapply(nfish_moved, FUN = function(x){
#                         x[which(is.na(x[, 11])), 11] <- 0
#                         # x$after_fishing <- x$moved - x[, 11]
#                         return(x)
# })





# + sum(temp_df[move_to, 'final'])
# sum(temp_df$value) - sum(temp_df$fish1samp, na.rm = TRUE)


# temp_df[move_from, ]

# sum(temp_df$after_fishing)
# sum(temp_df$final) 
# sum(temp_df$fish1samp, na.rm = TRUE)

# temp_df$update_locs <- temp_df$

# #2400 fish
# sum(temp_df$after_fishing)
# sum(temp_df$value) - sum(temp_df$fish1samp, na.rm = TRUE)
# sum(temp_df$after_fishing) + sum(temp_df$caught_samp)

# sum(moved_in) 

# sum(temp_df$moving)

# sum(temp_df$moved)
# temp_df[move_from, 'moved']
# temp_df[move_from, 'moved']



# sum(temp_df$caught_samp) == sum(temp_df$fish1samp, na.rm = TRUE)
# temp_df$final <- temp_df$after_fishing + ch

# #For the fishing locations, something different needs to happen
# #Subtract the fish that moved out originally
# temp_df[move_from, 'final'] <- temp_df[move_from, 'final'] - moved_in

# sum(temp_df$final)
# sum(temp_df$after_fishing) - sum(temp_df$final)

# # temp_df[move_from, 'final'] <- temp_df[move_from, 'final'] + temp_df[move_from, 'change']



# # simp_temp_df[move_from, 'final'] <- simp_temp_df[move_from, 'final'] - (sum(ch) - ch[move_from])
# temp_df[move_to, ]


# temp_df[move_from, ]
# temp_df[move_from, 'final'] <- temp_df[move_from, 'final'] - moved_in




# sum(temp_df$caught_samp)
# #Checks
# sum(temp_df$final)
# sum(temp_df$after_fishing) + sum(temp_df$fish1samp, na.rm = TRUE)



# sum(temp_df$final2)
# sum(temp_df$after_fishing)

# temp_df$final

# temp_df$final[move_from] <- 
# tt <- temp_df$final[move_from] - ch[move_from]

# temp_df[move_from, 'final'] - moved_in




# (temp_df$moved_in_samp[move_to])

# temp_df[move_from, 'final'] - (sum(ch) - ch[move_from])

#           simp_temp_df[move_from, 'final'] <- simp_temp_df[move_from, 'final'] - (sum(ch) - ch[move_from])
#           if(sum(simp_temp_df$after_fishing) != sum(simp_temp_df$final)) print('fuck')



# temp_df$final[move_from] <- temp_df$final[move_from] - ch[move_from]





          
# temp_df[move_from, 'final'] <- temp_df[move_from, 'final'] - (sum(ch) - ch[move_from])
# if(sum(temp_df$after_fishing) != sum(temp_df$final)) print('fuck')


#         #Loop over ranges now -----
#         for(dd in 1:length(ranges)){
#           common_inds <- which(temp_df$x %in% ranges[[dd]]$Var1 & 
#                   temp_df$y %in% ranges[[dd]]$Var2)

#           move_from <- which(temp_df[common_inds, 'unq'] %in% pasted_fish_locs[dd])
#           move_to <- which(temp_df[common_inds, 'unq'] %in% pasted_fish_locs[dd] == FALSE)
    
#           #Define probabilities of moving based on distribution of fish
#           #Currently based on the original distribution of the fish, although sampled 
#           #from multinomial draw
#           prob <- temp_df[common_inds, 'value'] #This used to be based on the difference between everything
#           prob <- prob / sum(prob) 

#           #If there are no fish, assign zero probability
#           if(sum(is.na(prob)) != 0) prob <- rep(0, length(common_inds))

#           #maybe include this at some point
#           # prob <- prob * ctl$move_out_prob #Control the proportion of fish that move out of fishing areas

#           #Number of fish that moved in
#           moved_in <- temp_df[common_inds[move_from], 'moved'] - temp_df[common_inds[move_from], 'value']
#           caught <- temp_df[common_inds[move_from], 9]

#           #Store the numbers of fish that moved in
#           nfish_move_out[[dd]] <- moved_in
          
#           #If prob is 0, don't catch any fish
#           if(sum(prob) != 0){
#             moved_in_samp <- rmultinom(1, size = moved_in, prob = prob)
#             caught_samp <- rmultinom(1, size = caught, prob = prob)
#           }
          
#           if(sum(prob) == 0){
#             moved_in_samp <- 0
#             caught_samp <- 0
#           }

#           temp_df[common_inds, 'moved_in_samp'] <- temp_df[common_inds, 'moved_in_samp'] + moved_in_samp
#           temp_df[common_inds, 'caught_samp'] <- temp_df[common_inds, 'caught_samp'] + caught_samp

# # if(dd == 2) browser()
#           #simplify the data.frame
#           simp_temp_df <- temp_df[common_inds, ]

#           ch <- simp_temp_df$moved_in_samp - simp_temp_df$caught_samp
#           simp_temp_df$final <- simp_temp_df$after_fishing + ch
#           simp_temp_df$final[move_from] <- simp_temp_df$final[move_from] - ch[move_from]
          

#           simp_temp_df[move_from, 'final'] <- simp_temp_df[move_from, 'final'] - (sum(ch) - ch[move_from])
#           if(sum(simp_temp_df$after_fishing) != sum(simp_temp_df$final)) print('fuck')

#           # temp_df[common_inds, ] <- simp_temp_df

# each_range[[dd]] <- simp_temp_df

#         }  

          # temp_df[common_inds[move_from], 'final'] <- 
          #   temp_df[common_inds[move_from] 'final'] - 

#Remove the fish that moved out and sampled from the fishing location
# temp_df[common_inds[move_from], 'final'] <- temp_df[common_inds[move_from], 'after_fishing'] - 
#   moved_in - caught
# temp_df[common_inds[move_to], 'final'] <- temp_df[common_inds[move_to], 'after_fishing'] + temp_df[common_inds[move_to], 'moved_in_samp'] - 
#   temp_df[common_inds[move_to], 'caught_samp']

# sum(temp_df[common_inds, 'final'])
# sum(temp_df[common_inds, 'after_fishing']) - caught


# sum(temp_df[common_inds, 'after_fishing'])
#           #Check the numbers here
#           # sum(temp_df[, 'moving'])
#           # sum(temp_df[, 'moved_in_samp'])
#           # sum(temp_df[, 'caught_samp'])
#         }   

# sum(temp_df[common_inds, "moved_in_samp"])

#       #Subtract all the fish that moved
#       fish_locs <- move_from_ind
#       no_fish_locs <- move_to_ind

#       #update the final column
#       temp_df$final <- temp_df$moved

#       #This step could be fucked
#       #Remove catches
#       temp_df[fish_locs, 'final'] <- temp_df[fish_locs, 'final'] - ldply(nfish_move_out)$V1

# temp_df[fish_locs, 'final'] - ldply(nfish_move_out)$V1      

#       #add the fish that move or stay
#       temp_df$final <- temp_df$final + temp_df$moved_in_samp
#       #Subtract fish that were caught
#       temp_df$final <- temp_df$final - temp_df$caught_samp
     