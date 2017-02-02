#' Fish population in a loop function

#' Function to fish population in a for loop. 

#' @param fish_area List that has the matrices of fish, needs to be list
#' @param loc_row Locations of fishing
#' @param ctl ctl file, from make_ctl function

#' @export

#Input fish_area, one row of location data frame and the ctl file

fish_pop_loop <- function(fish_area, loc_row, ctl = ctl, kk = 0){
    temp_loc <- c(loc_row$x, loc_row$y)

    fish1 <- fish_area[[1]][temp_loc[1], temp_loc[2]]
    fish2 <- fish_area[[2]][temp_loc[1], temp_loc[2]]
# if(kk == 5) cat('fish1=', fish1, 'fish2=', fish2, '\n')
fish1orig <- fish1    
fish2orig <- fish2

    tot_fish <- fish1 + fish2
    nsamps <- ctl$nhooks * ctl$nangs

    #Store numbers of fish caught
    temp_fish12 <- data.frame(nsamps = 1:nsamps, fish1 = rep(999, nsamps),
      fish2 = rep(999, nsamps))

#Do something to make sure the number of samples can't exceed the number of fish there
#I think looping through this thing for each individual hook leads to it being too 
    #easy to catch fish
    #Need to rewrite this as a binomial of nhooks
# if(ctl$browser) browser()
      
      for(nn in 1:nsamps){                    
        temp_samp <- sample_exp(nfish1 = fish1, nfish2 = fish2, 
          prob1 = ctl$prob1, prob2 = ctl$prob2)

        temp_fish12[nn, 2:3] <- temp_samp

        #Make sure that catch of fish can't exceed number of fish      
        if(fish1 - temp_samp$fish1 < 0) {
          print('sp1')
          browser()
          temp_samp$fish1 <- 0
        }

        if(fish2 - temp_samp$fish2 < 0){
          print('sp2')
          browser()
          temp_samp$fish2 <- 0
        }

        # if(fish1 - temp_samp$fish1 < 0 | 
        #   fish2 - temp_samp$fish2 < 0) browser()

        #update counts of fish1 and fish2
        fish1 <- fish1 - temp_samp$fish1
        fish2 <- fish2 - temp_samp$fish2
      }
    
    #Format Output
    #Update the fish matrices and return the location matrix with records
    samps <- data.frame(vessel = loc_row$vessel, x = loc_row$x, y = loc_row$y, 
      fish1samp = sum(temp_fish12$fish1), fish2samp = sum(temp_fish12$fish2))

    #Update fish_area
    new_fish1 <- fish1orig - samps$fish1samp
    new_fish2 <- fish2orig - samps$fish2samp
    fish_area[[1]][temp_loc[1], temp_loc[2]] <- new_fish1
    fish_area[[2]][temp_loc[1], temp_loc[2]] <- new_fish2

    return(list(samps = samps, fish_area = fish_area, orig = c(fish1orig, fish2orig)))

}


# if(sum(temp_fish12$fish1) > fish_area[[1]][temp_loc[1], temp_loc[2]] | sum(temp_fish12$fish2) > fish_area[[2]][temp_loc[1], temp_loc[2]]) browser()    
    # fish_area[[1]][temp_loc[1], temp_loc[2]] <- fish_area[[1]][temp_loc[1], temp_loc[2]] - 
    #   sum(temp_fish12$fish1)
    # fish_area[[2]][temp_loc[1], temp_loc[2]] <- fish_area[[2]][temp_loc[1], temp_loc[2]] - 
    #   sum(temp_fish12$fish2)




#Keep for debugging
  # for(pp in 1:nrow(location)){
  #   temp_loc <- location[pp, c('x', 'y')]
  #   fish1 <- fish_area[[1]][temp_loc$x, temp_loc$y]
  #   fish2 <- fish_area[[2]][temp_loc$x, temp_loc$y]

  #   tot_fish <- fish1 + fish2
  #   if(tot_fish == 0) next

  #   #Do this for 15 hooks per drop
  #   nsamps <- nhooks * ctl$nangs

  #   #Store number of fish 1 and 2 caught
  #   temp_fish12 <- data.frame(nsamps = 1:nsamps, fish1 = rep(999, nsamps), 
  #     fish2 = rep(999, nsamps))

  #   for(nn in 1:nsamps){
  #     temp_samp <- sample_exp(nfish1 = fish1, nfish2 = fish2, 
  #       prob1 = ctl$prob1, prob2 = ctl$prob2)
  #     temp_fish12[nn, 2:3] <- temp_samp

  #     #update counts of fish1 and fish2
  #     fish1 <- fish1 - temp_samp$fish1
  #     fish2 <- fish2 - temp_samp$fish2
  #   }  

  # }