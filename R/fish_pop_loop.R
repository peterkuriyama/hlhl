#' Fish population in a loop function

#' Function to fish population in a for loop. 

#' @param fish_area List that has the matrices of fish, needs to be list
#' @param loc_row Locations of fishing

#' @export

#Input fish_area, one row of location data frame and the ctl file

fish_pop_loop <- function(fish_area, loc_row, nhooks, nangs, prob1, prob2, comp_coeff){
    temp_loc <- c(loc_row$x, loc_row$y)

    fish1 <- fish_area[[1]][temp_loc[1], temp_loc[2]]
    fish2 <- fish_area[[2]][temp_loc[1], temp_loc[2]]

    fish1orig <- fish1    
    fish2orig <- fish2

    tot_fish <- fish1 + fish2
    nsamps <- nhooks * nangs

    #Store numbers of fish caught
    temp_fish12 <- data.frame(nsamps = 1:nsamps, fish1 = rep(999, nsamps),
      fish2 = rep(999, nsamps))
      
      for(nn in 1:nsamps){                    
        temp_samp <- sample_exp(nfish1 = fish1, nfish2 = fish2, 
          prob1 = prob1, prob2 = prob2, comp_coeff = comp_coeff)

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

