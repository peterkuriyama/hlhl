#' Exponential Fish Sampling
#' Function that samples based on exponential function. Catch probabilities depend on number
#' of fish and probabilities of catching each species. 

#'Function to fish the population
#'@param nfish1 Number of fish1
#'@param nfish2 Number of fish2
#'@param prob1 Probability of catching fish1
#'@param prob2 Probability of catching fish2
#'@param comp_coeff Competition coefficient. Higher the value the more likely to catch 
#' species1. Specified in ctl file. If NA, use other probability formulation

#'@examples
#' Put Example Here
#'@export

sample_exp <- function(nfish1, nfish2, prob1, prob2, comp_coeff){
  #------------------------------------------------
  #Define probabilities based on number of fish
  #Might need to adjust the shape of this curve
  #Can adjust these to account for behavior of certain species
  p1 <- 1 - exp(-nfish1 * prob1) #use prob 1 to define probability of catching fish 1
  p2 <- 1 - exp(-nfish2 *  prob2) #use prob2 to define probability of catching fish 2

  #Probability of catching a fish
  hook_prob <- 1 - ((1 - p1) * (1 - p2))
  
  fish <- rbinom(n = 1, size = 1,  prob = hook_prob)  

  #------------------------------------------------
  # Which fish was caught?
  #initially declare both as 0
  fish1 <- 0
  fish2 <- 0

  propfish1 <- nfish1 / (nfish1 + nfish2)
  #Scale this so that prob goes down as the numbers go down maybe?  
  #If a fish was caught determine if it was fish1 or fish2
  # if(fish == 1 & is.na(comp_coeff)){
    
  #   fish1 <- rbinom(n = 1, size = 1, prob = p1a)
  # }

  #Sampled directly in proportion to numbers
  if(fish == 1 & comp_coeff == 0.5){
    fish1 <- rbinom(n = 1, size = 1, prob = propfish1)
  }
  
  #Hyperstable, favor species 1
  if(fish == 1 & comp_coeff == 0.7){
    adj_comp_coeff <- 1 - exp(-6 * propfish1)
    if(propfish1 == 1) adj_comp_coeff <- 1

    fish1 <- rbinom(n = 1, size = 1, prob = adj_comp_coeff)
  }
  
  #Hyperdepletion, favor species 2
  if(fish == 1 & comp_coeff == 0.3){
    adj_comp_coeff <- log(1 - propfish1) / -6 #inverse of function 3
    if(propfish1 == 1) adj_comp_coeff <- 1
      
    fish1 <- rbinom(n = 1, size = 1, prob = adj_comp_coeff)
  }
  
  if(fish1 == 0 & fish == 1 & nfish2 != 0){    
    fish2 <- 1
  } 

  #Return values as data frame
  return(data.frame(fish1 = fish1, fish2 = fish2))
  
}

#Test the function
# sample_exp(nfish1 = 100, nfish2 = 200, prob1 = .01, prob2 = .01, comp_coeff = 0.3)

# nsamps <- 100
# fish1 <- 300
# fish2 <- 100
# prob1 <- .01
# prob2 <- .01
# comp_coeff <- 0.3

# temp_fish12 <- data.frame(nsamps = 1:nsamps, fish1 = rep(999, nsamps),
#       fish2 = rep(999, nsamps))
      
# for(nn in 1:nsamps){                    
#   temp_samp <- sample_exp(nfish1 = fish1, nfish2 = fish2, 
#     prob1 = prob1, prob2 = prob2, comp_coeff = comp_coeff)

#   #Make sure that catch of fish can't exceed number of fish      
#   if(fish1 - temp_samp$fish1 < 0) {
#     print('sp1')
#     temp_samp$fish1 <- 0
#   }

#   if(fish2 - temp_samp$fish2 < 0){
#     print('sp2')
#     # browser()
#     temp_samp$fish2 <- 0
#   }

#   temp_fish12[nn, 2:3] <- temp_samp
  
#   #update counts of fish1 and fish2
#   fish1 <- fish1 - temp_samp$fish1
#   fish2 <- fish2 - temp_samp$fish2

#   #Change the probabilities if there aren't any more fish
#   if(fish1 == 0) prob1 <- 0
#   if(fish2 == 0) prob2 <- 0
# }

# colSums(temp_fish12)
