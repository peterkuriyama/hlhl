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

#Scale this so that prob goes down as the numbers go down maybe?  

  #If a fish was caught determine if it was fish1 or fish2
  if(fish == 1 & is.na(comp_coeff)){
    p1a <- p1 / (p1 + p2)  
    fish1 <- rbinom(n = 1, size = 1, prob = p1a)
  }
  
  if(fish == 1 & is.na(comp_coeff) == FALSE){
    #Use the proportion of fish 1 to adjust the competition coefficient
    #Default value of 5 for rate of increase
    propfish1 <- nfish1 / (nfish1 + nfish2)
    adj_comp_coeff <- comp_coeff * (1 - exp(-5 * propfish1))    
    fish1 <- rbinom(n = 1, size = 1, prob = adj_comp_coeff)
  }
  
  if(fish1 == 0 & fish == 1){    
    fish2 <- 1
  } 
  
  #Return values as data frame
  return(data.frame(fish1 = fish1, fish2 = fish2))
  
}
