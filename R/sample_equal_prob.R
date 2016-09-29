#' Conduct Survey
#' Wrapper to repeatedly run fish_population function
#' Give function arguments for
#' This function initializes the spatial distribution of the fish population

#'@param p0 probability that each fish detects gear
#'@param nfish number of fish, probability calcuations are usually to the power nfish
#'@param nhooks number of hooks 

#' @keywords hook probability
#' @export
#' @examples
#' put example here dude
#'


#Same probability with each hook
# k <- 5 #number of hooks
# p0 <- 0.01 #probability of detection
# nfish <- 100 #number o fish

sample_equal_prob <- function(nfish = 100, nhooks = 5, p0 = .001, 
  samp_size = 3){
  if(nhooks > 5){
    nhooks <- 5
  }
  
  f0 <- 1 - p0 #probability of no capture
  f <- p0 / 5 #same for each fish
  hook_probs <- vector(length = nhooks + 1)

  hook_probs[1] <- f0 ** nfish
  hook_probs[2] <- 5 * ((f + f0) ** nfish - hook_probs[1])
  hook_probs[3] <- 10 * ((2 * f + f0) ** nfish - 2 * ((f + f0) ** nfish) + hook_probs[1])
  hook_probs[4] <- 10 * ((3 * f + f0) ** nfish - 3 * ((2 * f + f0) ** nfish) + 3 * ((f + f0) ** nfish) - hook_probs[1])
  hook_probs[5] <- 5 * ((4 * f + f0) ** nfish - 4 * ((3 * f + f0) ** nfish) + 6 * ((2 * f + f0) ** nfish) - 4 * ((f + f0) ** nfish) + hook_probs[1])
  hook_probs[6] <- 1 - sum(hook_probs[1:nhooks])
  
  hook_probs <- round(hook_probs, digits = 7)
 
  out <- base::sample(0:nhooks, size = samp_size, prob = hook_probs, replace = TRUE)
  out <- sum(out)

  return(out)
}

# sample_equal_prob(p0 = .4, nfish = 0, nhooks = 5, samp_size = 3)
