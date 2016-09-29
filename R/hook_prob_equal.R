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

sample_equal_prob <- function(p0 = .01, nfish = 100, nhooks = 5, ...){
  f0 <- 1 - p0 #probability of no capture
  f <- p0 / 5 #same for each fish
  hook_probs <- vector(length = k + 1)

  hook_probs[1] <- f0 ** nfish
  hook_probs[2] <- 5 * ((f + f0) ** nfish - hook_probs[1])
  hook_probs[3] <- 10 * ((2 * f + f0) ** nfish - 2 * ((f + f0) ** nfish) + hook_probs[1])
  hook_probs[4] <- 10 * ((3 * f + f0) ** nfish - 3 * ((2 * f + f0) ** nfish) + 3 * ((f + f0) ** nfish) - hook_probs[1])
  hook_probs[5] <- 5 * ((4 * f + f0) ** nfish - 4 * ((3 * f + f0) ** nfish) + 6 * ((2 * f + f0) ** nfish) - 4 * ((f + f0) ** nfish) + hook_probs[1])
  hook_probs[6] <- 1 - sum(hook_probs[1:k])

# browser()
  out <- sample(0:nhooks, size = 1, prob = hook_probs)

  return(out)
}

# sample_equal_prob(p0 = .5)
