#'Hook Probabilities
#'
#'Define probabilities of catching certain fish. Assumes that each fish selects a 
#'hook at random. Based on comments from 2012 CIE review.

#'@param nfish Number of fish in cell
#'@param p0 Probability that fish detects gear
#'@param nhooks Number of hooks, affects the probability f that a fish selects a hook

#' @return Vector of probabilities of catch 0, 1, 2, 3, 4, or 5 fish

#' @examples
#' hook_probs(nfish = 40, p0 = .4, nhooks = 5)

#' @export

hook_probs <- function(nfish, p0 = .4, nhooks = 5){
  f <- p0 / nhooks
  f0 <- 1 - p0
  
  prob0 <- f0 ^ nfish
  prob1 <- 5 * ((f + f0) ^ nfish - prob0) 
  prob2 <- 10 * ((2 * f + f0) ^ nfish - 2 * (f + f0) ^ nfish + f0 ^ nfish) 
  prob3 <- 10 * ((3 * f + f0) ^ nfish - 3 * (2* f + f0) ^ nfish + 3 * (f + f0) ^ nfish - f0 ^ nfish)
  prob4 <- 5 * ((4 * f + f0) ^ nfish - 4 * (3 * f + f0) ^ nfish + 6 * (2 * f + f0) ^ nfish - 
                  4 * (f + f0) ^ nfish + f0 ^ nfish)
  prob5 <- 1 - (prob0 + prob1 + prob2 + prob3 + prob4)
  
  probs <- c(prob0, prob1, prob2, prob3, prob4, prob5)
  return(probs)
}