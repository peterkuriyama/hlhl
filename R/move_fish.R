#' Move Fish
#' Function to move fish

#' @param fish_range1 The range of fish to calculate the numbers of fish from. Based on the "scope"
#' parameter specified in the ctl file
#' @param nfish_outside1 The number of fish outside the range
#' @param zero_index1 The location of fishing activity
#' @export

move_fish <- function(fish_range1 = fish_range, nfish_outside1 = nfish_outside,
  zero_index1 = zero_index){
  #define movement probabilities, currently nothing goes out of specified location
  #depend on number of fish relative to nfish outside

  probs <- fish_range1 / nfish_outside1

  #Create data frame of fish_range, easier to manipulate
  fish_df <- melt(fish_range1)
  fish_df$prob <- melt(probs)$value

  #if no fish within the scope, set movement probabilities to 0
  if(nfish_outside1 == 0){
    fish_df$prob <- 0
  }

  fish_df[zero_index1, 'prob'] <- 0

  #Use binomial distribution to find number of fish moving
  fish_df$moving <- apply(fish_df, MAR = 1, FUN = function(x){
    rbinom(n = 1, size = as.integer(x['value']), prob = x['prob'])
  })

  #now update the number of fish
  fish_df$moved <- fish_df$value - fish_df$moving #moved column indicates nfish after movement

  fish_df[zero_index1, 'moved'] <- fish_df[zero_index1, 'value'] + sum(fish_df$moving)

  return(fish_df)
}

  