#' sample_change

#' Function to calculate power of the survey

#' @param nsamps Number of samples
#' @param dep_fixed Fixed depletion level to compare power
#' @param dep_vec Vector of depletion levels to compare against dep_vec
#' @param input Input data that contains simulation results
#' @export

sample_change <- function(nsamps = 1000, dep_fixed, dep_vec, input){   
  high <- input %>% filter(dep == dep_fixed)
  
  ss <- lapply(dep_vec, FUN = function(dd){
          low <- input %>% filter(dep == dd)
          s2 <- sample(high$cpue, size = nsamps, replace = TRUE)
          s1 <- sample(low$cpue, size = nsamps, replace = TRUE)
          diffs <- s1 - s2
          outs <- c(median(diffs), as.numeric(quantile(diffs, c(.05, .95))))
      
          return(outs)
        })
  names(ss) <- dep_vec
  ss <- ldply(ss)
  names(ss) <- c('dep', 'med_cpue', 'cpue5', 'cpue95')
  ss$start_dep <- dep_fixed
  ss$dep <- as.numeric(ss$dep)
  ss$delta_dep <- ss$start_dep - ss$dep
  return(ss)
}