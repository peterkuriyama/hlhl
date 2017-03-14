#' Run replicates of things
#' Function to run replicates of change_two. Only save things for plot. This is because other issues
#' should have been addressed before running multiple iterations

#' @param niters Number of iterations
#' @param thing1 Vector of things to loop over
#' @param thing2 Vector of things to loop over
#' @param name1 Name of thing1 in the ctl file
#' @param name2 Name of thing1 in the ctl file
#' @param ctl Control file to modify
#' @param ncores Number of cores to use in computation
#' @param add_index Option to add Index of things to loop over, applicaple when looping over numbers of locations

#' @export

run_replicates <- function(niters, thing1, name1, thing2, name2, ncores, add_index = FALSE, ctl){
  
  start_time <- Sys.time()
  
  seeds <- 1:niters

  #Create output 
  reps <- vector('list', length = niters)

  for(ss in 1:length(seeds)){
    ctl$seed <- seeds[ss]
  
    reps[[ss]] <- change_two(thing1 = thing1, name1 = name1,
                             thing2 = thing2, name2 = name2, ncores = ncores, ctl = ctl)[[3]]  
  } 

  print(Sys.time() - start_time)

browser()
  return(reps)

}

