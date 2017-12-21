#' Function to modify two factors

#' Function to modify two factors

#' @param thing1 Vector of things to loop over
#' @param thing2 Vector of things to loop over
#' @param name1 Name of thing1 in the ctl file
#' @param name2 Name of thing1 in the ctl file
#' @param ctl Control file to modify
#' @param ncores Number of cores to use in computation
#' @param par_func Specify full name of function to run in parallel
#' @export

change_two <- function(thing1, thing2, name1, name2, ctl, ncores = 6,
  index1 = FALSE, index2 = FALSE, par_func = "run_scenario"){

  thing1_outs <- vector('list', length = length(thing1))

  #Specify which function to run in parallel
  #------------------------------------
  #Parallelize over run_scenario function
  if(par_func == 'run_scenario'){
    for(tt in 1:length(thing1)){
      ctl[name1] <- thing1[tt]
      thing1_outs[[tt]] <- run_scenario(ctl_start = ctl, loop_over = thing2,
        ncores = ncores, to_change = name2, add_index = index2, par_func = par_func)
    }
  }

  #------------------------------------
  #Parallelize ovr change_two function
  if(par_func == 'change_two'){
    
    if(class(thing1) != 'list'){
      ctl_list1 <- lapply(thing1, FUN = function(yy){
        ctl_temp <- ctl
        ctl_temp[name1] <- yy
        return(ctl_temp)
      })  
    }
    
    if(class(thing1) == 'list'){
      ctl_list1 <- lapply(thing1, FUN = function(yy){
        ctl_temp <- ctl
        ctl_temp[[name1]] <- yy
        return(ctl_temp)
      })
    }

    #Specify which call to use
    sys <- Sys.info()['sysname']
    
    #Do this with foreach
    # #Run simulations in parallel
    if(sys != "Windows"){
      thing1_outs <- mclapply(ctl_list1, mc.cores = ncores, FUN = function(xx){
        run_scenario(ctl_start = xx, loop_over = thing2, ncores = ncores, to_change = name2,
          par_func = par_func, add_index = index2)
      })
    }

    if(sys == 'Windows'){
      registerDoParallel(ncores)

index <- 1
run_scenario(ctl_start = ctl_list1[[index]], loop_over = thing2, to_change = name2,
             par_func = par_func, add_index = index2)

thing1_outs <- foreach(index = 1:length(ctl_list1), 
        .packages = c('plyr', 'dplyr', 'reshape2')) %dopar%
        run_scenario(ctl_start = ctl_list1[[index]], loop_over = thing2, to_change = name2,
          par_func = par_func, add_index = index2)

      stopImplicitCluster()
    } 
  }
  
  #------------------------------------
  #Format the output, should have thing1, thing2 as columns and the different results
  #Combine all the data frames so   

  #1. Format stuff to track numbers of fish
  fish_melt <- lapply(thing1_outs, FUN = function(xx){
    xx[[1]]
  })
  
  loc_out <- lapply(thing1_outs, FUN = function(xx){
    xx[[2]]
  })
  
  for_plot <- lapply(thing1_outs, FUN = function(xx){
    xx[[3]]
  })

  if(index1 == FALSE){
    names(fish_melt) <- as.character(thing1)
    names(loc_out) <- as.character(thing1)
    names(for_plot) <- as.character(thing1)
  }

  if(index1 == TRUE){
    names(fish_melt) <- as.character(1:length(thing1))
    names(samps) <- as.character(1:length(thing1))
    names(for_plot) <- as.character(1:length(thing1))
  }

  fish_melt <- ldply(fish_melt)
  loc_out <- ldply(loc_out)
  for_plot <- ldply(for_plot)

  names(fish_melt)[1] <- name1
  names(loc_out)[1] <- name1
  names(for_plot)[1] <- name1
  
  #Return outputs
  return(list(fish_melt = fish_melt, loc_out = loc_out, for_plot = for_plot))
}

