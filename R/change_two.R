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
  add_index = FALSE, par_func = "run_scenario"){
  thing1_outs <- vector('list', length = length(thing1))

  #Run the simulation
  
  #Specify which function to run in parallel
  #parallel run_scenario
  if(par_func == 'run_scenario'){

    for(tt in 1:length(thing1)){
      ctl[name1] <- thing1[tt]
      thing1_outs[[tt]] <- run_scenario(ctl_in = ctl, loop_over = thing2,
        ncores = ncores, to_change = name2, add_index = add_index, par_func = par_func)
    }
  }

  if(par_func == 'change_two'){

    ctl_list1 <- lapply(thing1, FUN = function(yy){
      ctl_temp <- ctl
      ctl_temp[name1] <- yy
      return(ctl_temp)
    })

    #Specify which call to use
    # sys <- Sys.info()['sysname']

    # #Run simulations in parallel
    # if(sys != "Windows"){
    #   thing1_outs <- mclapply(ctl_list, mc.cores = ncores, FUN = function(xx){
    #     run_scenario(ctl_in = xx, loop_over = thing2, ncores = ncores, to_change = name2,
    #       par_func = par_func)
    #   })
    # }
    browser()
    # if(sys == 'Windows'){
      cl <- makeCluster(getOption("cl.cores", ncores))
      aa <- clusterEvalQ(cl, library(hlsimulator))
      aa <- clusterEvalQ(cl, library(plyr))
      aa <- clusterEvalQ(cl, library(dplyr))
      aa <- clusterEvalQ(cl, library(reshape2))
      # dd <- clusterExport(cl, "ctl", envir = environment())
      dd <- clusterExport(cl, "par_func", envir = environment())

      thing1_outs <- parLapply(cl, ctl_list, function(xx){
        run_scenario(ctl_in = xx, loop_over = thing2, ncores = ncores,
          to_change = name2, par_func = par_func)
      })

      stopCluster(cl)
    # }

         
 #    if(sys == 'Windows'){
 #      cl <- makeCluster(getOption("cl.cores", ncores))
 
        
 #      out_list <- parLapply(cl, ctl_list, function(xx) {
 #        ctl <- xx
 #        out <- conduct_survey(ctl = xx)
 #        return(out)
 #      })
  
 #      stopCluster(cl)
 #    }
 #  }

    ctl_list <- lapply(thing1, FUN = function(yy) ctl[name1] <- yy)
  }




  
  #Format the output, should have thing1, thing2 as columns and the different results
  #Combine all the data frames so   

  #1. Format stuff to track numbers of fish
  fish_melt <- lapply(thing1_outs, FUN = function(xx){
    xx[[1]]
  })
  
  samps <- lapply(thing1_outs, FUN = function(xx){
    xx[[2]]
  })
  
  for_plot <- lapply(thing1_outs, FUN = function(xx){
    xx[[3]]
  })

  if(add_index == FALSE){
    names(fish_melt) <- as.character(thing1)
    names(samps) <- as.character(thing1)
    names(for_plot) <- as.character(thing1)
  }

  if(add_index == TRUE){
    names(fish_melt) <- as.character(1:length(thing1))
    names(samps) <- as.character(1:length(thing1))
    names(for_plot) <- as.character(1:length(thing1))
  }

  fish_melt <- ldply(fish_melt)
  samps <- ldply(samps)
  for_plot <- ldply(for_plot)

  names(fish_melt)[1] <- name1
  names(samps)[1] <- name1
  names(for_plot)[1] <- name1
  
  #Return outputs
  return(list(fish_melt = fish_melt, samps = samps, for_plot = for_plot))
}