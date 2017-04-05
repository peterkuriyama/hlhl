#' Function that takes fixed inputs and runs scenarios

#' Function that calls run_scenario. Allows for parallelization across greater number of cores
#' @param index Index of to_loop to subset
#' @param ctl1 Starting ctl list
#' @export

fixed_parallel <- function(index, ctl1, to_loop = to_loop){
  #Define shape_list internally to the function
  shape_list1 <- data.frame(scen = c('leftskew', 'rightskew', 'normdist', 'uniform', 'patchy'),
                            shapes1 = c(10, 1, 5, 1, .1),
                            shapes2 = c(1 , 10 ,5, 1, 10))
  shape_list1$for_plot <- c('Left Skew', 'Right Skew', 'Normal', 'Uniform', 'Patchy')
    
  ctl_temp <- ctl1
  tt <- to_loop[index, ]
  ctl_temp$nfish1 <- tt$nfish1
  ctl_temp$nfish2 <- tt$nfish2
  ctl_temp$shapes <- c(shape_list1[tt$shape_list_row, 2], shape_list1[tt$shape_list_row, 3])
  ctl_temp$comp_coeff <- tt$comp_coeff

  #Pick sites based on initial probabilites
  locs <- vector('list', length = tt$nreps)
  init1 <- initialize_population_prob(ctl = ctl_temp)
  for(jj in 1:tt$nreps){
    locs[[jj]] <- pick_sites_prob(nsites = tt$nsites, samp_option = tt$type,
      fish_mat = init1)
  }

  #Run the simulation with the locations 
  #Only keep the stuff for plot
  outs <- run_scenario(ctl_start = ctl_temp, loop_over = locs, add_index = TRUE,
                       par_func = 'change_two', to_change = 'location')[[3]]

  #Format the results to have the right information for plots
  outs$comp_coeff <- tt$comp_coeff
  outs$init_dist <- shape_list1[tt$shape_list_row, 'scen']
  outs$for_plot <- shape_list1[tt$shape_list_row, 'for_plot']
  outs$type <- tt$type
  outs$nsites <- tt$nsites
  outs$iter <- gsub('loc_case', "", outs$location)
  outs <- outs %>% filter(year == 1) #Filter results to only keep year 1 results
  outs$location <- NULL
  outs$index <- index
  outs$nfish1 <- tt$nfish1
  outs$nfish2 <- tt$nfish2
  return(outs)
}