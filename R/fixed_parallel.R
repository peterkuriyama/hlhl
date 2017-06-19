#' Function that takes fixed inputs and runs scenarios

#' Function that calls run_scenario. Allows for parallelization across greater number of cores
#' @param index Index of to_loop to subset
#' @param ctl1 Starting ctl list
#' @export

fixed_parallel <- function(index, ctl1, to_loop = to_loop, change_these){
  #--------------------------------------------------------------------
  #Define shape_list internally to the function
  shape_list1 <- data.frame(scen = c('leftskew', 'rightskew', 'normdist', 'uniform', 'patchy'),
                            shapes1 = c(10, 1, 5, 1, .1),
                            shapes2 = c(1 , 10 ,5, 1, 10))
  shape_list1$for_plot <- c('Left Skew', 'Right Skew', 'Normal', 'Uniform', 'Patchy')
  ctl_temp <- ctl1
  tt <- to_loop[index, ]
  
  #--------------------------------------------------------------------
  #Modify ctl_temp based on names of to_loop
  for(cc in 1:length(change_these)){
    change_these[cc]
    ctl_temp[change_these[cc]] <- tt[1, change_these[cc]]
  }

  #Change prob1 and prob2
  ctl_temp$prob1 <- tt$c1_sum * tt$comp_coeff
  ctl_temp$prob2 <- tt$c1_sum - ctl_temp$prob1

  ctl_temp$shapes <- c(shape_list1[tt$shape_list_row, 2], shape_list1[tt$shape_list_row, 3])

  #--------------------------------------------------------------------
  #Pick sites based on initial probabilites
  locs <- vector('list', length = tt$nreps)

  #Pick sites based on total number of fish
  # init1 <- initialize_population_prob(ctl = ctl_temp)
  c1 <- ctl_temp$prob1 / (ctl_temp$prob1 + ctl_temp$prob2)

  #add the two spp together
  initspp1 <- initialize_population(ctl = ctl_temp, nfish = ctl_temp$nfish1)
  initspp2 <- initialize_population(ctl = ctl_temp, nfish = ctl_temp$nfish2)

  #Pick sites based on the combined number of fish species
  init_prop <- initspp1 + initspp2
  init_prop <- init_prop / sum(init_prop)
  
  for(jj in 1:tt$nreps){
    locs[[jj]] <- pick_sites_prob(nsites = tt$nsites, samp_option = tt$type,
      fish_mat = init_prop)
  }

  #--------------------------------------------------------------------
  #Run the simulation with the locations 
  #Only keep the stuff for plot
  outs1 <- run_scenario(ctl_start = ctl_temp, loop_over = locs, add_index = TRUE,
                       par_func = 'change_two', to_change = 'location')

  out_site <- outs1[[2]]
  outs <- outs1[[3]]

  # outs <- run_scenario(ctl_start = ctl_temp, loop_over = locs, add_index = TRUE,
  #                      par_func = 'change_two', to_change = 'location')[[3]]

  #Format the results to have the right information for plots
  outs$init_dist <- shape_list1[tt$shape_list_row, 'scen']
  outs$for_plot <- shape_list1[tt$shape_list_row, 'for_plot']
  outs$type <- tt$type
  outs$nsites <- tt$nsites
  outs$iter <- gsub('loc_case', "", outs$location)
  outs$index <- index

  #Add on change_these in a for loop
  for(cc in 1:length(change_these)){
    outs[change_these[cc]] <- tt[1, change_these[cc]]  
  }

  outs <- outs %>% filter(year == 1) #Filter results to only keep year 1 results
  outs$location <- NULL

  return(list(outs = outs, site_cpue = out_site))

}
