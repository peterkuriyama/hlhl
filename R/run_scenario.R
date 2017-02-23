#' Run Scenarios

#' Function to run scenarios in parallel. Returns three things, a list of all the output from
#' conduct_survey, cpue summarized by each value in loop_over, and summarized data for each year. 

#' @param ctl Output from make_ctl function
#' @param loop_over Vector of values to loop over
#' @param ncores  Number of cores to run in parallel, default is 1
#' @param to_change Specify which value to modify in lapply statement. Should be the values in
#' loop_over and be a character string 

#' @examples
#' ctl1 <- make_ctl(distribute = 'patchy', mortality = 0, move_out_prob = .5,
#'       nfish1 = 0, nfish2 = 0, prob1 = .01, prob2 = 0, nyear = 15, scope = 1, seed = 4,
#'       location = one_loc, numrow = 1, numcol = 1)  
#' ttest <- run_scenario(ctl = ctl, loop_over = seq(100, 1500, by = 100), to_change = 'nfish1', 
#'   ncores = 6)
#' @export

run_scenario <- function(ctl_in, loop_over, ncores = 1, to_change){
  
  #Set up number of cores, default is 1
  # cl <- makeCluster(ncores)
  # registerDoParallel(cl)
  # cat(getDoParWorkers(), "cores registered", '\n')
  
  #Create list of ctls that based on inputs
  ctl_list <- lapply(loop_over, function(xx){
    ctl_temp <- ctl_in
    ctl_temp[to_change] <- xx
    return(ctl_temp)
  })

####FOR LOOP CHECK
  #Write this as a for loop
  # out_list <- vector('list', length = length(ctl_list))

  # for(ii in 1:length(ctl_list)){
  #   thing <- ctl_list[[ii]]
    
  #   out <- conduct_survey(ctl = thing)
  #   out_list[[ii]] <- out
  #   print(ii)
  # }

  
  # inner_function <- function(thing_in){
  #   out <- conduct_survey(ctl = thing_in)
  #   return(out)
  # }

  out_list <- mclapply(ctl_list, mc.cores = ncores, FUN = function(xx){
    print(xx[to_change])

    ctl <- xx
    out <- conduct_survey(ctl = ctl)
    return(out)
    
    # print(xx)
    # ctl_temp <- ctl_in    
    # ctl_temp[to_change] <- xx
    # out <- conduct_survey(ctl = ctl_temp)
  })


  # out_list <- lapply(loop_over, FUN = function(xx, ctl_in1 = ctl_in){
  #   print(xx)
  #   ctl_temp <- ctl_in1    
  #   ctl_temp[to_change] <- xx
  #   out <- conduct_survey(ctl = ctl_temp)
  # })
  
  # #Run outputs
  # out_list <- mclapply(loop_over, mc.cores = ncores, FUN = function(xx, ctl_in = ctl_in){
  #   print(xx)
  #   ctl_temp <- ctl_in    
  #   ctl_temp[to_change] <- xx
  #   out <- conduct_survey(ctl = ctl_temp)
  # })

  #Format inputs for plots
  inp_list <- lapply(out_list, function(xx){
    format_plot_input(out = xx)
  })

  #Format inp_list
  names(inp_list) <- as.character(loop_over)
  inp_df <- ldply(inp_list)
  names(inp_df)[1] <- to_change

  #Use substitute to group_by the character column name in to_change
  call <- substitute(inp_df %>% group_by(to_change, year, variable) %>% 
      summarize(cpue = mean(value), nfish = unique(nfish)) %>% as.data.frame, 
      list(to_change = as.name(to_change)))
  for_plot <- eval(call)

  # stopCluster(cl)

  #Now return everything
  return(list(outs = out_list, summ_out = inp_df, for_plot = for_plot))

}


