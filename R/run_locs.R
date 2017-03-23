#' Run Different Seeds with location scenarios

#' Function takes number of sites of differing quality and repeats replicate runs for different
#' seeds

# ' @param nbests Number of best sites, pick_sites function
# ' @param nmeds Number of medium sites, pick_sites function
# ' @param nbads Number of bad sites, pick_sites function
#' @param shape_list Shape Scenarios to run, must specify
#' @param loc_scenario Specify "pick" or "increasing"
#' @param loc_list List of locations to loop 
#' through, if loc_scenario == 'pick'
#' @param loc_vector Vector of number of locations to sample, 
#' if loc_scenario == 'increasing'
#' @param ncores Number of cores
#' @param ctl_o Original ctl list
#' @param thing1 Thing1 to loop over, see change_two function
#' @param name1 Thing1 to loop over, see change_two function
#' @export

run_locs <- function(shape_list, loc_scenario, loc_list,
  loc_vector, ncores, ctl_o, 
  thing1, name1, par_func = 'change_two',
  index1 = FALSE, index2 = TRUE){

  ctl_temp <- ctl_o
  
  shape_outs <- vector('list', length = nrow(shape_list))
  
  #loop over shape list
  for(ss in 1:nrow(shape_list)){
    #change ctl file
    ctl_temp$shapes <- c(shape_list[ss, 'shapes1'], shape_list[ss, 'shapes2'])

    init1 <- initialize_population(ctl = ctl_temp, ctl_temp$nfish1)

    #define fishing locations
    if(loc_scenario == 'pick'){
      locs <- lapply(1:nrow(loc_list), FUN = function(ll){
        pick_sites(nbest = loc_list[ll, 1], nmed = loc_list[ll, 2],
          nbad = loc_list[ll, 3], fish_mat = init1)
      })
    }

    if(loc_scenario == 'increasing'){
      locs <- lapply(1:length(loc_vector), FUN = function(ll){
        pick_sites(nbest = loc_vector[ll], fish_mat = init1)
      })
    }

    #Run simulation modifying these two things
    #Only save the info for plot
    shape_outs[[ss]] <- change_two(thing1 = fishes, thing2 = locs, 
      name1 = name1, name2 = 'location', ctl = ctl_temp,
      index1 = index1, index2 = index2, par_func = par_func, 
      ncores = ncores)[[3]]

  }
  
  #convert shape_outs into a data frame
  names(shape_outs) <- shape_list$scen
  shape_outs <- ldply(shape_outs)
  names(shape_outs)[1] <- 'init_dist'

  #Filter to only have year1
  shape_outs <- shape_outs %>% filter(year == 1) 
  
  #add depletion
  shape_outs %>% group_by(init_dist, spp) %>% 
    mutate(dep = nfish_total / max(nfish_total)) %>% as.data.frame -> shape_outs
  return(shape_outs)

}