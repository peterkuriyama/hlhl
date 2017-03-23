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
      name1 = name1, name = 'location', ctl = ctl_temp,
      index1 = index1, index2 = index2, par_func = par_func, 
      ncores = ncores)[[3]]

  }
# browser()
  
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


  # seed_outs <- vector('list', length = seeds)

  # for(ss in 1:seeds){
  #   #Change seed in the ctl file
  #   ctl_o$seed <- ss

  #   init_area1 <- initialize_population(ctl = ctl_o, nfish = ctl_o$nfish1)

  #   nsites_var <- expand.grid(1:nbests, 1:nmeds, 1:nbads)
  #   names(nsites_var) <- c('nbest', 'nmed', 'nbad')

  #   #remove any columns that have 0
  #   any_zero <- sapply(nsites_var, function(yy) sum(unique(yy) %in% 0))
  #   nsites_var[, as.numeric(which(any_zero == 1))] <- 0

  #   nsites_var$total <- rowSums(nsites_var)

  #   nsites_var <- subset(nsites_var, total == nsites)

  #   #Build up the call based on the columns available
  #   locs <- lapply(1:nrow(nsites_var), FUN = function(ff){
  #     pick_sites(nbest = nsites_var[ff, 1], nmed = nsites_var[ff, 2],
  #       nbad = nsites_var[ff, 3], fish_mat = init_area1)
  #   })

  #   #Now run simulation
  #   seed_outs[[ss]] <-  change_two(thing1 = thing1, name1 = name1,
  #     thing2 = locs, name2 = 'location', ctl = ctl_o, ncores = ncores, index1 = FALSE, 
  #     index2 = TRUE, par_func = par_func)[2:3]
  # }

  # locs_out <- lapply(seed_outs, FUN = function(ss){
  #   ss[[1]]
  # })
  # names(locs_out) <- as.character(1:seeds)
  # locs_out <- ldply(locs_out)
  # names(locs_out)[1] <- 'seed'

  # plots_out <- lapply(seed_outs, FUN = function(ss){
  #   ss[[2]]
  # })
  # names(plots_out) <- as.character(1:seeds)
  # plots_out <- ldply(plots_out)
  # names(plots_out)[1] <- 'seed'

  # return(list(loc_out = locs_out, for_plot = plots_out))
}