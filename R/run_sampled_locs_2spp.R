#' Run Simulation with preferential and random location sampling for two species and sampled locations

#' Run Simulation with preferential and random location sampling for two species and sampled locations

#' @param thing1 thing1 vector
#' @param name1 name of thing1 to loop over
#' @param thing2 thing2 vector
#' @param name2 name of thing2 to loop over
#' @param nreps Number of replicates
#' @param nsites_vec Vecotr of the number of sites to sample
#' @param shape_list List of shapes for beta distribution
#' @param ncores Number of cores
#' @param ctl_o Input ctl list

#' @export

run_sampled_locs_2spp <- function(thing1, thing2, name1, name2, nreps, nsites_vec,
  shape_list, ncores, ctl_o){

  #Random site sampling
  outs1 <- lapply(nsites_vec, FUN = function(nn){
    run_locs_2spp(shape_list = shape_list, loc_scenario = 'rand',
      ncores = ncores, ctl_o = ctl_o, thing1 = thing1, name1 = name1,
      thing2 = thing2, name2 = name2,
      nreps = nreps, nsites = nn)
  })

  names(outs1) <- as.character(nsites_vec)
  outs1 <- ldply(outs1)
  names(outs1)[1] <- 'nsites'
  outs1$type <- 'random'

  #Preferential site sampling
  outs2 <- lapply(nsites_vec, FUN = function(nn){
    run_locs_2spp(shape_list = shape_list, loc_scenario = 'pref',
      ncores = ncores, ctl_o = ctl_o, thing1 = thing1, name1 = name1,
      thing2 = thing2, name2 = name2,
      nreps = nreps, nsites = nn)
  })

  names(outs2) <- as.character(nsites_vec)
  outs2 <- ldply(outs2)
  names(outs2)[1] <- 'nsites'
  outs2$type <- 'preferential'
  
  outs <- rbind(outs1, outs2)

  return(outs)
}