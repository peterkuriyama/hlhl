#' Run Different Seeds with location scenarios

#' Function takes number of sites of differing quality and repeats replicate runs for different
#' seeds

#' @param nbests Number of best sites, pick_sites function
#' @param nmeds Number of medium sites, pick_sites function
#' @param nbads Number of bad sites, pick_sites function
#' @param seeds Number of seeds
#' @param ncores Number of cores
#' @param nsites Number of total sites
#' @param ctl_o Original ctl list
#' @param thing1 Thing1 to loop over, see change_two function
#' @param name1 Thing1 to loop over, see change_two function
#' @export

run_locs <- function(nbests = 5, nmeds = 5, nbads = 0, seeds, ncores, nsites = 10, 
  ctl_o , thing1, name1, par_func = 'change_two'){
  
  seed_outs <- vector('list', length = seeds)

  for(ss in 1:seeds){
    #Change seed in the ctl file
    ctl_o$seed <- ss

    init_area1 <- initialize_population(ctl = ctl_o, nfish = ctl_o$nfish1)

    nsites_var <- expand.grid(1:nbests, 1:nmeds, 1:nbads)
    names(nsites_var) <- c('nbest', 'nmed', 'nbad')

    #remove any columns that have 0
    any_zero <- sapply(nsites_var, function(yy) sum(unique(yy) %in% 0))
    nsites_var[, as.numeric(which(any_zero == 1))] <- 0

    nsites_var$total <- rowSums(nsites_var)

    nsites_var <- subset(nsites_var, total == nsites)

    #Build up the call based on the columns available
    locs <- lapply(1:nrow(nsites_var), FUN = function(ff){
      pick_sites(nbest = nsites_var[ff, 1], nmed = nsites_var[ff, 2],
        nbad = nsites_var[ff, 3], fish_mat = init_area1)
    })

    #Now run simulation
    seed_outs[[ss]] <-  change_two(thing1 = thing1, name1 = name1,
      thing2 = locs, name2 = 'location', ctl = ctl_o, ncores = ncores, index1 = FALSE, 
      index2 = TRUE, par_func = par_func)[2:3]
  }

  locs_out <- lapply(seed_outs, FUN = function(ss){
    ss[[1]]
  })
  names(locs_out) <- as.character(1:seeds)
  locs_out <- ldply(locs_out)
  names(locs_out)[1] <- 'seed'

  plots_out <- lapply(seed_outs, FUN = function(ss){
    ss[[2]]
  })
  names(plots_out) <- as.character(1:seeds)
  plots_out <- ldply(plots_out)
  names(plots_out)[1] <- 'seed'

  return(list(loc_out = locs_out, for_plot = plots_out))
}