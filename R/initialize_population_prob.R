#' Initialize Population Probabilities
#'
#' Initialize the spatial distribution of the fish population, units are probabilities. Used to select
#' sites for simulations with two species.

#' @param ctl List of control parameters from make_ctl function, description of arguments in 
#' make_ctl function
#' @param nfish Number of fish, use this to generate matrices for both species

#' @export

initialize_population_prob <- function(ctl){
  numrow <- ctl$numrow
  numcol <- ctl$numcol
  distribute <- ctl$distribute
  maxfish <- ctl$maxfish
  percent <- ctl$percent
  # seed <- ctl$seed
  area <- ctl$area

  #---------------------------------------------------------------------------------------------------------
  #Beta distributed fish distribution
  if(distribute == 'beta'){
    #reset seed for beta function
    set.seed(ctl$seed)
    
    #Fill in matrix of fish
    bsamps <- rbeta(ctl$numrow * ctl$numcol, shape1 = ctl$shapes[1], shape2 = ctl$shapes[2])
    bsamps <- bsamps / sum(bsamps)
    
    #Export file
    fishArea <- matrix(bsamps, nrow = ctl$numrow, ncol = ctl$numcol, byrow = FALSE)

  }

  return(fishArea)
}



