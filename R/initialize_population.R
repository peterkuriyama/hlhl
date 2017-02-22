#' Initialize Population
#'
#' Initialize the spatial distribution of the fish population
#' @param ctl List of control parameters from make_ctl function, description of arguments in 
#' make_ctl function
#' @param nfish Number of fish, use this to generate matrices for both species


#' @examples 
#' Uniformly distribute fish
#' control <- make_ctl()
#' initialize_population(ctl = control)

#' Distribute fish in upper right quadrant only
#' control <- make_ctl(distribute = 'area', area = 'upperright')
#' initialize_population(ctl = control)
#'
#' Patchily distribute fish
#' control <- make_ctl(distribute = 'patchy')
#' initialize_population(ctl = control)
#'
#' @export

initialize_population <- function(ctl, nfish){
  numrow <- ctl$numrow
  numcol <- ctl$numcol
  # nfish <- ctl$nfish
  distribute <- ctl$distribute
  maxfish <- ctl$maxfish
  percent <- ctl$percent
  seed <- ctl$seed
  area <- ctl$area

  #initial check
  if(distribute %in% c('area', 'patchy', 'uniform') == FALSE){
    stop('specify distribute as area, patchy, or uniform')
  } 

  #Create matrix of zeroes
  fishArea <- matrix(0, nrow = numrow, ncol = numcol)
  
  #Create data frame with matrix indices of interest
  samp.df <- expand.grid(1:numrow, 1:numcol) #rows and columns are set depending on arguments
  names(samp.df) <- c('x', 'y')

  #Set Seed, should apply to all downstream sampling function
  set.seed(seed)
  #---------------------------------------------------------------------------------------------------------
  # Uniformly populate matrix, work on this
  if(distribute == 'uniform'){
    
    #Modify number of fish that are allocated to each cell
    nfish.uni <- nfish - (nfish %% nrow(samp.df)) #number of fish for uniform allocation
    nfish <- nfish - nfish.uni
  }
  
  #---------------------------------------------------------------------------------------------------------
  #Patchily Distributed Fish
  if(distribute == 'patchy'){
    
    #Maybe specify percentage of things to pick ultimately??
    possible.picks <- expand.grid(1:numrow, 1:numcol)
    nsamps <- percent * nrow(possible.picks)

    samp.df <- possible.picks[sample(1:nrow(possible.picks), size = nsamps), ] 
  }
    
  if(distribute == 'patchy' & ctl$numrow == 1 & ctl$numrow == 1){
    samp.df <- possible.picks
  }  
  #---------------------------------------------------------------------------------------------------------
  #If distribution is area specific
  if(distribute == 'area'){
    
    #Adjust rows and columns depending on specified area
    if(area == 'upperleft'){
      rows <- 1:(numrow / 2)
      columns <- 1:(numcol / 2)
    }
    
    if(area == 'upperright'){
      rows <- 1:(numrow / 2)
      columns <- (1 + (numcol / 2)):numcol
    }
    
    if(area == 'lowerleft'){
      rows <- (1 + (numrow / 2)):numrow
      columns <- 1:(numcol / 2)
    }
    
    if(area == 'lowerright'){
      rows <- (1 + (numrow / 2)):numrow
      columns <- ((1 + numcol / 2)):numcol
    }

    if(area == 'lowerhalf'){
      rows <- (1 + numrow / 2):numrow
      columns <- 1:numcol
    }

    if(area == 'upperhalf'){
      rows <- 1:(numrow / 2)
      columns <- 1:numcol
    }

    if(area == 'righthalf'){
      rows <- 1:numrow
      columns <- (1 + numcol / 2):numcol
    }

    if(area == 'lefthalf'){
      rows <- 1:numrow
      columns <- 1:(numcol / 2)
    }
    #Create specific samp.df for area case
    samp.df <- expand.grid(rows, columns)
    names(samp.df) <- c('x', 'y')
  }

  #---------------------------------------------------------------------------------------------------------
  #Now sample fish
  samp.vec <- vector(length = nfish)
  counter <- 1
  
  #While loop generates samples
  while(nfish > 0){
    samp <- sample(1:maxfish, 1) #Maximum number of fish allowed per sample
    if(samp >= nfish) samp <- nfish #prevents nfish from being exceeded
    
    samp.vec[counter] <- samp #store value in counter
    
    nfish <- nfish - samp #update nfish
    counter <- counter + 1 #update counter
  }

  #Ensure that the length of sample vec is a multiple of number of rows in samp.df
  samp.vec <- c(samp.vec, rep(0, length(samp.vec) %% nrow(samp.df)))
  samp.mat <- matrix(samp.vec, nrow = nrow(samp.df))
  samp.df$fish <- rowSums(samp.mat)

  #Add uniform # of fish to each cell
  if(distribute == 'uniform'){
    samp.df$fish <- samp.df$fish + nfish.uni / nrow(samp.df)
  }
  
  #assign to fishing area
  for(ii in 1:nrow(samp.df)){
    fishArea[samp.df[ii, 1], samp.df[ii, 2]] <- samp.df[ii, 3]
  }  
  return(fishArea)
}