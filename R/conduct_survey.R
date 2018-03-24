#' Conduct Survey
#' Wrapper to repeatedly run fish_population function
#' Give function arguments for
#' This function initializes the spatial distribution of the fish population

#' @keywords survey
#' @param init_area list of intialized areas
#' @param ... arguments from other thing
#' @export
#' @examples
#'
#' ctl <- make_ctl()
#' conduct_survey(ctl)

conduct_survey <- function(init_area, ...){    
  nnyear <- list(...)$nyear
  nnangs <- list(...)$nangs
  nnhooks <- list(...)$nhook
  nndrops <- list(...)$ndrops
  dep_type <- list(...)$dep_type
  drop_samples <- vector('list', length = nnyear)
  fished_areas <- vector('list', length = nnyear)
  
  names(drop_samples) <- 1:nnyear
  names(fished_areas) <- 1:nnyear  

  #--------------------------------------------------------
  #MOve fish in or out depending on the dep_type specification
  
  #Extract fishing locations
  location <- list(...)$location  
  prop_moving <- list(...)$prop_moving #Proportion of fish moving
  
  #Extract the initial area to match locations with fishing locations
  the_init_area <- init_area[[1]]
  the_init_area <- melt(the_init_area)
  names(the_init_area) <- c('x', 'y', 'value')
  the_init_area$unq <- paste(the_init_area$x, the_init_area$y)

  location$unq <- paste(location$x, location$y)

  #Filter the fished locations
  fished_locs <- which(the_init_area$unq %in% location$unq)
  #which have fish also
  locs_with_fish <- which(the_init_area$value != 0)

  #----------------------------
browser()  
  #Meaning that the fishing locations are in the best habitat  
  #Fish move into the fishing locations
  if(dep_type == 'increasing'){
# print('moving fish in')    
    #-----Move fish out
    #Move fish from outside the fishing area into the fishing area
    fish_out <- the_init_area[-fished_locs, ]
    
    #Sample the locations of fish to move
    nsamps <- round(sum(fish_out$value) * prop_moving)

    fish_samps <- base::sample(x = rep(fish_out$unq, times = fish_out$value), 
      nsamps, replace = FALSE)

    fish_samps <- melt(table(fish_samps))
    names(fish_samps) <- c('unq', 'fish_out')
    fish_samps$unq <- as.character(fish_samps$unq)
    fish_out <- fish_out %>% left_join(fish_samps, by = 'unq') %>%
      replace_na(list(fish_out = 0))
    fish_out$updated_value <- fish_out$value - fish_out$fish_out

    #-----Move fish in
    #Add the fish to fishing areas that already have fish 
    fish_in <- the_init_area[fished_locs, ]

    fish_in_samps <- fish_in %>% filter(value != 0) %>% select(unq) %>%
      sample_n(size = nsamps, replace = T) %>% group_by(unq) %>% tally() %>%
      as.data.frame()
    names(fish_in_samps) <- c('unq', 'fish_in')
    fish_in <- fish_in %>% left_join(fish_in_samps, by = 'unq') %>%
      replace_na(list(fish_in = 0))
    fish_in$updated_value <- fish_in$value + fish_in$fish_in

    #-----Combine the in/out datasets and convert into a matrix for use
    fish_updated <- rbind(fish_in %>% select(x, y, updated_value, unq),
                          fish_out %>% select(x, y, updated_value, unq))
    #sort the data by x and y
    fish_updated <- fish_updated %>% arrange(y, x)
    init_area[[1]] <- matrix(fish_updated$updated_value, byrow = F, nrow = 30, ncol = 30)
  }

  #meaning that the fish in fishing locations are "depleted" or moved out
  if(dep_type == 'decreasing'){
# print('moving fish out')
    fish_out <- the_init_area[fished_locs, ]

    #Sample the fish
    nsamps <- round(sum(fish_out$value) * prop_moving)

    fish_samps <- sample(x = rep(fish_out$unq, times = fish_out$value), 
      nsamps, replace = FALSE)

    fish_samps <- melt(table(fish_samps))
    names(fish_samps) <- c('unq', 'fish_out')
    fish_samps$unq <- as.character(fish_samps$unq)

    fish_out <- fish_out %>% left_join(fish_samps, by = 'unq') %>%
      replace_na(list(fish_out = 0))
    fish_out$updated_value <- fish_out$value - fish_out$fish_out

     #-----Move fish in
    #Add the fish to fishing areas that already have fish 
    fish_in <- the_init_area[-fished_locs, ]

    fish_in_samps <- fish_in %>% select(unq) %>%
      sample_n(size = nsamps, replace = T) %>% group_by(unq) %>% tally() %>%
      as.data.frame()
    names(fish_in_samps) <- c('unq', 'fish_in')
    fish_in <- fish_in %>% left_join(fish_in_samps, by = 'unq') %>%
      replace_na(list(fish_in = 0))
    fish_in$updated_value <- fish_in$value + fish_in$fish_in

    #-----Combine the in/out datasets and convert into a matrix for use
    fish_updated <- rbind(fish_in %>% select(x, y, updated_value, unq),
                          fish_out %>% select(x, y, updated_value, unq))
    #sort the data by x and y
    fish_updated <- fish_updated %>% arrange(y, x)
    init_area[[1]] <- matrix(fish_updated$updated_value, byrow = F, nrow = 30, ncol = 30)
  }

  #--------------------------------------------------------
  #Fish Area Once
  after_first <- fish_population(fish_area = init_area,...)

  drop_samples[[1]] <- after_first$angler_samples
  fished_areas[[1]] <- after_first$updated_area

  temp_area <- after_first$updated_area #Define temporary area for use in the for loop

  #Specify movement function if there is one
  # movement_function <- ctl$movement_function

  #Get into this loop if nnyear is > 1  
  if(nnyear > 1){
    #Loop over years of survey, specified in ctl
    for(kk in 2:nnyear){
    
      #Leave this in case I want to add movement in 
      # temp_area[[1]] <- movement_function(temp_area[[1]], max_prob = ctl$max_prob, min_prob = ctl$min_prob)$final
      # temp_area[[2]] <- movement_function(temp_area[[2]], max_prob = ctl$max_prob, min_prob = ctl$min_prob)$final    
      temp <- fish_population(fish_area = temp_area, ...)
  
      # survey_samples[[kk]] <- temp$samples
      drop_samples[[kk]] <- temp$angler_samples
      fished_areas[[kk]] <- temp$updated_area
  
      #Redefine the fishing area
      temp_area <- temp$updated_area    
    }    
  }

  #manipulate samples  
  samples <- ldply(drop_samples)
  names(samples)[1] <- 'year'

  #calculate CPUE based on number of hooks
  effort <- nnhooks * nnangs * nndrops
  samples$cpue1 <- samples$fish1samp / effort
  samples$cpue2 <- samples$fish2samp / effort

  fished_areas[[nnyear + 1]] <- init_area
  names(fished_areas) <- c(paste0('year', 1:nnyear), 'year0')
  
  #define output list
  out <- list(fished_areas = fished_areas, samples = samples)
   
  return(out)
}
