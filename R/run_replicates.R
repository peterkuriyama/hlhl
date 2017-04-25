#' Run replicates of things
#' Function to run replicates of change_two. Only save things for plot. This is because other issues
#' should have been addressed before running multiple iterations

#' @param niters Number of iterations
#' @param thing1 Vector of things to loop over
#' @param thing2 Vector of things to loop over
#' @param name1 Name of thing1 in the ctl file
#' @param name2 Name of thing1 in the ctl file
#' @param ctl Control file to modify
#' @param ncores Number of cores to use in computation
#' @param add_index Option to add Index of things to loop over, applicaple when looping over numbers of locations

#' @export

run_replicates <- function(ctl_in){

  #Set initial conditions
  init_area1 <- initialize_population(ctl = ctl_in, nfish = ctl_in$nfish1)
  init_area2 <- initialize_population(ctl = ctl_in, nfish = ctl_in$nfish2)
  
  init_area = list(init_area1, init_area2)

  #Parse arguments from ctl_in
  nhooks <- ctl_in$nhooks
  nangs <- ctl_in$nangs
  prob1 <- ctl_in$prob1
  prob2 <- ctl_in$prob2
  comp_coeff <- ctl_in$comp_coeff
  numrow <- ctl_in$numrow
  numcol <- ctl_in$numcol
  rec_years <- ctl_in$rec_years
  rec_rate <- ctl_in$rec_rate
  nyear <- ctl_in$nyear
  ndrops <- ctl_in$ndrops
  location <- ctl_in$location
  scope <- ctl_in$scope
  mortality <- ctl_in$mortality
  niters <- ctl_in$niters

  #Create output list
  out_list <- vector('list', length = niters)

  #Run scenarios for each iteration
  for(ii in 1:niters){
    out_list[[ii]] <- conduct_survey(init_area = list(init_area1, init_area2), nhooks = nhooks, 
                                     nangs = nangs, prob1 = prob1, prob2 = prob2, 
                                     comp_coeff = comp_coeff, numrow = numrow, numcol = numcol,
                                     rec_years = rec_years, rec_rate = rec_rate, nyear = nyear, 
                                     ndrops = ndrops, location = location, scope = scope, 
                                     mortality = mortality)
  }
  
  #Parse output into dataframes
  #--------------------------------------------
  #1. First element of list
  fish_melt <- lapply(out_list, FUN = function(x){
    temp <- "["(x$fished_areas)
    return(melt(temp))
  })
  names(fish_melt) <- as.character(1:niters)
  fish_melt <- ldply(fish_melt)
  names(fish_melt) <- c('iter', 'x', 'y', 'value', 'spp', 'year')
  fish_melt$spp <- paste0('spp', fish_melt$spp )

  #--------------------------------------------
  #2. Number of fish after each sampling
  #Number of fish after each sampling
  nfish <-  lapply(out_list, FUN = function(x){
    temp <- "["(x$fished_areas)
    out <- melt(temp) %>% group_by(L1, L2) %>% summarize(nfish = sum(value)) %>% as.data.frame %>%
      dcast(L1 ~ L2, value.var = 'nfish')
  })

  names(nfish) <- as.character(1:length(nfish))
  nfish <- ldply(nfish)
  names(nfish) <- c('iter', 'year', 'nfish1', 'nfish2')

  #remove "year" from year values
  nfish$year <- as.numeric(gsub("year", "", nfish$year))
  nfish <- melt(nfish, id.vars = c('iter', 'year'))
  names(nfish)[3:4] <- c('spp', 'nfish_total')
  nfish$spp <- gsub("nfish", 'spp', nfish$spp )

  #Record number of fish caught each year
  nsamps <- lapply(out_list, FUN = function(x){
    x$samples %>% group_by(year) %>% summarize(fish1samp = sum(fish1samp), fish2samp = sum(fish2samp),
      cpue1 = mean(cpue1), cpue2 = mean(cpue2)) %>%
      arrange(desc(year)) %>% as.data.frame
  })
  
  names(nsamps) <- as.character(1:length(nsamps))
  nsamps <- ldply(nsamps)
  names(nsamps)[1] <- 'iter'
  nsamps$year <- as.numeric(nsamps$year)
  nsamps <- melt(nsamps, id.vars = c('iter', 'year'))

  nsamps$spp <- nsamps$variable
  nsamps$spp <- as.character(nsamps$spp)

  nsamps[grep('1', nsamps$spp), 'spp'] <- 'spp1'
  nsamps[grep('2', nsamps$spp), 'spp'] <- 'spp2'
  
  nsamps$variable <- as.character(nsamps$variable)
  nsamps[grep('samp', nsamps$variable), 'variable'] <- 'fishsamp'
  nsamps[grep('cpue', nsamps$variable), 'variable'] <- 'cpue'
  nsamps <- dcast(nsamps, iter + year + spp ~ variable)

  nall <- left_join(nfish, nsamps, by = c('iter', 'year', 'spp'))
  
  #arrange nall by index then year
  nall$iter <- as.numeric(nall$iter)
  nall <- nall %>% arrange(iter, year)
  
  #--------------------------------------------
  #3. Format input plots
  inp_list <- lapply(out_list, function(xx){
    format_plot_input(out = xx)
  })
  
  #Format inp_list
  names(inp_list) <- as.character(1:niters)
  inp_df <- ldply(inp_list)
  names(inp_df)[1] <- 'iter'
  
  #split up cpue and fish samples
  one <- inp_df %>% filter(spp == "spp1")
  done <- dcast(one, spp + year + iter + loc ~ variable, value.var = 'value')
  names(done)[5:6] <- c('cpue', 'fishsamp')
  
  two <- inp_df %>% filter(spp == "spp2")
  dtwo <- dcast(two, spp + year + iter + loc ~ variable, value.var = 'value')
  names(dtwo)[5:6] <- c('cpue', 'fishsamp')
    
  inp_df <- rbind(done, dtwo)

  for_plot <- inp_df %>% group_by(iter, year, spp) %>% 
      summarize(cpue = mean(cpue), fishsamp = mean(fishsamp)) %>% as.data.frame

  add_these <- c('nfish1', 'nfish2', 'prob1', 'prob2')
  already_in <- names(for_plot)[names(for_plot) %in% names(ctl_in)]

  still_add <- add_these[add_these %in% already_in == FALSE]

  #loop over still_add
  for(ll in 1:length(still_add)){
    run_this <- paste0("for_plot$", still_add[ll], " <- ", ctl_in[still_add][ll])
    eval(parse(text = run_this))    
  }

  #Summarize nall
  nall <- nall %>% group_by(iter, spp) %>% mutate(nfish_orig = nfish_total[1], 
    prop_of_unfished = fishsamp / nfish_orig, prop_of_pop = fishsamp / nfish_total) %>%
    as.data.frame
  
  #Now return everything
  return(list(fish_melt = fish_melt, loc_out = inp_df, for_plot = nall))

}

