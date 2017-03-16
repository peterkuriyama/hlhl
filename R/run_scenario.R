#' Run Scenarios

#' Function to run scenarios in parallel. Returns three things, a list of all the output from
#' conduct_survey, cpue summarized by each value in loop_over, and summarized data for each year. 

#' @param ctl_start Output from make_ctl function
#' @param loop_over Vector of values to loop over
#' @param ncores  Number of cores to run in parallel, default is 1
#' @param to_change Specify which value to modify in lapply statement. Should be the values in
#' @param add_index Specify add index or not, for locations
#' @param par_func Turn parallel run option on or off
#' loop_over and be a character string 

#' @examples
#' ctl1 <- make_ctl(distribute = 'patchy', mortality = 0, move_out_prob = .5,
#'       nfish1 = 0, nfish2 = 0, prob1 = .01, prob2 = 0, nyear = 15, scope = 1, seed = 4,
#'       location = one_loc, numrow = 1, numcol = 1)  
#' ttest <- run_scenario(ctl = ctl, loop_over = seq(100, 1500, by = 100), to_change = 'nfish1', 
#'   ncores = 6)
#' @export

run_scenario <- function(ctl_start, loop_over, ncores = 1, to_change, add_index = FALSE, par_func){
  start_time <- Sys.time()

  #--------------------------------------------------------------------------------
  #Run the function in parallel

  #Create list of ctls that based on inputs
  #If loop over is a vector or a list, replace to_change in ctl_temp with different
  #notation
  if(class(loop_over) != 'list'){
    ctl_list <- lapply(loop_over, function(xx){
      ctl_temp <- ctl_start
      ctl_temp[to_change] <- xx
      return(ctl_temp)
    })
  }

  if(class(loop_over) == 'list'){
    ctl_list <- lapply(loop_over, function(xx){
      ctl_temp <- ctl_start
      ctl_temp[[to_change]] <- xx
      return(ctl_temp)
    })
  }

  #Add index to ctl_list
  for(nn in 1:length(ctl_list)){
    ctl_list[[nn]]$nname <- nn
  }

  #-----------------------------------------
  #Run function as straight lapply if par_func == "change_two"

  if(par_func == "change_two"){
    out_list <- lapply(ctl_list, FUN = function(xx){
      out <- run_replicates(ctl_in = xx)
      return(out)
    })
  }
  
  #-----------------------------------------
  #Run this function in parallel if par_func == "run_scenario"
  if(par_func == "run_scenario"){
    #Specify operating system
    sys <- Sys.info()['sysname']
    
    #Run the mclapply call
    if(sys != "Windows"){
      out_list <- mclapply(ctl_list, mc.cores = ncores, FUN = function(xx){
        out <- run_replicates(ctl_in = xx)
        return(out)    
      })  
    }

    if(sys == 'Windows'){
      cl <- makeCluster(getOption("cl.cores", ncores))
      aa <- clusterEvalQ(cl, library(hlsimulator))
      aa <- clusterEvalQ(cl, library(plyr))
      aa <- clusterEvalQ(cl, library(dplyr))
      aa <- clusterEvalQ(cl, library(reshape2))
      # dd <- clusterExport(cl, "ctl", envir = environment())
        
      out_list <- parLapply(cl, ctl_list, function(xx) {
        out <- run_replicates(ctl_in = xx)
        return(out)
      })
  
      stopCluster(cl)
    }
  }
browser()
#see if this works on a pc  

  #--------------------------------------------------------------------------------
  #Dataframe to track changes in fish population
  
  #Just in case you need to track the changes in fish at each location
  fish_melt <- lapply(out_list, FUN = function(x){
    temp <- "["(x$fished_areas)
    return(melt(temp))
  })
  names(fish_melt) <- as.character(loop_over)
  fish_melt <- ldply(fish_melt)
  names(fish_melt) <- c(to_change, 'x', 'y', 'value', 'spp', 'year')
  fish_melt$spp <- paste0('spp', fish_melt$spp )


  #Number of fish after each sampling
  nfish <-  lapply(out_list, FUN = function(x){
    temp <- "["(x$fished_areas)
    out <- melt(temp) %>% group_by(L1, L2) %>% summarize(nfish = sum(value)) %>% as.data.frame %>%
      dcast(L1 ~ L2, value.var = 'nfish')
  })

  names(nfish) <- as.character(1:length(nfish))
  nfish <- ldply(nfish)
  names(nfish) <- c('index', 'year', 'nfish1', 'nfish2')

  #remove "year" from year values
  nfish$year <- as.numeric(gsub("year", "", nfish$year))
  nfish <- melt(nfish, id.vars = c('index', 'year'))
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
  names(nsamps)[1] <- 'index'
  nsamps$year <- as.numeric(nsamps$year)
  nsamps <- melt(nsamps, id.vars = c('index', 'year'))

  nsamps$spp <- nsamps$variable
  nsamps$spp <- as.character(nsamps$spp)

  nsamps[grep('1', nsamps$spp), 'spp'] <- 'spp1'
  nsamps[grep('2', nsamps$spp), 'spp'] <- 'spp2'
  
  nsamps$variable <- as.character(nsamps$variable)
  nsamps[grep('samp', nsamps$variable), 'variable'] <- 'fishsamp'
  nsamps[grep('cpue', nsamps$variable), 'variable'] <- 'cpue'
  nsamps <- dcast(nsamps, index + year + spp ~ variable)

  nall <- left_join(nfish, nsamps, by = c('index', 'year', 'spp'))
  
  #arrange nall by index then year
  nall$index <- as.numeric(nall$index)
  nall <- nall %>% arrange(index, year)

  #--------------------------------------------------------------------------------
  #Format input plots
  #Format inputs for plots
  inp_list <- lapply(out_list, function(xx){
    format_plot_input(out = xx)
  })
  
  #Format inp_list
  names(inp_list) <- as.character(loop_over)
  inp_df <- ldply(inp_list)
  names(inp_df)[1] <- to_change
  
  #split up cpue and fish samples
  if(to_change == 'location'){
    one <- inp_df %>% filter(spp == "spp1")
    done <- dcast(one, spp + location + year + loc ~ variable, value.var = 'value')
    names(done)[5:6] <- c('cpue', 'fishsamp')
  
    two <- inp_df %>% filter(spp == 'spp2')
    dtwo <- dcast(two, spp + location + year + loc ~ variable, value.var = 'value')
    names(dtwo)[5:6] <- c('cpue', 'fishsamp')  
  }

  if(to_change != 'location'){
    one <- inp_df %>% filter(spp == "spp1")
    call1 <- substitute(dcast(one, spp + year + to_change + loc ~ variable, value.var = 'value'),
      list(to_change = as.name(to_change)))
    done <- eval(call1)
    names(done)[5:6] <- c('cpue', 'fishsamp')
   
    two <- inp_df %>% filter(spp == 'spp2')
    call2 <- substitute(dcast(two, spp + year + to_change + loc ~ variable, value.var = 'value'),
      list(to_change = as.name(to_change)))
    dtwo <- eval(call2)
    names(dtwo)[5:6] <- c('cpue', 'fishsamp')   
  }
  
  inp_df <- rbind(done, dtwo)

  #--------------------------------------------------------------------------------
  #Format for_plot output
  #Use substitute to group_by the character column name in to_change
  call <- substitute(inp_df %>% group_by(to_change, year, spp) %>% 
      summarize(cpue = mean(cpue), fishsamp = mean(fishsamp)) %>% as.data.frame, 
      list(to_change = as.name(to_change)))
  for_plot <- eval(call)

  #add total number of fish in
  # call <- substitute(for_plot %>% group_by(to_change, year) %>% 
  #     mutate(nfish_tot = sum(nfish)) %>% as.data.frame, 
  #     list(to_change = as.name(to_change)))
  # for_plot <- eval(call)

  #order for_plot stuff
  if(class(loop_over) != 'list'){
    for_plot[, to_change] <- as.numeric(for_plot[, to_change])
    for_plot <- for_plot[order(for_plot[, to_change]), ]
  }

  #Add unique index if looping over a list of locations for example
  if(add_index == TRUE){
    col_1 <- unique(for_plot[1])
    col_1 <- col_1[order(sapply(col_1, FUN = nchar)), ]

    inds <- sapply(ctl_list, FUN = function(xx) xx$nn)
    ind_df <- data.frame(col_1, inds = inds )
    names(ind_df)[1] <- as.character(to_change)
    ind_df[, to_change] <- as.character(ind_df[, to_change])

    #Add index to inp_df
    idid <- left_join(inp_df, ind_df, by = as.character(to_change))
    idid[, to_change] <- idid$inds
    idid$inds <- NULL
    inp_df <- idid

    #Add index to for plot
    dd <- left_join(for_plot, ind_df, by = as.character(to_change))
    dd[, to_change] <- dd$inds
    dd$inds <- NULL
    for_plot <- dd
  }

  #Add in values from the ctl file
  add_these <- c('nfish1', 'nfish2', 'prob1', 'prob2')
  already_in <- names(for_plot)[names(for_plot) %in% names(ctl)]

  still_add <- add_these[add_these %in% already_in == FALSE]

  #loop over still_add
  for(ll in 1:length(still_add)){
    run_this <- paste0("for_plot$", still_add[ll], " <- ", ctl[still_add][ll])
    eval(parse(text = run_this))    
  }

  #Summarize nall
  nall <- nall %>% group_by(index, spp) %>% mutate(nfish_orig = nfish_total[1], 
    prop_of_unfished = fishsamp / nfish_orig, prop_of_pop = fishsamp / nfish_total) %>%
    as.data.frame
    
  print(Sys.time() - start_time)
  #--------------------------------------------------------------------------------
  #Now return everything
  return(list(fish_melt = fish_melt, loc_out = inp_df, for_plot = nall))

}


