#' Calculate median absolute relative error and slopes

#'Function to calculate MARE and slopes for simulated values

#' @param input Input, should have cpue of zero at depletion of zero and nhooks column

#' @export
calc_mare_slopes <- function(input){
# browser()
  #Calculate mare values
  input$dep_numeric <- as.numeric(as.character(input$dep))
  input <- input %>% mutate(error = cpue - dep_numeric, rel_error = error / dep_numeric,
      are = abs(rel_error)) 
  
  input_mare <- input %>% filter(is.na(are) == FALSE)
  
  input_mare <- input_mare %>% group_by(iter, nsites, init_dist, spp, type, nhooks) %>%
    summarize(avg_are = mean(are)) %>% group_by(nsites, init_dist, spp, type, nhooks) %>%
    summarize(med_are = median(avg_are)) %>% as.data.frame
  
  #----------------------------------------------------------------------------------
  #Calculate slopes
  # input$ind <- input$dep_numeric * 10
  input <- input %>% mutate(ind_lag = index - 1, ind_lead = index + 1) 
  
  #-----------------------------------------
  #Create data frames with lags
  lags <- input %>% select(nsites, init_dist, type, location, cpue, ind_lag, nhooks) 
  lags <- plyr::rename(lags, c('cpue' = 'lag_cpue', 'ind_lag' = 'index'))

  # names(lags)[5] <- 'lag_cpue'
  # names(lags)[6] <- 'ind'
  
  leads <- input %>% select(nsites, init_dist, type, location, cpue, ind_lead, nhooks) 
  leads <- plyr::rename(leads, c('cpue' = 'lead_cpue', 'ind_lead' = 'index'))
  # names(leads)[5] <- 'lead_cpue'
  # names(leads)[6] <- 'ind'
  
  #Add in the lags 
  input1 <- left_join(input, lags, by = c("nsites", 'init_dist', 'type',
    'location', 'index', 'nhooks'))
  
  #Add in the leads
  input1 <- left_join(input1, leads, by = c("nsites", 'init_dist', 'type',
    'location', 'index', 'nhooks'))
  
  #Overwrite input with the one that works
  input <- input1
  input <- plyr::rename(input, c("lag_cpue" = "p1", 'lead_cpue' = 'p2'))
  input <- plyr::rename(input, c("p1" = "lead_cpue", 'p2' = 'lag_cpue'))
  
  input <- input %>% arrange(nsites, init_dist, type, location, nhooks)
  
  #replace NAs with zeroes to lag_cpue column
  input[which(is.na(input$lag_cpue)), 'lag_cpue'] <- 0

  #Calculate slopes
  input$slope <- (input$lead_cpue - input$lag_cpue) / .2
  input$diff_from_one <- input$slope - 1
  
  #Remove the 
  # input <- input %>% filter(dep != 0)
  
  #Plot histograms
  # input %>% filter(nsites == 100, init_dist == 'patchy', 
  #   dep_numeric != 0, dep_numeric != 1) %>%
  #   ggplot() + geom_histogram(aes(x = diff_from_one), binwdith = .5) + 
  #   facet_wrap(~ dep_numeric + type, ncol = 2) +
  #   geom_vline(xintercept = 0, lty = 2)
    
  
  #Find 5%, median, and 95% difference in slope at each depletion level
  #for each scenario
  #Remove the zeroes
  input_nozero <- input %>% filter(dep != 0)
  
  slope_summ <- input_nozero %>% group_by(nsites, init_dist, type, dep_numeric, nhooks) %>%
    summarize(q5 = quantile(diff_from_one, 0.05, na.rm = T), 
              m5 = median(diff_from_one, na.rm = T),
              q95 = quantile(diff_from_one, .95, na.rm = T)) %>%
    as.data.frame
  
  #Summarize the slopes and plot
  slope_summ %>% filter(nsites %in% c(5, 20, 50, 100)) %>% ggplot() + 
    geom_point(aes(x = dep_numeric, y = m5, colour = type)) + 
    geom_hline(yintercept = 0, col = 'red') + 
    facet_wrap(~ nhooks)
  
  #Old calculations, input_mare values
  # input_mare <- input_mare %>% group_by(nsites, init_dist, spp, type) %>%
  #   summarize(min_are = min(are), med_are = median(are), max_are = max(are)) %>% 
  #   as.data.frame
  
  # to_plot <- left_join(to_plot, input_mare, by = c("nsites", "init_dist", 'spp', 'type'))
  outs <- list(mare_values = input_mare, slope_summary = slope_summ, onespp = input_nozero)
  return(outs)

}
