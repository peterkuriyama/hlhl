#' Plot annual cpue nfish
#'
#' Plot annual averaged cpue and true number of fish
#'
#' @param parsed_list output of survey_over_years melted for formatting
#' @param cex.axis spcify size of axis, all other cex paramters scaled to this
#' @param text_space spacing inbetween text descriptions in upper left of plot
#' @keywords plot
#' @export
#' @examples 
#' plot example

plot_annual_cpue_nfish <- function(parsed_list, cex.axis = 1.2, text_space = .25, 
  print_text = TRUE, ...){

  nfish_cpue <- merge(parsed_list$end_nfish %>% 
                        group_by(year) %>% 
                        summarise(nfish = sum(value)),
                      parsed_list$cpue %>% 
                        group_by(year) %>% 
                        summarise(cpue = mean(value)) %>% 
                        as.data.frame, by = 'year')

  #Get ellipses arguments
  dot_args <- list(...)

  #Get values from simulation to add descriptions to plots and filename
  nfish <- dot_args$nfish
  nlocs <- dot_args$nlocs
  distribute <- get('distribute', parent.frame())
  move_func_name <- dot_args$move_func_name

  nlocs_total <- parsed_list$init_nfish %>% filter(year == 1) %>% summarise(loc = length(value))

  perc_sampled <- nlocs / nlocs_total
  rand <- get('random_locations', parent.frame())
  nhooks <- dot_args$nhooks
  ndrops <- dot_args$ndrops
  scope <- dot_args$scope

  #----------------------------------------------------------------
  #Plot
  ifelse(is.null(dot_args$xlim_s), xlim_s <- c(0, nfish), 
    xlim_s <- dot_args$xlim_s)

  #specify x limit if it is missing
  plot(nfish_cpue$nfish, nfish_cpue$cpue, ylim = c(0, 1), pch = 19,
    xaxs = 'i', yaxs = 'i', xlim = xlim_s, type = 'o', axes = FALSE,
    ann = FALSE, cex = 1.2 * cex.axis, lwd = cex.axis * 1.2, xpd = TRUE)

  #----------------------------------------------------------------
  #Add axes

  axis(side = 1, at = pretty(xlim_s[1]:xlim_s[2]),
    labels = format(pretty(xlim_s[1]:xlim_s[2]), big.mark = ',', scientific = FALSE),
    cex.axis = cex.axis)

  mtext(side = 1, outer = T, "Number of Fish", line = -2, cex = cex.axis * 1.2)
  mtext(side = 2, outer = T, "CPUE", line = -1.4, cex = cex.axis * 1.2)
  # mtext(side = 2, outer = T, "CPUE (nfish / nhooks)")
  axis(side = 2, las = 2, cex.axis = cex.axis)

  #----------------------------------------------------------------
  #Add text
  #specify text spacing
  spacing <- seq(1, 10, by = (1 + text_space))

  if(print_text == TRUE)
  {
    mtext(side = 3, paste0(perc_sampled * 100, "% of ", nlocs_total ," locations sampled"), 
      adj = .01, line = -spacing[1], cex = cex.axis * .8)
    mtext(side = 3, paste0(format(nfish, big.mark = ','), ' fish initially'), 
      adj = .01, line = -spacing[2], cex = cex.axis * .8)
    
    if(distribute == 'area')
    {
      distribute <- dot_args$area
    }

    mtext(side = 3, paste0( distribute, ' Initial Fish Distribution'), 
      adj = .01, line = -spacing[3], cex = cex.axis * .8)
    
    if(move_func_name == 'move_fish_cw')
    {
      mtext(side = 3, 'Clockwise Fish Movement', 
        adj = .01, line = -spacing[4], cex = cex.axis * .8)  
    }

    if(move_func_name == 'move_fish_left')
    {
      mtext(side = 3, 'Offshore (left) Fish Movement', 
        adj = .01, line = -spacing[4], cex = cex.axis * .8)  
    }

    if(move_func_name == 'move_fish_none')
    {
      mtext(side = 3, 'No Fish Movement', 
        adj = .01, line = -spacing[4], cex = cex.axis * .8)  
    }

    if(rand == TRUE) 
    {
      mtext(side = 3, 'Random Location Sampling', 
        adj = .01, line = -spacing[5], cex = cex.axis * .8)
    }

    if(rand == FALSE) 
    {
      mtext(side = 3, 'Fixed Location Sampling', 
        adj = .01, line = -spacing[5], cex = cex.axis * .8)
    }

    mtext(side = 3, paste0(nhooks, ' hooks, ', ndrops, ' drops, ', 
      'fish move from ', scope, ' cells' ), adj = .01, line = -spacing[6],
      cex = cex.axis * .8)

  }
  

}

