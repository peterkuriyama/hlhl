#' Format Plot Input

#' Function to format the simulation output for plots. 

#' @param out output from conduct_survey function
#' @examples 
#' ctl <- make_ctl(distribute = 'uniform', mortality = .1, move_out_prob = .5,
#'                 nfish1 = 1000, nfish2 = 10000, prob1 = 1, prob2 = .3, nyear = 15, scope = 1)
#' out <- conduct_survey(ctl = ctl) 
#' format_plot_input(out)

#' @export

#plot cpue for each location
format_plot_input <- function(out){
  #--------------------------------------------------------------
  #Calculate true number of each species
  spp1 <- lapply(out$fished_areas, FUN = function(x) melt(x))
  spp1 <- ldply(spp1)
  names(spp1)[1] <- 'year'
  
  spp1$year <- as.numeric(substr(spp1$year, 5, nchar(spp1$year)))

  spp1 <- spp1 %>% group_by(year) %>% summarize(nfish = sum(value)) %>% 
            as.data.frame
  spp1$spp <- 'spp1'             

  spp2 <- lapply(out$fished_areas, FUN = function(x) melt(x))
  spp2 <- ldply(spp2)
  names(spp2)[1] <- 'year'
  
  spp2$year <- as.numeric(substr(spp2$year, 5, nchar(spp2$year)))

  spp2 <- spp2 %>% group_by(year) %>% summarize(nfish = sum(value)) %>% 
            as.data.frame
  spp2$spp <- 'spp2'            
  spps <- rbind(spp1, spp2)
  #--------------------------------------------------------------
  #plot indices from each location            
  samps <- melt(out$samples, id.vars = c("year", 'x', 'y') )
  samps$spp <- NA
  
  samps[grep('1', samps$variable), "spp"] <- 'spp1'
  samps[grep('2', samps$variable), "spp"] <- 'spp2'
  
  samps$loc <- paste(samps$x, samps$y)
  samps$variable <- as.character(samps$variable)
  samps$year <- as.numeric(samps$year)
  for_plot <- left_join(samps, spps, by = c('year', 'spp'))

  cpues <- for_plot %>% filter(variable %in% c('cpue1', 'cpue2'))
  cpues <- cpues[order(cpues$year), ]

  return(cpues)
}
