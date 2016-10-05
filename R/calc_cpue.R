#Calculate CPUE
#'Calculate CPUE

#' Function to calculate cpue

#'@param survey_res Output from conduct_survey function
#'@param cpue_method Method of calculating CPUE. Options are 'average' for calculating
#' straight average by year.
#'@export
#'@examples
#' ctl <- make_ctl(nhooks = 15, seed = 200, nfish = 10000, nyear = 50)
#' out <- conduct_survey(ctl)
#' cpue <- calc_cpue(out, ctl = ctl)

calc_cpue <- function(survey_res, ctl){
  cpue_method <- ctl$cpue_method

  samps <- melt(survey_res$samples, id.vars = c('year', 'vessel', 'x', 'y'))
  samps$year <- as.numeric(samps$year)
  samps$variable <- as.character(samps$variable)

  if(cpue_method == 'average'){
    samps %>% group_by(year) %>% summarize(avg_catch = mean(value)) %>% 
      as.data.frame %>% arrange(year) -> cpue

    cpue$avg_cpue <- cpue$avg_catch / ctl$nhooks
  }

  if(cpue_method == '75hooks'){
# browser()
    cpue <- melt(survey_res$samples, id.vars = c('year', 'vessel', 'x', 'y'))
    cpue$year <- as.numeric(cpue$year)
    
    cpue %>% group_by(year, vessel, x, y) %>% summarize(tot_fish = sum(value),
      avg_cpue = tot_fish / (ctl$nhooks * ctl$ndrops)) %>% group_by(year) %>% 
      summarize(avg_cpue = mean(avg_cpue)) %>% as.data.frame -> cpue

  }


  if(cpue_method == 'weighted_average'){

    #Weight by location
    samps %>% group_by(x, y) %>% mutate(site_avg = mean(value)) %>% 
      group_by(year, x, y) %>% mutate(year_site_avg = mean(value))
    
    samps %>% group_by(year, x, y) %>% mutate(avg = mean(value))

  }

  temp_areas <- lapply(survey_res$fished_areas, FUN = melt)
  temp_areas <- ldply(temp_areas)
  temp_areas$year <- as.numeric(ldply(strsplit(temp_areas$.id, split = "ar"))$V2)

  #Sum total fish by year
  temp_areas %>% group_by(year) %>% summarize(nfish = sum(value)) %>%
  as.data.frame -> total_fish  #group by year

  #Add column of nfish to annual_catch plot
  cpue <- inner_join(cpue, total_fish)

  return(cpue)
}