#Calculate CPUE
#'@param survey_res Output from conduct_survey function
#'@param cpue_method Method of calculating CPUE. Options are 'average' for calculating
#' straight average by year.
#'@export


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