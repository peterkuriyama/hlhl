#' Ploat Average CPUE

#' Function to plot the average cpue. Calls format_plot_input function

#' @param out Output from conduct_survey
#' @examples
#' ctl <- make_ctl(distribute = 'uniform', mortality = .1, move_out_prob = .5,
#'                 nfish1 = 1000, nfish2 = 10000, prob1 = 1, prob2 = .3, nyear = 15, scope = 1)
#' out <- conduct_survey(ctl = ctl) 
#' plot_average_cpue(out)
#' @export

plot_average_cpue <- function(out){
  cpues <- format_plot_input(out = out)
  cpues_avg <- cpues %>% group_by(year, variable, spp) %>% summarize(tot_cpue = mean(value),
    nfish = unique(nfish)) %>% as.data.frame

  ggplot(cpues_avg, aes(x = nfish, y = tot_cpue)) + geom_point() + geom_line() + 
    facet_wrap(~ spp) + ylim(0, 1) + 
    labs(x = "True Number of Fish", y = "Average CPUE") + theme_bw()
}