#' Compare three surveys
#'
#' Compare three surveys and plot the results
#'
#' @param numrow Number of rows in matrix
#' @param numcol Number of columns in matrix
#' @param nfish Number of fish to allocate among matrix
#' @param seed Set seed if distribute == random, defaults to 300
#' @param distribute Specify fish distribution to be 'uniform', 'patchy', or 'area' specific
#' @param maxfish Maximum number of fish that can be sampled at a time
#' @param percent percentage of area to sample. Only necessary if distribute == 'patchy'
#' @param area Specify area to distribute fish, options are 'upperleft', 'upperright', 'lowerleft', 'lowerright',
#' 'upperhalf', 'lowerhalf', 'righthalf', 'lefthalf' 
#'   Only necessary if distribute == 'area'
#' @param move_func input movement function name as text. Later parsed into function call
#' @keywords initialize
#' @export
#' @examples 
#' Put example in 

compare_three_surveys <- function(numrow = 10, numcol = 10, nfish = 1000, distribute,
  seed = 300, nyears = 15, location_list, random_locations = FALSE, nlocs = 10,
  move_func, nhooks, ndrops, scope = 0, pdf = FALSE,
  png = FALSE, ...){

  
  # if(is.character(move_func) == FALSE) stop('movement function must be character string')
  # move_func_name <- move_func
  # move_func <- eval(parse(text = move_func))

  # run <- survey_over_years(numrow = numrow, numcol = numcol, nfish = nfish, 
  #             distribute = distribute,
  #             seed = seed, nyears = nyears, location_list = location_list, 
  #             random_locations = random_locations, nlocs = nlocs, 
  #             move_func = move_func, nhooks = nhooks, ndrops = ndrops, 
  #             scope = scope, ...)
              

  # run <- parse_master_list(run)

  # if(pdf == TRUE){
  #   fn <- strsplit(move_func_name, split = '_')[[1]][3]
  #   pdf_name <- paste0('figs/', paste0(distribute, '_', nlocs, 'locs', '_', nfish, 'fish_', fn, '.pdf'))
  #   pdf(width = 7, height = 7, file = pdf_name)
  # }

  # if(png == TRUE){
  #   fn <- strsplit(move_func_name, split = '_')[[1]][3]
  #   png_name <- paste0('figs/', paste0(distribute, '_', nlocs, 'locs', '_', nfish, 'fish_', fn, '.png'))
  #   png(width = 7, height = 7, file = png_name, units = 'in', 
  #     res = 300)
  # }

  # plot_annual_cpue_nfish(parsed_list = run, nfish = nfish,
  #   distribute = distribute, seed = seed, nyears = nyears, 
  #   nlocs = nlocs, move_func_name = move_func_name, nhooks = nhooks,
  #   ndrops = ndrops, scope = scope, ...)

  # if(pdf + png > 0) dev.off() 

  # return(run)
}


