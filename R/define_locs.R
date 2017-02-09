#' Define Fishing Locations

#' Function to define fishing locations based on certain conditions

#' @param numrow Number of rows in fishing area
#' @param numcol Number of columns in fishing area
#' @param condition Condition used to subset the data. Input as text then parsed out
#' Subsets based on the expanded grid

#' @export

#' @examples
#' define_locs(condition = "y == 3 & x == 5")

define_locs <- function(numrow = 10, numcol = 10, condition){
  locs <- expand.grid(1:10, 1:10)
  locs$vessel <- 1
  names(locs)[1:2] <- c('x', 'y')
  locs <- locs[, c('vessel', 'x', 'y')]

  #Paste the text together, this uses dplyr currently and sub
  first_part <- "locs %>% filter("
  last_part <- ")"

  locs_out <- eval(parse(text = paste0(first_part, condition, last_part)))
  
  return(locs_out)
}

