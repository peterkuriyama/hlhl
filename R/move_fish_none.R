#' Move Fish None
#'
#' This function doesn't move any fish. Easiest to add no movement into the package
#' with this dummy function
#' @param fish_area Input matrix of distributed fish
#' @keywords movement
#' @export
#' @examples 
#' 

move_fish_none <- function(fish_area, ...){
  final <- fish_area
  return(list(init = fish_area, final = final))
}


