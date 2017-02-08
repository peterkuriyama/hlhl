#' Move Fish None
#'
#' This function doesn't move any fish. Easiest to add no movement into the package
#' with this dummy function
#' @param fish_area Input matrix of distributed fish
#' @param max_prob not applicable to this function
#' @param min_prob not applicable to this function
#' @keywords movement
#' @export
#' @examples 
#' 

move_fish_none <- function(fish_area, max_prob, min_prob){
  final <- fish_area
  return(list(init = fish_area, final = final))
}


