#' Get census
#' 
#' This function takes an input lakescape, a user-defined land area around a 
#' lake, and returns the total population for that area.
#' 
#' @param lakescape
#' @param keep_output
#' 
#' @export
#' @examples 
#' get_census()
get_census <- function(lscape, keep_output){
  
  
  
  if(keep_output){
    list(total_pop = total_pop,census = census)
  } else {
    total_pop
  }
}


