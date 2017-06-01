#' Get lakescape
#' 
#' The \code{get_lakescape} function uses a lake and a user defined landscape 
#' around that lake (i.e. the lakescape) to summarize various sources of 
#' ancillary geospatial data. 
#' 
#' @return Either a \code{sf} object with the lakescape as the geometry and 
#' summarized landscape values (e.g. total pop, total impervious, etc.) as 
#' columns or a \code{lakescape} object with the lakescape and output \code{sf} 
#' object for the census, and \code{raster} objects for the impervious and land 
#' cover. 
#' 
#' @param lake A valid \code{sf} object of the polygon(s) that make up the lake.
#' @param lakescape A valid \code{sf} object of the user-defined landscape 
#'                  surrounding the lake.
#' @param data_src A character vector indicating which data source(s) to 
#'                 summarize.  Valid entries include: "census", "lulc", or, 
#'                 "impervious"
#' @param keep_output A logical to indicate whether or not to save geospatial 
#'                    data for the census, lulc, or impervious in the lakescape
#' @param ... Placeholder, cuz we might need it.
#' 
#' @export
#' @examples 
#' get_lakescape()
get_lakescape <- function(lake, lakescape, 
                          data_src = c("census", "lulc", "impervious"),
                          keep_output = F, ...){
  lscape <- sf::st_difference(lakescape, lake)
  
  for(i in data_src){
    if(i == "census"){
      census <- get_census(lscape, keep_output)
    } else if(i == "lulc"){
      lulc <- get_lulc(lscape, keep_output)
    } else {
      imperv <- get_impervious(lscape, keep_output)
    }
  }
  #combine the output into the single sf or lakescape object
  
  lscape
}


