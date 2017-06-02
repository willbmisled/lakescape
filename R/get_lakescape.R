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
#' @param id A 1x1 data frame containing the id and name.  Defualt is 
#'           \code{data.frame(id = 1)}.
#' @param data_src A character vector indicating which data source(s) to 
#'                 summarize.  Valid entries include: "census", "lulc", or, 
#'                 "impervious"
#' @param keep_output A logical to indicate whether or not to save geospatial 
#'                    data for the census, lulc, or impervious in the lakescape
#' @param ... Placeholder, cuz we might need it.
#' 
#' @export
#' @examples 
#' data(lake_sf, package = "lakescape")
#' lakescape <- sf::st_buffer(lake_sf, 3000)
#' get_lakescape(lake_sf, lakescape, data.frame(COMID = lakescape$COMID),
#'               "census")
get_lakescape <- function(lake, lakescape, id = data.frame(id = 1),
                          data_src = c("census", "lulc", "impervious"),
                          keep_output = F, ...){
  if(is.na(st_crs(lakescape)) | is.na(st_crs(lake))){
    stop(paste("A valid coordinate reference system is required for lake and 
               lakescape.  In the meantime, enjoy this dad joke:\n", dadjoke::groan())
  }
  
  lscape <- sf::st_difference(st_geometry(lakescape), st_geometry(lake))
  #add id - need to find better way than df...
  if(!is.null(id)){
    lscape <- st_as_sf(cbind(id,lscape))
  }
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


