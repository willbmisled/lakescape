#' Make lakescape
#' 
#' @param lake
#' @param lakescape
#' @param data_src
#' 
#' @export
#' @examples 
#' make_lakescape()
make_lakescape <- function(lake, lakescape, 
                           data_src = c("census", "lulc", "impervious")){
  data_src <- match.arg(data_src)
  lakescape <- list()
  return(lakescape)
}


