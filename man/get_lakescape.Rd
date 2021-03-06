% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_lakescape.R
\name{get_lakescape}
\alias{get_lakescape}
\title{Get lakescape}
\usage{
get_lakescape(lake, lakescape, id = data.frame(id = 1),
  data_src = c("census", "lulc", "impervious"), keep_output = F, ...)
}
\arguments{
\item{lake}{A valid \code{sf} object of the polygon(s) that make up the lake.}

\item{lakescape}{A valid \code{sf} object of the user-defined landscape 
surrounding the lake.}

\item{id}{A 1x1 data frame containing the id and name.  Defualt is 
\code{data.frame(id = 1)}.}

\item{data_src}{A character vector indicating which data source(s) to 
summarize.  Valid entries include: "census", "lulc", or, 
"impervious"}

\item{keep_output}{A logical to indicate whether or not to save geospatial 
data for the census, lulc, or impervious in the lakescape}

\item{...}{Placeholder, cuz we might need it.}
}
\value{
Either a \code{sf} object with the lakescape as the geometry and 
summarized landscape values (e.g. total pop, total impervious, etc.) as 
columns or a \code{lakescape} object with the lakescape and output \code{sf} 
object for the census, and \code{raster} objects for the impervious and land 
cover.
}
\description{
The \code{get_lakescape} function uses a lake and a user defined landscape 
around that lake (i.e. the lakescape) to summarize various sources of 
ancillary geospatial data.
}
\examples{
data(lake_sf, package = "lakescape")
lakescape <- sf::st_buffer(lake_sf, 3000)
get_lakescape(lake_sf, lakescape, data.frame(COMID = lakescape$COMID),
              "census")
}
