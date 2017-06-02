#' Get census
#' 
#' This function takes an input lakescape, a user-defined land area around a 
#' lake, and returns the total population for that area.
#' 
#' @param lakescape
#' @param keep_output
#' @param api_key
#' 
#' @export
#' @examples 
#' get_census()
get_census <- function(lscape, keep_output, 
                       api_key = Sys.getenv("CENSUS_API_KEY")){
  
 browser()
  # Add in a check for census api key
  
  # Get total pop for each block group
  data("counties_20m_sf")
  if(sf::st_crs(counties_20m_sf) != sf::st_crs(lscape)){
    county <- sf::st_transform(counties_20m_sf, sf::st_crs(lscape))
  } else {
    county <- counties_20m_sf
  }
  cnt_idx<-unlist(sf::st_intersects(lscape,county))
  state_cnt <- data.frame(county[cnt_idx,][,c("STATEFP","COUNTYFP")])
  state_cnt$STATEFP <- as.numeric(as.character(state_cnt$STATEFP))
  state_cnt$COUNTYFP <- as.array(as.character(state_cnt$COUNTYFP))
  tot_pop <- data.frame()
  bg <- sf::st_as_sf(tigris::block_groups(state = state_cnt[1,1], 
                                          county = state_cnt[1,2],2010))
  bg <- bg[NULL,]
  for(i in seq_along(state_cnt[,1])){
    rin <- paste0("state:",state_cnt[i,1],"+county:",state_cnt[i,2])
    tot_pop <- rbind(tot_pop,censusapi::getCensus(name = "sf1",vintage = 2010, 
                         key = api_key,
                         vars = "P0010001", region = "block group:*", 
                         regionin = rin))
    bg <- rbind(bg,sf::st_as_sf(tigris::block_groups(state = state_cnt[i,1], 
                                            county = state_cnt[i,2],2010)))
    
  }
  bg <- sf::st_transform(bg,sf::st_crs(lscape))
  bg <- bg[unlist(sf::st_intersects(lscape,bg)),]
  lscape_bg <- sf::st_intersection(lscape,bg))
  #st_boundary plots 
  #need to figure out how to get the BG ids back onto lscape_bg
  #need to get density for BG need to get areas for bg in lscape and multiply by 
  #bg density.
  # Get Total area of block groups and total area of block groups inside of 
  # lakescape
  
  
  
 
  
  if(keep_output){
    list(total_pop = total_pop,census = census)
  } else {
    tot_pop
  }
}


