#' get_fips
#' 
#' This function takes an input landscape, a user-defined land area as an
#' [sf] polygon.returns the total population for that area.
#' Currently the function returns total population for the 2000 and 2010 censuses.
#' Additional years may be added later.
#' 
#' @param landscape
#' @param year
#' @param spatial #keep the spatial data? Default = FALSE
#' @param api_key #Your Census API key. Obtain one at http://api.census.gov/data/key_signup.html
#' 
#' @export
#' @examples 
#' get_census()
get_fips <- function(landscape, year = 2010, spatial = FALSE, 
                       api_key = Sys.getenv("CENSUS_API_KEY")){

 #browser()
 # check for census api key (modified from tidycensus::get_decennial)
  if (Sys.getenv("CENSUS_API_KEY") != "") {
   api_key<-Sys.getenv("CENSUS_API_KEY")
  } else if (is.null(api_key)) {
   stop("A Census API key is required.  Obtain one at http://api.census.gov/data/key_signup.html.")
  }
  
  #check that landscape is an sf object
  if (class(landscape)[1] != "sf") stop("The landscape input must be an [sf] object")
  
  #check that the landscape is projected 
  if(is.na(sf::st_crs(landscape)$epsg) | is.na(sf::st_crs(landscape)$proj4string)) stop("The landscape object is not projected; check [st_crs]")
  
  #check year == 2000 or year == 2010
  if(year != 2000 & year != 2010) stop("Year must == 2000 or 2010")

  # load the counties 
    #data("counties") ###################NOTE use this once package is ready  
  #is these are way to cache this for multiple calls to the same function?????????????????????????
    
    if(year == 2000) {
      load(here::here('data/county2000.rda'))
      county<-tiger2000_county20m
    } else {
      load(here::here('data/county2010.rda'))
      county<-tiger2010_county20m
    }
    
  #reproject input landscape to albers (even if already in albers: just as fast)
    landscape<-sf::st_transform(landscape, "+init=ESRI:102008") #reproject to North America Albers Equal Area Conic (ESRI:102008)
    
  #calc area of landscape-add to attributes
    landscape$ls_area<-sum(as.numeric(sf::st_area(landscape)), na.rm=TRUE)
    
    #get counties that lake intersects
    fips<-as.data.frame(dplyr::slice(county, unlist(sf::st_intersects(landscape, county))))
    
  #get the blockgroups for state(s) and county(s)
    blk<-list()
    for(i in c(1:nrow(fips))){
      blk[[i]]<-sf::st_as_sf(tigris::block_groups(state=as.numeric(fips$STATEFP[i]),county=as.numeric(fips$COUNTYFP[i]),year=year))
    }
    
    #rbind the blockgroups
    blockgrp<-blk[[1]]
    if(length(blk)>1) for(i in c(2:length(blk))) blockgrp<-rbind(blockgrp,blk[[i]])
    
    #reproject blockgrp
    blockgrp<-sf::st_transform(blockgrp,"+init=ESRI:102008")
    
    #calc area of blockgrp-add to attributes
    blockgrp$bg_area<-as.numeric(sf::st_area(blockgrp))
    
  #create intersection of landscape and blockgrp for export
    int<-sf::st_intersection(landscape,blockgrp)  
    
    #calc area of intersection polygons
    int$int_area<-as.numeric(sf::st_area(int))
    
    #calc proportion of original blockgroup included in int polygons
    int$pro_bg<-round(int$int_area / int$bg_area, 4)
    
    #calc proportion of original landscape included in intersection
    #note: landscapes that cross international borders or have missing data will have pro_ls < 1
    int$pro_ls<-round(sum(int$int_area) / int$ls_area[1], 2)
    
    
  # create output sf object "out"
    #select and rename attribute names
    #convert to data.frame select and rename
      if(year==2000) out<-dplyr::select(as.data.frame(int), geoid = BKGPIDFP00, aland = ALAND00, 
                                       awater = AWATER00, ls_area, bg_area, int_area, pro_bg, pro_ls)
      if(year==2010) out<-dplyr::select(as.data.frame(int), geoid = GEOID10, aland = ALAND10,
                                       awater = AWATER10, ls_area, bg_area, int_area, pro_bg, pro_ls)
    
    #convert geoid, aland and awater to numeric
      out<-dplyr::mutate(out, geoid=as.numeric(geoid), aland=as.numeric(aland), awater=as.numeric(awater))
   
    #if(spatial==TRUE) recombine out and geom to create sf object "out"
      if(spatial) out<-sf::st_as_sf(cbind(out,sf::st_geometry(int)))
      
  #return the output
    return(out) 
}
#######################################################eof#######################################

a10<-get_fips(landscape = lake1, year=2010, spatial = TRUE)
a00<-get_fips(landscape = lake1, year=2000, spatial = TRUE)

b10<-get_fips(landscape = lake, year=2010, spatial = TRUE)
b00<-get_fips(landscape = lake, year=2000, spatial = TRUE)

plot(a00[2])
plot(a10[3])
plot(b00[2])
plot(b10[3])
  
  
  # Get total pop for each block group
  data("counties_20m_sf")
  if(sf::st_crs(counties_20m_sf) != sf::st_crs(landscape)){
    county <- sf::st_transform(counties_20m_sf, sf::st_crs(landscape))
  } else {
    county <- counties_20m_sf
  }
  cnt_idx<-unlist(sf::st_intersects(landscape,county))
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
  bg <- sf::st_transform(bg,sf::st_crs(landscape))
  bg <- bg[unlist(sf::st_intersects(landscape,bg)),]
  landscape_bg <- sf::st_intersection(sf::st_union(landscape),sf::st_union(bg))
  #st_boundary plots 
  #need to figure out how to get the BG ids back onto landscape_bg
  #need to get density for BG need to get areas for bg in landscape and multiply by 
  #bg density.
  # Get Total area of block groups and total area of block groups inside of 
  # lakescape
  
  #add areas
  input$area_input <-as.numeric(st_area(input))
  blockgrp$area_blockgrp <-as.numeric(st_area(blockgrp))
  
  
  
 
  
  if(keep_output){
    list(total_pop = total_pop,census = census)
  } else {
    tot_pop
  }
}



#####################disregard
#counties download
tiger2000_county20m<-tigris::counties(resolution = "20m", year = 2000) #get the data
tiger2000_county20m<-sf::st_as_sf(tiger2000_county20m) #convert to sf object
tiger2000_county20m<-sf::st_transform(tiger2000_county20m, "+init=ESRI:102008") #reproject to North America Albers Equal Area Conic (ESRI:102008)
save(tiger2000_county20m, file=here::here('data/county2000.rda'))

tiger2010_county20m<-tigris::counties(resolution = "20m", year = 2010) #get the data
tiger2010_county20m<-sf::st_as_sf(tiger2010_county20m) #convert to sf object
tiger2010_county20m<-sf::st_transform(tiger2010_county20m, "+init=ESRI:102008") #reproject to North America Albers Equal Area Conic (ESRI:102008)
save(tiger2010_county20m, file=here::here('data/county2010.rda'))




  #counties download
  counties00<-tigris::counties(resolution = "20m", year = 2000) #get the data
  counties00<-sf::st_as_sf(counties00) #convert to sf object
  counties00<-sf::st_transform(counties00, "+init=ESRI:102008") #reproject to North America Albers Equal Area Conic (ESRI:102008)
  counties10<-tigris::counties(resolution = "20m", year = 2010) #get the data
  counties10<-sf::st_as_sf(counties10) #convert to sf object
  counties10<-sf::st_transform(counties10, "+init=ESRI:102008") #reproject to North America Albers Equal Area Conic (ESRI:102008)
  save(counties00, counties10, file=here::here('data/counties.rda'))
  
  
  
  counties00<-tigris::counties(resolution = "20m") #get the data
  counties00_20m<-tigris::counties(resolution = "20m", year = 2010) #get the data
  counties00_5m<-tigris::counties(resolution = "5m", year = 2000) #get the data
  
  
  save(counties00, file=here::here('data/temp.rda'))
  save(counties00_5m, file=here::here('data/temp5m.rda'))
  save(counties00_20m, file=here::here('data/temp20m.rda'))
  
  start<-Sys.time()
  load(here::here('data/temp.rda'))
  Sys.time()-start
  
  start<-Sys.time()
  load(here::here('data/temp20m.rda'))
  Sys.time()-start
  
  #select a lake and create a buffer around it
  #NLA06608-3890 #beach pond
  #NLA06608-2162 #yawgoo pond
  lake<-sf::st_read('inst/extdata/nla_lakemorphometry.shp')
  lake<-sf::st_as_sf(dplyr::filter(lake, nlaSITE_ID=='NLA06608-3890'))
  lake<-sf::st_buffer(lake, 3000)
  
  # select a border lake from Jeff's lake morpho and add a buffer
  lake1<-sf::st_read(dsn='Y:/data/lakeMorphometry/LakeMorphGdb.gdb', layer = "NorthEast01") 
  lake1<-sf::st_as_sf(dplyr::filter(lake1, COMID==166196269))
  lake1<-sf::st_buffer(lake1, 3000)



