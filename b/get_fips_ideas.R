library(sf)
library(tigris)
library(mapview)
library(tidyverse)
library(tidycensus)


#get NLA lakes and convert to sf object
nla<-st_read('inst/extdata/nla_lakemorphometry.shp')
    st_crs(nla)

#get tiger counties as sf
load('data/counties_20m_sf.rda')
    st_crs(counties_20m_sf)
    
#reproject to North America Albers Equal Area Conic (ESRI:102008)
    #Description: +proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m no_defs
 
    
nla<-st_transform(nla,"+init=ESRI:102008")
counties<-st_transform(counties_20m_sf,"+init=ESRI:102008")



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
    


#select a lake    
  input<-lake
    
#get counties that lake intersects
    fips<-slice(counties,unlist(st_intersects(input,counties)))%>% # select rows from counties that intersect input
      as.data.frame()%>%  # convert from sf object to data.frame
      select(STATEFP,COUNTYFP)%>%  # keep state and county fields 
      mutate(STATEFP=as.numeric(as.character(STATEFP)),COUNTYFP=as.numeric(as.character(COUNTYFP))) #convert from factor to numeric
    
#get the blockgroups for state(s) and county(s)
blk<-list()
for(i in c(1:nrow(fips))){
    blk[[i]]<-st_as_sf(block_groups(state=fips$STATEFP[i],county=fips$COUNTYFP[i],year=2010))
}

#rbind the blockgroups
blockgrp<-blk[[1]]
  if(length(blk)>1) for(i in c(2:length(blk))) blockgrp<-rbind(blockgrp,blk[[i]])

#reproject blockgrp
blockgrp<-st_transform(blockgrp,"+init=ESRI:102008")

#reduce to intersection with input
blockgrp<-slice(blockgrp,unlist(st_intersects(input,blockgrp)))

#plot input and blockgrp
plot(blockgrp[1])
plot(input[1], add = TRUE)

int<-st_intersection(input,blockgrp)

a<-get_decennial(geography = "block group" , state="44", county="003", variables = "P0010001", year = 2010)

table(int$GEOID10 %in% a$GEOID)

b<-block_groups(state="44",county="003",year=2010)

table(a$GEOID %in% b$GEOID10)

########
#add areas
input$area_input <-as.numeric(st_area(input$geometry))
blockgrp$area_blockgrp <-as.numeric(st_area(blockgrp$geometry))

#run the intersect function
int <- st_intersection(input, blockgrp)

plot(int[1])

mapview(int[1]) + mapview(lake)

#add area for the intersections
int$area_int <-as.numeric(st_area(int$geometry))

#check that the area_int == area_input
area_int<-sum(int$area_int, na.rm = TRUE)
area_input<-sum(input$area_input, na.rm = TRUE)
all.equal(area_int,area_input)

#add percent overlap between the input and the intersection to "int"
  #the idea is to make sure that all of the input area is included in the intercect.  This is important in areas like national borders 
  #where the input area may extend across and therefore be clipped.
  # input could also spill over a coastline and be clipped as well.
int$per_overlap<-round(100*area_int / area_input)

select(int, STATEFP10, COUNTYFP10, TRACTCE10, BLKGRPCE10, GEOID10, NAMELSAD10, MTFCC10, FUNCSTAT10, ALAND10, AWATER10, INTPTLAT10, INTPTLON10, COUNTYFP, STATEFP, area_blockgrp, area_int, per_overlap)

#group data by county area and calculate the total arable land area per county
#output as new tibble
tb_ArableByCounty <- int %>%
  group_by(County_UA) %>%
  summarise(areaArable = sum(areaArable))

#change data type of areaArable field to numeric (from 'S3: units' with m^2 suffix)
tb_ArableByCounty$areaArable <- as.numeric(tb_ArableByCounty$areaArable)

#join tibble to original county polygon shapefile and export as new shapefile
shp_out <- st_write(merge(counties, tb_ArableByCounty, by = 'County_UA'), "ArablebyCounty.shp")





