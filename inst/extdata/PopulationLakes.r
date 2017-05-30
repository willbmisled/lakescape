v='PopulationLakes_20130212.r'

#Enter Location of Pop Grid data 
  #BlockData<-'C:/Bryan/EPA/Data/Population/blkdensr'
  BlockData<-'L:/Public/Milstead_Lakes/Population/blkdensr'
  
#Enter loaction of the lake shapefile
  #LakeShp<-'C:/Bryan/EPA/Data/WaterbodyDatabase/Shapefiles/MRB1_WBIDLakes.shp' 
  LakeShp<-'L:/Public/Milstead_Lakes/WaterbodyDatabase/Shapefiles/MRB1_WBIDLakes.shp'

#load (install) required R packages
  if (!'sp' %in% installed.packages()) install.packages('sp',repos='http://cran.cnr.Berkeley.edu');require(sp) #CRS overlay
  if (!'maptools' %in% installed.packages()) install.packages('maptools',repos='http://cran.cnr.Berkeley.edu');require(maptools)  #readShapePoly
  if (!'rgeos' %in% installed.packages()) install.packages('rgeos',repos='http://cran.cnr.Berkeley.edu');require(rgeos) #gBuffer
  if (!'rgdal' %in% installed.packages()) install.packages('rgdal',repos='http://cran.cnr.Berkeley.edu');require(rgdal) #readGDAL  

#proj4string Projection strings to use with the R package SP
  #ESRI USA_Contiguous_Albers_Equal_Area_Conic_USGS_version
    AlbersContigUSGS<-CRS('+proj=aea +x_0=0 +y_0=0 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +lat_0=23 +units=m +datum=NAD83')
  #ESRI GCS_North_American_1983
    NAD83<-CRS("+proj=longlat +datum=NAD83")

#Get the lake shapefile
  MRB1Lakes <- readShapePoly(LakeShp, proj4string=NAD83)

#Reproject Lakes to the same CRS as the Population grid
#which(MRB1Lakes$WB_ID==22302965) #27357 index value for lake champlain
MRB1Lakes<-spTransform(MRB1Lakes[-27357,],AlbersContigUSGS)   #lake champlain removed

#Associate lake holes (islands) with the correct polygon.  This is an important step.
slot(MRB1Lakes, "polygons") <- lapply(slot(MRB1Lakes, "polygons"), checkPolygonsHoles)

##############################

####function to calculate the number of people and density in the buffer.
CalcDensity<- function(Buf,Population){ 
  gc() #release unused memory
  a<-!is.na(overlay(Population,Buf)) #select grid cells included in the buffer
  Pop<-round(sum(Population$band1[a],na.rm=T)) #sum population/grid cell for all cells in buffer
  BufferAreaKm2<-round(length(Population$band1[a])*30*30/1000000,3)   #area of buffer in km2 -includes NA pixels
  BufferAreaKm2Adj<-round(sum(!is.na(Population$band1[a]))*30*30/1000000,3)   #area of buffer in km2 -includes NA pixels
  PercentNA<-round(sum(is.na(Population$band1[a]))/length(Population$band1[a]),3)*100 #percent NA cells in buffer
  PeoplePerKm2<-round(Pop/BufferAreaKm2Adj,2)    #people per sq. km
  data.frame(Pop,PeoplePerKm2,BufferAreaKm2,BufferAreaKm2Adj,PercentNA)
  }
####function to calculate the number of people and density in the buffer.
PopDensity<- function(WB_ID,PlotYN){       #WB_ID ID of lake, PlotYN enter 'Y' to generate figure or 'N' for text only
  Start<-Sys.time()  #Record Start Time
  gc() #release unused memory
  Lake<-MRB1Lakes[match(WB_ID,MRB1Lakes$WB_ID),]  #select lake
  radius<-round(sqrt(Lake$AlbersArea/pi)) #Calculate the approximate lake radius as sqrt(Area/pi)
  #Get the buffers
    Buf300<-gDifference(gBuffer(Lake,width=300),Lake)
    Buf1000<-gDifference(gBuffer(Lake,width=1000),Lake)
    Buf2500<-gDifference(gBuffer(Lake,width=2500),Lake)
    BufRadius<-gDifference(gBuffer(Lake,width=radius),Lake)
  #Select offset and extent for maximum grid
    if(radius<=2500)B<-bbox(Buf2500) else B<-bbox(BufRadius)
      Offset<-c(round((3013041-B[2,2])/30)-2,round((B[1,1]-1089285)/30)-2)
      Extent<-c(round((B[2,2]-B[2,1])/30)+3,round((B[1,2]-B[1,1])/30)+3)
  #Get population grid data for the Largest Lake Buffer 
      #subset the population grid data
        #Population<-readGDAL(BlockData, offset=Offset, region.dim=Extent)
        Population<-readGDAL(BlockData, offset=Offset, region.dim=Extent)
  #plot grid and buffers    ###optional PlotYN='Y' to plot; PlotYN='N' to return text only.
    if(PlotYN=='Y'){
      image(Population)
      plot(Buf300,add=T)                  
      plot(Buf1000,add=T)
      plot(Buf2500,add=T)
      plot(BufRadius,add=T)
      title(main=paste('WB_ID = ',WB_ID),sub=v)
    }
 #calculate population and density for each buffer
    a<-data.frame(matrix(nrow=4,ncol=7)) 
    a[1,]<-c(WB_ID,300,CalcDensity(Buf300,Population)) 
    a[2,]<-c(WB_ID,1000,CalcDensity(Buf1000,Population)) 
    a[3,]<-c(WB_ID,2500,CalcDensity(Buf2500,Population)) 
    a[4,]<-c(WB_ID,radius,CalcDensity(BufRadius,Population))  
    names(a)<-c('WB_ID','BufWidthM','Pop','PopDensityKm2','BufferAreaKm2','BufferAreaKm2Adj','PercentNA')
    
print(paste(WB_ID,' Time Elapsed = ',round(Sys.time()-Start),' seconds')) #how long did the process take?
print('') #extra line

return(a)
}    
############################ 
#get a list of WB_ID's with SPARROW data
#require(RODBC)   #Package RODBC must be installed
#con <- odbcConnectAccess("L:/Public/Milstead_Lakes/WaterbodyDatabase/MRB1.mdb")
#Lakes <- sqlQuery(con, "
#SELECT tblWBID_SparrowLoadsDSS.WB_ID FROM tblWBID_SparrowLoadsDSS;
#")
#close(con)
#str(Lakes)
  
#examples  
#PopDensity(sample(MRB1Lakes$WB_ID,1),'Y')  #random lake without plot of grid and buffers   
#PopDensity(sample(MRB1Lakes$WB_ID,1),'N')  #random lake with results only       
#PopDensity(4290651,'Y')  #big lake     
#PopDensity(5195222,'Y')  #lake on the grid border
#PopDensity(1720193,'Y')  #lake cannot be buffered with R64 but works in R32.  Why?
#PopDensity(9326606,'Y')  #lake cannot be buffered with R64 but works in R32.  Why?
#PopDensity(6114592,'Y')  #this one looks like a cross section through the small intestines.
#PopDensity(Lakes[749,],'Y')
#i<-23;PopDensity(MRB1Lakes$WB_ID[i],'Y')

##loop to run a whole bunch of lakes
#start loop
B<-1  #row number to start processing lakes; this will be 1 to start.
N<-nrow(MRB1Lakes)    #last row to process; 
#N<-5    #last row to process; 
S<-100                 #save the work every "S" lakes
Counter<-0             #counter; should be zero to start
LakePop<-data.frame(matrix(NA,nrow=N*4,ncol=7))  #build data.frame to store the results
names(LakePop)<-c('WB_ID','BufWidthM','Pop','PopDensityKm2','BufferAreaKm2','BufferAreaKm2Adj','PercentNA')
File<-paste('L:/Public/Milstead_Lakes/RData/',v,'da',sep='')  #file to store data:  v is the version (see top of file)
#Start Loop 
for(i in c(B:N)){
LakePop[(4*i-3):(i*4),]<-PopDensity(MRB1Lakes$WB_ID[i],'N')
Counter<-Counter+1
if (Counter==S) save(LakePop,file=File)
if(Counter==S) Counter<-0
}
#End Loop

#add field to distinguish between fixed width and variable (radius) width buffers.  
#some radius buffers the same as the fixed width.
LakePop$BufType<-c('fixed','fixed','fixed','radius')
  #table(LakePop$BufType)
  #table(LakePop[LakePop$BufWidthM==300,'BufType'])
 
  
#save the data
#save(LakePop,file="L:/Public/Milstead_Lakes/RData/PopulationLakes_20130212.rda")  #save the last records.
#load("L:/Public/Milstead_Lakes/RData/PopulationLakes_20130212.rda")
#write.table(LakePop, file='c:/temp/LakePop.csv',row.names=F,sep=',')

# Write data to tblMNKA in fj.mdb
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("C:/Bryan/EPA/Data/WaterbodyDatabase/WaterbodyDatabase.mdb")
FJ <- sqlSave(con,dat=LakePop,tablename='LakeCensus2000Population',append=F,rownames=F)
close(con)


##Data Definitions
  #WB_ID:  Lake identification number
  #BufWidthM: (m) width of lake buffer for calculations
  #Pop:  (people) number of people in the buffer
  #PopDensityKm2:  (People/Km2) Pop/BufferAreaKm2Adj 
  #BufferAreaKm2:  (Km2) Area of entire buffer-number of grid cells*30*30/1000000 
  #BufferAreaKm2Adj (Km2) Area of Non-NA buffer-number of grid cells with data*30*30/1000000 
  #PercentNA:  (%) (number of NA grid cells)/(total number of grid cells)
  #BufType: "Fixed" = standard buffer width; "Radius" buffer width = radius of a circle with Area = lake area.
  


# Write data to tblMNKA in fj.mdb
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("C:/Bryan/EPA/Data/WaterbodyDatabase/WaterbodyDatabase.mdb")
FJ <- sqlSave(con,dat=LakeImperv,tablename='LakeNLCD2006ImperviousCover',append=F,rownames=F)
close(con)   
  
 





