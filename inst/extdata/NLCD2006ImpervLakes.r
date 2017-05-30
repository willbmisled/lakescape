v='NLCD2006ImpervLakes_20130212.r'

#Enter Location of NLCD Grid data 
  Imperv<-'L:/Public/Milstead_Lakes/NLCD2006/nlcd2006_impervious_5-4-11.img'
#Enter loaction of the lake shapefile
  MRB1Lakes<-'L:/Public/Milstead_Lakes/WaterbodyDatabase/Shapefiles/MRB1_WBIDLakes.shp'

#load (install) required R packages
  if (!'sp' %in% installed.packages()) install.packages('sp');require(sp) #CRS overlay
  if (!'maptools' %in% installed.packages()) install.packages('maptools');require(maptools)  #readShapePoly
  if (!'rgeos' %in% installed.packages()) install.packages('rgeos');require(rgeos) #gBuffer
  if (!'rgdal' %in% installed.packages()) install.packages('rgdal');require(rgdal) #readGDAL  
  if (!'raster' %in% installed.packages()) install.packages('raster');require(raster) #readGDAL  
  
#Projection info   
#ESRI USA_Contiguous_Albers_Equal_Area_Conic_USGS_version
AlbersContigUSGS<-CRS('+proj=aea +x_0=0 +y_0=0 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +lat_0=23 +units=m +datum=NAD83')  
#ESRI GCS_North_American_1983
    NAD83<-CRS("+proj=longlat +datum=NAD83")

#Get the lake shapefile
  MRB1Lakes <- readShapePoly(MRB1Lakes, proj4string=NAD83)

#Reproject Lakes to the same CRS as the Population grid
#which(MRB1Lakes$WB_ID==22302965) #27357 index value for lake champlain
MRB1Lakes<-spTransform(MRB1Lakes[-27357,],AlbersContigUSGS)   #lake champlain removed

#Associate lake holes (islands) with the correct polygon.  This is an important step.
slot(MRB1Lakes, "polygons") <- lapply(slot(MRB1Lakes, "polygons"), checkPolygonsHoles)

#get the NLCD grid data
Imperv<-raster(Imperv)
  #image(Imperv)
  #extent(Imperv)

#Imperv is the impervious cover for all lower 48 states.  Reduce to bbox(MRB1Lakes) + 10km
    B<-bbox(MRB1Lakes)  
    Extent<-c(B[1,1]-10000,B[1,2]+10000,B[2,1]-10000,B[2,2]+10000)
  #Get Impervious data for the Largest Lake Buffer + 3 30x30 pixels 
    Imperv<-crop(Imperv,Extent)
    
#save the data
File<-paste('L:/Public/Milstead_Lakes/RData/Data_',v,'da',sep='')  #file to store data:  v is the version (see top of file)
save(Imperv,MRB1Lakes,file=File)  #save the last records.
#load("L:/Public/Milstead_Lakes/RData/Data_NLCD2006ImpervLakes_20130131.rda")

################################


##############################
####function to calculate the impervious cover area and percent in the buffer.
CalcImperv<- function(Buf,Imp){ 
  gc() #release unused memory 
  Mask<-mask(Imp, Buf)  #extract impervious cover pixels in the buffer (all others changed to NA)
  a<-table(getValues(Mask),useNA='ifany')  #the NA are pixels outside of bbox of buffer.  Values of "127" are the real NA
  a<-na.exclude(data.frame(Percent=as.numeric(names(a)),a)[,-2]) #replace percents stored as factors with values
  a$ImpPix<-a$Percent*a$Freq/100 #processing step-convert frequency distribution to number of impervious pixels
  MissingPix<-ifelse(max(a$Percent)==127,a[a$Percent==127,'Freq'],0)  #number of missing pixels
  TotalPix<-sum(a$Freq)  #total number of pixels in buffer
  PercentNA<-round(MissingPix/TotalPix,3)*100  #percent NA cells in buffer
  BufAreaKm2<-round(sum(a$Freq)*30*30/1000000,3)  #total area of buffers (based on pixel number)-includes NA (value=127)
  a<-subset(a,a$Percent<=100)  #remove pixels with missing values (=127)
  BufAreaKm2Adj<-round(sum(a$Freq)*30*30/1000000,3)  #this doesn't include Pixels with missing data
  ImpervAreaKm2<-round(sum(a$ImpPix)*30*30/1000000,3) #calculate the area of impervious cover
  PercentImperv<-round(sum(ImpervAreaKm2)/sum(BufAreaKm2Adj),4)*100 #calculate the percent of impervious cover 
  data.frame(PercentImperv,ImpervAreaKm2,BufAreaKm2,BufAreaKm2Adj,PercentNA)
}
####function to calculate the number of people and density in the buffer.
####################works
Impervious<- function(WB_ID,PlotYN){       #WB_ID ID of lake, PlotYN enter 'Y' to generate figure or 'N' for text only
  Start<-Sys.time()  #Record Start Time
  gc() #release unused memory
  Lake<-MRB1Lakes[match(WB_ID,MRB1Lakes$WB_ID),]  #select lake
  radius<-round(sqrt(Lake$AlbersArea/pi)) #Calculate the approximate lake radius as sqrt(Area/pi)
  #Get the buffers
    Buf300<-gDifference(gBuffer(Lake,width=300),Lake)
    Buf1000<-gDifference(gBuffer(Lake,width=1000),Lake)
    Buf2500<-gDifference(gBuffer(Lake,width=2500),Lake)
    BufRadius<-gDifference(gBuffer(Lake,width=radius),Lake)
  #Select offset and extent for maximum grid    if(radius<=2500)B<-bbox(Buf2500) else B<-bbox(BufRadius)
    if(radius<=2500)B<-bbox(Buf2500) else B<-bbox(BufRadius)
    Extent<-c(B[1,1]-90,B[1,2]+90,B[2,1]-90,B[2,2]+90)
  #Get Impervious data for the Largest Lake Buffer + 3 30x30 pixels 
    Imp<-crop(Imperv,Extent)
  #plot grid and buffers    ###optional PlotYN='Y' to plot; PlotYN='N' to return text only.
    if(PlotYN=='Y'){
      image(Imp,main=paste('WB_ID = ',WB_ID))
      plot(Buf300,add=T)                  
      plot(Buf1000,add=T)
      plot(Buf2500,add=T)
      plot(BufRadius,add=T)
      title(sub=v)
      }
 #calculate imperv area and percent for each buffer
    a<-data.frame(matrix(nrow=4,ncol=7)) 
    a[1,]<-c(WB_ID,300,CalcImperv(Buf300,Imp)) 
    a[2,]<-c(WB_ID,1000,CalcImperv(Buf1000,Imp)) 
    a[3,]<-c(WB_ID,2500,CalcImperv(Buf2500,Imp)) 
    a[4,]<-c(WB_ID,radius,CalcImperv(BufRadius,Imp))  
    names(a)<-c('WB_ID','BufWidthM','PercentImperv','ImpervAreaKm2','BufferAreaKm2','BufferAreaKm2Adj','PercentNA')
print(paste(WB_ID,' Time Elapsed = ',round(Sys.time()-Start),' seconds')) #how long did the process take?
print('') #extra line

return(a)
}    
############################ 
 
#examples  
#
Impervious(sample(MRB1Lakes$WB_ID,1),'Y')  #random lake without plot of grid and buffers   
#Impervious(sample(MRB1Lakes$WB_ID,1),'N')  #random lake with results only       
#Impervious(4290651,'Y')  #big lake     
#Impervious(5195222,'Y')  #lake on the grid border
#Impervious(1720193,'Y')  #lake cannot be buffered with R64 but works in R32.  Why?
#Impervious(9326606,'Y')  #lake cannot be buffered with R64 but works in R32.  Why?
#Impervious(6114592,'Y')  #this one looks like a cross section through the small intestines.
#Impervious(Lakes[749,],'Y')
#i<-23;Impervious(MRB1Lakes$WB_ID[i],'Y')

##loop to run a whole bunch of lakes
LakeImperv<-data.frame(matrix(NA,nrow=N*4,ncol=7))  #build data.frame to store the results
names(LakeImperv)<-c('WB_ID','BufWidthM','PercentImperv','ImpervAreaKm2','BufferAreaKm2','BufferAreaKm2Adj','PercentNA')
File<-paste('L:/Public/Milstead_Lakes/RData/',v,'da',sep='')  #file to store data:  v is the version (see top of file)

#start loop
B<-1  #row number to start processing lakes; this will be 1 to start.
N<-nrow(MRB1Lakes)    #last row to process; 
#N<-5    #last row to process; 
S<-100                 #save the work every "S" lakes
Counter<-0             #counter; should be zero to start
for(i in c(B:N)){
LakeImperv[(4*i-3):(i*4),]<-Impervious(MRB1Lakes$WB_ID[i],'N')
Counter<-Counter+1
if (Counter==S) save(LakeImperv,file=File)
if(Counter==S) Counter<-0
}
#End Loop

#add field to distinguish between fixed width and variable (radius) width buffers.  
#some radius buffers the same as the fixed width.
LakeImperv$BufType<-c('fixed','fixed','fixed','radius')
  #table(LakeImperv$BufType)
  #table(LakeImperv[LakeImperv$BufWidthM==300,'BufType'])

#save the data
#save(LakeImperv,file="L:/Public/Milstead_Lakes/RData/NLCD2006ImpervLakes_20130212.rda")  #save the last records.
#load("L:/Public/Milstead_Lakes/RData/NLCD2006ImpervLakes_20130212.rda")
#write.table(LakeImperv, file='c:/temp/LakeImperv.csv',row.names=F,sep=',')

# Write data to tblMNKA in fj.mdb
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("C:/Bryan/EPA/Data/WaterbodyDatabase/WaterbodyDatabase.mdb")
FJ <- sqlSave(con,dat=LakeImperv,tablename='LakeNLCD2006ImperviousCover',append=F,rownames=F)
close(con)


##Data Definitions  n=28,121 (Lake Champlain excluded due to size and complexity)
  #WB_ID:  Lake identification number
  #BufWidthM: (m) width of lake buffer for calculations
  #PercentImperv:  (%) Percent impervious cover in buffer = ImpervAreaKm2/BufferAreaKm2Adj/100
  #ImpervAreaKm2:  (Km2) total impervious cover in the buffer
  #BufferAreaKm2:  (Km2) Area of entire buffer-number of grid cells*30*30/1000000 
  #BufferAreaKm2Adj (Km2) Area of Non-NA buffer-number of grid cells with data*30*30/1000000 
  #PercentNA:  (%) (number of NA grid cells)/(total number of grid cells)*100
  #BufType: "Fixed" = standard buffer width; "Radius" buffer width = radius of a circle with Area = lake area.
  
  
 