rm(list=ls())

library(sf)
library(ggplot2)
library(tidyverse)
library(ggmap)
library(ggrepel)
library(rstudioapi)
library(stringr)
library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(patchwork)
library(mapview)
register_google(key = "AIzaSyDHvaszi8kLu6t2pR1U_oz1MocuYMIeog4")


#read in FL unified reef map habitat data: 
# coral_sf =  st_read(dsn = "/Users/heidihirsh/Desktop/Final_UnifiedReefMap_Version2.0/FWC_UnifiedFloridaReefMap_v2.0.gdb", layer="UnifiedReefMap")
coral_sf =  st_read(dsn = "/Users/heidi.local/Desktop/FLK_data/Final_UnifiedReefMap_Version2.0/FWC_UnifiedFloridaReefMap_v2.0.gdb", layer="UnifiedReefMap")

#load benthic composition lookup table: 
# benthicLU = read.csv("/Users/heidihirsh/Desktop/FLKhabitat/LookUp/ClassLv4_BenthicLookup_V1_cleaner.csv")
# benthicLU = read.csv("/Users/heidi.local/Desktop/FLKhabitat/LookUp/ClassLv4_BenthicLookup_V1_cleaner.csv")
benthicLU = read.csv("/Users/heidi.local/Documents/Git_Projects/FLK-Modeling-Hirsh/ClassLv4_BenthicLookup_V1_cleaner_fixed.csv")


# year_fl =list.files(path = paste0("/Users/heidi.local/Desktop/bow_ties_sample_ids"),full.names=T)  
year_fl =list.files(path = paste0("/Users/heidi.local/Desktop/bow_ties_sample_ids_cleaned"),full.names=T)   #use file where I removed "12_20170801_072200" (empty shp)
length(year_fl)
yearBows=NULL

# y_i=5
#loop through years up here...
for (y_i in 1:length(year_fl)) {   
  splitYFile=str_split(string=year_fl[y_i],"/")
  splitYFile
  Year = splitYFile[[1]][6]
  Year
  
#read in all bow tie files for the year (identify them)
# bow_fl = list.files(path = paste0("/Users/heidi.local/Desktop/bow_ties_sample_ids/2021"),full.names=T)  
# bow_fl = list.files(path = paste0("/Users/heidi.local/Desktop/bow_ties_sample_ids/",Year),full.names=T)
bow_fl = list.files(path = paste0("/Users/heidi.local/Desktop/bow_ties_sample_ids_cleaned/",Year),full.names=T)  

bow_fl
# bow_fl
length(bow_fl)

#pull out relevant info (date, #days)
# f_i=14 #something weird at file 13 for 2017
bows=NULL
for (f_i in 1:length(bow_fl)) {   #loop through sampling sites for the given year

  splitBFile=str_split(string=bow_fl[f_i],"/")

  bowGroup = splitBFile[[1]][7]

  layerName = paste0('bow_ties_',bowGroup)

  
  #read shapefile (includes forward and backward bows for 1-7 day durations at the sampling site for a given date/time)
  bows = st_read(dsn = bow_fl[f_i],layer=layerName) #this is a DAY of forward and backward bows (not one bow)
  
  #pull visitID, date, time from bowGroup
  splitName=str_split(string=bowGroup,"_")

  # visitID=substr(bowGroup,10,nchar(bowGroup))
  visitID=bowGroup
  # year=substr(splitName[[1]][4],1,4)  
  # month=substr(splitName[[1]][4],5,6)  
  # day=substr(splitName[[1]][4],7,8)   
  # time=substr(splitName[[1]][5],1,6)   
  
  bows$year=Year
  bows$visitID=visitID
  # bows$date=paste0(year,month,day)
  # bows$year=year
  # bows$month=month
  # bows$day=day
  # bows$time=time
  bows$duration=bows$ndays
  # bows$name = paste0('bow_',year,month,day,'_',duration,'days') #need to use specific duration (loop through if we want this)
  
  bows$bowID = paste0(bows$visitID,"_",bows$simu,"_",bows$ndays)
  
  yearBows =  rbind(yearBows,bows)
  # dim(yearBows)
  print(paste0(f_i,' of ',length(bow_fl)))
  
}

}
View(yearBows)
# st_write(yearBows, '/Users/heidi.local/Desktop/FLK_data/Bows_AllYears.shp')

# ________________________________ADD VOLUME_____________________________________
#Calculate volume for each bow tie (use bathymetry average over "sector" x area)
#read in bathymetry data:
# bathy_sf=st_read(dsn = "/Users/heidihirsh/Desktop/FLK_data/mesh_florida",layer="mesh_florida")
bathy_sf=st_read(dsn = "/Users/heidi.local/Desktop/FLK_data/mesh_florida",layer="mesh_florida")
head(bathy_sf$bathymetry) #bathymetry (depth) for each polygon in the mesh

volumeBows=NULL
#read in bowties sf dataframe 
volumeBows = st_read('/Users/heidi.local/Desktop/FLK_data/Bows_AllYears.shp')
dim(volumeBows)
# 18494     8

volumeBows[, 'bowArea.m2'] = NA
volumeBows[, 'bowVol.m3'] = NA
# d_i =717
for (d_i in 1:nrow(volumeBows)) {
  Bow=volumeBows[d_i,]
  # mapview(Bow)
  
  print(paste0('starting bow ',d_i,' of ',nrow(volumeBows)))
  tick = Sys.time()
  
  #CRS matches!
  # st_crs(Bow) #"D_Unknown_based_on_WGS84_ellipsoid"
  # st_crs(bathy_sf) #"D_Unknown_based_on_WGS84_ellipsoid" 

  over_bath = st_intersection(bathy_sf,Bow)
  # st_crs(over_bath)  #"D_Unknown_based_on_WGS84_ellipsoid"
  over_bath$meshArea = st_area(over_bath)
  over_bath$meshVolume = over_bath$bathymetry * over_bath$meshArea
    
  volumeBows$bowArea.m2[d_i]=sum(over_bath$meshArea,na.rm=T)
  volumeBows$bowVol.m3[d_i]=sum(over_bath$meshVolume,na.rm=T)

  print('done with intersection')
  
  tock= Sys.time()
  elmin=round(difftime(tock,tick,units='mins'),1)
  
  print(paste0("Completed ",d_i," of ",nrow(volumeBows)," in ",elmin," min. Estimated time remaining: ",(nrow(volumeBows)-d_i)*elmin,' min. Current time: ',Sys.time()))
  
  
  
} #end loop to pull bathymetry data and calculate bow volume

# st_write(volumeBows, '/Users/heidi.local/Desktop/BowVol_start2Nov2022.shp')
# dim(volumeBows)




# ________________________________ADD BENTHIC INDICES_____________________________________

benthicBows=NULL
#read in bowties+volume sf dataframe 
benthicBows = st_read('/Users/heidi.local/Desktop/BowVol_start2Nov2022.shp')
dim(benthicBows) #18494    15
#don't need forward bows: 
benthicBows = benthicBows[which(benthicBows$simu=='backward'),]
dim(benthicBows) #9247   15

#limit to only 7day bows
# benthicBows = benthicBows[which(benthicBows$ndays==7),]
# dim(benthicBows) # 1321   15

benthicBows[, 'CALCi.m2'] = NA
benthicBows[, 'ALGi.m2'] = NA
benthicBows[, 'SGi.m2'] = NA
benthicBows[, 'NBi.m2'] = NA
benthicBows[, 'PercentCheck'] = NA

# b_i=1
for (b_i in 1:nrow(benthicBows)) {
  # for (b_i in 1:7) {
  Bbow=benthicBows[b_i,]
  print(paste0('starting bow ',b_i,' of ',nrow(benthicBows)))
  tick = Sys.time()
  # st_crs(Bbow) #D_Unknown_based_on_WGS84_ellipsoid
  # st_crs(coral_sf) #NAD83
  

  #Tranform coral_sf to match Bbow CRS:
  coral_sf.t = st_transform(coral_sf,st_crs(Bbow))
  # st_crs(coral_sf.t) #"D_Unknown_based_on_WGS84_ellipsoid"
  coral_sf.tv = coral_sf.t[which(st_is_valid(coral_sf.t)),]
  over_bow = st_intersection(coral_sf.tv,Bbow)
  # st_crs(over_bow)
  print('done with intersection')
  
  #Get area of each benthic class
  over_bow$class_area = as.numeric(st_area(over_bow)) #m2
  bowComp= over_bow %>% st_drop_geometry() %>% dplyr::select(ClassLv4,Shape_Area,class_area) %>% group_by(ClassLv4) %>% summarize(a=sum(class_area,na.rm=TRUE))
  bowComp$p = bowComp$a/st_area(Bbow) #!!change to Bbow.t if I do the tranformation in the other direction
  bowComp %>% arrange(desc(p))
  print(paste0('Percent sum check: ',sum(bowComp$p),' (should be <1)' ))
  
  # mapview(over_bow,zcol="ClassLv4")
  # plot(Bbow)
  
  BowWeight = left_join(bowComp,benthicLU,by='ClassLv4')
  BowWeight$NotBio = 1-BowWeight$Bio
  # isit1 = BowWeight$NotBio + BowWeight$coral + BowWeight$BioNoCoral
  # isit1 #something funky about discontinuous seagrass?
  
  #biomass index for each class
  BowWeight$Ci= BowWeight$a*BowWeight$coral
  BowWeight$Ai= BowWeight$a*BowWeight$Algae #fix spelling!
  BowWeight$Si= BowWeight$a*BowWeight$Seagrass
  BowWeight$NBi= BowWeight$a*BowWeight$NotBio
  
  #summarize for each biomass index (all classes contribute to each)
  benthicBows$CALCi.m2[b_i]=sum(BowWeight$Ci,na.rm=T)
  benthicBows$ALGi.m2[b_i]=sum(BowWeight$Ai,na.rm=T)
  benthicBows$SGi.m2[b_i]=sum(BowWeight$Si,na.rm=T)
  benthicBows$NBi.m2[b_i]=sum(BowWeight$NBi,na.rm=T)
  benthicBows$PercentCheck[b_i]=sum(bowComp$p)
  
  # BowWeight$tIndex = BowWeight$Ci + BowWeight$Ai + BowWeight$Si + BowWeight$NBi  #should this end up being the total area?
  # BowWeight$tIndex - BowWeight$a #why isn't this always 0?
  
  # bow_area = st_area(test) 
  # sum(BowWeight$a)/bow_area #(it should be less than 1)
  # sum(BowWeight$p) 
  
  tock= Sys.time()
  elmin=round(difftime(tock,tick,units='mins'),1)
  
  print(paste0("Completed ",b_i," of ",nrow(benthicBows)," in ",elmin," min. Estimated time remaining: ",(nrow(benthicBows)-b_i)*elmin,' min. Current time: ',Sys.time()))
  
  
  
} #end loop to pull benthic info

st_write(benthicBows, '/Users/heidi.local/Desktop/BowVolBenthic_allRTdays_start7Nov2022.shp')

# st_write(benthicBows, '/Users/heidi.local/Desktop/BowVolBenthic_7day_start7Nov2022.shp')
dim(benthicBows)
