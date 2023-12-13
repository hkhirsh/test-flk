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
library(leaflet)
register_google(key = "AIzaSyB3mq0nJtcNWvQUNuqH_t-MLxEUmwDDmGk") #api updated OCT23

#run original bowties (no land) on HH mac 12/24/2022
#update: i messed up the 12/24 run because I buffered in and out on the convex hulls (dumb). Need to rerun without doing that!

#read in FL unified reef map habitat data: 
coral_sf =  st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/Final_UnifiedReefMap_Version2.0/FWC_UnifiedFloridaReefMap_v2.0.gdb", layer="UnifiedReefMap")
# coral_sf =  st_read(dsn = "/Users/heidi.local/Desktop/FLK_data/Final_UnifiedReefMap_Version2.0/FWC_UnifiedFloridaReefMap_v2.0.gdb", layer="UnifiedReefMap")

#load benthic composition lookup table: 
benthicLU = read.csv("/Users/heidi.k.hirsh/Documents/Git_Projects/FLK-Modeling-Hirsh-copy-DONTTOUCH//ClassLv4_BenthicLookup_V1_Rank_fixed_NOland_Clean.csv")
# benthicLU = read.csv("/Users/heidihirsh/Documents/Git_Projects/FLK-Modeling-Hirsh/ClassLv4_BenthicLookup_V1_Rank_fixed_NOland_Clean.csv")
# benthicLU = read.csv("/Users/heidi.local/Documents/Git_Projects/FLK-Modeling-Hirsh/ClassLv4_BenthicLookup_V1_Rank_fixed_NOland_Clean.csv")

year_fl = list.files(path = paste0("/Users/heidi.k.hirsh/Desktop/old_bowties_no_land_noAug1"),full.names=T) 
## year_fl =list.files(path = paste0("/Users/heidihirsh/Desktop/FLK_data/concave_bowtie_thingies"),full.names=T)
# year_fl =list.files(path = paste0("/Users/heidi.local/Desktop/FLK_data/concave_bowtie_thingies"),full.names=T)  #use this to run concave version
length(year_fl)
yearBows=NULL

# y_i=5
#loop through years up here...
for (y_i in 1:length(year_fl)) {   
  splitYFile=str_split(string=year_fl[y_i],"/")
  # splitYFile
  Year = splitYFile[[1]][6]
  # Year
  
#read in all bow tie files for the year (identify them)
bow_fl = list.files(path = paste0("/Users/heidi.k.hirsh/Desktop/old_bowties_no_land_noAug1/",Year),full.names=T)
# bow_fl = list.files(path = paste0("/Users/heidi.local/Desktop/FLK_data/concave_bowtie_thingies/",Year),full.names=T)
#I removed the file for 12_20170801 
#for the convex approach I removed 12_20170801_072200
# bow_fl
# length(bow_fl)

#pull out relevant info (date, #days)
# f_i=1
bows=NULL
for (f_i in 1:length(bow_fl)) {   #loop through sampling sites for the given year

  splitBFile=str_split(string=bow_fl[f_i],"/")
  # splitBFile
  bowGroup = splitBFile[[1]][7]
  # bowGroup
  layerName = paste0('bowties_no_land_',bowGroup)
  # layerName = paste0('bowties_inter_mesh_',bowGroup)
  # layerName
  
  #read shapefile (includes forward and backward bows for 1-7 day durations at the sampling site for a given date/time)
  bows.original = st_read(dsn = bow_fl[f_i],layer=layerName) #this is a DAY of forward and backward bows (not one bow)
  
  #concave out and in 
  bows_utm17 = st_transform(bows.original, 26917)
  # View(bows_utm17)
  # class(bows_utm17$geometry)
  # plot(bows_utm17)
  
  D=300 #(buffer width in meters)
  bows.bo = st_buffer(bows_utm17, dist=D, nQuadSegs=30)  
  bows.bi = st_buffer(bows.bo, dist=-D, nQuadSegs=30)
  # View(bows.bi)
  
  # ploThis = bows.bi
  # ploThis_t = st_transform(ploThis,crs='+proj=longlat +datum=WGS84')
  # 
  # clrs = rainbow(7, start = 0, end = 0.8)
  # clrs_rev <- rev(rainbow(7, start = 0, end = 0.8))
  # d=ploThis_t$ndays
  # pal = colorNumeric(palette = clrs_rev, domain = min(d):max(d))
  # leaflet(data=ploThis_t) %>%
  #   addProviderTiles('Esri.WorldImagery') %>%
  #   addPolygons(data=subset(ploThis_t,simu=='backward'),fillOpacity=0.1,weight=1,color = ~pal(d))

  crs(bows.bi) # +proj=utm +zone=17 +datum=NAD83 +units=m +no_defs 
  crs(bows.original) #+proj=longlat +ellps=WGS84 +no_defs 
  bows=bows.bi
  # mapview(bows.original)
  # mapview(bows.bi)
  
  #pull visitID, date, time from bowGroup
  splitName=str_split(string=bowGroup,"_")
  # splitName
  
  visitID=bowGroup
  # visitID
  bows$year=Year
  bows$visitID=visitID
  bows$duration=bows$ndays
  bows$bowID = paste0(bows$visitID,"_",bows$simu,"_",bows$ndays)
  
  yearBows =  rbind(yearBows,bows)
  # dim(yearBows)
  print(paste0(f_i,' of ',length(bow_fl)))
  
}

}
View(yearBows)
# st_write(yearBows, '/Users/heidi.local/Desktop/FLK_data/ConcaveBufferedBows_AllYears_14Dec2022.shp')
#below was the latest saved file (oct15)
# st_write(yearBows, '/Users/heidihirsh/Desktop/ConvexBufferedBows_AllYears_24Dec2022.shp')
#new save (oct15)
st_write(yearBows, '/Users/heidi.k.hirsh/Desktop/ConvexBufferedBows_AllYears_15oct2023.shp')

#don't do volume intersection since ThomasD already calculated volume. So start with 'ConcaveBows_AllYears.shp' below

#Buffer out and back in (to capture shallow areas)






# ________________________________ADD BENTHIC INDICES_____________________________________


# benthicBows=yearBows

# #read in bowtiessf dataframe 
# # benthicBows = st_read('/Users/heidi.local/Desktop/BowVol_start2Nov2022.shp')
# benthicBows = st_read( '/Users/heidi.local/Desktop/FLK_data/ConcaveBufferedBows_AllYears.shp')
# benthicBows = st_read('/Users/heidi.local/Desktop/FLK_data/ConcaveBufferedBows_AllYears_13Dec2022.shp')
# benthicBows = st_read('/Users/heidi.local/Desktop/FLK_data/ConcaveBufferedBows_AllYears_14Dec2022.shp')


## to run from file
# benthicBows=NULL
# benthicBows = st_read('/Users/heidihirsh/Desktop/ConvexBufferedBows_AllYears_24Dec2022.shp')

dim(benthicBows)
#don't need forward bows: 
benthicBows = benthicBows[which(benthicBows$simu=='backward'),]
dim(benthicBows) 

#limit to only 7day bows
# benthicBows = benthicBows[which(benthicBows$ndays==7),]
# dim(benthicBows) # 1321   15

benthicBows[, 'CALCi.m2'] = NA
benthicBows[, 'ALGi.m2'] = NA
benthicBows[, 'SGi.m2'] = NA
benthicBows[, 'NBi.m2'] = NA
benthicBows[, 'NBi.m2'] = NA
benthicBows[, 'NCo.m2'] = NA
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
  # BowWeight$NBTest = 1-BowWeight$Bio
  # BowWeight$NBTestIndex = BowWeight$a*BowWeight$NBTest
  #NONONO BowWeight$NotBio = 1-BowWeight$Bio
  # I think this calculation would include land categories. Instead, use NotBio from LU
  
  #biomass index for each class
  BowWeight$Ci= BowWeight$a*BowWeight$Coral
  BowWeight$Ai= BowWeight$a*BowWeight$Algae #fix spelling!
  BowWeight$Si= BowWeight$a*BowWeight$Seagrass
  BowWeight$NBi= BowWeight$a*BowWeight$NotBio
  BowWeight$NCo= BowWeight$a*BowWeight$BioNoCoral #should be sum of seagrass and algae
  
  #summarize for each biomass index (all classes contribute to each)
  benthicBows$CALCi.m2[b_i]=sum(BowWeight$Ci,na.rm=T)
  benthicBows$ALGi.m2[b_i]=sum(BowWeight$Ai,na.rm=T)
  benthicBows$SGi.m2[b_i]=sum(BowWeight$Si,na.rm=T)
  benthicBows$NBi.m2[b_i]=sum(BowWeight$NBi,na.rm=T)
  benthicBows$NCo.m2[b_i]=sum(BowWeight$NCo,na.rm=T)
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

# st_write(benthicBows, '/Users/heidi.local/Desktop/Concave_BowBenthic_allRTdays_start13Dec2022.shp')
# st_write(benthicBows, '/Users/heidi.local/Desktop/Concave_BowBenthic_allRTdays_start14Dec2022.shp')
st_write(benthicBows, '/Users/heidihirsh/Desktop/Convex_BowBenthic_allRTdays_start24Dec2022.shp')

dim(benthicBows)

