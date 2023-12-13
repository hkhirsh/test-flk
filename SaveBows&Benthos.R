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
# library(tigris)
library(rgeos)
library(patchwork)
library(mapview)
register_google(key = "AIzaSyDHvaszi8kLu6t2pR1U_oz1MocuYMIeog4")




#read in FL unified reef map habitat data: 
coral_sf =  st_read(dsn = "/Users/heidihirsh/Desktop/Final_UnifiedReefMap_Version2.0/FWC_UnifiedFloridaReefMap_v2.0.gdb",
                    layer="UnifiedReefMap")

# #circle sectors
# circles= st_read(dsn = "/Users/heidihirsh/Desktop/FLKhabitat",layer="pts_buf_3km")

#load benthic composition lookup table: 
benthicLU = read.csv("/Users/heidihirsh/Desktop/FLKhabitat/LookUp/ClassLv4_BenthicLookup_V1_cleaner.csv")

#read in all files (identify them)
bow_fl = list.files(path = paste0("/Users/heidihirsh/Desktop/bow_ties_2016"),pattern='.shp',full.names=T)  
# bow_fl
ndates=  length(bow_fl)/7
# ndates

#pull out relevant info (date, #days)
# f_i=1
yearBows = NULL
for (f_i in 1:length(bow_fl)) {
  
  splitName=str_split(string=bow_fl[f_i],c("\\W"))
  # splitName
  bowGroup = splitName[[1]][6] 
  # bowGroup
  
  #read shapefile (save for each unique date and duration)
  bows = st_read(dsn = "/Users/heidihirsh/Desktop/bow_ties_2016",layer=bowGroup) #this is a DAY of forward and backward bows (not one bow)
  
  splitFile=str_split(string=bow_fl[f_i],"_")
  # splitFile
  year=substr(splitFile[[1]][5],1,4)  
  month=substr(splitFile[[1]][5],5,6)  
  day=substr(splitFile[[1]][5],7,8)  
  duration= substr(splitFile[[1]][6],1,1)  
  
  bows$date= paste0(year,month,day)
  bows$year=year
  bows$month=month
  bows$day=day
  bows$duration= substr(splitFile[[1]][6],1,1)  
  bows$name = paste0('bow_',year,month,day,'_',duration,'days')
  
  yearBows =  rbind(yearBows,bows)
  dim(yearBows)
  
}
View(yearBows)




yearBows[, 'CALCi.m2'] = NA
yearBows[, 'ALGi.m2'] = NA
yearBows[, 'SGi.m2'] = NA
yearBows[, 'NBi.m2'] = NA
yearBows[, 'PercentCheck'] = NA


# b_i=1
for (b_i in 1:nrow(yearBows)) {
# for (b_i in 1:7) {
  Bbow=yearBows[b_i,]
  print(paste0('starting bow ',b_i,' of ',nrow(yearBows)))
  tick = Sys.time()
        # st_crs(Bbow) #D_Unknown_based_on_WGS84_ellipsoid
        # st_crs(coral_sf) #NAD83
        
        # #Tranform Bbow to match coral_sf CRS:
        # Bbow.t=st_transform(Bbow,st_crs(coral_sf))
        # st_crs(Bbow.t) #NAD83
        # coral_sf.v = coral_sf[which(st_is_valid(coral_sf)),]
        # over_bow = st_intersection(coral_sf.v,Bbow.t)
        # st_crs(over_bow) #NAD83
        
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
        BowWeight$Ai= BowWeight$a*BowWeight$Alage #fix spelling!
        BowWeight$Si= BowWeight$a*BowWeight$Seagrass
        BowWeight$NBi= BowWeight$a*BowWeight$NotBio
        
        #summarize for each biomass index (all classes contribute to each)
        yearBows$CALCi.m2[b_i]=sum(BowWeight$Ci,na.rm=T)
        yearBows$ALGi.m2[b_i]=sum(BowWeight$Ai,na.rm=T)
        yearBows$SGi.m2[b_i]=sum(BowWeight$Si,na.rm=T)
        yearBows$NBi.m2[b_i]=sum(BowWeight$NBi,na.rm=T)
        yearBows$PercentCheck[b_i]=sum(bowComp$p)
  
        # BowWeight$tIndex = BowWeight$Ci + BowWeight$Ai + BowWeight$Si + BowWeight$NBi  #should this end up being the total area?
        # BowWeight$tIndex - BowWeight$a #why isn't this always 0?
        
        # bow_area = st_area(test) 
        # sum(BowWeight$a)/bow_area #(it should be less than 1)
        # sum(BowWeight$p) 
        
        tock= Sys.time()
        elmin=round(difftime(tock,tick,units='mins'),1)
        
        print(paste0("Completed ",b_i," of ",nrow(yearBows)," in ",elmin," min. Estimated time remaining: ",(nrow(yearBows)-b_i)*elmin,' min. Current time: ',Sys.time()))
        
        
        
} #end loop to pull benthic info


write.csv(yearBows, file='/Users/heidihirsh/Desktop/BowBenthos_7OCT2022.csv')    


















# #delete the code below once I figure out how to incorporate in the loop above.
# 
# ##______________________________________##
# #pair Bbow with benthic info (this will need to be moved inside the loop above)
# dim(bows)
# Bbow=bows[1,]
# class(Bbow)
# 
# st_crs(Bbow) #D_Unknown_based_on_WGS84_ellipsoid
# st_crs(coral_sf) #NAD83
# 
# # #Tranform Bbow to match coral_sf CRS:
# # Bbow.t=st_transform(Bbow,st_crs(coral_sf))
# # st_crs(Bbow.t) #NAD83
# # coral_sf.v = coral_sf[which(st_is_valid(coral_sf)),]
# # over_bow = st_intersection(coral_sf.v,Bbow.t)
# # st_crs(over_bow) #NAD83
# 
# #Tranform coral_sf to match Bbow CRS:
# coral_sf.t = st_transform(coral_sf,st_crs(Bbow))
# st_crs(coral_sf.t) #"D_Unknown_based_on_WGS84_ellipsoid"
# coral_sf.tv = coral_sf.t[which(st_is_valid(coral_sf.t)),]
# over_bow = st_intersection(coral_sf.tv,Bbow)
# st_crs(over_bow)
# 
# #Get area of each benthic class
# over_bow$class_area = as.numeric(st_area(over_bow)) #m2
# # over_bow$Shape_Area-over_bow$class_area
# 
# bowComp= over_bow %>% st_drop_geometry() %>% dplyr::select(ClassLv4,Shape_Area,class_area) %>% group_by(ClassLv4) %>% summarize(a=sum(class_area,na.rm=TRUE))
# bowComp$p = bowComp$a/st_area(Bbow)
# # bowComp$p = bowComp$a/st_area(Bbow.t)
# bowComp %>% arrange(desc(p))
# # write.csv(bowComp,file=paste0("/Users/heidihirsh/Desktop/FLKhabitat/Benthic_dfs/1day_BackBow_",bi,"_BowCompositionByArea_ClassLv4.csv"), row.names = FALSE)
# # View(bowComp)
# sum(bowComp$p)   #(for f_i=1) this is 2.996787 [1/m^2] when I match CRS to Bbow BUT it is 2.991217 [1/m^2] when I match the other direction
# #0.5364493 [1/m^2]
# #0.5363986 [1/m^2]
# 
# # mapview(over_bow,zcol="ClassLv4")
# # plot(Bbow)
# 
# BowWeight = left_join(bowComp,benthicLU,by='ClassLv4')
# # View(BowWeight)
# # sum(BowWeight$p)
# 
# BowWeight$NotBio = 1-BowWeight$Bio
# 
# isit1 = BowWeight$NotBio + BowWeight$coral + BowWeight$BioNoCoral
# isit1 #something funky about discontinuous seagrass?
# 
# # should these be weight * a or * p?
# BowWeight$Ci= BowWeight$a*BowWeight$coral
# BowWeight$Ai= BowWeight$a*BowWeight$Alage #fix spelling!
# BowWeight$Si= BowWeight$a*BowWeight$Seagrass
# BowWeight$NBi= BowWeight$a*BowWeight$NotBio
# 
# BowWeight$tIndex = BowWeight$Ci + BowWeight$Ai + BowWeight$Si + BowWeight$NBi  #should this end up being the total area?
# BowWeight$tIndex - BowWeight$a #why isn't this always 0?
# bow_area = st_area(test) 
# 
# # sum(BowWeight$a)/bow_area #(it should be less than 1)
# # sum(BowWeight$p) 


