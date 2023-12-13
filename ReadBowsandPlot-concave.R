#
##10/9/2023 
#yearBows loop not running
#MY needs for bowMap plot (not sure where it comes from) - unable to run downstream lines

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
register_google(key = "AIzaSyB3mq0nJtcNWvQUNuqH_t-MLxEUmwDDmGk") #new api OCT2023


year_fl =list.files(path = paste0("/Users/heidi.k.hirsh/Desktop/FLK_data/concave_bowtie_thingies"),full.names=T)   #use file where I removed "12_20170801_072200" (empty shp)
length(year_fl)

yearBows=NULL
# y_i=5
#loop through years up here...
for (y_i in 1:length(year_fl)) {   
  splitYFile=str_split(string=year_fl[y_i],"/")
  splitYFile
  Year = splitYFile[[1]][7]
  Year
  
#read in all bow tie files for the year (identify them)
bow_fl = list.files(path = paste0("/Users/heidihirsh/Desktop/FLK_data/concave_bowtie_thingies/",Year),full.names=T)  
# bow_fl = list.files(path = paste0("/Users/heidihirsh/Desktop/bow_ties_sample_ids_v2/",Year),full.names=T)  
bow_fl
length(bow_fl)


#pull out relevant info (date, #days)
# f_i=14 #something weird at file 13 for 2017
bows=NULL
for (f_i in 1:length(bow_fl)) {   #loop through sampling sites for the given year

  splitBFile=str_split(string=bow_fl[f_i],"/")

  bowGroup = splitBFile[[1]][8]

  layerName = paste0('bowties_inter_mesh_',bowGroup)

  
  #read shapefile (includes forward and backward bows for 1-7 day durations at the sampling site for a given date/time)
  bows = st_read(dsn = bow_fl[f_i],layer=layerName) #this is a DAY of forward and backward bows (not one bow)
  
  #pull visitID, date, time from bowGroup
  splitName=str_split(string=bowGroup,"_")
  
  visitID=bowGroup
  # year=substr(splitName[[1]][4],1,4)  

  bows$year=Year
  bows$visitID=visitID
  # bows$year=year

  bows$duration=bows$ndays
  # bows$name = paste0('bow_',year,month,day,'_',duration,'days') #need to use specific duration (loop through if we want this)
  
  bows$bowID = paste0(bows$visitID,"_",bows$simu,"_",bows$ndays)
  
  yearBows =  rbind(yearBows,bows)
  # dim(yearBows)
  print(paste0(f_i,' of ',length(bow_fl)))

}

}
View(yearBows)
# st_write(yearBows,'/Users/heidihirsh/Desktop/FLK_data/Bows_forPlotting_concave.shp')




#read in CC dataframe with PAR.
yearBows = st_read('/Users/heidi.k.hirsh/Desktop/FLK_data/Bows_forPlotting_concave.shp')
dim(yearBows)
# yearBows = st_read('/Users/heidihirsh/Desktop/FLK_data/Bows_forPlotting_Dec9.shp')
CC = read.csv('/Users/heidi.k.hirsh/Desktop/FLK_data/CCflk_plusBathy.csv')
# View(CC)
dim(CC) # 1611   63

#subset of chemistry samples that go with bow ties:
CCbt = subset(CC,Year %in% c('2012','2013','2014','2015','2016','2017','2018','2019','2020','2021'))

names(CCbt)
CCbt$visitID_ch1 =  paste(CCbt$SiteID,CCbt$UTCDate_Time)
CCbt$visitID_ch2 =    gsub(" ", "_" , CCbt$visitID_ch1, perl=TRUE)
CCbt$visitID =  gsub("[: -]", "" , CCbt$visitID_ch2, perl=TRUE)
#this should now match visitID in yearBows

yearBows$visitID
CCbt$visitID

CCbows=NULL
CCbows = left_join(yearBows, CCbt, by="visitID")
names(CCbows)
# View(CCbows)
dim(CCbows)
# st_write(CCbows,'/Users/heidihirsh/Desktop/FLK_data/concaveBows+CC_dec17.shp')


#limit CCbows to one MY (ex. 2018-)
CCbows =  st_read('/Users/heidi.k.hirsh/Desktop/FLK_data/concaveBows+CC_dec17.shp')

unique(CCbows$year)
length(unique(CCbows$MY))
length(unique(CCbows$ESTDate))

MonthYear = unique(CCbows$MY)
FLK_map=get_map(location=c(-80.99306,25.27732),zoom=8,maptype = "satellite")
# FLK_map=get_map(location=c(mean(st_bbox(ploThis)[c(1,3)]),mean(st_bbox(ploThis)[c(2,4)])),zoom=8,maptype = "satellite")

# b_i =20
#I want to loop through each month_year and plot the back bows for the reef sites and the forward bows for oceanic
for (b_i in 1:length(MonthYear)) {
plot_month= MonthYear[b_i]
# plot_month="2021-02"
ploThis = subset(CCbows, MY == plot_month)
xlim <- c(79.4,82.5)
ylim <- c(24,26)

bowMap= ggmap(FLK_map)+
  
  # coord_sf(xlim = xlim, ylim = ylim)+
  
  geom_sf(fill="gray",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==7), inherit.aes = FALSE)+
  geom_sf(fill="lightskyblue4",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==6), inherit.aes = FALSE)+
  geom_sf(fill="lightskyblue3",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==5), inherit.aes = FALSE)+
  geom_sf(fill="lightskyblue2",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==4), inherit.aes = FALSE)+
  geom_sf(fill="lightskyblue1",alpha=.33,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==3), inherit.aes = FALSE)+
  geom_sf(fill="lightskyblue",alpha=.4,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==2), inherit.aes = FALSE)+
  geom_sf(fill="blue",alpha=.5,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==1), inherit.aes = FALSE)+
  
  # geom_sf(fill="gray",alpha=.25,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Inshore' & duration==7), inherit.aes = FALSE)+
  # geom_sf(fill="tomato4",alpha=.25,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Inshore' & duration==6), inherit.aes = FALSE)+
  # geom_sf(fill="tomato3",alpha=.25,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Inshore' & duration==5), inherit.aes = FALSE)+
  # geom_sf(fill="tomato2",alpha=.25,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Inshore' & duration==4), inherit.aes = FALSE)+
  # geom_sf(fill="tomato1",alpha=.33,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Inshore' & duration==3), inherit.aes = FALSE)+
  # geom_sf(fill="tomato",alpha=.4,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Inshore' & duration==2), inherit.aes = FALSE)+
  # geom_sf(fill="red",alpha=.5,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Inshore' & duration==1), inherit.aes = FALSE)+
  # 
  geom_point(aes(x=Longitude,y=Latitude),color="yellow",data=subset(ploThis,Zone=='Inshore'))+
  geom_point(aes(x=Longitude,y=Latitude),color="green",data=subset(ploThis,Zone=='Mid channel'))+
  geom_point(aes(x=Longitude,y=Latitude),color="cyan",data=subset(ploThis,Zone=='Offshore'))+
  geom_point(aes(x=Longitude,y=Latitude),color="magenta",data=subset(ploThis,Zone=='Oceanic'))+
  # geom_point(aes(x=Longitude,y=Latitude),color="yellow",data=ploThis)+
  
  ggtitle(plot_month)+
  ylab('Latitude')+
  xlab('Longitude')+
  theme_bw()

bowMap
# ggsave(file=paste0("/Users/heidihirsh/Desktop/concaveMaps/",plot_month,"_concave_bowties_inshoreBACK.png"),bowMap)
}

# mapview(ploThis)

ploThis_t = st_transform(ploThis,crs='+proj=longlat +datum=WGS84')

#try leaflet
leaflet(data=ploThis_t) %>% 
  addProviderTiles('Esri.WorldImagery') %>%
  addPolygons(data=subset(ploThis_t,simu=='backward' & Zone=='Inshore' & duration==1),fillOpacity=.01) %>%
  addPolygons(data=subset(ploThis_t,simu=='backward' & Zone=='Inshore' & duration==7),color='cyan',fillOpacity=.01)  %>%
  addCircleMarkers(lng = ~ Longitude,~ Latitude,color='white')


# addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
#             opacity = 1.0, fillOpacity = 0.5,
#             fillColor = ~colorQuantile("YlOrRd", ALAND)(ALAND),
#             highlightOptions = highlightOptions(color = "white", weight = 2,
#                                                 bringToFront = TRUE))


       


#benthic habitat
coral_sf =  st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/Final_UnifiedReefMap_Version2.0/FWC_UnifiedFloridaReefMap_v2.0.gdb", layer="UnifiedReefMap")

bowTest=subset(ploThis_t,simu=='backward' & Zone=='Inshore' & duration==1)

#Tranform coral_sf to match Bbow CRS:
coral_sf.t = st_transform(coral_sf,st_crs(bowTest))
coral_sf.tv = coral_sf.t[which(st_is_valid(coral_sf.t)),]
over_bow = st_intersection(coral_sf.tv,bowTest)

mapview(over_bow,zcol="ClassLv4")

mapview(list(over_bow,bowTest),col.regions=list(zcol="ClassLv4","blue"),col=list(zcol="ClassLv4","blue"))
