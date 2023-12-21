#check to make sure sample points are inside polygons

rm(list=ls())
library(magick)
library(sf)
library(ggplot2)
library(tidyverse)
library(ggmap)
library(ggrepel)
library(rstudioapi)
library(stringr)
library(sp)
library(raster)
library(rgeos)
library(patchwork)
library(mapview)
library(leaflet)
register_google(key = "AIzaSyB3mq0nJtcNWvQUNuqH_t-MLxEUmwDDmGk") #new api OCT2023

FLKline <- st_read('/Users/heidi.k.hirsh/Desktop/KeysLine/Keys_LineV2.shp')
plot(FLKline)
# FLKlineSF=st_as_sf(FLKline, crs = st_crs(4326))
# mapview(FLKlineSF)

yearBows = st_read('/Users/heidi.k.hirsh/Desktop/FLK_data/Bows_forPlotting_14days_tri.shp')
dim(yearBows)
CC = read.csv('/Users/heidi.k.hirsh/Desktop/FLK_data/CCflk_plusBathy.csv') #this is not latest
dim(CC) # 1611   63

#subset of chemistry samples that go with bow ties:
CCbt = subset(CC,Year %in% c('2012','2013','2014','2015','2016','2017','2018','2019','2020','2021'))

names(CCbt)
CCbt$visitID_ch1 =  paste(CCbt$SiteID,CCbt$UTCDate_Time)
CCbt$visitID_ch2 =    gsub(" ", "_" , CCbt$visitID_ch1, perl=TRUE)
CCbt$visitID =  gsub("[: -]", "" , CCbt$visitID_ch2, perl=TRUE)
#this should now match visitID in yearBows

unique(yearBows$visitID)
unique(CCbt$visitID)

CCbows=NULL
CCbows = left_join(yearBows, CCbt, by="visitID")
names(CCbows)
dim(CCbows) #34188    73

MonthYear = unique(CCbows$MY)
CCbows_sf = st_as_sf(CCbows, coords = c("Longitude","Latitude"), crs = st_crs(4326))


FLK_map=get_map(location=c(-81.3,25),zoom=8,maptype = "satellite",scale="auto")
ggmap(FLK_map)+ scale_y_continuous(limits = c(24,26), expand = c(0, 0))

b_i=12
#I want to loop through each month_year and plot the back bows for the reef sites and the forward bows for oceanic
# for (b_i in 1:length(MonthYear)) {

plot_month= MonthYear[b_i]
ploThis = subset(CCbows_sf, MY == plot_month)
ploThis <- st_transform(ploThis, crs = 4326)
class(ploThis)

bowMap= ggmap(FLK_map)+
  
  geom_sf(fill="pink",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==14), inherit.aes = FALSE)+
  geom_sf(fill="tomato4",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==13), inherit.aes = FALSE)+
  geom_sf(fill="tomato3",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==12), inherit.aes = FALSE)+
  geom_sf(fill="tomato2",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==11), inherit.aes = FALSE)+
  geom_sf(fill="tomato1",alpha=.33,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==10), inherit.aes = FALSE)+
  geom_sf(fill="tomato",alpha=.4,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==9), inherit.aes = FALSE)+
  geom_sf(fill="red",alpha=.5,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==8), inherit.aes = FALSE)+

  geom_sf(fill="gray",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==7), inherit.aes = FALSE)+
  geom_sf(fill="lightskyblue4",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==6), inherit.aes = FALSE)+
  geom_sf(fill="lightskyblue3",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==5), inherit.aes = FALSE)+
  geom_sf(fill="lightskyblue2",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==4), inherit.aes = FALSE)+
  geom_sf(fill="lightskyblue1",alpha=.33,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==3), inherit.aes = FALSE)+
  geom_sf(fill="lightskyblue",alpha=.4,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==2), inherit.aes = FALSE)+
  geom_sf(fill="blue",alpha=.5,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==1), inherit.aes = FALSE)+

  geom_point(size=1,aes(x=Longitude,y=Latitude),color="yellow",data=subset(ploThis,Zone=='Inshore'))+
  geom_point(size=1,aes(x=Longitude,y=Latitude),color="green",data=subset(ploThis,Zone=='Mid channel'))+
  geom_point(size=1,aes(x=Longitude,y=Latitude),color="cyan",data=subset(ploThis,Zone=='Offshore'))+
  geom_point(size=1,aes(x=Longitude,y=Latitude),color="magenta",data=subset(ploThis,Zone=='Oceanic'))+
  
  geom_sf(colour='white',lwd=1,data=FLKline,inherit.aes = FALSE)+

  scale_y_continuous(limits = c(24.15,26.15), expand = c(0, 0))+
  ggtitle(plot_month)+
  ylab('Latitude')+
  xlab('Longitude')+
  theme_bw(base_size = 20)

bowMap

# name=paste0("/Users/heidi.k.hirsh/Desktop/14days_cropped/inshore/original/",plot_month,"_concave_bowties_inshoreBACK_tri_dec16T.png")
# crop=paste0("/Users/heidi.k.hirsh/Desktop/14days_cropped/inshore/cropped/",plot_month,"_concave_bowties_inshoreBACK_triCrop_dec16T.png")
# 
# ggsave(file=name,bowMap, width = 10, height = 8, dpi = 300, units = "in")
# # new = image_border(image_trim(image_read(name)),'white',"30x30")
# # image_write(new,crop)



leaflet(FLKline)


#try leaflet plot with 14 days. 
b_i = 34

plot_month= MonthYear[b_i]
ploThis = subset(CCbows_sf, MY == plot_month)
ploThis <- st_transform(ploThis, crs = 4326)

mapview(ploThis)


clrs = rainbow(14, start = 0, end = 0.8)
clrs_rev <- rev(rainbow(7, start = 0, end = 0.8))
d=ploThis$n_days
pal = colorNumeric(palette = clrs_rev, domain = min(d):max(d))

# FLKlineT <- spTransform(FLKline, CRS("+proj=longlat +datum=WGS84 +no_defs"))
FLKlineT <- st_transform(FLKline, crs = 4326)
goodLine <- st_zm(FLKline, drop = T, what = "ZM")

leaflet(data=ploThis) %>% 
  addProviderTiles('Esri.WorldImagery') %>%
  addPolygons(data=subset(ploThis,simu=='backward' & Zone=='Inshore' & n_days==2),fillOpacity=.2,weight=1,color = ~pal(d))  %>%
  addCircleMarkers(lng = ~ Longitude,~ Latitude,radius=1,color='white') %>%
  addPolylines(data=goodLine,color='white',weight=2,opacity=1)


leaflet(data=FLKlineT) %>% 
  addProviderTiles('Esri.WorldImagery') %>%
  addPolygons(data=FLKlineT)


leaflet(data=ploThis) %>% 
  addProviderTiles('Esri.WorldImagery') %>%
  addPolygons(data=subset(ploThis,simu=='backward' & Zone=='Inshore' & Sub_region=='MK'),fillOpacity=.02,weight=1,color = ~pal(d))  %>%
  # addCircleMarkers(lng = ~ dec.lon,~ dec.lat,radius=1,color='white')
  addCircleMarkers(lng = ~ Longitude,~ Latitude,radius=2,color='white') %>%
  addPolylines(data=goodLine,color='white',weight=4,opacity=1)





plot_month= MonthYear[i]
plot_month
ploThis = subset(CCbows_sf, MY == plot_month)
ploThis <- st_transform(ploThis, crs = 4326)

# CCpts=subset(CC, MY == plot_month)
# CCpts=CCpts %>% st_as_sf(coords=c('Longitude','Latitude'), crs=4326)
mapMe= data=subset(ploThis,simu=='backward' & Zone=='Inshore'& n_days==1)

# mapPts=ploThis[,c('Latitude','Longitude')] #this keeps geometry

install.packages('magrittr')
library(magrittr)

# mapPts=c(ploThis$Latitude,ploThis$Longitude)
mapPts = ploThis %>% extract2('Latitude')
mapPts 



#can I plot points directly from ploThis

mapview(mapMe)+mapview(mapPts)

        
class(ploThis)
head(ploThis)
View(ploThis)
length(unique(ploThis$visitID))
