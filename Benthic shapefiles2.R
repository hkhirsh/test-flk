#explore benthic habitat data and limit it by the spatial bounds of benthic "bow ties" 
#also includes code to make bows and habitat data spatially compatible (obviously)

## 10/6/2023 Code runs and does not depend on rgdal or rgeos

rm(list=ls())
clc()

library(sf)
library(sp)
library(raster)
library(ggplot2)
library(tidyverse)
library(tigris)
library(ggmap)
library(rstudioapi)
register_google(key = "AIzaSyB3mq0nJtcNWvQUNuqH_t-MLxEUmwDDmGk") #new api OCT2023


#sf (read simple features from shapefile)
bow_01sf=st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_7days",layer="bow_ties_1day")
bow_02sf=st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_7days",layer="bow_ties_2days")
bow_03sf=st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_7days",layer="bow_ties_3days")
bow_0sf4=st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_7days",layer="bow_ties_4days")
bow_05sf=st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_7days",layer="bow_ties_5days")
bow_06sf=st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_7days",layer="bow_ties_6days")
bow_07sf=st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_7days",layer="bow_ties_7days")


# ogrListLayers("/Users/heidihirsh/Desktop/Final_UnifiedReefMap_Version2.0/FWC_UnifiedFloridaReefMap_v2.0.gdb")

# coral_ogr =  readOGR(dsn = "/Users/heidihirsh/Desktop/Final_UnifiedReefMap_Version2.0/FWC_UnifiedFloridaReefMap_v2.0.gdb",
#                      layer="UnifiedReefMap")
# coral_noaa = readOGR(dsn = "/Users/heidihirsh/Desktop/Final_UnifiedReefMap_Version2.0/FWC_UnifiedFloridaReefMap_v2.0.gdb",
#                      layer="Keys_NOAA")
# coral_sefl = readOGR(dsn = "/Users/heidihirsh/Desktop/Final_UnifiedReefMap_Version2.0/FWC_UnifiedFloridaReefMap_v2.0.gdb",
#                      layer="SEFL_NCRI")

coral_sf =  st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/Final_UnifiedReefMap_Version2.0/FWC_UnifiedFloridaReefMap_v2.0.gdb",
                     layer="UnifiedReefMap")

test=bow_01sf[20,]

coral_sf.t = st_transform(coral_sf,st_crs(test))
# st_crs(test)
# st_crs(coral_sf)

# test = st_transform(test,st_crs(coral_sf))
coral_sf.tv = coral_sf.t[which(st_is_valid(coral_sf.t)),]
# table(st_is_valid(coral_sf.tv))
over_bow = st_intersection(coral_sf.tv,test)

bowExt = st_bbox(test)
coral_crop = st_crop(coral_sf.tv,bowExt)


FLK_map=get_map(location=c(mean(st_bbox(test)[c(1,3)]),mean(st_bbox(test)[c(2,4)])),zoom=8,maptype = "satellite")

cropTest= ggmap(FLK_map)+
  geom_sf(data=subset(coral_crop,ClassLv1 %in% c( "Individual or Aggregated Patch Reef","Pavement","Reef Rubble")),
          aes(fill=ClassLv1),alpha=.5,inherit.aes = FALSE)+
  geom_sf(data=test,color='white',lwd=2,fill=NA,inherit.aes = FALSE)+
  scale_x_continuous(limits=st_bbox(coral_crop)[c(1,3)])+
  scale_y_continuous(limits=st_bbox(coral_crop)[c(2,4)])+
  theme_bw()
cropTest
# ggsave(file="/Users/heidihirsh/Desktop/cropTest7.png",cropTest)
# ggsave(file="/Users/heidi.k.hirsh/Desktop/FLK Figures/cropTest.png",cropTest)

# corCov = ggplot()+
#   geom_sf(data=coral_crop,aes(fill=PercentCor),inherit.aes = FALSE)+
#   geom_sf(data=test,fill='white',alpha=0.4,inherit.aes = FALSE)
# corCov
# ggsave(file="/Users/heidi.k.hirsh/Desktop/FLK Figures/coral cover bow.png",corCov)


#can I limit to horseshoe? 
