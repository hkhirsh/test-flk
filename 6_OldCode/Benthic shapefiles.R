rm(list=ls())

# 10/6/2023 data loads but plot isn't working (didn't try commented out code)
# need to update code to avoid rgdal and rgeos!!

library(sf)
library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(tidyverse)
library(tigris)
library(rgeos)
library(ggmap)
library(rstudioapi)
register_google(key = "AIzaSyB3mq0nJtcNWvQUNuqH_t-MLxEUmwDDmGk") #new api OCT2023

## Read OGR vector maps into spatial objects (by data source name of shapefile)
##!!change read data since it currently depends on rgdal
bow_01=readOGR(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_7days",layer="bow_ties_1day")
bow_02=readOGR(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_7days",layer="bow_ties_2days")
bow_03=readOGR(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_7days",layer="bow_ties_3days")
bow_04=readOGR(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_7days",layer="bow_ties_4days")
bow_05=readOGR(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_7days",layer="bow_ties_5days")
bow_06=readOGR(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_7days",layer="bow_ties_6days")
bow_07=readOGR(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_7days",layer="bow_ties_7days")


keys_ext= extent(bow_01)
FLK_map=get_map(location=c(mean(st_bbox(bow_01)[c(1,3)]),mean(st_bbox(bow_01)[c(2,4)])),zoom=8,maptype = "satellite")

# seagrass_ogr = readOGR(dsn = "/Users/heidihirsh/Desktop/Seagrass_Habitat_in_Florida",layer="Seagrass_Habitat_in_Florida")
# coral_ogr =  readOGR(dsn = "/Users/heidihirsh/Desktop/Coral_and_Hard_Bottom_Habitats_in_Florida",layer="Coral_and_Hard_Bottom_Habitats_in_Florida")
# 
# seagrass_sf = st_read(dsn = "/Users/heidihirsh/Desktop/Seagrass_Habitat_in_Florida",layer="Seagrass_Habitat_in_Florida")
# coral_sf = st_read(dsn = "/Users/heidihirsh/Desktop/Coral_and_Hard_Bottom_Habitats_in_Florida",layer="Coral_and_Hard_Bottom_Habitats_in_Florida")

ogrListLayers("/Users/heidi.k.hirsh/Desktop/FLK_data/Final_UnifiedReefMap_Version2.0/FWC_UnifiedFloridaReefMap_v2.0.gdb")
#read in spatial benthic data from FL unified reef map
coral_ogr =  readOGR(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/Final_UnifiedReefMap_Version2.0/FWC_UnifiedFloridaReefMap_v2.0.gdb",
                     layer="UnifiedReefMap")

coral_noaa = readOGR(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/Final_UnifiedReefMap_Version2.0/FWC_UnifiedFloridaReefMap_v2.0.gdb",
                     layer="Keys_NOAA")

coral_sefl = readOGR(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/Final_UnifiedReefMap_Version2.0/FWC_UnifiedFloridaReefMap_v2.0.gdb",
                     layer="SEFL_NCRI")


# ogrDrivers()$name

head(coral_ogr@data)
colnames(coral_ogr@data)
coral_ogr@proj4string #+proj=longlat +datum=WGS84 +no_defs 
# seagrass_ogr@proj4string #+proj=longlat +datum=WGS84 +no_defs 

# spplot(coral_ogr[,6])


# subset_c = coral_ogr[bow_01,]

# class(coral_ogr)
# crs(coral_ogr) # +proj=longlat +datum=WGS84 +no_defs 
# extent(coral_ogr)
# coral_ogr$data
# coral_ogr$metadata
# summary(coral_ogr)
# plot(coral_ogr)
# glimpse(coral_ogr)

 
unique(coral_sf$DESCRIPT) # "Coral Reef"               "Hardbottom"               "Hardbottom with Seagrass" "Probable Hardbottom"   unique(seagrass_sf$DESCRIPT) # "Continuous Seagrass"             "Patchy (Discontinuous) Seagrass"

# crs = "+proj=longlat +datum=WGS84 +no_defs"
# window <- as(spatstat::as.extent(c(-81.92244 , -80.06368, 24.40989 , 26.14475)), "SpatialPolygons")
# raster::projection(window) <- crs
# coral2 = raster::intersect(coral_ogr, window)
# class(coral2)

# plot(coral_sf,
#      xlim=c(-81.92244 , -80.06368),
#      ylim=c(24.40989 , 26.14475))

# ggplot()+
#   geom_polygon(data=coral_sf, aes(x=long,y=lat,group=group))

# coral_ogrTidy = tidy(coral_ogr)
# ggplot(plot(coral_sp,


ggmap(FLK_map)+
  # geom_sf(data=seagrass_sf, inherit.aes = FALSE)+
  # geom_sf(data=coral_sf, inherit.aes = FALSE)+
  geom_sf(data=coral_ogr, inherit.aes = FALSE)+.   #added oct 6, 2023
  # geom_sf(data=bow_01, inherit.aes = FALSE)+
  # geom_sf(data=coral_sf, aes(fill=factor(DESCRIPT)))+
  # geom_point(aes(x=x_from,y=y_from),color="pink",data=subset(bow_01,simu==direc))+
  scale_x_continuous(limits=st_bbox(bow_01)[c(1,3)])+
  scale_y_continuous(limits=st_bbox(bow_01)[c(2,4)])+
  theme_bw()
 # +
  # theme(legend.position = "right")

