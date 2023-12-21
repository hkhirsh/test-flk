#turn CC points into kml for Google Earth
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
# library(rgdal)
library(raster)
library(rgeos)
library(patchwork)
library(mapview)
library(leaflet)
library(ggmap)
# library(maptools)
# library(rgdal)
register_google(key = "AIzaSyB3mq0nJtcNWvQUNuqH_t-MLxEUmwDDmGk") #new api OCT2023

CC = read.csv('/Users/heidi.k.hirsh/Desktop/FLK_data/CCflk_plusBathy.csv') #this is not latest
head(CC)


CCkml=CC[,c('Latitude','Longitude')]
# CCkml=CC[,c('Latitude','Longitude','Location','Zone','TIMESTAMP_UTC')]

head(CCkml)
# coordinates(CCkml)=c('Longitude','Latitude')
# proj4string(CCkml)<-CRS("+proj=longlat +datum=WGS84")


#data.frame to sf conversion
CCkml <- CCkml %>% 
  st_as_sf(coords=c('Longitude','Latitude'), crs=4326)

#Project it to geograpich coordinate system
CCkml_ll <- st_transform(CCkml, st_crs("+proj=longlat +datum=WGS84"))

class(CCkml_ll)
#Write it as kml file
st_write(CCkml_ll, "CCkml3.kml", driver="KML") 


