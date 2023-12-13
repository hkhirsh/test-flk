# Good for exploring concave bows (use this for prepping for presentation)
## 10/9/2023 pal call in leaflet not work (undefined?)

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

convex = st_read("/Users/heidi.k.hirsh/Desktop/FLK_data/inter_shp",layer='inter_all_triangles')
concave = st_read("/Users/heidi.k.hirsh/Desktop/FLK_data/inter_shp",layer='inter_triangles_with_particles')

# nc_utm17N <- st_transform(nc_wgs84, 26917)
#transform concave 
concave_utm17 = st_transform(concave, 26917)
# View(concave_utm17)
plot(concave_utm17)


# D=1/111.111*.5 #degrees per km
D=300
concave.bo = st_buffer(concave_utm17, dist=D, nQuadSegs=30)  
concave.bi = st_buffer(concave.bo, dist=-D, nQuadSegs=30)

# D2=300
# concave.bo1 = st_buffer(concave_utm17, dist=D2, nQuadSegs=30)  
# concave.bi1 = st_buffer(concave.bo1, dist=-D2, nQuadSegs=30)

# View(concave.bi)


ploThis = concave.bi
ploThis_t = st_transform(ploThis,crs='+proj=longlat +datum=WGS84')
# concave.bi1t =  st_transform(concave.bi1,crs='+proj=longlat +datum=WGS84')


clrs = rainbow(7, start = 0, end = 0.8)
clrs_rev <- rev(rainbow(7, start = 0, end = 0.8))
d=ploThis_t$ndays
pal = colorNumeric(palette = clrs_rev, domain = min(d):max(d))

nd=6
leaflet(data=ploThis_t) %>% 
  addProviderTiles('Esri.WorldImagery') %>%
  addPolygons(data=subset(ploThis_t,simu=='backward'),fillOpacity=0.1,weight=1,color = ~pal(d)) # %>%
  # addPolygons(data=subset(concave.bi1t,simu=='backward'& ndays==nd),fillOpacity=0,weight=2,color ="yellow")  %>%
  # addPolygons(data=subset(concave,simu=='backward' & ndays==nd),fillOpacity=.05,weight=2,color ="white") 




########
FLK_map=get_map(location=c(-80.99306,25.27732),zoom=8,maptype = "satellite")
# FLK_map=get_map(location=c(mean(st_bbox(convex)[c(1,3)]),mean(st_bbox(convex)[c(2,4)])),zoom=9,maptype = "satellite")
ploThis = concave.bi
# 
# keys = ggmap(FLK_map)+
#   geom_sf(fill="gray",alpha=.25,data=subset(ploThis,simu=='forward' & ndays==7), inherit.aes = FALSE)+
#   geom_sf(fill="tomato4",alpha=.25,data=subset(ploThis,simu=='forward' & ndays==6), inherit.aes = FALSE)+
#   geom_sf(fill="tomato3",alpha=.25,data=subset(ploThis,simu=='forward' & ndays==5), inherit.aes = FALSE)+
#   geom_sf(fill="tomato2",alpha=.25,data=subset(ploThis,simu=='forward' & ndays==4), inherit.aes = FALSE)+
#   geom_sf(fill="tomato1",alpha=.33,data=subset(ploThis,simu=='forward' & ndays==3), inherit.aes = FALSE)+
#   geom_sf(fill="tomato",alpha=.4,data=subset(ploThis,simu=='forward' & ndays==2), inherit.aes = FALSE)+
#   geom_sf(fill="red",alpha=.5,data=subset(ploThis,simu=='forward' & ndays==1), inherit.aes = FALSE)+
#   
#   geom_sf(fill="gray",alpha=.25,data=subset(ploThis,simu=='backward' & ndays==7), inherit.aes = FALSE)+
#   geom_sf(fill="lightskyblue4",alpha=.25,data=subset(ploThis,simu=='backward' & ndays==6), inherit.aes = FALSE)+
#   geom_sf(fill="lightskyblue3",alpha=.25,data=subset(ploThis,simu=='backward' & ndays==5), inherit.aes = FALSE)+
#   geom_sf(fill="lightskyblue2",alpha=.25,data=subset(ploThis,simu=='backward' & ndays==4), inherit.aes = FALSE)+
#   geom_sf(fill="lightskyblue1",alpha=.33,data=subset(ploThis,simu=='backward' & ndays==3), inherit.aes = FALSE)+
#   geom_sf(fill="lightskyblue",alpha=.4,data=subset(ploThis,simu=='backward' & ndays==2), inherit.aes = FALSE)+
#   geom_sf(fill="blue",alpha=.5,data=subset(ploThis,simu=='backward' & ndays==1), inherit.aes = FALSE)+
# 
#   ylab('Latitude')+
#   xlab('Longitude')+
#   theme_bw()
# keys
# # ggsave(file="/Users/heidihirsh/Desktop/concave-keys.png",keys)
# 
# zoom = ggmap(FLK_map)+
#     geom_sf(fill="gray",alpha=.25,data=subset(ploThis,simu=='forward' & ndays==7), inherit.aes = FALSE)+
#     geom_sf(fill="tomato4",alpha=.25,data=subset(ploThis,simu=='forward' & ndays==6), inherit.aes = FALSE)+
#     geom_sf(fill="tomato3",alpha=.25,data=subset(ploThis,simu=='forward' & ndays==5), inherit.aes = FALSE)+
#     geom_sf(fill="tomato2",alpha=.25,data=subset(ploThis,simu=='forward' & ndays==4), inherit.aes = FALSE)+
#     geom_sf(fill="tomato1",alpha=.33,data=subset(ploThis,simu=='forward' & ndays==3), inherit.aes = FALSE)+
#     geom_sf(fill="tomato",alpha=.4,data=subset(ploThis,simu=='forward' & ndays==2), inherit.aes = FALSE)+
#     geom_sf(fill="red",alpha=.5,data=subset(ploThis,simu=='forward' & ndays==1), inherit.aes = FALSE)+
#     
#     geom_sf(fill="gray",alpha=.25,data=subset(ploThis,simu=='backward' & ndays==7), inherit.aes = FALSE)+
#     geom_sf(fill="lightskyblue4",alpha=.25,data=subset(ploThis,simu=='backward' & ndays==6), inherit.aes = FALSE)+
#     geom_sf(fill="lightskyblue3",alpha=.25,data=subset(ploThis,simu=='backward' & ndays==5), inherit.aes = FALSE)+
#     geom_sf(fill="lightskyblue2",alpha=.25,data=subset(ploThis,simu=='backward' & ndays==4), inherit.aes = FALSE)+
#     geom_sf(fill="lightskyblue1",alpha=.33,data=subset(ploThis,simu=='backward' & ndays==3), inherit.aes = FALSE)+
#     geom_sf(fill="lightskyblue",alpha=.4,data=subset(ploThis,simu=='backward' & ndays==2), inherit.aes = FALSE)+
#     geom_sf(fill="blue",alpha=.5,data=subset(ploThis,simu=='backward' & ndays==1), inherit.aes = FALSE)+
#     
#     # geom_point(aes(x=Longitude,y=Latitude),color="yellow",data=ploThis)+
#     ylab('Latitude')+
#     xlab('Longitude')+
#     scale_x_continuous(limits=st_bbox(ploThis)[c(1,3)])+
#     scale_y_continuous(limits=st_bbox(ploThis)[c(2,4)])+
#     theme_bw()
# zoom  
# # ggsave(file="/Users/heidihirsh/Desktop/concave-zoom.jpg",zoom)


# mapview(ploThis)
crs(concave)
crs(ploThis)
# ploThis_t = st_transform(ploThis,crs='+proj=longlat +datum=WGS84')


#try leaflet
clrs = rainbow(7, start = 0, end = 0.8)
clrs_rev <- rev(rainbow(7, start = 0, end = 0.8))
d=ploThis_t$ndays
pal = colorNumeric(palette = clrs_rev, domain = min(d):max(d))

leaflet(data=ploThis) %>% 
  addProviderTiles('Esri.WorldImagery') %>%
  addPolygons(data=subset(ploThis_t,simu=='backward'),fillOpacity=.05,weight=1,color = ~pal(d)) 
  # addPolygons(data=subset(ploThis_t,simu=='backward' & ndays==1),fillOpacity=.01,weight=1) %>%
  # addPolygons(data=subset(ploThis_t,simu=='backward' & ndays==7),color='cyan',fillOpacity=.01,weight=1)  #%>%
  # addCircleMarkers(lng = ~ Longitude,~ Latitude,color='white')



## this leaflet plot doesn't run (10/9/23) 
# leaflet(data = CCflk) %>%
#   addProviderTiles('Esri.WorldImagery') %>%
#   addCircleMarkers(
#     # lng = ~ dec.lon,
#     # ~ dec.lat,
#     lng = ~ Longitude,
#     ~ Latitude,
#     color =  ~ pal(d),
#     radius = 5,
#     fillOpacity = 1,
#     stroke = FALSE,





#benthic habitat
coral_sf =  st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/Final_UnifiedReefMap_Version2.0/FWC_UnifiedFloridaReefMap_v2.0.gdb", layer="UnifiedReefMap")

bowTest=subset(ploThis_t,simu=='backward' & ndays==7)

#Tranform coral_sf to match Bbow CRS:
coral_sf.t = st_transform(coral_sf,st_crs(bowTest))
coral_sf.tv = coral_sf.t[which(st_is_valid(coral_sf.t)),]
over_bow = st_intersection(coral_sf.tv,bowTest)

mapView(over_bow,zcol="ClassLv4")

mapView(list(over_bow,bowTest),col.regions=list(zcol="ClassLv4","blue"),col=list(zcol="ClassLv4","blue"))
# mapview(list(over_bow,bowTest),col=list(zcol="ClassLv4","blue"))


## Try in ggplot