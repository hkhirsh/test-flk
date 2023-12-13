#
## 10/9/2023 potential problems with ggmap - jk just slow
#mapview also has some issues with coloring by class I think


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

# convex = st_read("/Users/heidihirsh/Desktop/FLK_data/inter_shp",layer='inter_all_triangles')
# concave = st_read("/Users/heidihirsh/Desktop/FLK_data/inter_shp",layer='inter_triangles_with_particles')

# CCbows =  st_read('/Users/heidihirsh/Desktop/FLK_data/concaveBows+CC_dec17.shp')


yearBows = st_read('/Users/heidi.k.hirsh/Desktop/FLK_data/Bows_forPlotting_concave.shp')
dim(yearBows)
CC = read.csv('/Users/heidi.k.hirsh/Desktop/FLK_data/CCflk_plusBathy.csv')
dim(CC) # 1611   63
CCbt = subset(CC,Year %in% c('2012','2013','2014','2015','2016','2017','2018','2019','2020','2021')) #subset of chemistry samples that go with bow ties:

CCbt$visitID_ch1 =  paste(CCbt$SiteID,CCbt$UTCDate_Time)
CCbt$visitID_ch2 =    gsub(" ", "_" , CCbt$visitID_ch1, perl=TRUE)
CCbt$visitID =  gsub("[: -]", "" , CCbt$visitID_ch2, perl=TRUE)
# yearBows$visitID
# CCbt$visitID
CCbows=NULL
CCbows = left_join(yearBows, CCbt, by="visitID")
dim(CCbows)

#read in the same convex bow...
VexBows = st_read('/Users/heidi.k.hirsh/Desktop/FLK_data/Bows_forPlotting_Dec9.shp')
CCvex = left_join(VexBows, CCbt, by="visitID")
dim(CCvex)
vex = subset(CCvex,Zone=="Inshore" & MY=="2021-05" & Location=="Middle_Keys" & SiteID=='7')
dim(vex)

concave = subset(CCbows,Zone=="Inshore" & MY=="2021-05" & Location=="Middle_Keys" & SiteID=='7')
dim(concave)
concave$SiteID
# plot(concave)
mapview(concave)

#transform concave 
concave_utm17 = st_transform(concave, 26917)


# D=1/111.111*.5 #degrees per km
D=300
concave.bo = st_buffer(concave_utm17, dist=D, nQuadSegs=30)  
concave.bi = st_buffer(concave.bo, dist=-D, nQuadSegs=30)

# D2=300
# concave.bo1 = st_buffer(concave_utm17, dist=D2, nQuadSegs=30)  
# concave.bi1 = st_buffer(concave.bo1, dist=-D2, nQuadSegs=30)

# View(concave.bi)


# ploThis = concave
# ploThis = concave.bo
ploThis = concave.bi
ploThis_t = st_transform(ploThis,crs='+proj=longlat +datum=WGS84')
plotOrig =st_transform(concave,crs='+proj=longlat +datum=WGS84')
vex_t =st_transform(vex,crs='+proj=longlat +datum=WGS84')

clrs = rainbow(7, start = 0, end = 0.8)
clrs_rev <- rev(rainbow(7, start = 0, end = 0.8))
d=ploThis_t$ndays
pal = colorNumeric(palette = clrs_rev, domain = min(d):max(d))





# nd=6
leaflet(data=ploThis_t) %>% 
  addProviderTiles('Esri.WorldImagery') %>%
  # addPolygons(data=subset(plotOrig,simu=='backward'),fillOpacity=.1,weight=1,color = ~pal(d))  %>%
  addPolygons(data=subset(ploThis_t,simu=='backward'),fillOpacity=0.1,weight=1,color = ~pal(d))  %>%
  addPolygons(data=subset(vex_t,simu=='backward'),fillOpacity=0.01,weight=2,color = ~pal(d))  %>%
  
  addCircleMarkers(lng = ~ dec.lon,~ dec.lat,radius=1,color='white')


  
#plot benthic environment
coral_sf =  st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/Final_UnifiedReefMap_Version2.0/FWC_UnifiedFloridaReefMap_v2.0.gdb",
                    layer="UnifiedReefMap")


#intersect with our example "bows"
test=subset(concave.bi,simu=="backward")
# concave = subset(CCbows,Zone=="Inshore" & MY=="2021-05" & Location=="Middle_Keys" & SiteID=='7')
test = subset(CCvex,Zone=="Inshore" & MY=="2021-05" & ndays=='1' & simu=="backward") #& Location=="Middle_Keys" & SiteID=='7')


plot(test)
coral_sf.t = st_transform(coral_sf,st_crs(test))
coral_sf.tv = coral_sf.t[which(st_is_valid(coral_sf.t)),]

over_bow = st_intersection(coral_sf.tv,test) #this takes a long time
bowExt = st_bbox(test)
coral_crop = st_crop(coral_sf.tv,bowExt) #this takes some time too


FLK_map=get_map(location=c(-80.99306,25.27732),zoom=9,maptype = "satellite") 

# FLK_map=get_map(location=c(mean(st_bbox(test)[c(1,3)]),mean(st_bbox(test)[c(2,4)])),zoom=8,maptype = "satellite")
# ggmap(FLK_map)
# a= ggmap(FLK_map)+
#   geom_sf(data=coral_crop,aes(fill=ClassLv4),alpha=.5,inherit.aes = FALSE)+
#   # geom_sf(data=test,color="white",lwd=1,fill=NA,inherit.aes = FALSE)+
#   geom_sf(data=subset(test,ndays=='7'),color="#FF0000",lwd=1,fill=NA,inherit.aes = FALSE)+
#   geom_sf(data=subset(test,ndays=='6'),color="#FFCC00",lwd=1,fill=NA,inherit.aes = FALSE)+
#   geom_sf(data=subset(test,ndays=='5'),color="#66FF00",lwd=1,fill=NA,inherit.aes = FALSE)+
#   geom_sf(data=subset(test,ndays=='4'),color="#00FF66",lwd=1,fill=NA,inherit.aes = FALSE)+
#   geom_sf(data=subset(test,ndays=='3'),color="#00CCFF",lwd=1,fill=NA,inherit.aes = FALSE)+
#   geom_sf(data=subset(test,ndays=='2'),color="#0000FF",lwd=1,fill=NA,inherit.aes = FALSE)+
#   geom_sf(data=subset(test,ndays=='1'),color="#CC00FF",lwd=1,fill=NA,inherit.aes = FALSE)+
#   scale_x_continuous(limits=st_bbox(coral_crop)[c(1,3)])+
#   scale_y_continuous(limits=st_bbox(coral_crop)[c(2,4)])+
#   theme(axis.text.x =element_blank(), axis.text.y =element_blank(),
#         axis.ticks.x =element_blank(), axis.ticks.y =element_blank(),
#         axis.title.x=element_blank(), axis.title.y=element_blank())+
#   theme(legend.position="bottom")
# a


b= ggmap(FLK_map)+
  # geom_sf(data=coral_crop,aes(fill=ClassLv4),alpha=.5,inherit.aes = FALSE)+
  geom_sf(data=subset(over_bow),aes(fill=ClassLv4),alpha=.5,inherit.aes = FALSE)+
  # geom_sf(data=subset(test,ndays=='7'),color="#FF0000",lwd=1,fill=NA,inherit.aes = FALSE)+
  # geom_sf(data=subset(test,ndays=='6'),color="#FFCC00",lwd=1,fill=NA,inherit.aes = FALSE)+
  # geom_sf(data=subset(test,ndays=='5'),color="#66FF00",lwd=1,fill=NA,inherit.aes = FALSE)+
  # geom_sf(data=subset(test,ndays=='4'),color="#00FF66",lwd=1,fill=NA,inherit.aes = FALSE)+
  # geom_sf(data=subset(test,ndays=='3'),color="#00CCFF",lwd=1,fill=NA,inherit.aes = FALSE)+
  # geom_sf(data=subset(test,ndays=='2'),color="#0000FF",lwd=1,fill=NA,inherit.aes = FALSE)+
  # geom_sf(data=subset(test,ndays=='1'),color="#CC00FF",lwd=1,fill=NA,inherit.aes = FALSE)+
  # scale_x_continuous(limits=st_bbox(coral_crop)[c(1,3)])+
  # scale_y_continuous(limits=st_bbox(coral_crop)[c(2,4)])+
  theme(axis.text.x =element_blank(), axis.text.y =element_blank(),
        axis.ticks.x =element_blank(), axis.ticks.y =element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())+
  theme(legend.position="bottom")
b


View(test)



#"#FF0000" "#FFCC00" "#66FF00" "#00FF66" "#00CCFF" "#0000FF" "#CC00FF"

mapview(over_bow)
mapview(over_bow,zcol="ClassLv4")

##load final dataframe to pull benthic indices for example "bows"
CCfull = read.csv('/Users/heidi.k.hirsh/Desktop/CCmodel_allYears_dec16.csv')
CCfull_ex = subset(CCfull,Zone=="Inshore" & MY=="2021-05" & Location=="Middle_Keys" & SiteID=='7')
dim(CCfull_ex)
# CALC_m2+
#   ALGi_m2+
#   SGi_m2, 
ex_ind = subset(CCfull_ex,select =c(ndays,volume,CALC_m2,ALGi_m2,SGi_m2))
View(ex_ind)

habitat = read.csv('/Users/heidi.k.hirsh/Desktop/FLK_data/exBio.csv')

library('ggplot2')
#bar plot with groups
ggplot(habitat,aes(x=ndays,y=bio,fill=type))+geom_bar(stat = "identity",  position = "dodge")+theme_bw()+
  xlab("number of days")+
  ylab("benthic index (km^2)")







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


# again, this is missing something??
# 
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

mapview(over_bow,zcol="ClassLv4")

mapview(list(over_bow,bowTest),col.regions=list(zcol="ClassLv4","blue"),col=list(zcol="ClassLv4","blue"))


mapview(over_bow,col.regions="ClassLv4")



+
  mapview(bowTest, col='blue')
  
