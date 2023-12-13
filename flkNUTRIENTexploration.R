# Play with CC data from FLK (shared by Ana 6/24/2022)
## Clear Workspace ----------------------------------------------------------
rm(list=ls())
runDate='2023oct8'

# Load Libraries ----------------------------------------------------------
library(raster)
library(dplyr)
library(ggmap)
library(plyr)
library(readr)
library(purrr)
library(ggplot2)
library(patchwork)
library(lme4)
library(lmerTest)
library(lubridate)
library(scales)
library(ncdf4)
library(jtools)
library(modeltime)
library(MuMIn)
library(tidyverse)
library(lubridate)
library(cmocean)
library(gridExtra)
library(cowplot)
library(rerddap)
library(openair)
library(janitor)  #use to remove duplicates (get_dupes) from model output
library(leaflet)
library(sf)
library(geosphere) #distance between points
register_google(key = "AIzaSyB3mq0nJtcNWvQUNuqH_t-MLxEUmwDDmGk") #api updated OCT23

#___________________________#
# Load Functions ----------------------------------------------------------
se = function(x,na.rm=T){return(sd(x,na.rm=na.rm)/sqrt(length(x)))}

hrs <- function(s) {x <- s * 3600;return(x)}

#___________________________#

# Load filtered carbonate chemistry data from Ana
# CCflk <- read.csv('/Users/heidi.k.hirsh/Desktop/FLK_data/FLK_filtered_ve4.csv')
CCnuts =  read.csv('/Users/heidi.k.hirsh/Desktop/FLK_data/WSv16_clean.csv')

# CCfull.dist= read.csv('/Users/heidi.k.hirsh/Desktop/CCfull+distance_Oct4.csv')
CCflk = read.csv('/Users/heidi.k.hirsh/Desktop/CCfull+distance_Oct4.csv')
 
#load hydrodynamics model grid
# hydroGrid = st_read('/Users/heidi.k.hirsh/Desktop/FLK_data/mesh_florida/mesh_florida.shp')
# plot(hydroGrid)

#make a spatial points data frame of sample locations and layer on top of levelplot and vectorplot
pts.df <- data.frame(lat = CCflk$dec.lat, lon= CCflk$dec.lon, zone= CCflk$Zone)   #points from CC data frame
Npts.df<- data.frame(lat = CCnuts$Latitude, lon= CCnuts$Longitude, CHLA= CCnuts$Chla) #nutrient data points

#removeNAS
dim(Npts.df)
row.has.na <- apply(Npts.df, 1, function(x){any(is.na(x))}) #find NAs
sum(row.has.na) # 688
Npts.df.filt <- Npts.df[!row.has.na,] #points that are not NA
summary(Npts.df.filt)
dim(Npts.df.filt)     #9692    3

coordinates(Npts.df.filt) <- ~lon+lat
# coordinates(pts.df.filt) <- ~lon+lat
# coordinates(pts.df)<- ~lon+lat
# plot(pts.df)

#plot nutrient points
# raster::crs(pts.df.filt) <- raster::projection(UVfield)
plot(Npts.df.filt)
# plot(pts.df.filt)


## !!fix crs function so this works!

keysLand = st_read('/Users/heidi.k.hirsh/Desktop/FLK_data/Florida_Shoreline_(1_to_40%2C000_Scale)/Florida_Shoreline_(1_to_40%2C000_Scale).shp')
# plot(keysLand,col='gray')
keysL = spTransform(keysLand, st_crs(Npts.df.filt)) #changed to st_crs (this doesn't solve problem)
crs(keysL)
crs(keysLand)


#get basic map of flk
myLocation = c(lon = -81.3429, lat = 24.7122)
p <- ggmap(get_googlemap(center = myLocation,
                         zoom = 8, scale = 2,
                         maptype ='satellite',
                         color = 'color'))
# p

# pp <- p + geom_jitter(data=CCnuts,aes(x = Longitude, y = Latitude,  col =as.factor(Chla),size=1/Year), height=.015,width=.015) #+
# pp
   
pp <- p + geom_point(data=CCnuts,aes(x = Longitude, y = Latitude, col =as.factor(Chla))) +
  # scale_color_gradient2(midpoint=0, low="red",high='blue')+
  theme(legend.position="none")+
  ylim(24.25,25.75)+
  xlim(-82,-80)
pp

#stack CC samples and nutrient sample maps
# big=p + geom_jitter(data=CCflk,aes(x = dec.lon, y = dec.lat,  col =as.factor(Year)), size =1,height=.005,width=.005)
# big
# big/pp


#use the plot where I can zoom and explore.
clrs = rainbow(10, start = 0, end = 0.8)
d=CCflk$DIC_umol_kg
pal = colorNumeric(palette = clrs, domain = min(d,na.rm=T):max(d,na.rm=T))

leaflet(data = CCflk) %>%
  addProviderTiles('Esri.WorldImagery') %>%
  addCircleMarkers(
    # lng = ~ dec.lon,
    # ~ dec.lat,
    lng = ~ Longitude,
    ~ Latitude,
    color =  ~ pal(d),
    radius = 5,
    fillOpacity = 1,
    stroke = FALSE,
    # label = ~ htmlEscape(paste0(round(CC$nTA_delta, digits = 2)))
  ) %>%
  addLegend(
    'bottomright',
    pal = pal,
    values =  ~ d,
    opacity = 1,
    title = "DIC"
  ) 



unique(CCflk$SiteID)




#limit to cruises in the keys
unique(CCnuts$Station)

flk_stations = unique(CCflk$SiteID)
flk_stations
length(flk_stations)
#copy code from concave modeling script

CCnuts_flk = subset(CCnuts, Station %in% flk_stations)
unique(CCnuts_flk$Station)
length(unique(CCnuts_flk$Station)) #one is missing (21LK)
#there's also a weird extra point when I plot chla

#leaflet plot for chla
# clrs = rainbow(10, start = 0, end = 0.8)
d=CCnuts_flk$Chla
# pal = colorNumeric(palette = clrs, domain = min(d,na.rm=T):max(d,na.rm=T))
pal = colorNumeric(palette = "plasma", domain = min(d,na.rm=T):max(d,na.rm=T))


leaflet(data = CCnuts_flk) %>%
  addProviderTiles('Esri.WorldImagery') %>%
  addCircleMarkers(
    lng = ~ Longitude,
    ~ Latitude,
    color =  ~ pal(d),
    radius = 5,
    fillOpacity = 1,
    stroke = FALSE,
  ) %>%
  addLegend(
    'bottomright',
    pal = pal,
    values =  ~ d,
    opacity = 1,
    title = "chlorophyl-a"
  ) 

#where are the super high chla data (everything looks low-backed up by histogram below)

#This is CHEATER CODE to make the below code work for my subset of CCnuts (CCnuts_flk)
CCnuts = CCnuts_flk

#match nutrients to CC dataframe by site name? spatial match is better
#plot "region" (defined in model script) versus chla

#pca plot of nutrient data
nutNames = names(CCnuts[11:19])

#untransformed data
nutPCA = princomp(scale(na.omit(CCnuts[,nutNames])))
plot(nutPCA)
biplot(nutPCA)

#transformed data: 
nutPCA = princomp(scale(na.omit(apply(CCnuts[,nutNames]+10^-6,2,log10))))
plot(nutPCA)
biplot(nutPCA)

#nutrient histograms:
hist(CCnuts$Chla)
hist(CCnuts$Chla,1000)
hist(log(CCnuts$Chla,1000))
hist(log(CCnuts$NO2,1000))
hist(log(CCnuts$NO3,1000))
hist(log(CCnuts$PO4,1000))



#save "water quality" metric to nutrients data frame 



#region code: 
#move this up to define region before limiting the dataframe
#____________________________________
bbLat = 25.1
bbLon = -80.25
# distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine)

CCnuts[, 'distBB.m'] = NA
for(d in 1:length(CCnuts[,1])){CCnuts$distBB.m[d] = distm(c(bbLat,CCnuts$Latitude[d]),c(bbLon,CCnuts$Longitude[d]),distGeo)}

#add a new column for distance (in m and then in km) from bb point
CCnuts$distBB.m
CCnuts$distBB.km = CCnuts$distBB.m/1000
CCnuts$distBB.km #too long!
unique(CCnuts$distBB.m)
CCnuts$invDistBB.m = 1/CCnuts$distBB.m


#unique numeric site IDS
# CCnuts = transform(CCnuts, numID = as.numeric(factor(SiteID))) #no site IDs in nutrient data

# plot(CCfull$numID,CCfull$distBB.m)
hBB =hclust(dist(CCnuts$distBB.m))
# plot(hBB)

CCnuts$distIndex = cutree(hBB,k=7) #other choices: 2, 4, 7, 12

# ggplot(CCfull,aes(x=numID,y=distBB.km,color=as.factor(distIndex)))+
#   geom_point(size=3,pch=1)+
#   theme_bw()

clrs = rainbow(10, start = 0, end = 0.8)
# d=CCnuts$distIndex
d=CCnuts$distBB.km
pal = colorNumeric(palette = clrs, domain = min(d,na.rm=T):max(d,na.rm=T))

leaflet(data = CCnuts) %>%
  addProviderTiles('Esri.WorldImagery') %>%
  addCircleMarkers(
    lng = ~ Longitude,
    ~ Latitude,
    color =  ~ pal(d),
    radius = 5,
    fillOpacity = 1,
    stroke = FALSE,
  ) %>%
  # addCircleMarkers(
  #   lng = ~ bbLon,
  #   ~ bbLat,
  #   color =  'white',
  #   radius = 10,
  #   fillOpacity = 1,
  #   stroke = FALSE,
  # )
  addLegend(
    'bottomright',
    pal = pal,
    values =  ~ d,
    opacity = 1,
    title = "distance index"
  ) 


#plot "region" versus various nutrient parameters
 
plot(CCnuts$distIndex,CCnuts$NO3)
plot(CCnuts$distIndex,CCnuts$NO2)
plot(CCnuts$distIndex,CCnuts$Chla)
plot(CCnuts$distIndex,CCnuts$PO4)
plot(CCnuts$distIndex,CCnuts$DIN)

#try versus "water quality" (Comp1 of pca)?