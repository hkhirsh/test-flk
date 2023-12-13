# Play with CC data from FLK (shared by Ana 6/24/2022)

## 10/9/2023 Running fine. Commented out code to define keysLand and hydroGrid because they took forever and then we never use them again (in this script at least)

## Clear Workspace ----------------------------------------------------------
rm(list=ls())
# runDate='2022Aug11'

# Load Libraries ----------------------------------------------------------
# TOO MANY - WHY???
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
library(streamMetabolizer)
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

register_google(key = "AIzaSyB3mq0nJtcNWvQUNuqH_t-MLxEUmwDDmGk") #new api OCT2023

#___________________________#
# Load Functions ----------------------------------------------------------
se = function(x,na.rm=T){return(sd(x,na.rm=na.rm)/sqrt(length(x)))}

hrs <- function(s) {x <- s * 3600;return(x)}

#___________________________#




# Load filtered carbonate chemistry data from Ana
CCflk <- read.csv('/Users/heidi.k.hirsh/Desktop/FLK_data/FLK_filtered_ve4.csv')
# View(CCflk)

#how many unique days in whole data set: 
length(unique(CCflk$UTCDate))


#add jday: (from Guam code)
CCflk$jday= yday(CCflk$datetime)

# p=ggplot(data=CCflk,aes(x=jday,y=DIC_umol_kg,color=as.factor(Year)))+
#   geom_point()+
#   # facet_grid("Year")+
#   theme_bw()+
#   theme(legend.position="bottom")+
#   scale_x_continuous(breaks = seq(0, 365, by = 10))
# p
# ggsave(file="/Users/heidihirsh/Desktop/FLKjdayYear.png",p,width=12,height=5)

#plot sampling jdays to compare each year

# offset=hrs(1)
# timestep=hrs(3)
# CC$TIMESTAMP_UTC_03h = as_datetime((round((as.numeric(CC$TIMESTAMP_UTC)+offset)/timestep,0)*timestep)-offset,origin='1970-01-01')
# 
# CC$jday = yday(CC$TIMESTAMP_UTC_03h)
# CC$day_frac = hour(hms(format(CC$TIMESTAMP_UTC_03h, format = "%H:%M:%S")))/24
# CC$jday_frac = CC$jday+CC$day_frac

CCflk_simple=NULL
#save simple dataframe for emmanuel and thomas
CCflk_simple = subset(CCflk,select =c(Latitude,Longitude,UTCDate_Time,SiteID,Location))
# CCflk_simple$Latitude = round(CCflk_simple$Latitude, digits=6)
# CCflk_simple$Longitude = round(CCflk_simple$Longitude, digits=6)
# View(CCflk_simple)
CCflk_simple$sampleID = paste(CCflk_simple$SiteID,CCflk_simple$UTCDate_Time)
#round latitude and longitude to 6 decimals.

# test = CCflk_simple$Latitude
# test2=round(test,digits=6)
# test-test2

# write.csv(CCflk_simple, file='/Users/heidihirsh/Desktop/FLKChemistrySamples.csv')

# CCflk19 = CCflk[which(CCflk$Year=='2019'),]
# CCflkSub = CCflk[which(CCflk$UTCDate=='19-05-05'),]

#load hydrodynamics model grid from Emmanuel and Thomas
# hydroGrid = raster::shapefile('/Users/heidihirsh/Desktop/mesh_florida/mesh_florida.shp')

# hydroGrid = st_read('/Users/heidi.k.hirsh/Desktop/FLK_data/mesh_florida/mesh_florida.shp') 
# plot(hydroGrid) #This takes a long time and is not worth plotting
# wait, I never use this again. Why do I load it here?? (commeting out, Oct2023)

#make a spatial points data frame of sample locations and layer on top of levelplot and vectorplot
pts.df <- data.frame(lat = CCflk$dec.lat, lon= CCflk$dec.lon, zone= CCflk$Zone)

#remove any NAs
row.has.na <- apply(pts.df, 1, function(x){any(is.na(x))})
sum(row.has.na) #1
pts.df.filt <- pts.df[!row.has.na,]
summary(pts.df.filt)
dim(pts.df.filt) # 1611    3

coordinates(pts.df.filt) <- ~lon+lat
# raster::crs(pts.df.filt) <- raster::projection(UVfield)
plot(pts.df.filt)


# keysLand = st_read('/Users/heidi.k.hirsh/Desktop/FLK_data/Florida_Shoreline_(1_to_40%2C000_Scale)/Florida_Shoreline_(1_to_40%2C000_Scale).shp')
# plot(keysLand,col='gray')
# keysL = spTransform(keysLand, crs(pts.df.filt))
# crs(keysL)
#*Don't use keysLand again either


#get basic map of flk
myLocation = c(lon = -81.3429, lat = 24.7122)
myLocation
p <- ggmap(get_googlemap(center = myLocation,
                         zoom = 8, scale = 2,
                         maptype ='satellite',
                         color = 'color'))

# pp <- p + geom_jitter(data=CCflk,aes(x = dec.lon, y = dec.lat,  col =as.factor(Year)), size =1,height=.005,width=.005) +
# pp <- p + geom_jitter(data=CCflk,aes(x = dec.lon, y = dec.lat,  col =as.factor(Year)), size =1,height=.015,width=.015) +
# pp <- p + geom_point(data=CCflk,aes(x = dec.lon, y = dec.lat,  col =as.factor(Year)), size =1) +
# pp <- p + geom_point(data=CCflk,aes(x = dec.lon, y = dec.lat,  col =as.factor(Year),size=1/Year)) +
pp <- p + geom_jitter(data=CCflk,aes(x = dec.lon, y = dec.lat,  col =as.factor(Year),,size=1/Year), height=.015,width=.015) +
  
  
  # scale_color_gradient2(midpoint=0, low="red",high='blue')+
  # theme(legend.position="bottom")+
  ylim(24.25,25.75)+
  xlim(-82,-80)
pp


# big=p + geom_jitter(data=CCflk,aes(x = dec.lon, y = dec.lat,  col =as.factor(Year)), size =1,height=.005,width=.005) 
# big
# 
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




