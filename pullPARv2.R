## I think this was an attempt to pull PAR from Viirs 4k data
# This is not currently where PAR is coming from in the model
# Why do we only load data from 2016? 

# 10/5/2023 Code that is not commented out currently runs. 

library(rerddap)
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
# library(tigris)
library(rgeos)
library(patchwork)
library(mapview)
library(lubridate)
# library(plotKML)
library(stars) #st_rasterize


#load bow ties dataframe to get bounds
BBB = st_read('/Users/heidi.k.hirsh/Desktop/FLK_Data/BowBenthosVol_13OCT2022.shp') #this has benthic indices AND volume

#load CC dataframe to pull appropriate dates
# CC = read.csv('/Users/heidi.k.hirsh/Desktop/FLK_Data/CCmodel_17OCT2022off.csv')
# head(CC)
# unique(CC$year) #only2016? 



CCfull.all = read.csv('/Users/heidi.k.hirsh/Desktop/CCmodel_allYears+Nuts_13nov2023.csv') #new file with nutrients added
CCfull.all = CCfull.all[which(CCfull.all$ndays ==1),]


#limit to one set of bows (or else we will do them all too many times)
length(unique(CCfull.all$Date)) #95
length(unique(CCfull.all$datetime)) #1371. #pull 30 all data within 30 days prior this datetime
length(CCfull.all$UTCDate_Time) #9639
length(unique(CCfull.all$UTCDate_Time)) #1371


## Define maximum area where we should look (area that includes all backwards, 7-day bows)
#use this for now, eventually I might want the 14 day polygons
Bounds = BBB %>% filter(duratin==7 & simu=="backward") %>%  st_bbox() 
# Bounds
# 360-84.09811  275.9019
# 360-80.05108  279.9489


# Pull satellite VIIRS PAR data (2012-present) over full bounds--------------------------------------------------
# https://coastwatch.pfeg.noaa.gov/erddap/griddap/index.html?page=1&itemsPerPage=1000
# erdVH2018par1day #not sure this actually exists
# erdVH2018par8day
# erdVH2018parmday

#define boundary for pulling data
#use same bounds for each: (Bounds)
# xmin      ymin      xmax      ymax 
# -84.09811  23.94544 -80.05108  25.73530 
latMin = Bounds[2] #ymin
latMax = Bounds[4] #ymax
lonMin = Bounds[1] #xmin
lonMax = Bounds[3] #xmax

#define time vector to pull (window extending 30 days back from sampling datetime)
#loop through all samples

class(CCfull.all$UTCDate_Time)
CCfull.all$UTCDate_Time =as.POSIXct(CCfull.all$UTCDate_Time,'%Y-%m-%d %H:%M:%S', tz='UTC')

# t_i=294
#should go 1371 loops (stopped at 271)
for(t_i in 1:length(CCfull.all$UTCDate_Time)) {
  
  print(paste0('starting PAR ',t_i,' of ',length(CCfull.all$UTCDate_Time)))
  tick = Sys.time()
  
  lastT = CCfull.all$UTCDate_Time[t_i]
  timeLim_sec = 30*24*60*60 #second in 30 days
  firstT = lastT - timeLim_sec
  
  start= as.character(firstT)
  stop= as.character(lastT)
  startID = gsub('\\D', '', start)
  stopID = gsub('\\D', '', stop)

  par_g8<-griddap("erdVH2018par8day",
                  time=c(start,stop),
                  latitude=c(latMin,latMax),
                  longitude=c(lonMin,lonMax),
                  fmt = "nc",
                  # url='https://coastwatch.pfeg.noaa.gov/erddap/',
                  store=disk(path ='/Users/heidi.k.hirsh/Desktop/viirsPAR_8day_30day/'))
  
  targetfilename8 = paste0('/Users/heidi.k.hirsh/Desktop/viirsPAR_8day_30day/PAR_VIIRS4k_8DAY_',startID,'_',stopID,'_loop=',t_i,'.nc')
  ncstatus = file.rename(par_g8$summary$filename, targetfilename8)
  
  
  par_g8<-griddap("erdVH2018parmday",
                  time=c(start,stop),
                  latitude=c(latMin,latMax),
                  longitude=c(lonMin,lonMax),
                  fmt = "nc",
                  # url='https://coastwatch.pfeg.noaa.gov/erddap/',
                  store=disk(path ='/Users/heidi.k.hirsh/Desktop/viirsPAR_Month_30day/'))

  targetfilenameM = paste0('/Users/heidi.k.hirsh/Desktop/viirsPAR_Month_30day/PAR_VIIRS4k_MONTH_',startID,'_',stopID,'_loop=',t_i,'.nc')
  ncstatus = file.rename(par_g8$summary$filename, targetfilenameM)
 
  
  tock= Sys.time()
  elmin=round(difftime(tock,tick,units='mins'),1)
  print(paste0("Completed ",t_i," of ",length(CCfull.all$UTCDate_Time)," in ",elmin," min. Estimated time remaining: ",(length(CCfull.all$UTCDate_Time)-t_i)*elmin,' min. Current time: ',Sys.time()))
  
  
}



# #get appropriate time (days when samples were collected)
# ptime= unique(CC$UTCDate)
# ptime= ymd(ptime)
# ptime= as.character(ptime)
# class(ptime)
# # length(ptime) #14 for 2016
# 
# 
# i=1
# for(i in 1:length(ptime)){
# 
#   par_g8<-griddap("erdVH2018par8day",
#                   # time=c(ptime[i],ptime[i]),
#                   latitude=c(latMin,latMax),
#                   longitude=c(lonMin,lonMax),
#                   fmt = "nc",
#                   # url='https://coastwatch.pfeg.noaa.gov/erddap/',
#                   store=disk(path ='/Users/heidi.k.hirsh/Desktop/PAR2016test/'))
# 
#   targetfilename8 = paste0('/Users/heidi.k.hirsh/Desktop/PAR2016test/PAR_VIIRS4k_8DAY_',ptime[i],'.nc')
#   ncstatus = file.rename(par_g8$summary$filename, targetfilename8)
#   
#   
#   par_g8<-griddap("erdVH2018parmday",
#                   # time=c(ptime[i],ptime[i]),
#                   latitude=c(latMin,latMax),
#                   longitude=c(lonMin,lonMax),
#                   fmt = "nc",
#                   # url='https://coastwatch.pfeg.noaa.gov/erddap/',
#                   store=disk(path ='/Users/heidi.k.hirsh/Desktop/PAR2016test/'))
#   
#   targetfilenameM = paste0('/Users/heidi.k.hirsh/Desktop/PAR2016test/PAR_VIIRS4k_MONTH_',ptime[i],'.nc')
#   ncstatus = file.rename(par_g8$summary$filename, targetfilenameM)
# 
# }




#need to rasterize PAR data so we can mask the shallow water

#mask PAR and cut to bow extents
#if 75% of pixel is below 30m then mask -> convert to NA


bathy_sf=st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_Data/mesh_florida",layer="mesh_florida")
class(bathy_sf) 
head(bathy_sf)

bathy_r = st_rasterize(bathy_sf %>% dplyr::select(bathymetry, geometry))
image(bathy_r)
class(bathy_r)


