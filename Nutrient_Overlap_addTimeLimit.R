#Nutrient_Overlap_addTimeLimit
# use new nutrient dataframe from Alex (limited to the keys only sites)

## Clear Workspace ----------------------------------------------------------
rm(list=ls())
runDate='2023nov2'

# Load Libraries ----------------------------------------------------------
library(stringr)
library(dplyr)
library(mapview)
library(raster)
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
library(stringi)
library(janitor)  #use to remove duplicates (get_dupes) from model output
library(leaflet)
library(sf)
# library(spatialEco) #new_shape <- point.in.poly(pnts, ind_adm)
library(geosphere) #distance between points
register_google(key = "AIzaSyB3mq0nJtcNWvQUNuqH_t-MLxEUmwDDmGk") #api updated OCT23

sf_use_s2(FALSE) #nutrient point overlap with polygons will not work without this line!

#___________________________#

## Load (latest) carbonate chemistry dataframe
CCflk = read.csv('/Users/heidi.k.hirsh/Desktop/CCfull+distance_Oct4.csv')

## Load nutrient data (limited to Keys by Alex)
CCnuts = read.csv('/Users/heidi.k.hirsh/Desktop/WS_Keys_Data_1998-2022_hh.csv')

## Check station number for station 21 (name does not match between the two dataframes)
## Rename station "21" as "21LK" in CCnuts
CCnuts$Station[which(CCnuts$Station == "21")] = "21LK"
dim(CCnuts) #3739   17
#also remove station KW1 (north of Key West)
CCnuts = CCnuts[which(CCnuts$Station != "KW1"),] 
dim(CCnuts) #3717   17


## Reformat Date Time for CCnuts
#need to make CCnuts$UTCDate_Time
CCnuts$Date #"6/7/04"   "6/7/04"  I hate this formatting
CCnuts$Date.yyyymmdd = as.Date(CCnuts$Date, format= "%m/%d/%y")
#GMT and UTC can be considered the same in this data

## Combine date and time in one column
CCnuts$dateTime = str_c(CCnuts$Date.yyyymmdd, ' ', CCnuts$GMT)

## Convert to timestamps POSIXct
CCnuts$UTCDate_Time = as.POSIXct(CCnuts$dateTime, tz="UTC") 
class(CCnuts$UTCDate_Time) #"POSIXct" "POSIXt" 

## Create visitID for CCnuts (station + date + time)
visitID_1 = paste(CCnuts$Station,CCnuts$UTCDate_Time)
visitID_2 = gsub(" ", "_" ,visitID_1, perl=TRUE)
CCnuts$visitID =  gsub("[: -]", "" , visitID_2, perl=TRUE) 
# dim(CCnuts) #3717   21

#the pesky point is visitID = 	19_20160728_013600 (limit CCnuts to anything that does not have that visitID)
CCnuts = CCnuts[which(CCnuts$visitID !='19_20160728_013600'),]  
# dim(CCnuts) # 3716   21


## Limit CCnuts to the same years as carbonate chemistry df (we can remove this step if we later limit the bow overlap to relevant time window)
CCnuts$Year = format(CCnuts$UTCDate_Time, format="%Y")
# unique(CCnuts$Year) 
CCnuts = subset(CCnuts,Year %in% c('2012','2013','2014','2015','2016','2017','2018','2019','2020','2021'))
CCnuts$Year = as.numeric(CCnuts$Year)
# unique(CCnuts$Year)
# dim(CCnuts) #1476   22 (removed a lot of years we don't need and added 'Year' column)


## Convert CCnuts to a simple features df
# class(CCnuts) # "data.frame"
CCnuts.sf = st_as_sf(CCnuts, coords = c("Longitude","Latitude"), crs = st_crs(4326))      
# class(CCnuts.sf)  #"sf"         "data.frame"
# st_crs(CCnuts.sf) #"WGS 84"


## Read in bow tie polygons 
# should I use it here or the information in CCflk? Does CCflk preserve the polygons? I don't think CCflk has the geometry information anymore... 
# maybe I should ultimately be doing this overlap in the same script where I overlap/summarize benthic data? 

## Read in the bow ties df that already includes the benthic data too.
# BBB = st_read('/Users/heidi.k.hirsh/Desktop/FLK_data/Concave_BowBenthic_allRTdays_start14Dec2022.shp') #used for NOV3 run

# IDtab=table(BBB$visitID) #site_date_time
# class(BBB) #"sf"         "data.frame"
BBB = BBB[which(BBB$simu=='backward'),] #only need backward


## Save data and time from visitID (necessary for limiting matching nutrient data to previous 24 hours (or other timeframe))
#isolating the date and time is tricky because some site IDs include underscores (so string split of the whole ID doesn't work as easily as it should)
## Replace last underscore 
BBB$visitID2 = stri_replace_last_fixed(BBB$visitID, '_', ' ')
BBB$visitID2

## Isolate date and time piece (everything after underscore)
BBB$DT = sapply(str_split(BBB$visitID2,pattern= '_'),tail,1)
BBB$DT

## Format as a date-time element
BBB$UTCDate_Time <- as.POSIXct(as.character(BBB$DT), format = "%Y%m%d%H%M%S", tz="UTC")
BBB$dateTime

## Initiate new df for Bows+benthic+nutrients
nutBows=BBB

## Create empty columns to store summarized nutrient data
nutBows[, 'SST'] = NA
nutBows[, 'SSS'] = NA
nutBows[, 'Chla'] = NA
nutBows[, 'Phaeophytin'] = NA
nutBows[, 'NH4'] = NA
nutBows[, 'PO4'] = NA
nutBows[, 'NO3.NO2'] = NA
nutBows[, 'NO2'] = NA
nutBows[, 'NO3'] = NA
nutBows[, 'Si'] = NA

# head(nutBows)
# b_i=1
# b_i=242
# b_i=241
b_i=35

Len=dim(nutBows)[1]
Len

##loop through each polygon
for (b_i in 1:Len) {
# for (b_i in 30:40) {
  # NbowT=nutBows[b_i,]
  # Nbow <- st_make_valid(NbowT). 
  # print(st_is_valid(Nbow))
  Nbow = nutBows[b_i,]
  
  print(paste0('starting bow ',b_i,' of ',nrow(nutBows)))
  tick = Sys.time()
  
  Nbow = st_transform(Nbow, crs=st_crs(4326))
  # mapview(Nbow)
  
  ##filter to include the points inside the polygon
  # inpts= st_intersection(CCnuts.sf, Nbow)  #this joins the points and polygon but I want a df of subset point data
  inpts= st_filter(CCnuts.sf, Nbow)
  
  
  # dim(inpts)
  # mapview(Nbow)+mapview(inpts,zcol='Station')
  
  ##filter by time interval (only before sample date - how far? 24 hours?)
  ##select only points w/in last 24 hours (or only select time matching the time represented by the bow tie (1-7 days)?)

  #subset points that are within the past 24 hours of Nbow$UTCDate_Time
  
  lastT = Nbow$UTCDate_Time
  timeLim_sec = Nbow$ndays*24*60*60
  firstT = Nbow$UTCDate_Time - timeLim_sec
  # lastT
  # timeLim_sec
  # firstT
  
  inpts_subset= subset(inpts, inpts$UTCDate_Time >= firstT & inpts$UTCDate_Time <= lastT)
  # inpts_subset= inpts[inpts$UTCDate_Time >= firstT & inpts$UTCDate_Time <= lastT] #why doesn't this also work?
  
  #what do I do if there are no nutrient samples inside that window (example: b_i=35, Nbow$UTCDate_Time="2012-02-28 09:47:00 UTC")
  if (dim(inpts_subset)[1] == 0) next   #this should result in that polygon having NA assigned for nutrient columns

  # inpts_subset$UTCDate_Time
  # dim(inpts)
  # dim(inpts_subset)
  # mapview(inpts)+mapview(inpts_subset,color='red')
  
  # mapview(Nbow)+mapview(inpts,zcol='Station')+mapview(inpts_subset,col.regions='red')
  # mapview(Nbow)+mapview(inpts,zcol='Date')+mapview(inpts_subset,col.regions='red')
  
  # mapview(Nbow)+mapview(inpts_subset,col.regions='red')
  

  ##summarize nutrients for each bow
  ##summarize nutrient data (mean?) and assign that value to the polygon row
  nutBows$SST[b_i]=mean(inpts_subset$SST,na.rm=T)
  nutBows$SSS[b_i]=mean(inpts_subset$SSS,na.rm=T)
  nutBows$Chla[b_i]=mean(inpts_subset$Chla,na.rm=T)
  nutBows$Phaeophytin[b_i]=mean(inpts_subset$Phaeophytin,na.rm=T)
  nutBows$NH4[b_i]=mean(inpts_subset$NH4,na.rm=T)
  nutBows$PO4[b_i]=mean(inpts_subset$PO4,na.rm=T)
  nutBows$NO3.NO2[b_i]=mean(inpts_subset$NO3,na.rm=T)
  nutBows$NO2[b_i]=mean(inpts_subset$NO2,na.rm=T)
  nutBows$NO3[b_i]=mean(inpts_subset$NO3,na.rm=T)
  nutBows$Si[b_i]=mean(inpts_subset$Si,na.rm=T)

  tock= Sys.time()
  elmin=round(difftime(tock,tick,units='mins'),1)
  print(paste0("Completed ",b_i," of ",nrow(nutBows)," in ",elmin," min. Estimated time remaining: ",(nrow(nutBows)-b_i)*elmin,' min. Current time: ',Sys.time()))

}
# View(nutBows)
head(nutBows)


#how many polygons did not have paired nutrient sample data? 
#count NAs in each column
names(nutBows)

#shouldn't they be the same number for each?
sum(is.na(nutBows$Chla)) # 2528
sum(is.na(nutBows$SST)) # 2063
sum(is.na(nutBows$NO2)) # 2099
sum(is.na(nutBows$Si)) # 2099


# st_write(nutBows, '/Users/heidi.k.hirsh/Desktop/nutBows_space&time_3Nov23.shp')
testBow = st_read('/Users/heidi.k.hirsh/Desktop/nutBows_space&time_3Nov23.shp')

# st_write(nutBows, '/Users/heidi.k.hirsh/Desktop/nutBows_test.shp')
# test= st_read('/Users/heidi.k.hirsh/Desktop/nutBows_test.shp')

names(testBow)

#end up with new dataframe with bows+ benthos+ nutrients (need to pair with carbonate chem)

