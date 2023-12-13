#Keys_Nutrients

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
library(janitor)  #use to remove duplicates (get_dupes) from model output
library(leaflet)
library(sf)
# library(spatialEco) #new_shape <- point.in.poly(pnts, ind_adm)
library(geosphere) #distance between points
sf_use_s2(FALSE)
register_google(key = "AIzaSyB3mq0nJtcNWvQUNuqH_t-MLxEUmwDDmGk") #api updated OCT23

#___________________________#

## Load (latest) carbonate chemistry dataframe
CCflk = read.csv('/Users/heidi.k.hirsh/Desktop/CCfull+distance_Oct4.csv')

## Load nutrient data (limited to Keys by Alex)
CCnuts = read.csv('/Users/heidi.k.hirsh/Desktop/WS_Keys_Data_1998-2022_hh.csv')

# plotNuts = st_as_sf(CCnuts, coords = c("Longitude","Latitude"), crs = st_crs(4326))  
# pesky =  subset(plotNuts,Station %in% c('19'))
# mapview(plotNuts,zcol="Station")
# mapview(pesky,zcol="Station")
# #we later remove this station after visitID is assigned


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
CCnuts$Date.yyyymmdd
# class(CCnuts$Date.yyyymmdd) #"Date"
# class(CCnuts$GMT) #"character"
#GMT and UTC are basically the same

## Combine date and time in one column
CCnuts$dateTime = str_c(CCnuts$Date.yyyymmdd, ' ', CCnuts$GMT)
CCnuts$dateTime
# class(CCnuts$dateTime) #"character"

## Convert to timestamps POSIXct
CCnuts$UTCDate_Time = as.POSIXct(CCnuts$dateTime, tz="UTC") # format="%Y-%m-%d %H:%M:%S" (it didn't work when I included the format)
CCnuts$UTCDate_Time 
class(CCnuts$UTCDate_Time) #"POSIXct" "POSIXt" 

## Create visitID for CCnuts (station + date + time)
visitID_1 = paste(CCnuts$Station,CCnuts$UTCDate_Time)
visitID_2 = gsub(" ", "_" ,visitID_1, perl=TRUE)
CCnuts$visitID =  gsub("[: -]", "" , visitID_2, perl=TRUE) 
# head(visitID_1)
# head(visitID_2) #underscores added
# head(CCnuts$visitID) # : and - removed in times
# dim(CCnuts) #3717   21

#the pesky point is visitID = 	19_20160728_013600
#limit CCnuts to anything that does not have that visitID
CCnuts = CCnuts[which(CCnuts$visitID !='19_20160728_013600'),]  
# table(CCnuts$visitID)
dim(CCnuts) # 3716   21


## Limit CCnuts to the same years as carbonate chemistry df (we can remove this step if we later limit the bow overlap to relevant time window)
CCnuts$Year = format(CCnuts$UTCDate_Time, format="%Y")
unique(CCnuts$Year) 
CCnuts = subset(CCnuts,Year %in% c('2012','2013','2014','2015','2016','2017','2018','2019','2020','2021'))
CCnuts$Year = as.numeric(CCnuts$Year)
unique(CCnuts$Year)
dim(CCnuts) #1476   22 (removed a lot of years we don't need and added 'Year' column)

#no further formatting of CCnuts done in this step 
#keep relevant CCnuts data and pair with polygons in CCflk (exact visitIDs do not need to match)

class(CCnuts) # "data.frame"
# conver CCnuts to sf dataframe 
CCnuts.sf = st_as_sf(CCnuts, coords = c("Longitude","Latitude"), crs = st_crs(4326))        #,crs=st_crs(Nbow)) crs=32617.  4326
class(CCnuts.sf)
st_crs(CCnuts.sf) #"WGS 84"


# read in bow tie polygons 
# should I use it here or the information in CCflk? Does CCflk preserve the polygons? I don't think CCflk has the geometry information anymore... 

# maybe I should ultimately be doing this overlap in the same script where I overlap/summarize benthic data? 

#read in the bow ties dataframe that already includes the benthic data too.
BBB = st_read('/Users/heidi.k.hirsh/Desktop/FLK_data/Concave_BowBenthic_allRTdays_start14Dec2022.shp')
# IDtab=table(BBB$visitID) #site_date_time
class(BBB) #"sf"         "data.frame"
BBB = BBB[which(BBB$simu=='backward'),] #only need backward
# dim(BBB) 
# head(BBB)

##divide visitID to pull date and time 
splitID = str_split(BBB$visitID,pattern= '_')
splitID
# Time = sapply(str_split(BBB$visitID,pattern= '_'),tail,-1)
Time = sapply(str_split(BBB$visitID,pattern= '_'),tail,1)
Time 
Time = as.factor(Time)
Time #should be able to turn this into a date time element...
library('chron')
test=as.character(times(gsub("(..)(..)(..)", "\\1:\\2:\\3", Time)))
test
class(test)
# as.Date(test,'%H:%M:%S').  #don't do this. 

#replace the last _ in the ID
library('stringi')
# new = stri_replace_last(BBB$visitID, ' ', )

#translate example to work for last "_" instead of comma
# locating the places of comma occurrence 
# loc_comma <- str_locate_all(str, ",")[[1]]   

# BBB$visitID[1]
# loc_und <- str_locate_all(BBB$visitID, "_") [[1]]   #try just first date
# loc_und
#
# # calculating the number of positions  
# # in comma array 
# # comma_ele <- nrow(loc_comma) 
# und_ele <- nrow(loc_und) 
# und_ele
# 
# # fetching the last comma position 
# # last_loc_comma <- loc_comma[ comma_ele , ]  
# last_loc_ele <- loc_und[und_ele, ]
# last_loc_ele
# 
# # replace the last comma with & symbol  
# # str_sub(str, last_loc_comma[1],  
# #         last_loc_comma[2]) <- " &"  
# test2=str_sub(BBB$visitID, last_loc_ele[1],  
#         last_loc_ele[2]) <- ""  
# 



## Replace last underscore 
BBB$visitID2 = stri_replace_last_fixed(BBB$visitID, '_', ' ')
# BBB$visitID2 

## Isolate date and time piece (everything after underscore)
BBB$DT = sapply(str_split(BBB$visitID2,pattern= '_'),tail,1)
# BBB$DT

## Format as a date-time element
BBB$dateTime <- as.POSIXct(as.character(BBB$DT) , format = "%Y%m%d%H%M%S", tz="UTC")
# BBB$dateTime

head(BBB$dateTim,100)

##how do I get the second to last element?
BBB2 = BBB %>% separate(visitID, c('site', 'date', 'time')) #this doesn't work when the site names have "_" in them
# head(BBB2)
# BBB2$date =  as.Date(BBB2$date, "%Y%m%d")

nutBows=BBB


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

head(nutBows)
# b_i=1
# b_i=242
# b_i=241

Len=dim(nutBows)[1]

##loop through each polygon
for (b_i in 1:Len) {
  NbowT=nutBows[b_i,]
  Nbow <- st_make_valid(NbowT)
  print(st_is_valid(Nbow))
  
  print(paste0('starting bow ',b_i,' of ',nrow(nutBows)))
  tick = Sys.time()
  
  Nbow = st_transform(Nbow, crs=st_crs(4326))
  # mapview(Nbow)
  
  ##filter to include the points inside the polygon
  inpts= st_intersection(CCnuts.sf, Nbow)
  # inpts= st_filter(CCnuts.sf, Nbow)
  # dim(inpts)
  # mapview(Nbow)+mapview(inpts,zcol='Station')
  
  ##filter by time interval (only before sample date - how far? 24 hours?)
  ##select only points w/in last 24 hours (or only select time matching the time represented by the bow tie (1-7 days)?)
  # inpts_subset=inpts[inpts$dateTime <= as.POSIXct(Nbow$dateTime) - as.difftime(days = 1),]
  
  
  
  ##summarize nutrients for each bow
  ##summarize nutrient data (mean?) and assign that value to the polygon row
  nutBows$SST[b_i]=mean(inpts$SST,na.rm=T)
  nutBows$SSS[b_i]=mean(inpts$SSS,na.rm=T)
  nutBows$Chla[b_i]=mean(inpts$Chla,na.rm=T)
  nutBows$Phaeophytin[b_i]=mean(inpts$Phaeophytin,na.rm=T)
  nutBows$NH4[b_i]=mean(inpts$NH4,na.rm=T)
  nutBows$PO4[b_i]=mean(inpts$PO4,na.rm=T)
  nutBows$NO3.NO2[b_i]=mean(inpts$NO3,na.rm=T)
  nutBows$NO2[b_i]=mean(inpts$NO2,na.rm=T)
  nutBows$NO3[b_i]=mean(inpts$NO3,na.rm=T)
  nutBows$Si[b_i]=mean(inpts$Si,na.rm=T)

  tock= Sys.time()
  elmin=round(difftime(tock,tick,units='mins'),1)
  print(paste0("Completed ",b_i," of ",nrow(nutBows)," in ",elmin," min. Estimated time remaining: ",(nrow(nutBows)-b_i)*elmin,' min. Current time: ',Sys.time()))


}

head(nutBows)

# st_write(nutBows, '/Users/heidi.k.hirsh/Desktop/nutBows_test.shp')

test= st_read('/Users/heidi.k.hirsh/Desktop/nutBows_test.shp')


# then that dataframe needs to be bound to CCflk (use BBB input with benthos already added as input to this step???)





# 
# BBB$visitID
# BBB$visitID2=gsub("[_]", "" , BBB$visitID, perl=TRUE) 
# 
# 
# numberstring_split <- substring(BBB$visitID2[2], start = -15, stop = length(BBB$visitID2[2]))
# 
# myTest <- substr(BBB$visitID2[2], start = -15, stop = 200)
# myTest <- substr(BBB$visitID2[2], start = -14, stop = 200)
# 
# 
# head(BBB)







  
  
  # for (b_i in 1:dim(BBB)[1]) {
  #   Nbow=BBB[b_i,]
  #   Nbow = st_transform(Nbow, crs=st_crs(4326))
  #   inpts= st_filter(CCnuts.sf, Nbow)
  #   inpts_subset=inpts[inpts$dateTime >= as.POSIXct(Nbow$dateTime) - as.difftime(days = 1),]
  #   #Nbow$mean_chl=mean(inpts_subset$chl)
  #   BBB[b_i, "mean_chl"]=mean(inpts_subset$chl)
  #   BBB[b_i, "mean_temp"]=mean(inpts_subset$temp)
  #   BBB[b_i, "mean_no3"]=mean(inpts_subset$no3)
  # }

  
 #end loop to pull benthic info
