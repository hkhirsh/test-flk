#Keys_Nutrients

# I build off of this script in Nutrient_Overlap and Nutrient_Matching
# This is the original troubleshooting script to find the nonmatching nutrient samples (between CCflk and CCnuts) and clean up accordingly

# use new nutrient dataframe from Alex (limited to the keys only sites)

## Clear Workspace ----------------------------------------------------------
rm(list=ls())
runDate='2023oct29'

# Load Libraries ----------------------------------------------------------
library(stringr)
# library(raster)
library(dplyr)
# library(ggmap)
# library(plyr)
# library(readr)
# library(purrr)
# library(ggplot2)
# library(patchwork)
# library(lme4)
# library(lmerTest)
# library(lubridate)
# library(scales)
# library(ncdf4)
# library(jtools)
# library(modeltime)
# library(MuMIn)
# library(tidyverse)
# library(lubridate)
# library(cmocean)
# library(gridExtra)
# library(cowplot)
# library(rerddap)
# library(openair)

# library(janitor)  #use to remove duplicates (get_dupes) from model output
# library(leaflet)
# library(sf)
# library(geosphere) #distance between points
register_google(key = "AIzaSyB3mq0nJtcNWvQUNuqH_t-MLxEUmwDDmGk") #api updated OCT23

#___________________________#

# Load filtered carbonate chemistry data from Ana
# CCflk <- read.csv('/Users/heidi.k.hirsh/Desktop/FLK_data/FLK_filtered_ve4.csv')
# CCfull.dist= read.csv('/Users/heidi.k.hirsh/Desktop/CCfull+distance_Oct4.csv')
# unique(CCfull.dist$year) #2012 2014 2015 2016 2017 2018 2019 2020 2021
CCflk = read.csv('/Users/heidi.k.hirsh/Desktop/CCfull+distance_Oct4.csv')
CCflk$visitID


#limit nutrients data to 2012-2021
# CCnuts =  read.csv('/Users/heidi.k.hirsh/Desktop/FLK_data/WSv16_clean.csv')
# oldNuts = read.csv('/Users/heidi.k.hirsh/Desktop/FLK_data/WSv16_clean.csv')
CCnuts = read.csv('/Users/heidi.k.hirsh/Desktop/WS_Keys_Data_1998-2022_hh.csv')
dim(CCnuts) # 3739   17

plotNuts = st_as_sf(CCnuts, coords = c("Longitude","Latitude"), crs = st_crs(4326))  
pesky =  subset(plotNuts,Station %in% c('19'))
# pesky=plotNuts
<<<<<<< HEAD

# is station 16 closest to big pine key
# pesky =  subset(plotNuts,Station %in% c('16'))

=======
mapview(plotNuts,zcol="Station")
>>>>>>> c9b0d09 (Recommit changes that we undid)
mapview(pesky,zcol="Station")


#check station number for station 21
CCnuts$Station[which(CCnuts$Station == "21")] = "21LK"
dim(CCnuts) #3739   17
#also remove station KW1 (north of keys)
CCnuts = CCnuts[which(CCnuts$Station != "KW1"),] 
dim(CCnuts) #3717   17
<<<<<<< HEAD
=======

# plotNewNuts = st_as_sf(CCnuts, coords = c("Longitude","Latitude"), crs = st_crs(4326))  
# mapview(plotNewNuts,zcol="Station") #"pesky point is removed after isolating visitID

>>>>>>> c9b0d09 (Recommit changes that we undid)

#need to make CCnuts$UTCDate_Time
CCnuts$Date #"6/7/04"   "6/7/04"   "6/7/04" I hate this formatting
CCnuts$Date.yyyymmdd = as.Date(CCnuts$Date, format= "%m/%d/%y")
<<<<<<< HEAD
=======
CCnuts$Date.yyyymmdd
>>>>>>> c9b0d09 (Recommit changes that we undid)
# class(CCnuts$Date.yyyymmdd)
# class(CCnuts$GMT) #GMT and UTC are basically the same

#Combine date and time in one column
CCnuts$dateTime = str_c(CCnuts$Date.yyyymmdd, ' ', CCnuts$GMT)
<<<<<<< HEAD
# class(CCnuts$dateTime)
=======
CCnuts$dateTime
# class(CCnuts$dateTime)

>>>>>>> c9b0d09 (Recommit changes that we undid)
##convert to POSIXct
CCnuts$UTCDate_Time = as.POSIXct(CCnuts$dateTime, tz="UTC") # format="%Y-%m-%d %H:%M:%S" (it didn't work when I included the format)
CCnuts$UTCDate_Time 
#create visitID (station + date + time)
visitID_1 = paste(CCnuts$Station,CCnuts$UTCDate_Time)
visitID_2 = gsub(" ", "_" ,visitID_1, perl=TRUE)
CCnuts$visitID =  gsub("[: -]", "" , visitID_2, perl=TRUE) 
<<<<<<< HEAD
=======
CCnuts$visitID
>>>>>>> c9b0d09 (Recommit changes that we undid)
# unique(CCnuts$visitID)
# unique(CCnuts$Station)
# View(CCnuts)

<<<<<<< HEAD
dim(CCnuts)
=======
dim(CCnuts) #3717   21
>>>>>>> c9b0d09 (Recommit changes that we undid)

#the pesky point is visitID = 	19_20160728_013600
#limit CCnuts to anything that does not have that visitID
CCnuts = CCnuts[which(CCnuts$visitID !='19_20160728_013600'),]  
# table(CCnuts$visitID)
<<<<<<< HEAD
dim(CCnuts) # 3738   21 
=======
dim(CCnuts) # 3716   21
>>>>>>> c9b0d09 (Recommit changes that we undid)



# limit nutrients to same time period as carbonate chemistry df
#need to isolate year
CCnuts$Year = format(CCnuts$UTCDate_Time, format="%Y")
unique(CCnuts$Year) #[1] "1998" "1999" "2000" "2001" "2002" "2003" "2004" "2005" "2006" "2007" "2008" "2009" "2010" "2011" "2012" "2014" "2015" "2016" "2017" "2018" "2019" "2020" "2021" "2022"
# test = CCnuts[which(CCnuts$Year==c(2012:2021))] #why doesn't this work?
CCnuts = subset(CCnuts,Year %in% c('2012','2013','2014','2015','2016','2017','2018','2019','2020','2021'))
CCnuts$Year = as.numeric(CCnuts$Year)
unique(CCnuts$Year)
dim(CCnuts) #1476   22
<<<<<<< HEAD


#nutrients pca
#pca plot of nutrient data
nutNames = names(CCnuts[10:17])
=======

##_______________nutrients pca
#pca plot of nutrient data
nutNames = names(CCnuts[10:17])

#untransformed data
nutPCA = princomp(scale(na.omit(CCnuts[,nutNames])))
plot(nutPCA)
biplot(nutPCA)

#transformed data: 
nutPCA = princomp(scale(na.omit(apply(CCnuts[,nutNames]+10^-6,2,log10))))
plot(nutPCA)
biplot(nutPCA)
biplot(nutPCA, col=c("grey", "black"), xlab="", ylab="")
title(xlab="PC1", ylab="PC2", mgp=c(2.2, 2.2, 0))

##_______________


>>>>>>> c9b0d09 (Recommit changes that we undid)

#untransformed data
nutPCA = princomp(scale(na.omit(CCnuts[,nutNames])))
plot(nutPCA)
biplot(nutPCA)

#transformed data: 
nutPCA = princomp(scale(na.omit(apply(CCnuts[,nutNames]+10^-6,2,log10))))
plot(nutPCA)
biplot(nutPCA)
biplot(nutPCA, col=c("grey", "black"), xlab="", ylab="")
title(xlab="PC1", ylab="PC2", mgp=c(2.2, 2.2, 0))

#combine CCfull.dist and CCnuts (carbonate chem+ and nutrients)
# class(CCnuts$UTCDate_Time) #"POSIXct" "POSIXt" 
# class(CCfull.dist$UTCDate_Time) #"character"
# class(CCflk$UTCDate_Time) #"character"
CCflk$UTCDate_Time = as.POSIXct(CCflk$UTCDate_Time, tz="UTC")

# View(CCflk[which(is.na(match(CCflk$visitID,CCnuts$visitID))),])
dim(CCflk)

# A = (CCflk[which(is.na(match(CCflk$visitID,CCnuts$visitID))),])
# table(A$SiteID)/7
# table(CCflk$SiteID)/7
# CCflk %>% filter(SiteID =='21LK') %>% select(visitID)
# CCnuts %>% filter(Station =='21LK') %>% select(visitID)

# table(CCnuts$Station)
# 
# CCtest=NULL
# CCtest= CCnuts %>% left_join(CCflk)
# # View(CCtest)
# 
# CCtest = merge(CCnuts, CCflk, by = "UTCDate_Time")

#did lat/lon match up correctly - some are not on the 1:1 line ARGH
plot(CCtest$Latitude.x,CCtest$Latitude.y)
plot(CCtest$Longitude.x,CCtest$Longitude.y)

#combine CCfull.dist and CCnuts (carbonate chem+ and nutrients)
# class(CCnuts$UTCDate_Time) #"POSIXct" "POSIXt" 
# class(CCfull.dist$UTCDate_Time) #"character"
# class(CCflk$UTCDate_Time) #"character"
CCflk$UTCDate_Time = as.POSIXct(CCflk$UTCDate_Time, tz="UTC")
class(CCflk$UTCDate_Time) #"POSIXct" "POSIXt" 

# View(CCflk[which(is.na(match(CCflk$visitID,CCnuts$visitID))),])
dim(CCflk)


#Troubleshoot to determine sites that do not match
# A = (CCflk[which(is.na(match(CCflk$visitID,CCnuts$visitID))),])
# table(A$SiteID)/7
# table(CCflk$SiteID)/7
# CCflk %>% filter(SiteID =='21LK') %>% select(visitID)
# CCnuts %>% filter(Station =='21LK') %>% select(visitID)

# table(CCnuts$Station)
# CCtest=NULL
CCtest= CCnuts %>% left_join(CCflk)
# View(CCtest)
# 
# CCtest = merge(CCnuts, CCflk, by = "UTCDate_Time")

#did lat/lon match up correctly - some are not on the 1:1 line ARGH
plot(CCtest$Latitude.x,CCtest$Latitude.y)
plot(CCtest$Longitude.x,CCtest$Longitude.y)


# read in bowties
BBB = st_read('/Users/heidi.k.hirsh/Desktop/FLK_data/Concave_BowBenthic_allRTdays_start14Dec2022.shp')
# IDtab=table(BBB$visitID) #site_date_time
class(BBB) #"sf"         "data.frame"
BBB = BBB[which(BBB$simu=='backward'),] #only need backward
dim(BBB) 

b_i=1
Nbow=BBB[b_i,]

st_crs(Nbow)
class(Nbow)
# plot(Nbow)

# pesky =  subset(CCnuts.sf,Station %in% c('19'))
# pesky$Year = as.character(pesky$Year)
# # pesky =  subset(CCnuts.sf,Year %in% c('2016'))
# #the pesky point is visitID = 	19_20160728_013600
# table(pesky$visitID)

# mapview(pesky,zcol="Year")

mapview(CCnuts.sf,zcol='Station')+ 
  mapview(Nbow)

class(CCnuts)
CCnuts.sf = st_as_sf(CCnuts, coords = c("Longitude","Latitude"), crs = st_crs(4326))        #,crs=st_crs(Nbow)) crs=32617.  4326

Nbow = st_transform(Nbow, crs=st_crs(4326))

mapview(CCnuts.sf)+ 
  mapview(Nbow)

CCnuts[,c("Longitude","Latitude")]
class(CCnuts$Longitude)




inpts= st_filter(CCnuts.sf, Nbow)

mapview(inpts)+
  mapview(Nbow)

<<<<<<< HEAD
Nbow 
=======

#try adding nutrients to Nbow
Nbow$SST = mean(inpts$SST)
>>>>>>> c9b0d09 (Recommit changes that we undid)


# st_overlaps(Nbow,CCnuts.sf)

# CCflk.sf = st_as_sf(CCflk, coords = c("Longitude","Latitude"), crs=32617)        #,crs=st_crs(Nbow))
# mapview(CCflk.sf)+ 
#   mapview(Nbow)



# CCnuts$Latitude
# names(CCnuts)
# class(CCnuts.sf)
# st_crs(CCnuts.sf)
# st_overlaps(Nbow,CCnuts.sf)






















# #create simple data frame of nutrient coordinates
# Npts.df<- data.frame(lat = CCnuts$Latitude, lon= CCnuts$Longitude, CHLA= CCnuts$Chla) #nutrient data points
# coordinates(Npts.df) = ~ lat + lon
# # mapview(Npts.df) #this looks weird
# Npts.sp = SpatialPoints(Npts.df)

# class(Npts.df) #sp!
# Npts.dft = st_transform(Npts.df, st_crs(Nbow)) #can't transform a dataframe (still doesn't work as sp)
# Npts.spt = st_transform(Npts.sp, st_crs(Nbow)) #nope.

# bowNuts = st_intersection(Npts.df,Bbow)
#maybe st_overlaps instead
#or maybe just over() from sp


