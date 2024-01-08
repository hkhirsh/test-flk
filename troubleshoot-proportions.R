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
library(raster)
library(mapview)
library(leaflet)
library(dplyr)
library(stats)
register_google(key = "AIzaSyB3mq0nJtcNWvQUNuqH_t-MLxEUmwDDmGk") #new api OCT2023


## Load data from Thomas
propTime = st_read('/Users/heidi.k.hirsh/Desktop/refloridakeysline/time_north_shp/time_north.shp')
names(propTime)

## Restructure data from Thomas so that the columns for each time fraction (ex: north_1d) are including in "North_Fraction" and "South_Fraction" columns
#I think I need to pivot_longer to accomplish this
propTime.L = pivot_longer(data=propTime,cols=-c(simu,sample_id,geometry),names_to="side.ndays",values_to="fraction")
head(propTime.L)

## Now I need to separate out the number of days into new "ndays" column

propTime.L <- propTime.L %>% separate(side.ndays, c("side", "ndays"), "_")
head(propTime.L)

propTime.L $ndays= as.numeric(str_extract(propTime.L $ndays, "[0-9]+")) #drop the "d" and only keep number of days
table(is.na(test$fraction)) #why are some fractions NA? 

# final = reshape(test, idvar = "side", timevar = "fraction", direction = "wide")
# final = pivot_wider(data=test,cols=c(Side,fraction),names_from='Side',values_from='fraction') #do not specify cols? 
propTime.W = pivot_wider(data=test,names_from='side',values_from='fraction')
propTime.W 

summary(propTime.W$north)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.00000 0.00000 0.00000 0.08140 0.02251 0.99845     113 

summary(propTime.W$south)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.00155 0.97749 1.00000 0.91860 1.00000 1.00000     113 

#reaname fraction columns: 
propTime.W=propTime.W %>% rename(north_fraction=north,south_fraction=south)

#NEXT: pair this data from CC dataframe to match up site zones and regions for further exploration

View(propTime.W)

#don't do the loop! use separate() above

# split = str_split(string = propTime.L$side.ndays, "_")
# #save side (of line - north or south) and ndays columns in a loop
# sides=NULL
# nd=NULL
# for (i in 1:dim(propTime.L)[1]) { 
# sides[i]= split[[i]] [1]
# n.temp = split[[i]] [2]
# nd[i]=as.numeric(str_extract(n.temp, "[0-9]+"))
# }
# 
# propTime.L$side = sides
# propTime.L$ndays = nd
# head(propTime.L)
# 
# #I think I need to drop the side.ndays column
# test = propTime.L[-side.ndays]
# propTime.W = pivot_wider(data=propTime.L,cols=c(side,fraction),names_from='side',values_from='fraction')
# head(propTime.W)
