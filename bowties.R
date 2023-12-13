#not a useful script'
#just plots bows (with mapview) then makes a map of seagrass for unknown reasons (for all of FL)

## 10/9/2023

## Clear Workspace ----------------------------------------------------------
rm(list=ls())
# runDate='2022Aug11'
runDate='2023Oct9'

# Load Libraries ----------------------------------------------------------
## WHY ARE THERE SO MANY????
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
library(mapview)

hydroGrid = st_read('/Users/heidi.k.hirsh/Desktop/FLK_data/mesh_florida/mesh_florida.shp')


bows1= st_read('/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_7days/bow_ties_1day.shp')
bows2= st_read('/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_7days/bow_ties_2days.shp')
bows3= st_read('/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_7days/bow_ties_3days.shp')
bows4= st_read('/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_7days/bow_ties_4days.shp')
bows5= st_read('/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_7days/bow_ties_5days.shp')
bows6= st_read('/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_7days/bow_ties_6days.shp')
bows7= st_read('/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_7days/bow_ties_7days.shp')
bt = st_read('/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_7days/bow_ties_20190505.shp') #one particular day (May 5, 2019)
  
mapview(bows2)
mapview(bt)
mapview(list(bows1,bt))

mapview(bows7)
mapview(bows3)
mapview(list(bows1,bows7))


seagrass = st_read('/Users/heidi.k.hirsh/Desktop/FLK_data/Seagrass_Habitat_in_Florida/Seagrass_Habitat_in_Florida.shp')
# plot(seagrass) #this takes WAY to long
mapview(seagrass)
