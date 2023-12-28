rm(list = ls())

library(sf)
library(ggplot2)
library(tidyverse)
library(ggmap)
library(ggrepel)
library(rstudioapi)
library(stringr)
library(sp)
library(raster)
library(rgeos)
library(patchwork)
library(mapview)
library(leaflet)
register_google(key = "AIzaSyB3mq0nJtcNWvQUNuqH_t-MLxEUmwDDmGk") #api updated OCT23

#setting up to run with latest bowites 12/28/2023
#run original bowties (no land) on HH mac 12/24/2022
#update: i messed up the 12/24 run because I buffered in and out on the convex hulls (dumb). Need to rerun without doing that!

#read in FL unified reef map habitat data:
coral_sf =  st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/Final_UnifiedReefMap_Version2.0/FWC_UnifiedFloridaReefMap_v2.0.gdb", layer = "UnifiedReefMap")

#load benthic composition lookup table:
benthicLU = read.csv("/Users/heidi.k.hirsh/Documents/Git_Projects/FLK-Modeling-Hirsh-copy-DONTTOUCH//ClassLv4_BenthicLookup_V1_Rank_fixed_NOland_Clean.csv")


bow_fl = list.files(path = paste0("/Users/heidi.k.hirsh/Desktop/bow_ties_shp_extended/bow_ties_shp_extended_tri/14days" ), full.names = T)
head(bow_fl)
length(bow_fl) #1220



# f_i=1100 #one with a split name
# #loop through years up here...
# f_i=77
yearBows = NULL
# for (f_i in 1:length(bow_fl)) {
for (f_i in 1:5) {
    
  splitBFile = str_split(string = bow_fl[f_i], "/")
  splitBFile
  layerName = splitBFile[[1]][8]
  layerName
  
  bows=NULL
  #I think I can skip to the defining the layerName here (as vID) then go straight to the st_read line without starting a second loop (since not divided by year). sorry I'm making a mess future Heidi, but I think this will ultimately clean it up actually.
  
  # #read in all bow tie files for the year (identify them)
  # #for the new shapefile structure this reads in all files for each site/date (unique visitID)
  # # bow_fl = list.files(path = paste0("/Users/heidi.k.hirsh/Desktop/old_bowties_no_land_noAug1/",Year),full.names=T)
  # bow_fl = list.files(path = paste0("/Users/heidi.k.hirsh/Desktop/bow_ties_shp_extended/bow_ties_shp_extended_tri/14days/",vID),full.names=T)
  # bow_fl=bow_fl[4]
  # #need to select the shp file (is it always the 4th file?)
  #
  # # bow_fl = list.files(path = paste0("/Users/heidi.local/Desktop/FLK_data/concave_bowtie_thingies/",Year),full.names=T)
  # #I removed the file for 12_20170801
  # #for the convex approach I removed 12_20170801_072200
  # # bow_fl
  # # length(bow_fl)
  #
  # #check for file to remove. already gone (I only see: 12_20170131_032400_14days)
  #
  # #pull out relevant info (date, #days)
  # f_i=1
  # bows=NULL
  # for (f_i in 1:length(bow_fl)) {   #loop through sampling sites for the given year
  #
  #   splitBFile=str_split(string=bow_fl[f_i],"/")
  #   splitBFile
  # bowGroup = splitBFile[[1]][8]
  # bowGroup
  
  
  bows.original = st_read(dsn = bow_fl[f_i], layer = layerName)
  head(bows.original)
  dim(bows.original)

  #concave out and in
  bows_utm17 = st_transform(bows.original, 26917)
  # View(bows_utm17)
  # class(bows_utm17$geometry)
  # plot(bows_utm17)
  # mapview(bows_utm17)
  crs(bows_utm17) #NAD83
  
  D = 300 #(buffer width in meters)
  bows.bo = st_buffer(bows_utm17, dist = D, nQuadSegs = 30)
  bows.bi = st_buffer(bows.bo, dist = -D, nQuadSegs = 30)
  # View(bows.bi)

  #pull visitID information and separate out year
  splitName = str_split(string = layerName, "_")
  splitName
  visitID = sub("_[^_]*$", "", layerName)  #drop "_14days"
  visitID
  ID_cuTime = sub("_[^_]*$", "", visitID)   
  ID_cuTime 
  ID_date = strsplit(ID_cuTime, "_(?!.*_)", perl = TRUE)[[1]]
  ID_date
  Date = ID_date[-1]
  Date
  Year = substr(Date, 1, 4)
  Year
  
  bows=bows.bi
  bows$year = Year
  bows$year
  #how can I isolate year from the name? since sites are sometimes separated by _)
  bows$visitID = visitID
  bows$visitID
  bows$duration = bows$n_days
  bows$duration
  bows$bowID = paste0(bows$visitID, "_", bows$simu, "_", bows$n_days)
  bows$bowID  

  head(bows)

  yearBows =  rbind(yearBows, bows)
  # dim(yearBows)
  print(paste0(f_i, ' of ', length(bow_fl)))
  
}

View(yearBows)
# st_write(yearBows, '/Users/heidi.local/Desktop/FLK_data/ConcaveBufferedBows_AllYears_14Dec2022.shp')
#below was the latest saved file (oct15)
# st_write(yearBows, '/Users/heidihirsh/Desktop/ConvexBufferedBows_AllYears_24Dec2022.shp')
#new save (oct15)
st_write(yearBows,'/Users/heidi.k.hirsh/Desktop/Concave14Bows_AllYears_subset5_dec28.shp')

#don't do volume intersection since ThomasD already calculated volume. So start with 'ConcaveBows_AllYears.shp' below
#Buffer out and back in (to capture shallow areas)

#PLOT BOWS
# mapview(bows_utm17, color='red')+mapview(bows.bo)
# 
# # ploThis = bows.original
# # ploThis = bows.bo
# ploThis = bows.bi
# crs(ploThis)
# ploThis_t = st_transform(ploThis,crs='+proj=longlat +datum=WGS84')
# crs(ploThis)
# 
# clrs = rainbow(14, start = 0, end = 0.8)
# clrs_rev <- rev(rainbow(14, start = 0, end = 0.8))
# d=ploThis_t$n_days
# pal = colorNumeric(palette = clrs_rev, domain = min(d):max(d))
# leaflet(data=ploThis_t) %>%
#   addProviderTiles('Esri.WorldImagery') %>%
#   addPolygons(data=subset(ploThis_t,simu=='backward'),fillOpacity=0.1,weight=1,color = ~pal(d))

#***CHECK CRS for combining with other data!


# ________________________________ADD BENTHIC INDICES_____________________________________


benthicBows=yearBows

## to run from file
# benthicBows=NULL
# benthicBows = st_read('/Users/heidihirsh/Desktop/ConvexBufferedBows_AllYears_24Dec2022.shp')

dim(benthicBows)
#don't need forward bows:
benthicBows = benthicBows[which(benthicBows$simu == 'backward'), ]
#limit to 7 days (or fewer)
benthicBows = benthicBows[which(benthicBows$n_days <=7),]
dim(benthicBows)
# unique(benthicBows$simu)
# unique(benthicBows$n_days)



benthicBows[, 'CALCi.m2'] = NA
benthicBows[, 'ALGi.m2'] = NA
benthicBows[, 'SGi.m2'] = NA
benthicBows[, 'NBi.m2'] = NA
benthicBows[, 'NBi.m2'] = NA
benthicBows[, 'NCo.m2'] = NA
benthicBows[, 'PercentCheck'] = NA

# b_i=1
for (b_i in 1:nrow(benthicBows)) {
  # for (b_i in 1:7) {
  Bbow = benthicBows[b_i, ]
  print(paste0('starting bow ', b_i, ' of ', nrow(benthicBows)))
  tick = Sys.time()
  # st_crs(Bbow) #D_Unknown_based_on_WGS84_ellipsoid
  # st_crs(coral_sf) #NAD83
  
  
  #Tranform coral_sf to match Bbow CRS:
  coral_sf.t = st_transform(coral_sf, st_crs(Bbow))
  # st_crs(coral_sf.t) #"D_Unknown_based_on_WGS84_ellipsoid"
  coral_sf.tv = coral_sf.t[which(st_is_valid(coral_sf.t)), ]
  over_bow = st_intersection(coral_sf.tv, Bbow)
  # st_crs(over_bow)
  print('done with intersection')
  
  #Get area of each benthic class
  over_bow$class_area = as.numeric(st_area(over_bow)) #m2
  bowComp = over_bow %>% st_drop_geometry() %>% dplyr::select(ClassLv4, Shape_Area, class_area) %>% group_by(ClassLv4) %>% summarize(a =
                                                                                                                                       sum(class_area, na.rm = TRUE))
  bowComp$p = bowComp$a / st_area(Bbow) #!!change to Bbow.t if I do the tranformation in the other direction
  bowComp %>% arrange(desc(p))
  print(paste0('Percent sum check: ', sum(bowComp$p), ' (should be <1)'))
  
  # mapview(over_bow,zcol="ClassLv4")
  # plot(Bbow)
  
  BowWeight = left_join(bowComp, benthicLU, by = 'ClassLv4')
  # BowWeight$NBTest = 1-BowWeight$Bio
  # BowWeight$NBTestIndex = BowWeight$a*BowWeight$NBTest
  #NONONO BowWeight$NotBio = 1-BowWeight$Bio
  # I think this calculation would include land categories. Instead, use NotBio from LU
  
  #biomass index for each class
  BowWeight$Ci = BowWeight$a * BowWeight$Coral
  BowWeight$Ai = BowWeight$a * BowWeight$Algae #fix spelling!
  BowWeight$Si = BowWeight$a * BowWeight$Seagrass
  BowWeight$NBi = BowWeight$a * BowWeight$NotBio
  BowWeight$NCo = BowWeight$a * BowWeight$BioNoCoral #should be sum of seagrass and algae
  
  #summarize for each biomass index (all classes contribute to each)
  benthicBows$CALCi.m2[b_i] = sum(BowWeight$Ci, na.rm = T)
  benthicBows$ALGi.m2[b_i] = sum(BowWeight$Ai, na.rm = T)
  benthicBows$SGi.m2[b_i] = sum(BowWeight$Si, na.rm = T)
  benthicBows$NBi.m2[b_i] = sum(BowWeight$NBi, na.rm = T)
  benthicBows$NCo.m2[b_i] = sum(BowWeight$NCo, na.rm = T)
  benthicBows$PercentCheck[b_i] = sum(bowComp$p)
  
  # BowWeight$tIndex = BowWeight$Ci + BowWeight$Ai + BowWeight$Si + BowWeight$NBi  #should this end up being the total area?
  # BowWeight$tIndex - BowWeight$a #why isn't this always 0?
  
  # bow_area = st_area(test)
  # sum(BowWeight$a)/bow_area #(it should be less than 1)
  # sum(BowWeight$p)
  
  tock = Sys.time()
  elmin = round(difftime(tock, tick, units = 'mins'), 1)
  
  print(
    paste0(
      "Completed ",
      b_i,
      " of ",
      nrow(benthicBows),
      " in ",
      elmin,
      " min. Estimated time remaining: ",
      (nrow(benthicBows) - b_i) * elmin,
      ' min. Current time: ',
      Sys.time()
    )
  )
  
  
  
} #end loop to pull benthic info

# st_write(benthicBows, '/Users/heidi.local/Desktop/Concave_BowBenthic_allRTdays_start13Dec2022.shp')
# st_write(benthicBows, '/Users/heidi.local/Desktop/Concave_BowBenthic_allRTdays_start14Dec2022.shp')
st_write(
  benthicBows,
  '/Users/heidihirsh/Desktop/Convex_BowBenthic_allRTdays_start24Dec2022.shp'
)

dim(benthicBows)
