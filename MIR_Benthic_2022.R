# play with MIR benthic data

## Clear Workspace ----------------------------------------------------------
rm(list=ls())

# Load Libraries ----------------------------------------------------------
library(ggmap)
library(ggplot2)
library(patchwork)
library(leaflet)
library(sf)
library(mapview)
library(lubridate)
# register_google(key = "your API key goes here")    #https://rpubs.com/jcraggy/841199 
#mapview should work without the API 
#don't forget to update file paths :) 


##___________________________##

## Load benthic data from John (Oct 18, 2023)
MIR = read.csv('/Users/heidi.k.hirsh/Desktop/MIR_2022_benthic_cover.csv')
# names(MIR)


# Make a datestamp (separate YEAR, MONTH, DAY is annoying)
# unique(MIR$YEAR)           #all 2022
MIR$Date = make_date(year=MIR$YEAR, month=MIR$MONTH, day=MIR$DAY)
# unique(MIR$Date)
# length(unique(MIR$Date))   #16 sampling dates

## This will be easier if we turn it into spatial data (I like simple features, sf)
# class(MIR) #data.frame 
MIR_sf = st_as_sf(MIR, coords = c("LON_DEGREES","LAT_DEGREES"), crs = st_crs(4326))  
# class(MIR_sf)
# crs(MIR_sf) #WGS 84
# st_crs(MIR_sf) #WGS 84 / Pseudo-Mercator
## this will make it easier for this data to "play with" other spatial data

unique(MIR_sf$COVER_CAT_NAME)
length(unique(MIR_sf$COVER_CAT_NAME))   # 37 unique categories

mapview(MIR_sf)


## Load MIR shapefiles from Katey 

mirPoly_shp = st_read('/Users/heidi.k.hirsh/Desktop/remirdata/MIR Shp/MIR Site Polygons.shp')

class(mirPoly_shp) # "sf"         "data.frame"
# plot(mirPoly_shp)
# mapview(mirPoly_shp) #need to reproject?

# coordinates(mirPoly_shp) #doesn't work

library(rgdal)
mir_sp <- readOGR('/Users/heidi.k.hirsh/Desktop/remirdata/MIR Shp/MIR Site Polygons.shp')
class(mir_sp)
names(mir_sp)
mapview(mir_sp,zcol="MIRSite")


#sub in other shapefiles from Katey
mirContRest_sp <- readOGR('/Users/heidi.k.hirsh/Desktop/MIR Shapefiles/Control and Restoration Areas/MIR_CA_RMA.shp')      #"MIRSite"    "Aream2"     "Areakm2"    "Shape__Are" "Shape__Len"
# names(mirContRest_sp)
# head(mirContRest_sp)
mapview(mirContRest_sp,zcol="ReefSite")

#this one doesn't work for plotting. WHY?
mirMonitoring_sp <- readOGR('/Users/heidi.k.hirsh/Desktop/MIR Shapefiles/Monitoring Subplots/MonitoringSubplotCoordinates.shp')    #"Site"       "Monitoring" "Monitori_1" "OriginalPl" "Original_1" "ConfirmedI" "Confirme_1" "GlobalID"   "OSTag"      "ISTag"     
mapview(mirMonitoring_sp) #,zcol="Site")
mapview(mirMonitoring_sp,zcol="Monitori_1") #Restoration or Control


mirOverall_sp <- readOGR('/Users/heidi.k.hirsh/Desktop/MIR Shapefiles/Overall Site/MIR Site Polygons.shp')    # "MIRSite"    "Aream2"     "Areakm2"    "Shape__Are" "Shape__Len"
mapview(mirOverall_sp,zcol="MIRSite")

# mapview(mirOverall_sp,zcol="MIRSite")+
  mapview(mirContRest_sp)+
  mapview(mirSegments_sp,zcol="Site")


mirSegments_sp <- readOGR('/Users/heidi.k.hirsh/Desktop/MIR Shapefiles/Site Segments/MIR_Segments.shp')   #"SegmentId"  "Site"       "Site_Segme" "Shape__Are" "Shape__Len" "GlobalID"   "CreationDa" "Creator"    "EditDate"   "Editor"    
mapview(mirSegments_sp,zcol="Site")
# mapview(mirSegments_sp,zcol="SegmentId")



#plot with points
# pal <- magma(n = length(unique(mirSegments_sp$Site)), direction = -1)
mapview(MIR_sf,zcol="MPA_NAME") + 
  mapview(mirContRest_sp)+
  mapview(mirSegments_sp,zcol='Site')
  
  # mapview(MIR_sf,zcol="MPA_NAME",col.regions = pal) 





# 
# #Try just one site
# cheeca = subset(mirPoly_shp,MIRSite=="Cheeca")
# # plot(cheeca)
# # class(cheeca) #"sf"         "data.frame"
# # crs(cheeca) #WGS 84 it should already match the other data? 
# # st_crs(cheeca) #WGS 84 / Pseudo-Mercator
# st_crs(cheeca)==st_crs(MIR_sf) #but they don't
# mapview(cheeca) #doesn't work
# 
# #Transform Cheeca to match points
# 
# cheeca.t =  st_transform(cheeca,st_crs(MIR_sf))
# st_crs(cheeca.t)==st_crs(MIR_sf) #now this is true
# # plot(cheeca.t)
# mapview(cheeca.t) #still doesn't work
# 
# 
# plot(cheeca[1])
# plot(mirPoly_shp[1])

# test = st_transform(cheeca,crs='+proj=longlat +datum=WGS84')
# mapview(test)


##_____________MAPVIEW_____________##
## click on data point to get all the linked data
## data frame must be a spatial object (use MIR_sf created above)
# mapview(MIR_sf,zcol="MAX_DEPTH")
mapview(MIR_sf,zcol="MPA_NAME") #northernmost "unprotected" site looks like Horseshoe Reef
mapview(MIR_sf,zcol="ZONE_NAME") 

library(viridis)
pal <- magma(n = length(unique(MIR_sf$MPA_NAME)), direction = -1)
mapview(mir_sp,zcol="MIRSite") + 
          mapview(MIR_sf,zcol="MPA_NAME",col.regions = pal) 




##_____________LEAFLET_____________##
## https://rstudio.github.io/leaflet/
## good for zooming and exploring over satellite imagery
## need to use original MIR data frame

## define color palette
clrs = rainbow(10, start = 0, end = 0.8)
d=MIR$MAX_DEPTH #choose data variable for color scaling
pal = colorNumeric(palette = clrs, domain = min(d,na.rm=T):max(d,na.rm=T))
 
leaflet(data = MIR) %>%
  addProviderTiles('Esri.WorldImagery') %>%
  addCircleMarkers(
    lng = ~ LON_DEGREES,      #use you coordinate names here
    ~ LAT_DEGREES,
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
    title = "Depth"          #change based on your preferred data
  ) 


##_____________GGMAP_____________##
## (https://www.nceas.ucsb.edu/sites/default/files/2020-04/ggmapCheatsheet.pdf)
FLK_map=get_map(location=c(-80.99306,25.27732),zoom=8,maptype = "satellite")
# ggmap(FLK_map)         #if you want to just view the base map

ggmap(FLK_map)+
    geom_point(aes(x=LON_DEGREES,y=LAT_DEGREES),data=MIR,color=MIR$MAX_DEPTH)+
    # geom_polygon(data=mir_sp)+
    ylab('Latitude')+
    xlab('Longitude')+
    theme(legend.position="right")+    #I don't know why the legend is not showing up
    theme_bw()



