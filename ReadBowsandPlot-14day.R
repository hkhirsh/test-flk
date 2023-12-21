##10/9/2023 
#yearBows loop not running
#MY needs for bowMap plot (not sure where it comes from) - unable to run downstream lines

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
# library(rgdal)
library(raster)
library(rgeos)
library(patchwork)
library(mapview)
library(leaflet)
register_google(key = "AIzaSyB3mq0nJtcNWvQUNuqH_t-MLxEUmwDDmGk") #new api OCT2023

# FLKline <- st_read('/Users/heidi.k.hirsh/Desktop/rekmztoshp/Heidi_Keys_Lin_FeatureToLine.shp')
# mapview(FLKline)
# plot(FLKline)
# FLKlineSF=st_as_sf(FLKline, crs = st_crs(4326))
# mapview(FLKlineSF)
  
#read in all bow tie files for the year (identify them)
# bow_fl = list.files(path = paste0("/Users/heidi.k.hirsh/Desktop/bow_ties_shp_extended/bow_ties_shp_extended/14days"),full.names=T)  
bow_fl = list.files(path = paste0("/Users/heidi.k.hirsh/Desktop/bow_ties_shp_extended/bow_ties_shp_extended_tri/14days"),full.names=T)
bow_fl
length(bow_fl)


#pull out relevant info (date, #days)
# f_i=14 #something weird at file 13 for 2017
# f_i = 1
yearBows=NULL
bows=NULL
# for (f_i in 1:10) {   #loop through sampling sites for the given year
for (f_i in 1:length(bow_fl)) {   #loop through sampling sites for the given year

  splitBFile=str_split(string=bow_fl[f_i],"/")
  # splitBFile  
  
  bowGroup = splitBFile[[1]][8]
  # bowGroup
  
  #read shapefile (includes forward and backward bows for 1-7 day durations at the sampling site for a given date/time)
  bows = st_read(dsn = bow_fl[f_i],layer=bowGroup) #this is a DAY of forward and backward bows (not one bow)
  
  #pull visitID, date, time from bowGroup
  splitName=str_split(string=bowGroup,"_14days") #need to remove the "_14days" at the end
  # splitName
  
  visitID=splitName[[1]][1]
  # year=substr(splitName[[1]][4],1,4)  
  
  bows$year=substr(splitName[[1]][1],3,6)
  bows$visitID=visitID
  bows$duration=bows$n_days
  bows$bowID = paste0(bows$visitID,"_",bows$simu,"_",bows$n_days)
  
  yearBows =  rbind(yearBows,bows)
  # dim(yearBows)
  print(paste0(f_i,' of ',length(bow_fl)))
}

# st_write(yearBows,'/Users/heidi.k.hirsh/Desktop/FLK_data/Bows_forPlotting_14days.shp')
# st_write(yearBows,'/Users/heidi.k.hirsh/Desktop/FLK_data/Bows_forPlotting_14days_tri2.shp')


#read in CC dataframe with PAR.
# yearBows = st_read('/Users/heidi.k.hirsh/Desktop/FLK_data/Bows_forPlotting_concave.shp')
# yearBows = st_read('/Users/heidi.k.hirsh/Desktop/FLK_data/Bows_forPlotting_14days.shp')
yearBows = st_read('/Users/heidi.k.hirsh/Desktop/FLK_data/Bows_forPlotting_14days_tri.shp')


dim(yearBows)
CC = read.csv('/Users/heidi.k.hirsh/Desktop/FLK_data/CCflk_plusBathy.csv') #this is not latest

# View(CC)
dim(CC) # 1611   63

#subset of chemistry samples that go with bow ties:
CCbt = subset(CC,Year %in% c('2012','2013','2014','2015','2016','2017','2018','2019','2020','2021'))

names(CCbt)
CCbt$visitID_ch1 =  paste(CCbt$SiteID,CCbt$UTCDate_Time)
CCbt$visitID_ch2 =    gsub(" ", "_" , CCbt$visitID_ch1, perl=TRUE)
CCbt$visitID =  gsub("[: -]", "" , CCbt$visitID_ch2, perl=TRUE)
#this should now match visitID in yearBows

unique(yearBows$visitID)
unique(CCbt$visitID)

CCbows=NULL
CCbows = left_join(yearBows, CCbt, by="visitID")
names(CCbows)

# View(CCbows)
dim(CCbows) #34188    73


# st_write(CCbows,'/Users/heidi.k.hirsh/Desktop/14dBows_TRI+CC_dec1.shp',append=FALSE)
dim(yearBows) #34160     8
dim(CCbt) #1377   66


## start here just to plot


#limit CCbows to one MY (ex. 2018-)
# CCbows =  st_read('/Users/heidi.k.hirsh/Desktop/FLK_data/14dBows+CC_dec1.shp')
# CCbows =  st_read('/Users/heidi.k.hirsh/Desktop/FLK_data/14dBows_tri+CC_dec1.shp')
dim(CCbows)

unique(CCbows$year)
length(unique(CCbows$MY))
length(unique(CCbows$ESTDate))

MonthYear = unique(CCbows$MY)

# test= st_as_sf(CCbows, coords = c("Longitude","Latitude"), crs = st_crs(4326))
# test=yearBows[1, ]
# test2 <- st_transform(test, crs = 4326)
# ggmap(FLK_map)+geom_sf(test2, inherit.aes = FALSE)


# FLK_map=get_map(location=c(-80.99306,25.27732),zoom=7,maptype = "satellite")
FLK_map=get_map(location=c(-81.3,25),zoom=8,maptype = "satellite",scale="auto")
ggmap(FLK_map)+
  # scale_x_continuous(limits = c(80,82.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(24,26), expand = c(0, 0))




# FLK_map=get_map(location=c(mean(st_bbox(ploThis)[c(1,3)]),mean(st_bbox(ploThis)[c(2,4)])),zoom=8,maptype = "satellite")

# crs(CCbows)
CCbows_sf = st_as_sf(CCbows, coords = c("Longitude","Latitude"), crs = st_crs(4326))
# crs(CCbows_sf)
# 
# #try to get polygons to plot (I think it has to be a crs issue)
# b_i=4
# plot_month= MonthYear[b_i]
# ploThis = subset(CCbows_sf, MY == plot_month &simu=='backward' & Zone=='Inshore' & duration==14)
# # ploThis = subset(CCbows_sf, MY == plot_month)
# 
# mapview(ploThis)
# class(ploThis)
# crs(ploThis) #maybe crs is wrong for 14days?
# ploThis <- st_transform(ploThis, crs = 4326)
# 
# bowMap= ggmap(FLK_map)+
#   # geom_sf(mapping=aes(x=Longitude,y=Latitude),data=ploThis)+ #,simu=='backward' & Zone=='Inshore' & duration==7), inherit.aes = FALSE)
#   # geom_sf(data=ploThis) #+ #, inherit.aes = FALSE)+
#   geom_sf(fill="gray",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==14), inherit.aes = FALSE)+
#   # coord_sf(crs = st_crs(4326)) +
#   geom_point(aes(x=Longitude,y=Latitude),color="yellow",data=subset(ploThis,Zone=='Inshore')) #+
# # geom_point(aes(x=Longitude,y=Latitude),color="green",data=subset(ploThis,Zone=='Mid channel'))+
# # geom_point(aes(x=Longitude,y=Latitude),color="cyan",data=subset(ploThis,Zone=='Offshore'))+
# # geom_point(aes(x=Longitude,y=Latitude),color="magenta",data=subset(ploThis,Zone=='Oceanic'))
# 
# bowMap


bowMap=NULL
bowMap2=NULL
bowMap3=NULL
bowMap4=NULL

b_i=1
#I want to loop through each month_year and plot the back bows for the reef sites and the forward bows for oceanic
for (b_i in 1:length(MonthYear)) {

  
plot_month= MonthYear[b_i]
ploThis = subset(CCbows_sf, MY == plot_month)
ploThis <- st_transform(ploThis, crs = 4326)
class(ploThis)

# unique(ploThis$simu)
# unique(ploThis$Zone) #only mid and oceanic for some
# unique(ploThis$duration)

# xlim <- c(79.4,82.5)
# xlim <- c(80,83)
# ylim <- c(24,26)

bowMap= ggmap(FLK_map)+
  
  geom_sf(fill="pink",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==14), inherit.aes = FALSE)+
  geom_sf(fill="tomato4",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==13), inherit.aes = FALSE)+
  geom_sf(fill="tomato3",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==12), inherit.aes = FALSE)+
  geom_sf(fill="tomato2",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==11), inherit.aes = FALSE)+
  geom_sf(fill="tomato1",alpha=.33,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==10), inherit.aes = FALSE)+
  geom_sf(fill="tomato",alpha=.4,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==9), inherit.aes = FALSE)+
  geom_sf(fill="red",alpha=.5,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==8), inherit.aes = FALSE)+

  geom_sf(fill="gray",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==7), inherit.aes = FALSE)+
  geom_sf(fill="lightskyblue4",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==6), inherit.aes = FALSE)+
  geom_sf(fill="lightskyblue3",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==5), inherit.aes = FALSE)+
  geom_sf(fill="lightskyblue2",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==4), inherit.aes = FALSE)+
  geom_sf(fill="lightskyblue1",alpha=.33,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==3), inherit.aes = FALSE)+
  geom_sf(fill="lightskyblue",alpha=.4,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==2), inherit.aes = FALSE)+
  geom_sf(fill="blue",alpha=.5,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==1), inherit.aes = FALSE)+

  geom_point(size=1,aes(x=Longitude,y=Latitude),color="yellow",data=subset(ploThis,Zone=='Inshore'))+
  geom_point(size=1,aes(x=Longitude,y=Latitude),color="green",data=subset(ploThis,Zone=='Mid channel'))+
  geom_point(size=1,aes(x=Longitude,y=Latitude),color="cyan",data=subset(ploThis,Zone=='Offshore'))+
  geom_point(size=1,aes(x=Longitude,y=Latitude),color="magenta",data=subset(ploThis,Zone=='Oceanic'))+
  
  # geom_sf(colour='white',lwd=1,data=FLKline,inherit.aes = FALSE)+

  scale_y_continuous(limits = c(24.15,26.15), expand = c(0, 0))+
  ggtitle(plot_month)+
  ylab('Latitude')+
  xlab('Longitude')+
  theme_bw(base_size = 20)
  # theme(text=element_text(size=18))
  # theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

bowMap
# ggsave(file="/Users/heidi.k.hirsh/Desktop/line.png",bowMap, width = 10, height = 8, dpi = 300, units = "in")

# 
# # test = bowMap +
# #   coord_fixed(ratio = 1/cos(mean(ylim) * pi / 180) *0.8)
# # test
# 
# # ggsave(file=paste0("/Users/heidi.k.hirsh/Desktop/test_14days/",plot_month,"_concave_bowties_oceanicBACK_tri.png"),bowMap) 
# name=paste0("/Users/heidi.k.hirsh/Desktop/14days_cropped/midchannel/original/",plot_month,"_concave_bowties_midBACK_tri_dec11.png")
# # crop=paste0("/Users/heidi.k.hirsh/Desktop/14days_cropped/midchannel/cropped/",plot_month,"_concave_bowties_midBACK_triCrop_dec11.png")
# 
# # name=paste0("/Users/heidi.k.hirsh/Desktop/14days_cropped/offshore/original/",plot_month,"_concave_bowties_offshoreBACK_tri_dec11.png")
# # crop=paste0("/Users/heidi.k.hirsh/Desktop/14days_cropped/offshore/cropped/",plot_month,"_concave_bowties_offshoreBACK_triCrop_dec11.png")
# 
name=paste0("/Users/heidi.k.hirsh/Desktop/14days_cropped/inshore/original/",plot_month,"_concave_bowties_inshoreBACK_tri_dec16T.png")
crop=paste0("/Users/heidi.k.hirsh/Desktop/14days_cropped/inshore/cropped/",plot_month,"_concave_bowties_inshoreBACK_triCrop_dec16T.png")
# 
# # name=paste0("/Users/heidi.k.hirsh/Desktop/14days_cropped/oceanic/original/",plot_month,"_concave_bowties_oceanicBACK_tri_dec11.png")
# # crop=paste0("/Users/heidi.k.hirsh/Desktop/14days_cropped/oceanic/cropped/",plot_month,"_concave_bowties_oceanicBACK_triCrop_dec11.png")
# 
ggsave(file=name,bowMap, width = 10, height = 8, dpi = 300, units = "in")
# new = image_border(image_trim(image_read(name)),'white',"30x30")
# image_write(new,crop)


# offshore
# bowMap2= ggmap(FLK_map)+
# 
#   geom_sf(fill="pink",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Offshore' & duration==14), inherit.aes = FALSE)+
#   geom_sf(fill="tomato4",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Offshore' & duration==13), inherit.aes = FALSE)+
#   geom_sf(fill="tomato3",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Offshore' & duration==12), inherit.aes = FALSE)+
#   geom_sf(fill="tomato2",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Offshore' & duration==11), inherit.aes = FALSE)+
#   geom_sf(fill="tomato1",alpha=.33,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Offshore' & duration==10), inherit.aes = FALSE)+
#   geom_sf(fill="tomato",alpha=.4,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Offshore' & duration==9), inherit.aes = FALSE)+
#   geom_sf(fill="red",alpha=.5,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Offshore' & duration==8), inherit.aes = FALSE)+
# 
#   geom_sf(fill="gray",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Offshore' & duration==7), inherit.aes = FALSE)+
#   geom_sf(fill="lightskyblue4",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Offshore' & duration==6), inherit.aes = FALSE)+
#   geom_sf(fill="lightskyblue3",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Offshore' & duration==5), inherit.aes = FALSE)+
#   geom_sf(fill="lightskyblue2",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Offshore' & duration==4), inherit.aes = FALSE)+
#   geom_sf(fill="lightskyblue1",alpha=.33,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Offshore' & duration==3), inherit.aes = FALSE)+
#   geom_sf(fill="lightskyblue",alpha=.4,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Offshore' & duration==2), inherit.aes = FALSE)+
#   geom_sf(fill="blue",alpha=.5,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Offshore' & duration==1), inherit.aes = FALSE)+
# 
#   geom_point(size=1,aes(x=Longitude,y=Latitude),color="yellow",data=subset(ploThis,Zone=='Inshore'))+
#   geom_point(size=1,aes(x=Longitude,y=Latitude),color="green",data=subset(ploThis,Zone=='Mid channel'))+
#   geom_point(size=1,aes(x=Longitude,y=Latitude),color="cyan",data=subset(ploThis,Zone=='Offshore'))+
#   geom_point(size=1,aes(x=Longitude,y=Latitude),color="magenta",data=subset(ploThis,Zone=='Oceanic'))+
# 
#   scale_y_continuous(limits = c(24,26), expand = c(0, 0))+
#   ggtitle(plot_month)+
#   ylab('Latitude')+
#   xlab('Longitude')+
#   theme_bw(base_size = 20)
# 
# name2=paste0("/Users/heidi.k.hirsh/Desktop/14days_cropped/offshore/original/",plot_month,"_concave_bowties_offshoreBACK_tri_dec11.png")
# # crop=paste0("/Users/heidi.k.hirsh/Desktop/14days_cropped/offshore/cropped/",plot_month,"_concave_bowties_offshoreBACK_triCrop_dec11.png")
# ggsave(file=name2,bowMap2, width = 10, height = 8, dpi = 300, units = "in")

# #oceanic
# bowMap3= ggmap(FLK_map)+
# 
#   geom_sf(fill="pink",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Oceanic' & duration==14), inherit.aes = FALSE)+
#   geom_sf(fill="tomato4",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Oceanic' & duration==13), inherit.aes = FALSE)+
#   geom_sf(fill="tomato3",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Oceanic' & duration==12), inherit.aes = FALSE)+
#   geom_sf(fill="tomato2",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Oceanic' & duration==11), inherit.aes = FALSE)+
#   geom_sf(fill="tomato1",alpha=.33,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Oceanic' & duration==10), inherit.aes = FALSE)+
#   geom_sf(fill="tomato",alpha=.4,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Oceanic' & duration==9), inherit.aes = FALSE)+
#   geom_sf(fill="red",alpha=.5,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Oceanic' & duration==8), inherit.aes = FALSE)+
# 
#   geom_sf(fill="gray",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Oceanic' & duration==7), inherit.aes = FALSE)+
#   geom_sf(fill="lightskyblue4",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Oceanic' & duration==6), inherit.aes = FALSE)+
#   geom_sf(fill="lightskyblue3",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Oceanic' & duration==5), inherit.aes = FALSE)+
#   geom_sf(fill="lightskyblue2",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Oceanic' & duration==4), inherit.aes = FALSE)+
#   geom_sf(fill="lightskyblue1",alpha=.33,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Oceanic' & duration==3), inherit.aes = FALSE)+
#   geom_sf(fill="lightskyblue",alpha=.4,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Oceanic' & duration==2), inherit.aes = FALSE)+
#   geom_sf(fill="blue",alpha=.5,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Oceanic' & duration==1), inherit.aes = FALSE)+
# 
#   geom_point(size=1,aes(x=Longitude,y=Latitude),color="yellow",data=subset(ploThis,Zone=='Inshore'))+
#   geom_point(size=1,aes(x=Longitude,y=Latitude),color="green",data=subset(ploThis,Zone=='Mid channel'))+
#   geom_point(size=1,aes(x=Longitude,y=Latitude),color="cyan",data=subset(ploThis,Zone=='Offshore'))+
#   geom_point(size=1,aes(x=Longitude,y=Latitude),color="magenta",data=subset(ploThis,Zone=='Oceanic'))+
# 
#   scale_y_continuous(limits = c(24,26), expand = c(0, 0))+
#   ggtitle(plot_month)+
#   ylab('Latitude')+
#   xlab('Longitude')+
#   theme_bw(base_size = 20)
# 
# name3=paste0("/Users/heidi.k.hirsh/Desktop/14days_cropped/oceanic/original/",plot_month,"_concave_bowties_oceanicBACK_tri_dec12.png")
# # crop=paste0("/Users/heidi.k.hirsh/Desktop/14days_cropped/oceanic/cropped/",plot_month,"_concave_bowties_oceanicBACK_triCrop_dec11.png")
# ggsave(file=name3,bowMap3, width = 10, height = 8, dpi = 300, units = "in")
# 
# 
# 
# #oceanic forward
# bowMap4= ggmap(FLK_map)+
# 
#   geom_sf(fill="pink",alpha=.25,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Offshore' & duration==14), inherit.aes = FALSE)+
#   geom_sf(fill="tomato4",alpha=.25,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Offshore' & duration==13), inherit.aes = FALSE)+
#   geom_sf(fill="tomato3",alpha=.25,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Offshore' & duration==12), inherit.aes = FALSE)+
#   geom_sf(fill="tomato2",alpha=.25,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Offshore' & duration==11), inherit.aes = FALSE)+
#   geom_sf(fill="tomato1",alpha=.33,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Offshore' & duration==10), inherit.aes = FALSE)+
#   geom_sf(fill="tomato",alpha=.4,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Offshore' & duration==9), inherit.aes = FALSE)+
#   geom_sf(fill="red",alpha=.5,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Offshore' & duration==8), inherit.aes = FALSE)+
# 
#   geom_sf(fill="gray",alpha=.25,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Offshore' & duration==7), inherit.aes = FALSE)+
#   geom_sf(fill="lightskyblue4",alpha=.25,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Offshore' & duration==6), inherit.aes = FALSE)+
#   geom_sf(fill="lightskyblue3",alpha=.25,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Offshore' & duration==5), inherit.aes = FALSE)+
#   geom_sf(fill="lightskyblue2",alpha=.25,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Offshore' & duration==4), inherit.aes = FALSE)+
#   geom_sf(fill="lightskyblue1",alpha=.33,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Offshore' & duration==3), inherit.aes = FALSE)+
#   geom_sf(fill="lightskyblue",alpha=.4,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Offshore' & duration==2), inherit.aes = FALSE)+
#   geom_sf(fill="blue",alpha=.5,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Offshore' & duration==1), inherit.aes = FALSE)+
# 
#   geom_point(size=1,aes(x=Longitude,y=Latitude),color="yellow",data=subset(ploThis,Zone=='Inshore'))+
#   geom_point(size=1,aes(x=Longitude,y=Latitude),color="green",data=subset(ploThis,Zone=='Mid channel'))+
#   geom_point(size=1,aes(x=Longitude,y=Latitude),color="cyan",data=subset(ploThis,Zone=='Offshore'))+
#   geom_point(size=1,aes(x=Longitude,y=Latitude),color="magenta",data=subset(ploThis,Zone=='Oceanic'))+
# 
#   scale_y_continuous(limits = c(24,26), expand = c(0, 0))+
#   ggtitle(plot_month)+
#   ylab('Latitude')+
#   xlab('Longitude')+
#   theme_bw(base_size = 20)
# bowMap4

# name4=paste0("/Users/heidi.k.hirsh/Desktop/14days_cropped/ocean_forward/original/",plot_month,"_concave_bowties_oceanicFORWARD_tri_dec12.png")
# name4=paste0("/Users/heidi.k.hirsh/Desktop/14days_cropped/offshore_forward/original/",plot_month,"_concave_bowties_offshoreFORWARD_tri_dec12.png")
# 
# ggsave(file=name4,bowMap4, width = 10, height = 8, dpi = 300, units = "in")


}



# bowMap= ggmap(FLK_map)+
# 
#   # coord_sf(xlim = xlim, ylim = ylim)+
# 
# geom_sf(fill="pink",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==14), inherit.aes = FALSE)+
# geom_sf(fill="tomato4",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==13), inherit.aes = FALSE)+
# geom_sf(fill="tomato3",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==12), inherit.aes = FALSE)+
# geom_sf(fill="tomato2",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==11), inherit.aes = FALSE)+
# geom_sf(fill="tomato1",alpha=.33,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==10), inherit.aes = FALSE)+
# geom_sf(fill="tomato",alpha=.4,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==9), inherit.aes = FALSE)+
# geom_sf(fill="red",alpha=.5,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==8), inherit.aes = FALSE)+
# 
# geom_sf(fill="gray",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==7), inherit.aes = FALSE)+
# geom_sf(fill="lightskyblue4",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==6), inherit.aes = FALSE)+
# geom_sf(fill="lightskyblue3",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==5), inherit.aes = FALSE)+
# geom_sf(fill="lightskyblue2",alpha=.25,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==4), inherit.aes = FALSE)+
# geom_sf(fill="lightskyblue1",alpha=.33,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==3), inherit.aes = FALSE)+
# geom_sf(fill="lightskyblue",alpha=.4,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==2), inherit.aes = FALSE)+
# geom_sf(fill="blue",alpha=.5,lwd=0,data=subset(ploThis,simu=='backward' & Zone=='Inshore' & duration==1), inherit.aes = FALSE)+
# 
#   # geom_sf(fill="gray",alpha=.25,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Inshore' & duration==7), inherit.aes = FALSE)+
#   # geom_sf(fill="tomato4",alpha=.25,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Inshore' & duration==6), inherit.aes = FALSE)+
#   # geom_sf(fill="tomato3",alpha=.25,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Inshore' & duration==5), inherit.aes = FALSE)+
#   # geom_sf(fill="tomato2",alpha=.25,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Inshore' & duration==4), inherit.aes = FALSE)+
#   # geom_sf(fill="tomato1",alpha=.33,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Inshore' & duration==3), inherit.aes = FALSE)+
#   # geom_sf(fill="tomato",alpha=.4,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Inshore' & duration==2), inherit.aes = FALSE)+
#   # geom_sf(fill="red",alpha=.5,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Inshore' & duration==1), inherit.aes = FALSE)+
# 
#   geom_point(aes(x=Longitude,y=Latitude),color="yellow",data=subset(ploThis,Zone=='Inshore'))+
#   geom_point(aes(x=Longitude,y=Latitude),color="green",data=subset(ploThis,Zone=='Mid channel'))+
#   geom_point(aes(x=Longitude,y=Latitude),color="cyan",data=subset(ploThis,Zone=='Offshore'))+
#   geom_point(aes(x=Longitude,y=Latitude),color="magenta",data=subset(ploThis,Zone=='Oceanic'))+
# 
#   # geom_point(aes(x=Longitude,y=Latitude),color="yellow",data=ploThis)+
# 
#   ggtitle(plot_month)+
#   ylab('Latitude')+
#   xlab('Longitude')+
#   theme_bw()
# 
# bowMap
# # ggsave(file=paste0("/Users/heidi.k.hirsh/Desktop/concaveMaps/",plot_month,"_concave_bowties_inshoreBACK.png"),bowMap)
# ggsave(file=paste0("/Users/heidi.k.hirsh/Desktop/test_14days/",plot_month,"_concave_bowties_inshoreBACK_tri.png"),bowMap)


# 
# bowMap= ggmap(FLK_map)+
#   
#   # coord_sf(xlim = xlim, ylim = ylim)+
#   
#   geom_sf(fill="pink",alpha=.25,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Inshore' & duration==14), inherit.aes = FALSE)+
#   geom_sf(fill="tomato4",alpha=.25,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Inshore' & duration==13), inherit.aes = FALSE)+
#   geom_sf(fill="tomato3",alpha=.25,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Inshore' & duration==12), inherit.aes = FALSE)+
#   geom_sf(fill="tomato2",alpha=.25,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Inshore' & duration==11), inherit.aes = FALSE)+
#   geom_sf(fill="tomato1",alpha=.33,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Inshore' & duration==10), inherit.aes = FALSE)+
#   geom_sf(fill="tomato",alpha=.4,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Inshore' & duration==9), inherit.aes = FALSE)+
#   geom_sf(fill="red",alpha=.5,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Inshore' & duration==8), inherit.aes = FALSE)+
#   
#   geom_sf(fill="gray",alpha=.25,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Inshore' & duration==7), inherit.aes = FALSE)+
#   geom_sf(fill="lightskyblue4",alpha=.25,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Inshore' & duration==6), inherit.aes = FALSE)+
#   geom_sf(fill="lightskyblue3",alpha=.25,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Inshore' & duration==5), inherit.aes = FALSE)+
#   geom_sf(fill="lightskyblue2",alpha=.25,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Inshore' & duration==4), inherit.aes = FALSE)+
#   geom_sf(fill="lightskyblue1",alpha=.33,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Inshore' & duration==3), inherit.aes = FALSE)+
#   geom_sf(fill="lightskyblue",alpha=.4,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Inshore' & duration==2), inherit.aes = FALSE)+
#   geom_sf(fill="blue",alpha=.5,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Inshore' & duration==1), inherit.aes = FALSE)+
#   
#   # geom_sf(fill="gray",alpha=.25,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Inshore' & duration==7), inherit.aes = FALSE)+
#   # geom_sf(fill="tomato4",alpha=.25,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Inshore' & duration==6), inherit.aes = FALSE)+
#   # geom_sf(fill="tomato3",alpha=.25,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Inshore' & duration==5), inherit.aes = FALSE)+
#   # geom_sf(fill="tomato2",alpha=.25,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Inshore' & duration==4), inherit.aes = FALSE)+
#   # geom_sf(fill="tomato1",alpha=.33,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Inshore' & duration==3), inherit.aes = FALSE)+
#   # geom_sf(fill="tomato",alpha=.4,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Inshore' & duration==2), inherit.aes = FALSE)+
#   # geom_sf(fill="red",alpha=.5,lwd=0,data=subset(ploThis,simu=='forward' & Zone=='Inshore' & duration==1), inherit.aes = FALSE)+
#   
#   geom_point(aes(x=Longitude,y=Latitude),color="yellow",data=subset(ploThis,Zone=='Inshore'))+
#   geom_point(aes(x=Longitude,y=Latitude),color="green",data=subset(ploThis,Zone=='Mid channel'))+
#   geom_point(aes(x=Longitude,y=Latitude),color="cyan",data=subset(ploThis,Zone=='Offshore'))+
#   geom_point(aes(x=Longitude,y=Latitude),color="magenta",data=subset(ploThis,Zone=='Oceanic'))+
#   
#   # geom_point(aes(x=Longitude,y=Latitude),color="yellow",data=ploThis)+
#   
#   ggtitle(plot_month)+
#   ylab('Latitude')+
#   xlab('Longitude')+
#   theme_bw()
# 
# bowMap
# ggsave(file=paste0("/Users/heidi.k.hirsh/Desktop/concaveMaps/",plot_month,"_concave_bowties_inshoreBACK_tri.png"),bowMap)
# # ggsave(file=paste0("/Users/heidi.k.hirsh/Desktop/test_14days/",plot_month,"_concave_bowties_inshoreFORWARD2.png"),bowMap)
# 



# mapview(ploThis)

ploThis_t = st_transform(ploThis,crs='+proj=longlat +datum=WGS84')

#try leaflet
leaflet(data=ploThis_t) %>% 
  addProviderTiles('Esri.WorldImagery') %>%
  addPolygons(data=subset(ploThis_t,simu=='backward' & Zone=='Inshore' & duration==1),fillOpacity=.01) %>%
  addPolygons(data=subset(ploThis_t,simu=='backward' & Zone=='Inshore' & duration==7),color='cyan',fillOpacity=.01)  %>%
  addCircleMarkers(lng = ~ Longitude,~ Latitude,color='white')


# addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
#             opacity = 1.0, fillOpacity = 0.5,
#             fillColor = ~colorQuantile("YlOrRd", ALAND)(ALAND),
#             highlightOptions = highlightOptions(color = "white", weight = 2,
#                                                 bringToFront = TRUE))


       


#benthic habitat
coral_sf =  st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/Final_UnifiedReefMap_Version2.0/FWC_UnifiedFloridaReefMap_v2.0.gdb", layer="UnifiedReefMap")

bowTest=subset(ploThis_t,simu=='backward' & Zone=='Inshore' & duration==1)

#Tranform coral_sf to match Bbow CRS:
coral_sf.t = st_transform(coral_sf,st_crs(bowTest))
coral_sf.tv = coral_sf.t[which(st_is_valid(coral_sf.t)),]
over_bow = st_intersection(coral_sf.tv,bowTest)

mapview(over_bow,zcol="ClassLv4")

mapview(list(over_bow,bowTest),col.regions=list(zcol="ClassLv4","blue"),col=list(zcol="ClassLv4","blue"))





#try leaflet plot with 14 days. 


plot_month= MonthYear[b_i]
ploThis = subset(CCbows_sf, MY == plot_month)
ploThis <- st_transform(ploThis, crs = 4326)
class(ploThis)

mapview(plotThis)


clrs = rainbow(14, start = 0, end = 0.8)
clrs_rev <- rev(rainbow(7, start = 0, end = 0.8))
d=ploThis$n_days
pal = colorNumeric(palette = clrs_rev, domain = min(d):max(d))


leaflet(data=ploThis) %>% 
  addProviderTiles('Esri.WorldImagery') %>%
  addPolygons(data=subset(ploThis,simu=='backward' & Zone=='Inshore' & n_days==2),fillOpacity=0,weight=1,color = ~pal(d))  %>%
  # addPolygons(data=subset(ploThis_t,simu=='forward'),fillOpacity=0.1,weight=1,color = ~pal(d))  %>%
  addCircleMarkers(lng = ~ dec.lon,~ dec.lat,radius=1,color='white')


