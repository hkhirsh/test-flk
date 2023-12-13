#trial code (for 2016) to link bows to CC data by date/time
## this is how I can potentially pair with nutrients right? 
# match by latitude and longitude? 

## 10/9/2023 loops run, but the plots only plot one line (something wrong with spatial formatting?)

rm(list=ls())

library(sf)
library(ggplot2)
library(tidyverse)
library(ggmap)
library(ggrepel)
library(rstudioapi)
library(stringr)
register_google(key = "AIzaSyB3mq0nJtcNWvQUNuqH_t-MLxEUmwDDmGk") #api updated OCT23

#read in all files (identify them)
bow_fl = list.files(path = paste0("/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_2016"),pattern='.shp',full.names=T)  
bow_fl
ndates=  length(bow_fl)/7
ndates

#pull out relevant info (date, #days)
# f_i=1
yearBows = NULL
for (f_i in 1:length(bow_fl)) {

splitName=str_split(string=bow_fl[f_i],c("\\W"))
splitName
bowGroup = splitName[[1]][9] 

#read shapefile (save for each unique date and duration)
bows = st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_2016",layer=bowGroup)

splitFile=str_split(string=bow_fl[f_i],"_")
# splitFile
year=substr(splitFile[[1]][6],1,4)  
month=substr(splitFile[[1]][6],5,6)  
day=substr(splitFile[[1]][6],7,8)  
duration= substr(splitFile[[1]][7],1,1)  

bows$date= paste0(year,month,day)
bows$year=year
bows$month=month
bows$day=day
bows$duration= substr(splitFile[[1]][6],1,1)  
bows$name = paste0('bow_',year,month,day,'_',duration,'days')

#concatenate x and y from
#import site name 
#then table site by date by direction by duration (should be same forward and backward)
# bows$x_y = past

yearBows =  rbind(yearBows,bows)
dim(yearBows)
}



# FLK_map=get_map(location=c(mean(st_bbox(subBow)[c(1,3)]),mean(st_bbox(subBow)[c(2,4)])),zoom=8,maptype = "satellite")
# FLK_map=get_map(location=c(mean(st_bbox(yearBows)[c(1,3)]),mean(st_bbox(yearBows)[c(2,4)])),zoom=8,maptype = "satellite")
FLK_map=get_map(location=c(-81,25.7),zoom=8,maptype = "satellite")

#build lookup table to match shared sites (and info) with specific coordinates for each day
CCflk <- read.csv('/Users/heidi.k.hirsh/Desktop/FLK_data/FLK_filtered_ve4.csv')
#specific coordinates are Latitude and Longitude and shared coordinates are dec.lat and dec.lon

#table we gave Belgians was UTC time!
#this is the data the Belgians have: 
CCsimple = read.csv('/Users/heidi.k.hirsh/Desktop/FLK_data/FLK_SamplePoints.csv')
CCsimple$Latitude4=format(round(CCsimple$Latitude,digits=4))
CCsimple$Longitude4=format(round(CCsimple$Longitude,digits=4))
#should subset to only 2016
#try left joining yearBows with CCsimple (should be same coordinates for sure)
test=yearBows
test$x_from4 = format(round(test$x_from,digits=4))
test$y_from4 = format(round(test$y_from,digits=4))

CCsimpTestJ=left_join(test,CCsimple,by=c('x_from4'='Longitude4','y_from4'='Latitude4'))
sum(is.na(CCsimpTestJ$Latitude))
# View(CCsimpTestJ)
dim(CCsimpTestJ) #3402   17
dim(yearBows) #3150   10

#run subBow to get FLK_map for this plot
ggmap(FLK_map)+
  geom_point(data=CCflk,aes(x=dec.lon,y=dec.lat),color="cyan",size=2)+
  geom_point(data=CCflk,aes(x=Longitude,y=Latitude),color="magenta",size=.5)+
  # ylim(24.3,26)+
  theme_bw()

#simplified CCflk look up table to join to yearBows
CCflkLU = subset(CCflk,select =c(Latitude,Longitude,dec.lat,dec.lon,UTCDate_Time,datetime,SiteID,Location,Zone,Sub_region,CTDID))
# CCflkLU$LatitudeCopy = CCflk$Latitude
# CCflkLU$LongitudeCopy = CCflk$Longitude
CCflkLU$y_from= CCflk$Latitude
CCflkLU$x_from= CCflk$Longitude

#match column names yearBows$x_from is CCflkLU$Longitude and yearBows$y_from is CCflkLU$Latitude
# test=left_join(yearBows,CCflkLU,by=c('x_from'='Longitude','y_from'='Latitude'))
JyearBows=left_join(yearBows,CCflkLU,by=c('x_from','y_from'))
sum(is.na(JyearBows$Latitude))
dim(JyearBows) 

ggmap(FLK_map)+
  # geom_point(data=JyearBows,aes(x=dec.lon,y=dec.lat),color="cyan",size=2)+
  geom_point(data=JyearBows,aes(x=x_from,y=y_from),color="magenta",size=.5)+
  geom_point(data=JyearBows,aes(x=Longitude,y=Latitude),color="yellow",size=.5)+
  ylim(24.3,26)+
  theme_bw()

# plot(JyearBows$LatitudeCopy,JyearBows$y_from)



#subset for one date 
length(unique(yearBows$date))
date2016 =unique(yearBows$date)
# "20160104" "20160105" "20160314" "20160315" "20160509" "20160510" "20160725" "20160726" "20160919" "20160920" "20160923" "20161114" "20161115" "20161118"

# subBow1 = yearBows[which(yearBows$date=='20160104'),] #good
# dim(subBow1) # 210  10
# subBow2 = yearBows[which(yearBows$date=='20160105'),] #good
# dim(subBow2) # 322  10
#so the points are fine when I read them in separately but not together (both dates)

# subBow = yearBows[which(yearBows$date==c('20160104','20160105')),]  # dim= 266  10 #not enough data!
# subBow = yearBows[which(yearBows$date==20160104|20160105),] #this just spits out yearBows
subBow = yearBows[yearBows$date %in% c('20160104','20160105'),] #select one set of sample bows

dim(subBow) # 532 10   #no longer exists
table(subBow$simu)


ggmap(FLK_map)+
  geom_sf(fill="blue",alpha=.1,data=subset(subBow,duration==7 & simu=='backward'), inherit.aes = FALSE)+
  geom_sf(fill="lightskyblue4",alpha=.25,data=subset(subBow,duration==6 & simu=='backward'), inherit.aes = FALSE)+
  geom_sf(fill="lightskyblue3",alpha=.25,data=subset(subBow,duration==5 & simu=='backward'), inherit.aes = FALSE)+
  geom_sf(fill="lightskyblue2",alpha=.25,data=subset(subBow,duration==4 & simu=='backward'), inherit.aes = FALSE)+
  geom_sf(fill="lightskyblue1",alpha=.33,data=subset(subBow,duration==3 & simu=='backward'), inherit.aes = FALSE)+
  geom_sf(fill="lightskyblue",alpha=.4,data=subset(subBow,duration==2 & simu=='backward'), inherit.aes = FALSE)+
  geom_sf(fill="blue",alpha=.5,data=subset(subBow,duration==1 & simu=='backward'), inherit.aes = FALSE)+

  # geom_sf(fill="red",alpha=.1,data=subset(subBow,duration==7 & simu=='forward'), inherit.aes = FALSE)+
  # geom_sf(fill="tomato4",alpha=.25,data=subset(subBow,duration==6 & simu=='forward'), inherit.aes = FALSE)+
  # geom_sf(fill="tomato3",alpha=.25,data=subset(subBow,duration==5 & simu=='forward'), inherit.aes = FALSE)+
  # geom_sf(fill="tomato2",alpha=.25,data=subset(subBow,duration==4 & simu=='forward'), inherit.aes = FALSE)+
  # geom_sf(fill="tomato1",alpha=.33,data=subset(subBow,duration==3 & simu=='forward'), inherit.aes = FALSE)+
  # geom_sf(fill="tomato",alpha=.4,data=subset(subBow,duration==2 & simu=='forward'), inherit.aes = FALSE)+
  geom_sf(fill="red",alpha=.5,data=subset(subBow,duration==1 & simu=='forward'), inherit.aes = FALSE)+
  geom_point(aes(x=x_from,y=y_from),color="yellow",size=1.5,data=subset(subBow,duration==1 & simu=='forward'))+
  geom_point(aes(x=x_from,y=y_from),color="cyan",size=.5,data=subset(subBow,duration==1 & simu=='backward'))+
  # geom_text_repel(aes(x=x_from,y=y_from,label=1:19),color="black",data=subset(subBow,duration==1 & simu=="backward"))+
  scale_x_continuous(limits=st_bbox(subBow)[c(1,3)])+
  scale_y_continuous(limits=st_bbox(subBow)[c(2,4)])+
  theme_bw()


ggmap(FLK_map)+
  geom_sf(fill="blue",alpha=.1,data=subset(subBow,duration==7 & simu=='backward'), inherit.aes = FALSE)+
  geom_sf(fill="lightskyblue4",alpha=.25,data=subset(subBow,duration==6 & simu=='backward'), inherit.aes = FALSE)+
  geom_sf(fill="lightskyblue3",alpha=.25,data=subset(subBow,duration==5 & simu=='backward'), inherit.aes = FALSE)+
  geom_sf(fill="lightskyblue2",alpha=.25,data=subset(subBow,duration==4 & simu=='backward'), inherit.aes = FALSE)+
  geom_sf(fill="lightskyblue1",alpha=.33,data=subset(subBow,duration==3 & simu=='backward'), inherit.aes = FALSE)+
  # geom_sf(fill="lightskyblue",alpha=.4,data=subset(subBow,duration==2 & simu=='backward'), inherit.aes = FALSE)+ #this is the only line that plots???
  geom_sf(fill="blue",alpha=.5,data=subset(subBow,duration==1 & simu=='backward'), inherit.aes = FALSE)+
  geom_sf(fill="red",alpha=.5,data=subset(subBow,duration==1 & simu=='forward'), inherit.aes = FALSE)+
  geom_point(aes(x=x_from,y=y_from),color="yellow",size=1.5,data=subset(subBow,duration==1 & simu=='forward'))+
  geom_point(aes(x=x_from,y=y_from),color="cyan",size=.5,data=subset(subBow,duration==1 & simu=='backward'))+

  scale_x_continuous(limits=st_bbox(subBow)[c(1,3)])+
  scale_y_continuous(limits=st_bbox(subBow)[c(2,4)])+
  theme_bw()



# nsites= dim(subBow)[1]/2/7
# nsites
# unique(JyearBows$Zone) #"Inshore"     NA            "Offshore"    "Mid channel" "Oceanic"    
# JyearBows$date=as.numeric(JyearBows$date)
# class(JyearBows$date)
# date2016 =unique(JyearBows$date)
# dd=date2016[1]
# dd

plotBows= JyearBows[JyearBows$date %in% c('20160104','20160105'),]
# plotBows= JyearBows

ggmap(FLK_map)+
  # geom_sf(fill="blue",alpha=.1,data=subset(plotBows,duration==7 & simu=='backward' & Zone=='Inshore' ), inherit.aes = FALSE)+
  # geom_sf(fill="lightskyblue4",alpha=.25,data=subset(plotBows,duration==6 & simu=='backward' & Zone=='Inshore' ), inherit.aes = FALSE)+
  # geom_sf(fill="lightskyblue3",alpha=.25,data=subset(plotBows,duration==5 & simu=='backward' & Zone=='Inshore' ), inherit.aes = FALSE)+
  geom_sf(fill="lightskyblue2",alpha=.25,data=subset(plotBows,duration==4 & simu=='backward' & Zone=='Inshore' ), inherit.aes = FALSE)+
  geom_sf(fill="lightskyblue1",alpha=.33,data=subset(plotBows,duration==3 & simu=='backward' & Zone=='Inshore' ), inherit.aes = FALSE)+
  geom_sf(fill="lightskyblue",alpha=.4,data=subset(plotBows,duration==2 & simu=='backward' & Zone=='Inshore' ), inherit.aes = FALSE)+
  geom_sf(fill="blue",alpha=.5,data=subset(plotBows,duration==1 & simu=='backward' & Zone=='Inshore' ), inherit.aes = FALSE)+
  
  # geom_sf(fill="red",alpha=.1,data=subset(plotBows,duration==7 & simu=='forward' & Zone=='oceanic' ), inherit.aes = FALSE)+
  # geom_sf(fill="tomato4",alpha=.25,data=subset(plotBows,duration==6 & simu=='forward' & Zone=='oceanic' ), inherit.aes = FALSE)+
  # geom_sf(fill="tomato3",alpha=.25,data=subset(plotBows,duration==5 & simu=='forward' & Zone=='oceanic' ), inherit.aes = FALSE)+
  geom_sf(fill="tomato2",alpha=.25,data=subset(plotBows,duration==4 & simu=='forward' & Zone=='oceanic' ), inherit.aes = FALSE)+
  geom_sf(fill="tomato1",alpha=.33,data=subset(plotBows,duration==3 & simu=='forward' & Zone=='oceanic' ), inherit.aes = FALSE)+
  geom_sf(fill="tomato",alpha=.4,data=subset(plotBows,duration==2 & simu=='forward' & Zone=='oceanic' ), inherit.aes = FALSE)+
  geom_sf(fill="red",alpha=.5,data=subset(plotBows,duration==1 & simu=='forward' & Zone=='oceanic' ), inherit.aes = FALSE )+
  
  geom_point(aes(x=x_from,y=y_from,color=Zone),size=1.5,data=subset(plotBows,duration==1 & simu=='forward' ))+
  # geom_point(aes(x=x_from,y=y_from),color="yellow",size=1.5,data=subset(plotBows,duration==1 & simu=='forward'))+
  # geom_point(aes(x=x_from,y=y_from),color="cyan",size=.5,data=subset(plotBows,duration==1 & simu=='backward'))+
  # geom_text_repel(aes(x=x_from,y=y_from,label=1:19),color="black",data=subset(plotBows,duration==1 & simu=="backward"))+
  # scale_x_continuous(limits=st_bbox(plotBows)[c(1,3)])+
  # scale_y_continuous(limits=st_bbox(plotBows)[c(2,4)])+
  theme_bw()



ggmap(FLK_map)+
  geom_sf(fill="lightskyblue2",alpha=.25,data=subset(plotBows,duration==4 & simu=='backward' & Zone=='Inshore' ), inherit.aes = FALSE)+
  geom_sf(fill="lightskyblue1",alpha=.33,data=subset(plotBows,duration==3 & simu=='backward' & Zone=='Inshore' ), inherit.aes = FALSE)+
  # geom_sf(fill="lightskyblue",alpha=.4,data=subset(plotBows,duration==2 & simu=='backward' & Zone=='Inshore' ), inherit.aes = FALSE)+ #this is the only line that plots
  geom_sf(fill="blue",alpha=.5,data=subset(plotBows,duration==1 & simu=='backward' & Zone=='Inshore' ), inherit.aes = FALSE)+
  # 
  geom_sf(fill="tomato2",alpha=.25,data=subset(plotBows,duration==4 & simu=='forward' & Zone=='oceanic' ), inherit.aes = FALSE)+
  geom_sf(fill="tomato1",alpha=.33,data=subset(plotBows,duration==3 & simu=='forward' & Zone=='oceanic' ), inherit.aes = FALSE)+
  geom_sf(fill="tomato",alpha=.4,data=subset(plotBows,duration==2 & simu=='forward' & Zone=='oceanic' ), inherit.aes = FALSE)+
  geom_sf(fill="red",alpha=.5,data=subset(plotBows,duration==1 & simu=='forward' & Zone=='oceanic' ), inherit.aes = FALSE )+
  
  geom_point(aes(x=x_from,y=y_from,color=Zone),size=1.5,data=subset(plotBows,duration==1 & simu=='forward' ))+
  
  theme_bw()







#try subsetting by dates: 
date2016 =unique(JyearBows$date)
dd=date2016[1:14]
dd

ggmap(FLK_map)+
  # geom_sf(fill="blue",alpha=.1,data=subset(plotBows,duration==7 & simu=='backward' & Zone=='Inshore' ), inherit.aes = FALSE)+
  # geom_sf(fill="lightskyblue4",alpha=.25,data=subset(plotBows,duration==6 & simu=='backward' & Zone=='Inshore' ), inherit.aes = FALSE)+
  # geom_sf(fill="lightskyblue3",alpha=.25,data=subset(plotBows,duration==5 & simu=='backward' & Zone=='Inshore' ), inherit.aes = FALSE)+
  geom_sf(fill="lightskyblue2",alpha=.25,data=subset(plotBows,duration==4 & simu=='backward' & Zone=='Inshore' & date==dd), inherit.aes = FALSE)+
  geom_sf(fill="lightskyblue1",alpha=.33,data=subset(plotBows,duration==3 & simu=='backward' & Zone=='Inshore' & date==dd), inherit.aes = FALSE)+
  geom_sf(fill="lightskyblue",alpha=.4,data=subset(plotBows,duration==2 & simu=='backward' & Zone=='Inshore' & date==dd), inherit.aes = FALSE)+
  geom_sf(fill="blue",alpha=.5,data=subset(plotBows,duration==1 & simu=='backward' & Zone=='Inshore' & date==dd), inherit.aes = FALSE)+
  
  # geom_sf(fill="red",alpha=.1,data=subset(plotBows,duration==7 & simu=='forward' & Zone=='oceanic' ), inherit.aes = FALSE)+
  # geom_sf(fill="tomato4",alpha=.25,data=subset(plotBows,duration==6 & simu=='forward' & Zone=='oceanic' ), inherit.aes = FALSE)+
  # geom_sf(fill="tomato3",alpha=.25,data=subset(plotBows,duration==5 & simu=='forward' & Zone=='oceanic' ), inherit.aes = FALSE)+
  geom_sf(fill="tomato2",alpha=.25,data=subset(plotBows,duration==4 & simu=='forward' & Zone=='oceanic' & date==dd), inherit.aes = FALSE)+
  geom_sf(fill="tomato1",alpha=.33,data=subset(plotBows,duration==3 & simu=='forward' & Zone=='oceanic' & date==dd), inherit.aes = FALSE)+
  geom_sf(fill="tomato",alpha=.4,data=subset(plotBows,duration==2 & simu=='forward' & Zone=='oceanic' & date==dd), inherit.aes = FALSE)+
  geom_sf(fill="red",alpha=.5,data=subset(plotBows,duration==1 & simu=='forward' & Zone=='oceanic' & date==dd), inherit.aes = FALSE )+
  
  geom_point(aes(x=x_from,y=y_from,color=Zone),size=1.5,data=subset(plotBows,duration==1 & simu=='forward' & date==dd))+
  # geom_point(aes(x=x_from,y=y_from),color="yellow",size=1.5,data=subset(plotBows,duration==1 & simu=='forward'))+
  # geom_point(aes(x=x_from,y=y_from),color="cyan",size=.5,data=subset(plotBows,duration==1 & simu=='backward'))+
  # geom_text_repel(aes(x=x_from,y=y_from,label=1:19),color="black",data=subset(plotBows,duration==1 & simu=="backward"))+
  # scale_x_continuous(limits=st_bbox(plotBows)[c(1,3)])+
  # scale_y_continuous(limits=st_bbox(plotBows)[c(2,4)])+
  theme_bw()

