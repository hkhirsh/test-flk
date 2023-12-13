#Nutrient_Matching - remove bow overlaps here (this is a script I can build off if I need to match the nutrient samples to the carbonate chem samples by date/time/site)
# For now (10/29) I am pursuing bow overlap of nutrients to use the most spatially relevant nutrients (not the exact colocated samples)

# use new nutrient dataframe from Alex (limited to the keys only sites)

## Clear Workspace ----------------------------------------------------------
rm(list=ls())
runDate='2023nov27'

# Load Libraries ----------------------------------------------------------
library(stringr)
# library(raster)
library(dplyr)
library(ggmap)

library(tidyr)
library(factoextra)
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
library(sf)
library(mapview)
library(geosphere) #distance between points
# library(geosphere) #distance between points
register_google(key = "AIzaSyB3mq0nJtcNWvQUNuqH_t-MLxEUmwDDmGk") #api updated OCT23

#___________________________#

## Load (latest) carbonate chemistry dataframe
# CCflk = read.csv('/Users/heidi.k.hirsh/Desktop/CCfull+distance_Oct4.csv')
# dim(CCflk)

#read in dataframe where nutrients are already paired spatially
# CCfull.all = read.csv('/Users/heidi.k.hirsh/Desktop/CCmodel_allYears+Nuts_13nov2023.csv') #new file with nutrients added
# dim(CCfull.all)
CCflk =  read.csv('/Users/heidi.k.hirsh/Desktop/CCmodel_allYears+Nuts_13nov2023.csv') #new file with (spatial) nutrients added
names(CCflk)[names(CCflk) == 'SSS'] <- 'SSS.sp'
names(CCflk)[names(CCflk) == 'SST'] <- 'SST.sp'
names(CCflk)[names(CCflk) == 'Chla'] <- 'Chla.sp'
names(CCflk)[names(CCflk) == 'Phphytn'] <- 'Phphytn.sp'
names(CCflk)[names(CCflk) == 'NH4'] <- 'NH4.sp'
names(CCflk)[names(CCflk) == 'PO4'] <- 'PO4.sp'
names(CCflk)[names(CCflk) == 'NO3_NO2'] <- 'NO3_NO2.sp'
names(CCflk)[names(CCflk) == 'NO2'] <- 'NO2.sp'
names(CCflk)[names(CCflk) == 'NO3'] <- 'NO3.sp'
names(CCflk)[names(CCflk) == 'Si'] <- 'Si.sp'
# 
# names(CCflk)


## Load nutrient data (limited to Keys by Alex)
CCnuts = read.csv('/Users/heidi.k.hirsh/Desktop/WS_Keys_Data_1998-2022_hh.csv')

plotNuts = st_as_sf(CCnuts, coords = c("Longitude","Latitude"), crs = st_crs(4326))  

pesky =  subset(plotNuts,Station %in% c('19'))
mapview(plotNuts,zcol="Station")
mapview(pesky,zcol="Station")

## Check station number for station 21 (name does not match between the two dataframes)
## Rename station "21" as "21LK" in CCnuts
CCnuts$Station[which(CCnuts$Station == "21")] = "21LK"
dim(CCnuts) #3739   17
#also remove station KW1 (north of Key West)
CCnuts = CCnuts[which(CCnuts$Station != "KW1"),] 
dim(CCnuts) #3717   17

# plotNewNuts = st_as_sf(CCnuts, coords = c("Longitude","Latitude"), crs = st_crs(4326))
# mapview(plotNewNuts,zcol="Station") #"pesky point is removed after isolating visitID

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
head(visitID_1)
visitID_2 = gsub(" ", "_" ,visitID_1, perl=TRUE)
head(visitID_2) #underscores added
CCnuts$visitID =  gsub("[: -]", "" , visitID_2, perl=TRUE) 
head(CCnuts$visitID) # : and - removed in times
dim(CCnuts) #3717   21

#the pesky point is visitID = 	19_20160728_013600
#limit CCnuts to anything that does not have that visitID
CCnuts = CCnuts[which(CCnuts$visitID !='19_20160728_013600'),]  
# table(CCnuts$visitID)
dim(CCnuts) # 3716   21


## Limit CCnuts to the same years as carbonate chemistry df
#need to isolate year
CCnuts$Year = format(CCnuts$UTCDate_Time, format="%Y")
unique(CCnuts$Year) 
CCnuts = subset(CCnuts,Year %in% c('2012','2013','2014','2015','2016','2017','2018','2019','2020','2021'))
CCnuts$Year = as.numeric(CCnuts$Year)
unique(CCnuts$Year)
dim(CCnuts) #1476   22 (removed a lot of years we don't need and added 'Year' column)




## Combine CCflk and CCnuts (carbonate chem+ and nutrients) 
## Date time format must match
# class(CCnuts$UTCDate_Time) #"POSIXct" "POSIXt" 
# class(CCflk$UTCDate_Time) #"character"
CCflk$UTCDate_Time = as.POSIXct(CCflk$UTCDate_Time, tz="UTC")
class(CCflk$UTCDate_Time) #"POSIXct" "POSIXt" 


head(CCflk$visitID)
length(unique(CCflk$visitID)) #1376

head(CCnuts$visitID)
length(unique(CCnuts$visitID)) #1476

## Which do not match? 
# View(CCflk[which(is.na(match(CCflk$visitID,CCnuts$visitID))),]) 
noMatch = CCflk[which(is.na(match(CCflk$visitID,CCnuts$visitID))),]
length(unique(noMatch$visitID)) #2
dim(noMatch) #14 93 (14 samples do not have matching nutrients - but it's actually 2 because CCflk has all 7 days of bow ties)
noMatch$visitID
unique(noMatch$visitID) #"6.5_20180309_000000"  "21LK_20201213_082700"

# dim(CCflk)
# # Should I remove these two visitIDs from CCflk?  
# test = subset(CCflk, !(visitID %in% c('6.5_20180309_000000','21LK_20201213_082700')))
# dim(CCflk)-dim(test) # 14 0 (successfully removed the missing IDs from CCflk)
# #lenght of CCflk visitIDs should now match CCnuts
# # length(unique(CCflk$visitID)) 
# length(unique(test$visitID)) #1374 (it makes sense that it is 2 shorter, but why is CCnuts$visitID 100 longer?!!)
# #ohhh I think CCnuts has IDs that CCflk does not have, but that doesn't matter because we just want the nutrients available to match the carbonate chemistry. 

#remove the IDs that do not match CCnuts 
CCflk = subset(CCflk, !(visitID %in% c('6.5_20180309_000000','21LK_20201213_082700')))
noMatch = CCflk[which(is.na(match(CCflk$visitID,CCnuts$visitID))),]
length(unique(noMatch$visitID)) #empty


# table(CCnuts$Station)
# CCtest=NULL
# CCtest= CCnuts %>% left_join(CCflk)
# View(CCtest)

CCm = merge(CCnuts, CCflk, by = "UTCDate_Time")

names(CCm)


#did lat/lon match up correctly - some are not on the 1:1 line ARGH
plot(CCm$Latitude.x,CCm$Latitude.y)
plot(CCm$Longitude.x,CCm$Longitude.y)

#compare point nutrients versus spatial nutrients



# code for nutrient exploration over regions 

#how many rows have NA nutrients (should lack all nutrient columns -   "SST"   "SSS"  "Chla"     "Phphytn"     "NH4"       "PO4"    "NO3_NO2"     "NO2"     "NO3"    "Si")
# Nutcols = c("SST","SSS","Chla","Phphytn","NH4","PO4","NO3_NO2","NO2","NO3","Si") #why are the names different here? 
Nutcols = c("SST","SSS","Chla","Phaeophytin","NH4","PO4","NO3.NO2","NO2","NO3","Si") #why are the names different here? 
# names(CCm)
Drop = which(is.na(CCm[,Nutcols]),arr.ind=T)[,1]
CCfull = CCm[-Drop,]
dim(CCfull) # 8582  114
dim(CCfull)/dim(CCm) #88%





#add distance index column (if not already present)
bbLat = 25.1
bbLon = -80.25

CCfull[, 'distBB.m'] = NA
for(d in 1:length(CCfull[,1])){CCfull$distBB.m[d] = distm(c(bbLat,CCfull$Latitude.x[d]),c(bbLon,CCfull$Longitude.y[d]),distGeo)}
CCfull$distBB.km = CCfull$distBB.m/1000
CCfull$invDistBB.m = 1/CCfull$distBB.m

hBB =hclust(dist(CCfull$distBB.m))
CCfull$distIndex = cutree(hBB,k=7) #other choices: 2, 4, 7, 12
unique(CCfull$distIndex)

#reorder region names
class(CCfull$Sub_region)
CCfull$Sub_region = factor(CCfull$Sub_region, levels = c("BB","UK","MK","LK"))

class(CCfull$distIndex)
CCfull$distIndex = as.character(CCfull$distIndex)
CCfull$distIndex = factor(CCfull$distIndex, levels = c('1','6','5','7','2','3','4'))


# plot distance index relative to "origin" point (bbLat,bbLon)
CC_sf = st_as_sf(CCfull, coords = c("Longitude.x","Latitude.x"), crs = st_crs(4326)) 

#add point used as reference for distance index: 
origin = data.frame(bbLat,bbLon)
origin$distIndex=0
origin = st_as_sf(origin, coords=c("bbLon","bbLat"), crs=st_crs(4326))
origin

mapview(CC_sf, zcol='distIndex')+
  mapview(origin,cex=8,col.regions='red',color='red')



#compare point nutrients versus spatially summarized nutrients for CC sample points


ggplot(data=CCfull,aes(x=Chla,y=Chla.sp)) +
  # geom_point(aes(color=as.factor(ndays)))+  
  # geom_point(aes(color=sub_Region))+  
  geom_point(aes(color=distIndex))+
  scale_color_brewer(palette="Spectral") +
  theme_bw()

ggplot(data=CCfull,aes(x=NO3,y=NO3.sp)) +
  geom_point(aes(color=as.factor(ndays)))+
  scale_color_brewer(palette="Spectral") +
  theme_bw()

ggplot(data=CCfull,aes(x=SSS,y=SSS.sp)) +
  geom_point(aes(color=as.factor(ndays)))+
  scale_color_brewer(palette="Spectral") +
  theme_bw()

ggplot(data=CCfull,aes(x=SST,y=SST.sp)) +
  geom_point(aes(color=as.factor(ndays)))+
  scale_color_brewer(palette="Spectral") +
  theme_bw()








# plotCC = CCfull[which(CCfull$Zone=='Inshore'),]
plotCC = CCfull

# boxplot(Chla~Sub_region, data=CCfull)
ggplot(data=plotCC, aes(x=Sub_region, y=Chla)) + geom_boxplot(aes(fill=Sub_region))  + scale_y_log10() + facet_wrap(.~Zone) + theme_bw()
ggplot(data=plotCC, aes(x=Sub_region, y=NO3)) + geom_boxplot(aes(fill=Sub_region)) + scale_y_log10()  + facet_wrap(.~Zone) + theme_bw()

ggplot(data=plotCC)

ggplot(data=plotCC, aes(x=Sub_region, y=Phaeophytin)) + geom_boxplot(aes(fill=Sub_region))+ scale_y_log10() + facet_wrap(.~Zone) + theme_bw()
ggplot(data=plotCC, aes(x=Sub_region, y=NH4)) + geom_boxplot(aes(fill=Sub_region))+ scale_y_log10() + facet_wrap(.~Zone) + theme_bw()
ggplot(data=plotCC, aes(x=Sub_region, y=PO4)) + geom_boxplot(aes(fill=Sub_region))+ scale_y_log10() + facet_wrap(.~Zone) + theme_bw()
ggplot(data=plotCC, aes(x=Sub_region, y=NO3.NO2)) + geom_boxplot(aes(fill=Sub_region))+ scale_y_log10() + facet_wrap(.~Zone) + theme_bw()
ggplot(data=plotCC, aes(x=Sub_region, y=NO2)) + geom_boxplot(aes(fill=Sub_region))+ scale_y_log10() + facet_wrap(.~Zone) + theme_bw()
ggplot(data=plotCC, aes(x=Sub_region, y=Si)) + geom_boxplot(aes(fill=Sub_region))+ scale_y_log10() + facet_wrap(.~Zone) + theme_bw()   
ggplot(data=plotCC, aes(x=Sub_region, y=SST)) + geom_boxplot(aes(fill=Sub_region))+ scale_y_log10() + facet_wrap(.~Zone) + theme_bw()
ggplot(data=plotCC, aes(x=Sub_region, y=SSS)) + geom_boxplot(aes(fill=Sub_region))+ scale_y_log10() + facet_wrap(.~Zone) + theme_bw()


# boxplot(Chla~distIndex, data=CCfull)
ggplot(data=plotCC, aes(x=distIndex, y=Chla)) + geom_boxplot(aes(fill=distIndex)) + scale_y_log10()  + facet_wrap(.~Zone) + theme_bw()
ggplot(data=plotCC, aes(x=distIndex, y=NO3)) + geom_boxplot(aes(fill=distIndex))  + scale_y_log10() + facet_wrap(.~Zone) + theme_bw()

ggplot(data=plotCC, aes(x=distIndex, y=Phaeophytin)) + geom_boxplot(aes(fill=distIndex)) + scale_y_log10() + facet_wrap(.~Zone) + theme_bw()
ggplot(data=plotCC, aes(x=distIndex, y=NH4)) + geom_boxplot(aes(fill=distIndex)) + scale_y_log10() + facet_wrap(.~Zone) + theme_bw()
ggplot(data=plotCC, aes(x=distIndex, y=PO4)) + geom_boxplot(aes(fill=distIndex)) + scale_y_log10() + facet_wrap(.~Zone) + theme_bw()
ggplot(data=plotCC, aes(x=distIndex, y=NO3.NO2)) + geom_boxplot(aes(fill=distIndex)) + scale_y_log10()+ facet_wrap(.~Zone) + theme_bw()
ggplot(data=plotCC, aes(x=distIndex, y=NO2)) + geom_boxplot(aes(fill=distIndex)) + scale_y_log10()+ facet_wrap(.~Zone) + theme_bw()
ggplot(data=plotCC, aes(x=distIndex, y=Si)) + geom_boxplot(aes(fill=distIndex)) + scale_y_log10()+ facet_wrap(.~Zone) + theme_bw()   #facet_wrap(.~Zone, ncol=1)
ggplot(data=plotCC, aes(x=distIndex, y=SST)) + geom_boxplot(aes(fill=distIndex)) + scale_y_log10()+ facet_wrap(.~Zone) + theme_bw()
ggplot(data=plotCC, aes(x=distIndex, y=SSS)) + geom_boxplot(aes(fill=distIndex)) + scale_y_log10()+ facet_wrap(.~Zone) + theme_bw()




# plotCC = CCfull[which(CCfull$Zone=='Inshore'),]




##_______________nutrients pca
#pca plot of nutrient data
# nutNames = names(CCnuts[10:17])
# nutNames = names(CCfull[11:18]) #for point nutrients

#add a dummy column 
dim(CCfull)
# CCfull=cbind(CCfull, dummy=1)
dim(CCfull)
names(CCfull)
# unique(CCfull$dummy)

nutNames = names(CCfull[42:49]) #for spatial nutrients
nutNames
# nutNames = append(nutNames, 'dummy')
# nutNames

#crop CCfull to only the rows that have the data used for the pca
dim(CCfull) #8582  125 (same # as nutPCA obs)
Drop = which(is.na(CCfull[,nutNames]),arr.ind=T)[,1]
CCpca = CCfull[-Drop,]
dim(CCpca) 
dim(CCpca)/dim(CCfull)
names(CCpca)


#scale nutrients first
scaleNut = scale(na.omit(CCpca[,nutNames]))
head(scaleNut)
scaleNut = cbind(scaleNut, dummy=1)
head(scaleNut)

#untransformed data
# nutPCA = princomp(scale(na.omit(CCnuts[,nutNames])))
nutPCA = princomp(scaleNut)
biplot(nutPCA)

# dim(CCfull)
# dim(na.omit(CCfull[,nutNames]))
# plot(nutPCA)
# biplot(nutPCA)
# nutPCA

#transformed data:
# aaa = CCpca[,nutNames]
# head(aaa)
# aaaa = apply(CCpca[,nutNames]+10^-6,2,log10)
# head(aaaa)

# nutPCAt = princomp(scale(na.omit(apply(CCpca[,nutNames]+10^-6,2,log10))))
scaleNutT = scale(na.omit(apply(CCpca[,nutNames]+10^-6,2,log10)))
head(scaleNutT)
scaleNutT = cbind(scaleNutT, dummy=1)
head(scaleNutT)
nutPCAt = princomp(scaleNutT)


plot(nutPCAt)
biplot(nutPCAt)
nutPCAt
#why does this have 1312 observations versus 8582?

dim(CCfull) #8582  125 (same # as nutPCA obs)

comps = nutPCAt$scores
dim(comps)

se = function(x,na.rm=T){return(sd(x,na.rm=na.rm)/sqrt(length(x)))}


#there must be a loop to pull means and se!



R
i=1


R = unique(as.character(CCpca$Sub_region))
scores = NULL
scores = data.frame(matrix(ncol=5, nrow=length(R)))
colnames(scores) = c('Sub_region','comp1_mean','comp1_se','comp2_mean','comp2_se')
scores

for (i in 1:length(R)) {
  comps.i = subset(comps,CCpca$Sub_region==R[i])
  scores$Sub_region[i] = R[i]
  scores$comp1_mean[i] = mean(comps.i[,1])
  scores$comp1_se[i] = se(comps.i[,1])
  scores$comp2_mean[i] = mean(comps.i[,2])
  scores$comp2_se[i] = se(comps.i[,2])
}


scoresALL = scores  %>% gather (key = Stat, value=Value, comp1_mean:comp2_se)
scoresALL
ggplot(scoresALL, aes(x=Stat, y=Value, fill = as.factor(Sub_region))) + geom_col(position='dodge')+ theme_bw()


ggplot(scores, aes(x=comp1_mean, y=comp2_mean, color=as.factor(Sub_region))) + 
  geom_point()+
  geom_errorbar(aes(xmin=comp1_mean-comp1_se,xmax=comp1_mean+comp1_se),width=0)+
  geom_errorbar(aes(ymin=comp2_mean-comp2_se,ymax=comp2_mean+comp2_se),width=0)+
  theme_bw()



#try for distance index
scores = data.frame(matrix(ncol=5, nrow=length(R)))
colnames(scores) = c('distIndex','comp1_mean','comp1_se','comp2_mean','comp2_se')

R = unique(CCpca$distIndex)
i=1
for (i in 1:length(R)) {
  comps.i = subset(comps,CCpca$distIndex==R[i])
  scores$distIndex[i] = R[i]
  scores$comp1_mean[i] = mean(comps.i[,1])
  scores$comp1_se[i] = se(comps.i[,1])
  scores$comp2_mean[i] = mean(comps.i[,2])
  scores$comp2_se[i] = se(comps.i[,2])
}
scores

scoresALL = scores  %>% gather (key = Stat, value=Value, comp1_mean:comp2_se)
scoresALL
ggplot(scoresALL, aes(x=Stat, y=Value, fill = as.factor(distIndex))) + geom_col(position='dodge')



# comps.BB = subset(comps,CCpca$Sub_region=='BB')
# # length(comps.BB[1,]) #8
# # length(comps.BB[,1]) #1058
# BB.mean1 = mean(comps.BB[,1])
# BB.se1 = se(comps.BB[,1])
# BB.mean2 = mean(comps.BB[,2])
# BB.se2 = se(comps.BB[,2])
# BB.mean1
# BB.se1
# BB.mean2
# BB.se2
# 
# comps.UK = subset(comps,CCpca$Sub_region=='UK')
# UK.mean1 = mean(comps.UK[,1])
# UK.se1 = se(comps.UK[,1])
# UK.mean2 = mean(comps.UK[,2])
# UK.se2 = se(comps.UK[,2])
# 
# comps.MK = subset(comps,CCpca$Sub_region=='MK')
# MK.mean1 = mean(comps.MK[,1])
# MK.se1 = se(comps.MK[,1])
# MK.mean2 = mean(comps.MK[,2])
# MK.se2 = se(comps.MK[,2])
# 
# comps.LK = subset(comps,CCpca$Sub_region=='LK')
# LK.mean1 = mean(comps.LK[,1])
# LK.se1 = se(comps.LK[,1])
# LK.mean2 = mean(comps.LK[,2])
# LK.se2 = se(comps.LK[,2])


#I think I need to limit it to the rows that have spatial data



biplot(nutPCA, col=c("grey", "black"), xlab="", ylab="")

# library(factoextra)
#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/
fviz_pca(nutPCAt,geom= 'point')


fviz_pca_biplot(nutPCAt, 
                # col= CCpca$Sub_region,
                # geom.ind='point',
                col=CCpca$Sub_region,
                pointshape=CCpca$Season,
                # addEllipses = TRUE,
                # habillage = CCfull$Sub_region,
                geom = 'point',
                col.var='black',
                # col.var = "black", repel = TRUE,
                legend.title='Region'
                )

fviz_pca_biplot(nutPCAt, geom='',col.var='black')+    #,alpha=0)+
  geom_point(aes(shape=factor(CCpca$Sub_region), colour=CCpca$Zone))+
  guides(shape = guide_legend(title = "shape"),
         colour = guide_legend(title = "color"))

fviz_pca_biplot(nutPCAt, col= CCpca$distIndex,
                # addEllipses = TRUE,
                # habillage = CCfull$distIndex,
                geom = 'point', 
                col.var='black',
                # col.var = "black", repel = TRUE,
                legend.title='Region'
)

col.ind = iris$Species, palette = "jco", 
addEllipses = TRUE, label = "var",
col.var = "black", repel = TRUE,
legend.title = "Species") 

#color biplot by sub region and by distance index

title(xlab="PC1", ylab="PC2", mgp=c(2.2, 2.2, 0))

##_______________
