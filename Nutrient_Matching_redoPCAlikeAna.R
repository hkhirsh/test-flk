#Nutrient_Matching - remove bow overlaps here (this is a script I can build off if I need to match the nutrient samples to the carbonate chem samples by date/time/site)
# For now (10/29) I am pursuing bow overlap of nutrients to use the most spatially relevant nutrients (not the exact colocated samples)

# use new nutrient dataframe from Alex (limited to the keys only sites)

## Clear Workspace ----------------------------------------------------------
rm(list=ls())
runDate='2023dec11'

# Load Libraries ----------------------------------------------------------
library(stringr)
library(dplyr)
library(ggmap)
library(tidyr)
library(factoextra)
library(sf)
library(mapview)
library(geosphere) #distance between points
library(vegan)
library(ggfortify)
register_google(key = "AIzaSyB3mq0nJtcNWvQUNuqH_t-MLxEUmwDDmGk") #api updated OCT23



se = function(x,na.rm=T){return(sd(x,na.rm=na.rm)/sqrt(length(x)))}
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
# names(CCflk)

## Load nutrient data (limited to Keys by Alex)
CCnuts = read.csv('/Users/heidi.k.hirsh/Desktop/WS_Keys_Data_1998-2022_hh.csv')

# plotNuts = st_as_sf(CCnuts, coords = c("Longitude","Latitude"), crs = st_crs(4326))  
# pesky =  subset(plotNuts,Station %in% c('19'))
# mapview(plotNuts,zcol="Station")
# mapview(pesky,zcol="Station")

## Check station number for station 21 (name does not match between the two dataframes)
## Rename station "21" as "21LK" in CCnuts
CCnuts$Station[which(CCnuts$Station == "21")] = "21LK"
dim(CCnuts) #3739   17
#also remove station KW1 (north of Key West)
CCnuts = CCnuts[which(CCnuts$Station != "KW1"),] 
dim(CCnuts) #3717   17

#need to make CCnuts$UTCDate_Time
CCnuts$Date #"6/7/04"   "6/7/04" 
CCnuts$Date.yyyymmdd = as.Date(CCnuts$Date, format= "%m/%d/%y")
head(CCnuts$Date.yyyymmdd)
#GMT and UTC are basically the same for the Keys

## Combine date and time in one column
CCnuts$dateTime = str_c(CCnuts$Date.yyyymmdd, ' ', CCnuts$GMT)
head(CCnuts$dateTime)

## Convert to timestamps POSIXct
CCnuts$UTCDate_Time = as.POSIXct(CCnuts$dateTime, tz="UTC") # format="%Y-%m-%d %H:%M:%S" (it didn't work when I included the format)
head(CCnuts$UTCDate_Time)

## Create visitID for CCnuts (station + date + time)
visitID_1 = paste(CCnuts$Station,CCnuts$UTCDate_Time)
head(visitID_1)
visitID_2 = gsub(" ", "_" ,visitID_1, perl=TRUE)
head(visitID_2) #underscores added
CCnuts$visitID =  gsub("[: -]", "" , visitID_2, perl=TRUE) 
head(CCnuts$visitID) # : and - removed in times
# dim(CCnuts) #3717   21

#the pesky point is visitID = 	19_20160728_013600
#limit CCnuts to anything that does not have that visitID
CCnuts = CCnuts[which(CCnuts$visitID !='19_20160728_013600'),]  
# dim(CCnuts) # 3716   21


## Limit CCnuts to the same years as carbonate chemistry df
#need to isolate year
CCnuts$Year = format(CCnuts$UTCDate_Time, format="%Y")
# unique(CCnuts$Year) 
CCnuts = subset(CCnuts,Year %in% c('2012','2013','2014','2015','2016','2017','2018','2019','2020','2021'))
CCnuts$Year = as.numeric(CCnuts$Year)
# unique(CCnuts$Year)
# dim(CCnuts) #1476   22 (removed a lot of years we don't need and added 'Year' column)


## Combine CCflk and CCnuts (carbonate chem+ and nutrients) 
## Date time format must match
CCflk$UTCDate_Time = as.POSIXct(CCflk$UTCDate_Time, tz="UTC")
# class(CCflk$UTCDate_Time) #"POSIXct" "POSIXt" 

head(CCflk$visitID)
# length(unique(CCflk$visitID)) #1376
head(CCnuts$visitID)
# length(unique(CCnuts$visitID)) #1476

## Which do not match? 
# View(CCflk[which(is.na(match(CCflk$visitID,CCnuts$visitID))),]) 
noMatch = CCflk[which(is.na(match(CCflk$visitID,CCnuts$visitID))),]
length(unique(noMatch$visitID)) #2
dim(noMatch) #14 93 (14 samples do not have matching nutrients - but it's actually 2 because CCflk has all 7 days of bow ties)
noMatch$visitID
unique(noMatch$visitID) #"6.5_20180309_000000"  "21LK_20201213_082700"

#remove the IDs that do not match CCnuts 
CCflk = subset(CCflk, !(visitID %in% c('6.5_20180309_000000','21LK_20201213_082700')))
noMatch = CCflk[which(is.na(match(CCflk$visitID,CCnuts$visitID))),]
length(unique(noMatch$visitID)) #empty

CCm = merge(CCnuts, CCflk, by = "UTCDate_Time")
names(CCm)

#did lat/lon match up correctly - some are not on the 1:1 line ARGH
plot(CCm$Latitude.x,CCm$Latitude.y)
plot(CCm$Longitude.x,CCm$Longitude.y)

# code for nutrient exploration over regions 

# Nutcols = c("SST","SSS","Chla","Phphytn","NH4","PO4","NO3_NO2","NO2","NO3","Si") #why are the names different here?
Nutcols = c("SST","SSS","Chla","Phaeophytin","NH4","PO4","NO3.NO2","NO2","NO3","Si") #why are the names different here?
#I am already dropping the NA values here
Drop = which(is.na(CCm[,Nutcols]),arr.ind=T)[,1]
CCfull = CCm[-Drop,]
# dim(CCfull) # 8582  114
# dim(CCfull)/dim(CCm) #88%

#reorder region names
CCfull$Sub_region = factor(CCfull$Sub_region, levels = c("BB","UK","MK","LK"))


##_______________nutrients pca
#pca plot of nutrient data

# #add a dummy column 
# # dim(CCfull)
# CCfull=cbind(CCfull, dummy=1)
# # dim(CCfull)
# # names(CCfull)
# # unique(CCfull$dummy)

# nutNames = names(CCfull[42:49]) #for spatial nutrients 
# Don't use NO3.NO2

#original nutrient values
names1 = names(CCfull[11:14])
names2 = names(CCfull[16:18])

#spatial nutrients
# names1 = names(CCfull[42:45])
# names2 = names(CCfull[47:49])
nutNames =c(names1,names2)
nutNames
# nutNames = append(nutNames, 'dummy')
# nutNames
CCfull$Chla 

#crop CCfull to only the rows that have the data used for the pca
# Drop = which(is.na(CCfull[,nutNames]),arr.ind=T)[,1]  #NA values were already dropped above
# CCpca = CCfull[-Drop,]
CCpca= CCfull
dim(CCpca) #6762  122
# dim(CCpca)/dim(CCfull) #78%
# names(CCpca)


#scale nutrients first
scaleNut = scale(na.omit(CCpca[,nutNames]))
head(scaleNut)
# scaleNut = cbind(scaleNut, dummy=1)
# head(scaleNut)

#untransformed data
nutPCA = princomp(scaleNut)
biplot(nutPCA)



#transformed data:
# nutPCAt = princomp(scale(na.omit(apply(CCpca[,nutNames]+10^-6,2,log10))))
scaleNutT = scale(na.omit(apply(CCpca[,nutNames]+10^-6,2,log10)))
head(scaleNutT)
nutPCAt = princomp(scaleNutT)
biplot(nutPCAt)

# #add dummy column (this doesn't change anything)
# scaleNutT_d = cbind(scaleNutT, dummy=1)
# head(scaleNutT_d)
# nutPCAt_d = princomp(scaleNutT_d)
# biplot(nutPCAt_d)


comps = nutPCAt$scores
dim(comps)

#Loop to pull means and se!
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

# scoresALL = scores  %>% gather (key = Stat, value=Value, comp1_mean:comp2_se)
# scoresALL
# ggplot(scoresALL, aes(x=Stat, y=Value, fill = as.factor(Sub_region))) + geom_col(position='dodge')+ theme_bw()
ggplot(scores, aes(x=comp1_mean, y=comp2_mean, color=as.factor(Sub_region))) + 
  geom_point()+
  geom_errorbar(aes(xmin=comp1_mean-comp1_se,xmax=comp1_mean+comp1_se),width=0)+
  geom_errorbar(aes(ymin=comp2_mean-comp2_se,ymax=comp2_mean+comp2_se),width=0)+
  theme_bw()



#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/
fviz_pca(nutPCAt,geom= 'point')

fviz_pca_biplot(nutPCAt, geom='',col.var='black')+
  geom_point(aes(shape=factor(CCpca$Sub_region), colour=CCpca$Season))+
  # scale_color_gradient(low='red',high='white')+
  guides(shape = guide_legend(title = "Region"),
         colour = guide_legend(title = "color"))


##_______________
#try Ana's code: 
# CC.rda1 <- rda(PCA.data[, -(8:11)], scale=TRUE)
# 
# # Checking if the PC axes are meaningful
# eigenval <- CC.rda1$CA$eig   # Here you will get the eigenvalues
# sitecoord <- CC.rda1$CA$u[,1:2]   # The site coordinates along PC1 and PC2
# eig <- data.frame(eigenval)
# eig$nb <- c(1:length(eigenval))
# eig$prop <- eig$eigenval/sum(eig$eigenval)
# eig

# CC.rda2 <- prcomp(PCA.data[, -(8:11)], scale=TRUE)
CC.rda2 <- prcomp(scaleNut)
summary(CC.rda2)
CC.rda2t = prcomp(scaleNutT)
summary(CC.rda2t)

PCA.data = CCpca
par(mfrow=c(1,1))

# autoplot(CC.rda2, data = PCA.data, colour="Season", 
#          shape="Sub_region",
#          loadings = TRUE, loadings.colour = 'black',
#          loadings.label = TRUE, loadings.label.size = 3,
#          loadings.label.vjust = -1.0,
#          loadings.label.colour="black",
#          frame = TRUE, frame.type = 'norm')+
#   facet_wrap(~Zone)+
#   theme_bw()

autoplot(CC.rda2t, data = PCA.data, colour="Season", 
         shape="Sub_region",
         loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 3,
         loadings.label.vjust = -1.0,
         loadings.label.colour="black",
         frame = TRUE, frame.type = 'norm')+
  facet_wrap(~Zone) +
  theme_bw()



