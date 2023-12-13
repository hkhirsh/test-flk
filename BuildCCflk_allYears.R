#builds the "master" CC dataframe. Main step is binding CC (CCflk with bathymetry) to the BBB dataframe (bows)

## 10/9/2023 this script appears to be running correctly

#build modeling CCflk df

library(sf)
library(ggplot2)
library(tidyverse)
library(ggmap)
library(ggrepel)
library(rstudioapi)
library(stringr)
library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(patchwork)
library(mapview)
library(lubridate)

#read in CC dataframe with PAR. 
# CC = read.csv('/Users/heidihirsh/Desktop/CCflk_plusPAR_full.csv') 
CC = read.csv('/Users/heidi.k.hirsh/Desktop/FLK_data/CCflk_plusBathy.csv') 
# View(CC)
dim(CC) # 1611   63

BBB = st_read('/Users/heidi.k.hirsh/Desktop/FLK_data/Concave_BowBenthic_allRTdays_start14Dec2022.shp')
# BBB = st_read('/Users/heidihirsh/Desktop/FLK_data/BowVolBenthic_allRTdays_start7Nov2022.shp')
# BBB = st_read('/Users/heidihirsh/Desktop/FLK_data/BowBenthosVol_13OCT2022.shp') #this has benthic indices AND volume
# BenthBows = st_read('/Users/heidihirsh/Desktop/BowBenthos_7OCT2022_save2.shp') 
dim(BBB) #3437   14  (old - #9247   15) #OCT90 this is now 9632   14
View(BBB)
class(BBB$visitID)

IDtab=table(BBB$visitID)
# summary(IDtab)
# min(IDtab)
# max(IDtab)


#make item for each file: 
#note we removed 8/1/2017 from the thingies because it did not work in the intersection loop
bowIDfiles= list.files(path = "/Users/heidi.k.hirsh/Desktop/FLK_data/concave_bowtie_thingies",recursive=T,pattern='.shp') #list every file in every folder
# bowIDfiles= list.files(path = "/Users/heidihirsh/Desktop/bow_ties_sample_ids",recursive=T,pattern='.shp') #list every file in every folder
length(bowIDfiles)

splitBfile=str_split(string=bowIDfiles,"/") 
splitBfile.m = matrix(unlist(splitBfile),ncol=3,byrow=T)
fileSVID = substr(splitBfile.m[,3],10,nchar(splitBfile.m[,3])-4) #not supposed to include nter_mesh_ part?

table(BBB$ndays)

#subset of chemistry samples that go with bow ties: 
# unique(CC$Year) #no 2013?
CCbt = subset(CC,Year %in% c('2012','2013','2014','2015','2016','2017','2018','2019','2020','2021'))
# unique(CCbt$Year)
dim(CCbt) #1377   63

# CCbt_no.oc = CCbt[which(CCbt$Zone!='Oceanic'),]  #not oceanic!
# unique(CCbt_no.oc$Zone)
# dim(CCbt_no.oc) #1204   63
# test = CCbt[which(CCbt$Zone=='Inshore'),]
# test = CCbt[which(CCbt$Zone=='Mid channel'),]
# test = CCbt[which(CCbt$Zone=='Offshore'),]
# test = CCbt[which(CCbt$Zone=='Oceanic'),]
# dim(test)

# names(BBB)                
# dim(BBB) #9247   15
# length(unique(BBB$visitID)) #1321 (dim/7)
# head(BBB$visitID)

names(CCbt)
CCbt$visitID_ch1 =  paste(CCbt$SiteID,CCbt$UTCDate_Time)
CCbt$visitID_ch2 =    gsub(" ", "_" , CCbt$visitID_ch1, perl=TRUE)
CCbt$visitID =  gsub("[: -]", "" , CCbt$visitID_ch2, perl=TRUE) # these should now match the BBB IDs

ins=names(IDtab)
leftOuts = CCbt %>% filter(!visitID%in%ins)
unique(leftOuts$Date) # nothing left out

dim(leftOuts)
leftOuts$visitID


setdiff(ins,fileSVID)
sampleSVID = unique(CCbt$visitID)

setdiff(fileSVID,ins)  #only one different
"12_20170801_072200"%in%sampleSVID #not included
table(table(CCbt$visitID))

#currently not working because names are formatted different. I'm confident both have 1376 now
# setdiff(sampleSVID,fileSVID) # (was 55) 
# length(setdiff(sampleSVID,fileSVID))

# table(leftOuts$Zone)
# table(leftOuts$Year)
# table(leftOuts$Month)
# table(leftOuts$datetime)
# length(leftOuts$datetime)
# unique(leftOuts$datetime)
# range(leftOuts$UTCDate_Time)
# dim(leftOuts)

# head(CCbt$visitID_ch1)
# head(CCbt$visitID_ch2)
# head(CCbt$visitID)
length(unique(CCbt$visitID)) #1376 (why are there more in the CC dataframe? )
dim(CCbt) #cc has one extra


CCmodel=NULL
CCmodel=BBB %>% left_join(CCbt)
##take out the simplification for now:
# CCmodel = BBB[,-c(1,4,6,15)] %>% left_join(CCbt[,c("visitID","Region","Year","Latitude","Longitude",
#                                                    "Sample_Depth_m","DIC_umol_kg","TA_umol_kg","Aragonite_Sat_W",
#                                                    "Salinity_Bottle","Temperature_C","Survey_design","Season",
#                                                    "Month","dec.lon","dec.lat","Zone","Sub_region","jday.utc","jday.lst",
#                                                    "hrod.lst", "PAR_MODIS_DAILY"  ,"PAR_MODIS_8DAY" , 
#                                                    "PAR_MODIS_MON", "pointDepth" )],by=c("visitID"))

# dim(CCbt)#1377   66
# length(unique(CCbt$visitID))  #1376 (not 1377?)
# dim(BBB)#9247   15
# length(unique(BBB$visitID)) #1321
# dim(CCmodel)#9254   80   (9254/7=1322)
# length(unique(CCmodel$visitID)) #1321

modID = table(CCmodel$visitID)
modID[which(modID>7)]

CCmodel %>% filter(visitID =="EK_IN_20211204_171800") %>% View
# why are these doubled??

table(table(which(is.na(CCmodel),arr.ind=T)[,1]))  #only 2 rows have NAs 
# table(which(is.na(CCmodel),arr.ind=T)[,1])


CCmodel.df = CCmodel %>% st_drop_geometry()
dim(CCmodel.df) 
dim(CCmodel) 

# I need delta DIC and delta TA: 
unique(CC$Zone)

CCoceanRef = CCmodel.df %>% dplyr::filter(Zone=="Oceanic") %>% dplyr::group_by(Year,Month) %>% 
  dplyr::summarize(TAoce_mean= mean(TA_umol_kg),
            DICoce_mean= mean(DIC_umol_kg),
            ARAGoce_mean= mean(Aragonite_Sat_W),
            Toce_mean= mean(Temperature_C),
            Soce_mean= mean(Salinity_CTD))

CCmodel.df= left_join(CCmodel.df,CCoceanRef,by=c("Year","Month"))

#calculate deltas
CCmodel.df$TA_delta = CCmodel.df$TA_umol_kg - CCmodel.df$TAoce_mean
CCmodel.df$DIC_delta = CCmodel.df$DIC_umol_kg - CCmodel.df$DICoce_mean
CCmodel.df$ARAG_delta = CCmodel.df$Aragonite_Sat_W - CCmodel.df$ARAGoce_mean

dim(CCmodel.df)
head(CCmodel.df)

#This is the latest write out code (oct 2023 - commented it out so I do not overwrite)
# write.csv(CCmodel.df, file='/Users/heidihirsh/Desktop/CCmodel_allYears_dec16.csv')



# write.csv(CCmodel.df, file='/Users/heidihirsh/Desktop/CCmodel_allYears_nov10.csv')
# write.csv(CCmodel.df, file='/Users/heidihirsh/Desktop/CCmodel_allYears_nov9.csv')
