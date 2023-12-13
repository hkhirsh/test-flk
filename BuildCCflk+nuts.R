#TOSS THIS CODE! not used. wrong file. 


# Developing code to bind together CC dataframe and bows (BBB)
# I believe this code only joins the data for 2016
## 10/9/2023 All uncommented code seems to run fine with updated paths


## 11/13/2023 Try running with BBB as BBB+nuts instead. 

#Maybe this is the wrong file to build off of. I need whichever code originally built CCmodel_allYears_nov9!

#build modeling CCflk df
rm(list=ls())
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
names(CC)

# BenthBows = st_read('/Users/heidihirsh/Desktop/BowBenthos_7OCT2022_save2.shp') 
# BBB = st_read('/Users/heidi.k.hirsh/Desktop/FLK_data/BowBenthosVol_13OCT2022.shp') #this has benthic indices AND volume



BBB = st_read('/Users/heidi.k.hirsh/Desktop/nutBows_space&time_3Nov23.shp') #no volume? #shit. this version doesn't have date column or x_from or y_from
# BBBnuts = st_read('/Users/heidi.k.hirsh/Desktop/nutBows_space&time_3Nov23.shp')
names(BBB)
BBB$DT

dim(CC) # 1611   63
unique(CC$Year) #no 2013?

CCbt = subset(CC,Year %in% c('2012','2013','2014','2015','2016','2017','2018','2019','2020','2021'))
unique(CCbt$Year)
dim(CCbt) #1377   63
CCbt_no.oc = CCbt[which(CCbt$Zone!='Oceanic'),]  #not oceanic!
unique(CCbt_no.oc$Zone)
dim(CCbt_no.oc) #1204   63

CC16 = CC[which(CC$Year=='2016'),]
dim(CC16) #225  63
# head(CC16$Longitude)
# head(BBB$x_from)
# head(CC16$Latitude)
# head(BBB$y_from)

#need to add column like BBB$date to CC16 (YYYYMMDD)
BBB$date
#Build it using CC16$ESTDate ("2016-05-09")
class(CC16$ESTDate)
class(BBB$date)

#split date components
# CC16[c('YYYY','MM','DD')] = str_split_fixed(CC16$ESTDate,'-',3)
# head(CC16$YYYY)
# head(CC16$MM)
# head(CC16$DD)

#recombine without dashes: 
# CC16$dateMatch = str_c(CC16$YYYY,'',CC16$MM,'',CC16$DD)

CC16$dateMatch=ymd(CC16$UTCDate)
CC16$dateMatch
BBB$dateMatch=ymd(BBB$date)
BBB$dateMatch
# CC16$dateMatch=as.numeric(ymd(CC16$UTCDate))
# BBB$dateMatch=as.numeric(ymd(BBB$date))
# class(CC16$dateMatch)
# class(BBB$dateMatch)


#match CC16$dateMatch to BBB$date
#match CC16$Latitude) to BBB$y_from
#match CC16$Longitude to BBB$x_from
# dim(CC16) #225  64
# dim(BBB) #3150   17


#these are WAY too rounded (no decimals!)
BBB$latRnd = trunc(BBB$y_from,5)
BBB$lonRnd = trunc(BBB$x_from,5)
CC16$latRnd = trunc(CC16$Latitude,5)
CC16$lonRnd = trunc(CC16$Longitude,5)

#try rounding instead
BBB$latRnd = round(BBB$y_from,5)
BBB$lonRnd = round(BBB$x_from,5)
CC16$latRnd = round(CC16$Latitude,5)
CC16$lonRnd = round(CC16$Longitude,5)

# class(BBB$latRnd)
# class(BBB$lonRnd)
# class(CC16$latRnd)
# class(CC16$lonRnd)



#left join or merge
# CCmodel = merge(BBB,CC16,by=c("date"=="dateMatch","y_from"=="Latitude","x_from"=="Longitude"))

# length(unique(CC16$dateMatch)) #14
# length(unique(BBB$dateMatch))  #14
# length(unique(CC16$lonRnd))   #217
# length(unique(BBB$lonRnd))    #217
# length(unique(CC16$latRnd))   #220
# length(unique(BBB$latRnd))    #220

                
CCmodel=NULL
CCmodel = BBB %>% left_join(CC16,by=c("dateMatch","latRnd","lonRnd"))

dim(BBB)
dim(CC16)
dim(CCmodel)


table(table(which(is.na(CCmodel),arr.ind=T)[,1]))  #only 2 rows have NAs 
# table(which(is.na(CCmodel),arr.ind=T)[,1])
table(CC16$SiteID)

# BBB[300,]

dim(CCmodel)

CC16 %>% filter(dateMatch==ymd('20160105') & (abs(latRnd-24.45515)<0.01))
CC16$latRnd

CCmodel.df = CCmodel %>% st_drop_geometry()
dim(CCmodel.df) #36834    81
dim(CCmodel) #36834    82

# I need delta DIC and delta TA: 
unique(CC$Zone)

#calculate a mean offshore reference value (this is not the "right" way to do this but I want to set up the downstream code)
# CCmodel_oceanic= CCmodel.df[which(CCmodel.df$Zone=='Oceanic'),]
# TAoff_mean= mean(CCmodel_oceanic$TA_umol_kg)
# DICoff_mean= mean(CCmodel_oceanic$DIC_umol_kg)
# ARAGoff_mean= mean(CCmodel_oceanic$Aragonite_Sat_W)

#need to pair value for the appropriate oceanic sample: 
#I'm cheating for now and setting the endmembers as the same value
# CCmodel.df$TA_offshore=TAoff_mean
# CCmodel.df$DIC_offshore=DICoff_mean
# CCmodel.df$Arag_offshore=ARAGoff_mean


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

#the "right" way is to average the oceanic values for each sample cruise (collection of 2-3 dates)
# unique(CCmodel.df$UTCDate)
# unique(CCmodel.df$Month)
#loop through dates (really want to loop through cruises)
#can I just loop through unique months? 

# months = unique(CCmodel.df$Month)
# 
# CCmodel.df[, 'TA_offshore'] = NA
# CCmodel.df[, 'DIC_offshore'] = NA
# CCmodel.df[, 'Arag_offshore'] = NA
# 
# m_i=3
# for(m_i in 1:length(months)){
#   #define df for one month
#   CCmonth = CCmodel.df[which(CCmodel.df$Month==months[m_i]),]
#   
#   CCmonth_oceanic = CCmonth[which(CCmonth$Zone=='Oceanic'),]
#   
#   TAoff_mean= mean(CCmonth_oceanic$TA_umol_kg)
#   DICoff_mean= mean(CCmonth_oceanic$DIC_umol_kg)
#   ARAGoff_mean= mean(CCmonth_oceanic$Aragonite_Sat_W)
#   
#   CCmonth$TA_offshore=TAoff_mean
#   CCmonth$DIC_offshore=DICoff_mean
#   CCmonth$Arag_offshore=ARAGoff_mean
#   
#   #how do I get it back in CCmodel.df with all months? 
#   
# }
# 
# 
# 
# CCmodel.df[, 'TA_offshore'] = NA
# CCmodel.df[, 'DIC_offshore'] = NA
# CCmodel.df[, 'Arag_offshore'] = NA
# #Use IF instead 
# m_i=1
# for(m_i in 1:length(months)){
# 
#   if (CCmodel.df$Month == months[m_i]) { 
#   
#   CCmonth_oceanic = CCmonth[which(CCmonth$Zone=='Oceanic'),]
#   
#   TAoff_mean= mean(CCmonth_oceanic$TA_umol_kg)
#   DICoff_mean= mean(CCmonth_oceanic$DIC_umol_kg)
#   ARAGoff_mean= mean(CCmonth_oceanic$Aragonite_Sat_W)
#   
#   CCmodel.df$TA_offshore=TAoff_mean
#   CCmodel.df$DIC_offshore=DICoff_mean
#   CCmodel.df$Arag_offshore=ARAGoff_mean
#   }
#   
#   #how do I get it back in CCmodel.df with all months? 
#   
# }











# #calculate deltas
# CCmodel.df$TA_delta = CCmodel.df$TA_umol_kg - CCmodel.df$TA_offshore
# CCmodel.df$DIC_delta = CCmodel.df$DIC_umol_kg - CCmodel.df$DIC_offshore
# CCmodel.df$ARAG_delta = CCmodel.df$Aragonite_Sat_W - CCmodel.df$Arag_offshore

#reformat duration (RT stand in) column
# CCmodel.df$

# write.csv(CCmodel.df, file='/Users/heidihirsh/Desktop/CCmodel_17OCT2022off.csv')
# write.csv(CCmodel.df, file='/Users/heidihirsh/Desktop/CCmodel_27OCT2022off.csv')
# write.csv(CCmodel.df, file='/Users/heidihirsh/Desktop/CCmodel_halloween.csv')
