#model flk!
#this is the same script as the region one (pre region play)

# Clear workspace
rm(list=ls())
# runDate='16dec2022'
# runDate='14sept2023'
runDate='9oct2023'

# Load Libraries ---------------------------------------------------------------
library(dplyr)
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
library(janitor)  #use to remove duplicates (get_dupes) from model output\
library(GGally)
library(geosphere) #distance between points


#___________________________#
# Load Functions ---------------------------------------------------------------
se = function(x,na.rm=T){return(sd(x,na.rm=na.rm)/sqrt(length(x)))}

hrs <- function(s) {x <- s * 3600;return(x)}

traintest=function(mod,train_p=.8,rep=F,nruns=100,metric="adj.r.squared"){
  if(mod$call[[2]]=="RY ~ 1"){
    rsqtt=summary(mod)[metric]
  }else{
    modData=mod$model
    dataN=nrow(modData)
    trainN=floor(dataN*train_p)
    rsqtt=rep(NA,nruns)
    for(i in 1:nruns){
      trainset=sample(x = 1:dataN,size = trainN,replace = rep)
      testset=setdiff(1:dataN,trainset)
      trainData=modData[trainset,]
      testData=modData[testset,]
      ttmod_call=mod$call
      ttmod_call[[3]]=trainData
      ttmod=eval(ttmod_call)
      sttmod=summary(ttmod)
      ptt=predict(ttmod,newdata=testData)
      opmod=lm(testData$RY~ptt)
      sopmod=summary(opmod)
      rsqtt[i]=unlist(sopmod[metric])
    }
  }
  return(rsqtt)
}

#___________________________#
# Load Modeling Dataframe (CC) --------------------------------------------

#Model variables: 
# Residence Time (use duration in days represented by bow tie footprint) - not yet! (just use 3 days)
# Benthic Indices (CALC_m2, SGi_m2, ALGi_m2)
# PAR (PAR_MODIS_8DAY, PAR_MODIS_DAILY, PAR_MODIS_MON)
# Hour of day (hrod.lst)
# Volume


CCfull = read.csv('/Users/heidi.k.hirsh/Desktop/CCmodel_allYears_dec16.csv')
dim(CCfull)# 9639   87 

names(CCfull)

#only use backward bows 
CCfull = CCfull[which(CCfull$simu=='backward'),] #should already be only backward (still 9254 x 88)
dim(CCfull) # 9639   87 (already only backward)

# test= CCfull[which(CCfull$Zone=='Inshore'),]
# test= CCfull[which(CCfull$Zone=='Mid channel'),]
# test= CCfull[which(CCfull$Zone=='Offshore'),]
# test= CCfull[which(CCfull$Zone=='Oceanic'),]
# test= CCfull[which(CCfull$Zone!='Oceanic'),]  #not oceanic!
# dim(test)

#limit to only inshore: 
CCfull = CCfull[which(CCfull$Zone=='Inshore'),]
# CCfull = CCfull[which(CCfull$Zone=='Mid channel'),]
# CCfull = CCfull[which(CCfull$Zone!='Oceanic'),]  #not oceanic!
dim(CCfull) #2744   87
unique(CCfull$Zone)


# CCfull$inverseVol = 1/CCfull$bowVol_m3 
CCfull$inverseVol = 1/CCfull$bwVl_m3
CCfull$inverseVol = 1/CCfull$volume

CCfull$invHab = 1/CCfull$pointDepth
# CCfull$bowVol_m3
# CCfull$inverseVol


# unique(CCfull$duratin)
# #reformat duration (RT) column 
# CC$Duration = paste0('Dur_',CC$duratin,'_days')
# # CC$Duration


bbLat = 25.1
bbLon = -80.25
# distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine)

CCfull[, 'distBB.m'] = NA
for(d in 1:length(CCfull[,1])){CCfull$distBB.m[d] = distm(c(bbLat,CCfull$Latitude[d]),c(bbLon,CCfull$Longitude[d]),distGeo)}

CCfull$distBB.m
CCfull$distBB.km = CCfull$distBB.m/1000
CCfull$distBB.km #too long!
unique(CCfull$distBB.m)
CCfull$invDistBB.m = 1/CCfull$distBB.m


#unique numeric site IDS
CCfull = transform(CCfull, numID = as.numeric(factor(SiteID)))


# plot(CCfull$numID,CCfull$distBB.m)
hBB =hclust(dist(CCfull$distBB.m))
# plot(hBB)

CCfull$distIndex = cutree(hBB,k=7) #other choices: 2, 4, 7, 12


# ggplot(CCfull,aes(x=numID,y=distBB.m,color=as.factor(distIndex)))+
#   geom_point(size=3,pch=1)+
#   theme_bw()






ModCol=NULL

##Choose response variables: 
RYs = c("DIC_delta","TA_delta","ARAG_delta")

##Choose PAR
# PARs=c("PAR_MODIS_8DAY","PAR_MODIS_DAILY","PAR_MODIS_MON")
# PARs=c("PAR_MODIS_8DAY")
PARs=c("PAR_MODIS_MON")

# ##Choose "Residence Time" (for now we are using "duration" (number of days in bow tie))
# RTs = c("Dur_1_days","Dur_2_days","Dur_3_days","Dur_4_days","Dur_5_days","Dur_6_days","Dur_7_days")


# 
# RYi=1
# PARi=1
# RTi=7

for(RYi in 1:length(RYs)){
  for(PARi in 1:length(PARs)){
    for(RTi in 1:length(unique(CCfull$duratin))){
      
      dim(CCfull)
      CC = CCfull[which(CCfull$duratin==RTi),]
      dim(CC)
      
      RYchosen.txt=RYs[RYi]       
      CC$RY=CC[,RYchosen.txt]
      PARchosen.txt=PARs[PARi]
      CC$PARchosen=CC[,PARchosen.txt]
      CC.=CC[which(!is.na(CC$PARchosen)),]
      dim(CC.)
      
      # trainset=sample(1:nrow(CC.),round(.8*nrow(CC.)),replace=F)
      # testset=setdiff(1:nrow(CC.),trainset)
      # trainset
      # testset
      # length(trainset) #51
      # length(testset) #13
      

      #very very simple
      # lm(formula = RY ~ (invDistBB.m:DICoce_mean) + I(invHab * inverseVol) * 
      #      (CALC_m2 + ALGi_m2 + SGi_m2) * (PARchosen + hrod.lst), data = CC.[trainset, 
      #      ])
      
      # m_simp = lm(RY~ (invDistBB.m:DICoce_mean) +
      #               I(invHab *inverseVol * CALC_m2*PARchosen) +
      #               I(invHab *inverseVol * CALC_m2*hrod.lst) +
      #               I(invHab *inverseVol * ALGi_m2*PARchosen) +
      #               I(invHab *inverseVol * ALGi_m2*hrod.lst) +
      #               I(invHab *inverseVol * SGi_m2*PARchosen) +
      #               I(invHab *inverseVol * SGi_m2*hrod.lst),
      #             data=CC.[trainset,])
      
      # m_simp = lm(RY~ (invDistBB.m:DICoce_mean) +
      #               invHab *
      #               inverseVol *
      #               (I(CALC_m2*PARchosen) +
      #               I(CALC_m2*hrod.lst) +
      #               I(ALGi_m2*PARchosen) +
      #               I(ALGi_m2*hrod.lst) +
      #               I(SGi_m2*PARchosen) +
      #               I(SGi_m2*hrod.lst)), 
      #             data=CC.[trainset,])
      
      # sumMsimp= summary(m_simp)
      # sumMsimp
      # #new model
      # m_new = lm(RY~ invDistBB.m *
      #   (Toce_mean+Soce_mean+TAoce_mean+DICoce_mean+ARAGoce_mean) +
      #     invHab * (CALC_m2+ALGi_m2+SGi_m2) *
      #     (Temperature_C+Salinity_CTD+PARchosen+hrod.lst+Month)* #+DIC_umol_kg+TA_umol_kg+Aragonite_Sat_W)
      #     inverseVol,
      #   data=CC.[trainset,])
      # 
      # sumMnew= summary(m_new)
      # 
      # smnew.df = sumMnew$coefficients %>% as.data.frame()
      # smnew.df[order(abs(smnew.df$`t value`),decreasing=T),]

      # https://bookdown.org/carillitony/bailey/chp7.html


      # m_new1 = lm(RY~ invDistBB.m +
      #              (Toce_mean+Soce_mean+TAoce_mean+DICoce_mean+ARAGoce_mean) +
      #              invHab + (CALC_m2+ALGi_m2+SGi_m2) *
      #              (Temperature_C+Salinity_CTD+PARchosen+hrod.lst+Month)* #+DIC_umol_kg+TA_umol_kg+Aragonite_Sat_W)
      #              inverseVol,
      #            data=CC.[trainset,])
      # 
      # 
      # pm_new = predict(m_new,newdata=CC.[testset,],type="response")
      # 
      # plot(CC.[testset,"RY"],pm_new)
      # cor(CC.[testset,"RY"],pm_new)^2
      # 
      # sm_new = step(m_new)

# 
#       summary(m_new)
#       AIC(m_new)
#       anova(m_new,m0.7)
# 
#       anova(m_new,m_new1,sm_new)
#       AIC(m_new,m_new1,sm_new)
      
      
      
      
      
      
      
      #just benthic indices (coral, seagrass, algae)
      m0=lm(RY~
              CALC_m2+
              ALGi_m2+
              SGi_m2, 
            data=CC.)
      sm0=summary(m0)
      sm0$adj.r.squared
      m0.tt=mean(traintest(m0))
      
      #add hour of day (solar)
      m0.1=lm(RY~
                CALC_m2+
                ALGi_m2+
                SGi_m2+
                hrod.lst,
              data=CC.)
      sm0.1=summary(m0.1)
      sm0.1$adj.r.squared
      m0.1.tt=mean(traintest(m0.1))
      
      #add PAR
      m0.2=lm(RY~
                CALC_m2+
                ALGi_m2+
                SGi_m2+
                hrod.lst+
                PARchosen,
              data=CC.)
      sm0.2=summary(m0.2)
      sm0.2$adj.r.squared
      m0.2.tt=mean(traintest(m0.2))
      
      #add ocean
      m0.3=lm(RY~
                CALC_m2+
                ALGi_m2+
                SGi_m2+
                hrod.lst+
                PARchosen+
                TAoce_mean+DICoce_mean+ARAGoce_mean,#+
                # Toce_mean+Soce_mean,
              data=CC.)
      sm0.3=summary(m0.3)
      sm0.3$adj.r.squared
      m0.3.tt=mean(traintest(m0.3))
      
      #add volume 
      m0.33=lm(RY~
                CALC_m2+
                ALGi_m2+
                SGi_m2+
                hrod.lst+
                PARchosen+
                TAoce_mean+DICoce_mean+ARAGoce_mean+
              # Toce_mean+Soce_mean,
                inverseVol,
                # bowVol_m3,
              data=CC.)
      sm0.33=summary(m0.3)
      sm0.33$adj.r.squared
      m0.33.tt=mean(traintest(m0.33))
      
      #add depth (rename models so they are in order with sensible names)
      m0.4=lm(RY~
                CALC_m2+
                ALGi_m2+
                SGi_m2+
                hrod.lst+
                PARchosen+
                TAoce_mean+DICoce_mean+ARAGoce_mean+
              # Toce_mean+Soce_mean,
                inverseVol+
                # bowVol_m3+
                pointDepth,
              data=CC.)
      sm0.4=summary(m0.4)
      sm0.4$adj.r.squared
      m0.4.tt=mean(traintest(m0.4))
      
      #add month
      m0.5=lm(RY~ 
                CALC_m2+
                ALGi_m2+
                SGi_m2+
                hrod.lst+
                PARchosen+
                inverseVol+
                TAoce_mean+DICoce_mean+ARAGoce_mean+
                # Toce_mean+Soce_mean,
                # bowVol_m3+
                pointDepth+ ##added depth!
                Month,
              data=CC.)
      sm0.5=summary(m0.5)
      sm0.5$adj.r.squared
      m0.5.tt=mean(traintest(m0.5))
      
      #add temperature 
      m0.6=lm(RY~ 
                CALC_m2+
                ALGi_m2+
                SGi_m2+
                hrod.lst+
                PARchosen+
                inverseVol+
                TAoce_mean+DICoce_mean+ARAGoce_mean+
                # Toce_mean+Soce_mean,
                # bowVol_m3+
                pointDepth+ 
                Month+
                Temperature_C,
              data=CC.)
      sm0.6=summary(m0.6)
      sm0.6$adj.r.squared
      m0.6.tt=mean(traintest(m0.6))
      
      #add salinity
      m0.66=lm(RY~ 
                CALC_m2+
                ALGi_m2+
                SGi_m2+
                hrod.lst+
                PARchosen+
                inverseVol+
                TAoce_mean+DICoce_mean+ARAGoce_mean+
                # Toce_mean+Soce_mean,
                # bowVol_m3+
                pointDepth+ 
                Month+
                Temperature_C+
                Salinity_Bottle,
              data=CC.)
      sm0.66=summary(m0.6)
      sm0.66$adj.r.squared
      m0.66.tt=mean(traintest(m0.66))
      
      #add region interaction 
      m0.7=lm(RY~ Sub_region*(
        CALC_m2+
          ALGi_m2+
          SGi_m2+
          hrod.lst+
          PARchosen+
          inverseVol+
          TAoce_mean+DICoce_mean+ARAGoce_mean+
          # Toce_mean+Soce_mean,
          # bowVol_m3+
          pointDepth+
          Month+
          Salinity_Bottle+
          Temperature_C),
        data=CC.)
      sm0.7=summary(m0.7)
      sm0.7$adj.r.squared
      m0.7.tt=mean(traintest(m0.7))
      AIC(m0.7)
      
      # #add continuous distance variable (distBB.m)
      # m0.8=lm(RY~ distBB.m*(
      #           CALC_m2+
      #           ALGi_m2+
      #           SGi_m2+
      #           hrod.lst+
      #           PARchosen+
      #           inverseVol+
      #           TAoce_mean+DICoce_mean+ARAGoce_mean+
      #           # Toce_mean+Soce_mean,
      #           # bowVol_m3+
      #           pointDepth+
      #           Month+
      #           Salinity_Bottle+
      #           Temperature_C),
      #         data=CC.)
      # sm0.8=summary(m0.8)
      # sm0.8$adj.r.squared
      # m0.8.tt=mean(traintest(m0.8))
      # 
      # #add categorical distance variable (distIndex))
      # m0.9=lm(RY~ distIndex*(
      #           CALC_m2+
      #           ALGi_m2+
      #           SGi_m2+
      #           hrod.lst+
      #           PARchosen+
      #           inverseVol+
      #           TAoce_mean+DICoce_mean+ARAGoce_mean+
      #           # Toce_mean+Soce_mean,
      #           # bowVol_m3+
      #           pointDepth+
      #           Month+
      #           Salinity_Bottle+
      #           Temperature_C),
      #         data=CC.)
      # sm0.9=summary(m0.9)
      # sm0.9$adj.r.squared
      # m0.9.tt=mean(traintest(m0.9))
      

    
      
      ModAdd=data.frame(
        ModName=c("BenT0","BenT0.Hr","BenT0.Hr.PAR","BenT0.Hr.PAR.Oce","BenT0.Hr.PAR.Oce.Vol",
                  "BenT0.Hr.PAR.Oce.Vol.Dep","BenT0.Hr.PAR.Oce.Vol.Dep.Mon",
                  "BenT0.Hr.PAR.Oce.Vol.Dep.Mon.T","BenT0.Hr.PAR.Oce.Vol.Dep.Mon.T.S","BenT0.Hr.PAR.Oce.Vol.Dep.Mon.T.S.Reg"),
                  #"BenT0.Hr.PAR.Oce.Vol.Dep.Mon.T.S.Dcont","BenT0.Hr.PAR.Oce.Vol.Dep.Mon.T.S.Dcat"),
       Adj.Rsq=c(sm0$adj.r.squared,sm0.1$adj.r.squared,sm0.2$adj.r.squared,sm0.3$adj.r.squared,sm0.33$adj.r.squared,
                            sm0.4$adj.r.squared,sm0.5$adj.r.squared,sm0.6$adj.r.squared,sm0.66$adj.r.squared,sm0.7$adj.r.squared),
                            # sm0.8$adj.r.squared,sm0.9$adj.r.squared),
                            AIC=AIC(m0,m0.1,m0.2,m0.3,m0.33,m0.4,m0.5,m0.6,m0.66,m0.7)$AIC,
                            dAIC=min(AIC(m0,m0.1,m0.2,m0.3,m0.33,m0.4,m0.5,m0.6,m0.66,m0.7)$AIC)-AIC(m0,m0.1,m0.2,m0.3,m0.33,m0.4,m0.5,m0.6,m0.66,m0.7)$AIC,
                            BIC=BIC(m0,m0.1,m0.2,m0.3,m0.33,m0.4,m0.5,m0.6,m0.66,m0.7)$BIC,
                            dBIC=min(BIC(m0,m0.1,m0.2,m0.3,m0.33,m0.4,m0.5,m0.6,m0.66,m0.7)$BIC)-BIC(m0,m0.1,m0.2,m0.3,m0.33,m0.4,m0.5,m0.6,m0.66,m0.7)$BIC,
                            Adj.Rsq.TrainTest=c(m0.tt,m0.1.tt,m0.2.tt,m0.3.tt,m0.33.tt,m0.4.tt,m0.5.tt,m0.6.tt,m0.66.tt,m0.7.tt),
                            Response=RYchosen.txt,
                            PAR_chosen=PARchosen.txt,
                            N=nrow(CC.),
                            RTduration=paste0(RTi,'_days'))
                  
        ModAdd$ModName=factor(ModAdd$ModName,levels=c("BenT0","BenT0.Hr","BenT0.Hr.PAR",
                                                                "BenT0.Hr.PAR.Oce","BenT0.Hr.PAR.Oce.Vol",
                                                                "BenT0.Hr.PAR.Oce.Vol.Dep","BenT0.Hr.PAR.Oce.Vol.Dep.Mon",
                                                                "BenT0.Hr.PAR.Oce.Vol.Dep.Mon.T","BenT0.Hr.PAR.Oce.Vol.Dep.Mon.T.S",
                                                                "BenT0.Hr.PAR.Oce.Vol.Dep.Mon.T.S.Reg"))
                  
      
      # ModAdd=data.frame(
      #   # ModName=c(paste0("BenT0.",RTi,'days'),paste0("BenT0.Hr.",RTi,'days'),paste0("BenT0.Hr.PAR.",RTi,'days'),paste0("BenT0.Hr.PAR.Vol.",RTi,'days')),
      #   # ModName=c("BenT0","BenT0.Hr","BenT0.Hr.PAR","BenT0.Hr.PAR.Vol","BenT0.Hr.PAR.Vol.Mon","BenT0.Hr.PAR.Vol.Mon.Reg"),
      #   ModName=c("BenT0","BenT0.Hr","BenT0.Hr.PAR","BenT0.Hr.PAR.Vol","BenT0.Hr.PAR.Vol.Dep","BenT0.Hr.PAR.Vol.Dep.Mon","BenT0.Hr.PAR.Vol.Dep.Mon.TS",
      #             "BenT0.Hr.PAR.Vol.Dep.Mon.TS.Reg","BenT0.Hr.PAR.Vol.Dep.Mon.TS.Dcont","BenT0.Hr.PAR.Vol.Dep.Mon.TS.Dcat"),
      #   Adj.Rsq=c(sm0$adj.r.squared,sm0.1$adj.r.squared,sm0.2$adj.r.squared,sm0.3$adj.r.squared,sm0.4$adj.r.squared,sm0.5$adj.r.squared,sm0.6$adj.r.squared,
      #             sm0.7$adj.r.squared,sm0.8$adj.r.squared,sm0.9$adj.r.squared),
      #   AIC=AIC(m0,m0.1,m0.2,m0.3,m0.4,m0.5,m0.6,m0.7,m0.8,m0.9)$AIC,
      #   dAIC=min(AIC(m0,m0.1,m0.2,m0.3,m0.4,m0.5,m0.6,m0.7,m0.8,m0.9)$AIC)-AIC(m0,m0.1,m0.2,m0.3,m0.4,m0.5,m0.6,m0.7,m0.8,m0.9)$AIC,
      #   BIC=BIC(m0,m0.1,m0.2,m0.3,m0.4,m0.5,m0.6,m0.7,m0.8,m0.9)$BIC,
      #   dBIC=min(BIC(m0,m0.1,m0.2,m0.3,m0.4,m0.5,m0.6,m0.7,m0.8,m0.9)$BIC)-BIC(m0,m0.1,m0.2,m0.3,m0.4,m0.5,m0.6,m0.7,m0.8,m0.9)$BIC,
      #   Adj.Rsq.TrainTest=c(m0.tt,m0.1.tt,m0.2.tt,m0.3.tt,m0.4.tt,m0.5.tt,m0.6.tt,m0.7.tt,m0.8.tt,m0.9.tt),
      #   Response=RYchosen.txt,
      #   PAR_chosen=PARchosen.txt,
      #   N=nrow(CC.),
      #   RTduration=paste0(RTi,'_days'))
      # 
      # ModAdd$ModName=factor(ModAdd$ModName,levels=c("BenT0","BenT0.Hr","BenT0.Hr.PAR","BenT0.Hr.PAR.Vol","BenT0.Hr.PAR.Vol.Dep","BenT0.Hr.PAR.Vol.Dep.Mon","BenT0.Hr.PAR.Vol.Dep.Mon.TS",
      #                                               "BenT0.Hr.PAR.Vol.Dep.Mon.TS.Reg","BenT0.Hr.PAR.Vol.Dep.Mon.TS.Dcont","BenT0.Hr.PAR.Vol.Dep.Mon.TS.Dcat"))
      # ModAdd
      
      ModCol=rbind(ModCol,ModAdd)
      
    }
  }
}

# View(ModCol) #should be 252 rows (3responsesx3PARSx7durationsx4models)
dim(ModCol)

ModColDups = ModCol %>% get_dupes(dAIC,Adj.Rsq)
dim(ModColDups)



#response
PRlu=c("DIC","TA","Aragonite Saturation")
names(PRlu)=unique(ModCol$Response)
ModCol$PrettyResponse=PRlu[ModCol$Response]
ModCol$PrettyResponse=factor(ModCol$PrettyResponse,levels = c("DIC","TA","Aragonite Saturation"))


#HR?
HR_inclLU=c("NO","YES","YES","YES","YES","YES","YES","YES","YES","YES")
names(HR_inclLU)=unique(ModCol$ModName)
ModCol$HR_included=HR_inclLU[ModCol$ModName]

# PAR USED
PAR_chosenlu=c("NO","NO","YES","YES","YES","YES","YES","YES","YES","YES")
names(PAR_chosenlu)=unique(ModCol$ModName)
ModCol$PAR_included=PAR_chosenlu[ModCol$ModName]

# OCEAN USED
OCE_chosenlu=c("NO","NO","NO","YES","YES","YES","YES","YES","YES","YES")
names(OCE_chosenlu)=unique(ModCol$ModName)
ModCol$OCE_included=OCE_chosenlu[ModCol$ModName]

#Volume?
VOL_inclLU=c("NO","NO","NO","NO","YES","YES","YES","YES","YES","YES")
names(VOL_inclLU)=unique(ModCol$ModName)
ModCol$VOL_included=VOL_inclLU[ModCol$ModName]

#Depth?
DEP_inclLU=c("NO","NO","NO","NO","NO","YES","YES","YES","YES","YES")
names(DEP_inclLU)=unique(ModCol$ModName)
ModCol$DEP_included=DEP_inclLU[ModCol$ModName]

#Month?
MON_inclLU=c("NO","NO","NO","NO","NO","NO","YES","YES","YES","YES")
names(MON_inclLU)=unique(ModCol$ModName)
ModCol$MON_included=MON_inclLU[ModCol$ModName]

#Temp?
TEMP_inclLU=c("NO","NO","NO","NO","NO","NO","NO","YES","YES","YES")
names(TEMP_inclLU)=unique(ModCol$ModName)
ModCol$TEMP_included=TEMP_inclLU[ModCol$ModName]

#Sal?
SAL_inclLU=c("NO","NO","NO","NO","NO","NO","NO","NO","YES","YES")
names(SAL_inclLU)=unique(ModCol$ModName)
ModCol$SAL_included=SAL_inclLU[ModCol$ModName]

#Region Interaction?
REG_inclLU=c("NO","NO","NO","NO","NO","NO","NO","NO","NO","YES")
names(REG_inclLU)=unique(ModCol$ModName)
ModCol$REG_included=REG_inclLU[ModCol$ModName]


#update chosens
ModCol$PAR_chosen[which(ModCol$PAR_included=="NO")]='PAR_NONE_NONE'

#Build ID
ModCol$ModID=apply(ModCol[,c("ModName","Response","PAR_chosen")],1,paste0,collapse="_")

#Included Case Variable
iALL=1:nrow(ModCol)
iHR=which(ModCol$HR_included=="YES")
iPAR=which(ModCol$PAR_included=="YES")
iOCE=which(ModCol$OCE_included=="YES")
iVOL=which(ModCol$VOL_included=="YES")
iDEP=which(ModCol$DEP_included=="YES")
iMON=which(ModCol$MON_included=="YES")
iTEMP=which(ModCol$TEMP_included=="YES")
iSAL=which(ModCol$SAL_included=="YES")
iREG=which(ModCol$REG_included=="YES")

#THERE SHOULD BE 10
ModCol$HR.PAR.VOL.DEP.MON.TEMP.REG=                                      "BEN XhrX XparX XoceX XvolX XdepX XmonX XtempX XsalX XregX"
ModCol$HR.PAR.VOL.DEP.MON.TEMP.REG[setdiff(setdiff(iHR,iPAR),iOCE)]=     "BEN HR XparX XoceX XvolX XdepX XmonX XtempX XsalX XregX"
ModCol$HR.PAR.VOL.DEP.MON.TEMP.REG[setdiff(intersect(iHR,iPAR),iOCE)]=   "BEN HR PAR XoceX XvolX XdepX XmonX XtempX XsalX XregX"
ModCol$HR.PAR.VOL.DEP.MON.TEMP.REG[intersect(intersect(iHR,iPAR),iOCE)]= "BEN HR PAR OCE XvolX XdepX XmonX XtempX XsalX XregX"
ModCol$HR.PAR.VOL.DEP.MON.TEMP.REG[intersect(intersect(iHR,iOCE),iVOL)]= "BEN HR PAR OCE VOL XdepX XmonX XtempX XsalX XregX"
ModCol$HR.PAR.VOL.DEP.MON.TEMP.REG[intersect(intersect(iHR,iVOL),iDEP)]= "BEN HR PAR OCE VOL DEP XmonX XtempX XsalX XregX"
ModCol$HR.PAR.VOL.DEP.MON.TEMP.REG[intersect(intersect(iHR,iDEP),iMON)]= "BEN HR PAR OCE VOL DEP MON XtempX XsalX XregX"
ModCol$HR.PAR.VOL.DEP.MON.TEMP.REG[intersect(intersect(iHR,iMON),iTEMP)]="BEN HR PAR OCE VOL DEP MON TEMP XsalX XregX"
ModCol$HR.PAR.VOL.DEP.MON.TEMP.REG[intersect(intersect(iHR,iTEMP),iSAL)]="BEN HR PAR OCE VOL DEP MON TEMP SAL XregX"
ModCol$HR.PAR.VOL.DEP.MON.TEMP.REG[intersect(intersect(iHR,iSAL),iREG)]= "BEN HR PAR OCE VOL DEP MON TEMP SAL REG"

length(unique(ModCol$HR.PAR.VOL.DEP.MON.TEMP.REG))

# ModCol$HR.PAR.VOL="BEN XhrX XparX XvolX"
# ModCol$HR.PAR.VOL[setdiff(setdiff(iHR,iPAR),iVOL)]="BEN HR XparX XvolX"
# ModCol$HR.PAR.VOL[setdiff(intersect(iHR,iPAR),iVOL)]="BEN HR PAR XvolX"
# ModCol$HR.PAR.VOL[intersect(intersect(iHR,iPAR),iVOL)]="BEN HR PAR VOL"
# ModCol$HR.PAR.VOL=factor(ModCol$HR.PAR.VOL)
# unique(ModCol$HR.PAR.VOL)
#add a variable to communicate which duration is used


# xcross= c(-400,-250,-500)
# ycross = c(0,.0125,0.005)
# xstar= c(0,0,0)
# ystar=c(.15,.12,.3)
# PrettyResponse= as.factor(c('DIC','TA','Aragonite Saturation'))
# markers = data.frame(PrettyResponse,xcross,ycross,xstar,ystar)

#just dic and ta:
xcross= c(-400,-250)
ycross = c(0,.0125)
xstar= c(0,0)
ystar=c(.15,.12)
PrettyResponse= as.factor(c('DIC','TA'))
markers = data.frame(PrettyResponse,xcross,ycross,xstar,ystar)


model_shapes        <- c(4,3,8,21,22,24,25,23,9,11) #need 10
# model_shapes        <- c(4,3,21,22,23,24,25,11,8,9) #need 10

length(unique(ModCol$HR.PAR.VOL.DEP.MON.TEMP.REG))


model_SHAPE_scale  <- scale_shape_manual(name = "Model Parameters Used", values = model_shapes,
                                         breaks = c("BEN XhrX XparX XoceX XvolX XdepX XmonX XtempX XsalX XregX","BEN HR XparX XoceX XvolX XdepX XmonX XtempX XsalX XregX",
                                                    "BEN HR PAR XoceX XvolX XdepX XmonX XtempX XsalX XregX","BEN HR PAR OCE XvolX XdepX XmonX XtempX XsalX XregX",      
                                                    "BEN HR PAR OCE VOL XdepX XmonX XtempX XsalX XregX","BEN HR PAR OCE VOL DEP XmonX XtempX XsalX XregX","BEN HR PAR OCE VOL DEP MON XtempX XsalX XregX",            
                                                    "BEN HR PAR OCE VOL DEP MON TEMP XsalX XregX","BEN HR PAR OCE VOL DEP MON TEMP SAL XregX","BEN HR PAR OCE VOL DEP MON TEMP SAL REG"),
                                         labels = c("Benthic Only", 
                                                    "Benthic + Hour of Day", 
                                                    "BEN + HR + PAR",
                                                    "BEN + HR + PAR + Oceanic",
                                                    "BEN + HR + PAR + OCE + 1/Volume",
                                                    "BEN + HR + PAR + OCE + 1/VOL + Depth",
                                                    "BEN + HR + PAR + OCE + 1/VOL + DEP + Month",
                                                    "BEN + HR + PAR + OCE + 1/VOL + DEP + MON + Temperature",
                                                    "BEN + HR + PAR + OCE + 1/VOL + DEP + MON + TEMP + SAL",
                                                    "BEN + HR + PAR + OCE + 1/VOL + DEP + MON + TEMP + SAL + Region(INT)"))


#10 models with distance

# model_SHAPE_scale  <- scale_shape_manual(name = "Model Parameters Used", values = model_shapes,
#                                          breaks = c("BEN XhrX XparX XvolX XmonX XregX","BEN HR XparX XvolX XdepX XmonX XtempX XregX","BEN HR PAR XvolX XdepX XmonX XtempX XregX",
#                                                     "BEN HR PAR VOL XdepX XmonX XtempX XregX","BEN HR PAR VOL DEP XmonX XtempX XregX","BEN HR PAR VOL DEP MON XtempX XregX","BEN HR PAR VOL DEP MON TEMP XregX",
#                                                     "BEN HR PAR VOL DEP MON TEMP REG","BEN HR PAR VOL DEP MON TEMP DCON","BEN HR PAR VOL DEP MON TEMP DCAT"),
#                                          labels = c("Benthic Only", "Benthic + Hour of Day", "BEN + HR + PAR",
#                                                     # "BEN + HR + PAR + Volume","BEN + HR + PAR + VOL + Depth","BEN + HR + PAR + VOL + DEP + Month","BEN + HR + PAR + VOL + DEP + MON + Temperature",
#                                                     # "BEN + HR + PAR + VOL + DEP + MON + TEMP + Region(INT)",
#                                                     # "BEN + HR + PAR + VOL + DEP + MON + TEMP + Dist_cont(INT)","BEN + HR + PAR + VOL + DEP + MON + TEMP + Dist_cat(INT)"))
#                                           
#                                                     #labels for inverse volume models
#                                                     "BEN + HR + PAR + 1/Volume","BEN + HR + PAR + 1/VOL + Depth","BEN + HR + PAR + 1/VOL + DEP + Month","BEN + HR + PAR + 1/VOL + DEP + MON + Temperature",
#                                                     "BEN + HR + PAR + 1/VOL + DEP + MON + TEMP + Region(INT)",
#                                                     "BEN + HR + PAR + 1/VOL + DEP + MON + TEMP + Dist_cont(INT)","BEN + HR + PAR + 1/VOL + DEP + MON + TEMP + Dist_cat(INT)"))

# model_shapes        <- c(4,21,22,23,24,25,8,9)
# model_SHAPE_scale  <- scale_shape_manual(name = "Model Parameters Used", values = model_shapes,
#                                          breaks = c("BEN XhrX XparX XvolX XmonX XregX", "BEN HR XparX XvolX XmonX XregX", "BEN HR PAR XvolX XmonX XregX",
#                                                     "BEN HR PAR VOL XmonX XregX", "BEN HR PAR VOL MON XregX","BEN HR PAR VOL MON REG",
#                                                     "BEN HR PAR VOL MON DCON","BEN HR PAR VOL MON DCAT"),
#                                          labels = c("Benthic Only", "Benthic + Hour of Day", "BEN + HR + PAR",
#                                                     "BEN + HR + PAR + Volume","BEN + HR + PAR + VOL + Month","BEN + HR + PAR + VOL + MON + Region(int)",
#                                                     "BEN + HR + PAR + VOL + MON + Dist_continuous","BEN + HR + PAR + VOL + MON + Dist_categorical"))


# model_shapes        <- c(4,21,22,23,24,25)
# model_SHAPE_scale  <- scale_shape_manual(name = "Model Parameters Used", values = model_shapes,
#                                          breaks = c("BEN XhrX XparX XvolX XmonX XregX", "BEN HR XparX XvolX XmonX XregX", "BEN HR PAR XvolX XmonX XregX",
#                                                     "BEN HR PAR VOL XmonX XregX", "BEN HR PAR VOL MON XregX","BEN HR PAR VOL MON REG"),
#                                          labels = c("Benthic Only", "Benthic + Hour of Day", "BEN + HR + PAR",
#                                                     "BEN + HR + PAR + Volume","BEN + HR + PAR + VOL + Month","BEN + HR + PAR + VOL + MON + Region(int)"))

# model_shapes        <- c(22,23,24,21)
# model_SHAPE_scale  <- scale_shape_manual(name = "Model Parameters Used", values = model_shapes,
#                                               breaks = c("BEN XhrX XparX XvolX", "BEN HR XparX XvolX", "BEN HR PAR XvolX", "BEN HR PAR VOL"),
#                                               labels = c("Benthic Only", "Benthic + Hour of Day", "Benthic + HR + PAR", "Benthic + HR + PAR + Volume"))

par_colors        <- c("#94940F","#00B4E0","#AC4694","black")
# parF_colors        <- c("#0F4694","#2472E0","#00B5E0","white")
par_COLOR_scale       <- scale_color_manual(name = "Model PAR Used", values = par_colors,
                                              breaks = c("PAR_MODIS_8DAY", "PAR_MODIS_DAILY", "PAR_MODIS_MON", "PAR_NONE_NONE"),
                                              labels =c("MODIS 8DAY", "MODIS DAILY", "MODIS MONTHLY", "NO PAR"))
par_FILL_scale       <- scale_fill_manual(name = "Model PAR Used", values = par_colors,
                                            breaks = c("PAR_MODIS_8DAY", "PAR_MODIS_DAILY", "PAR_MODIS_MON", "PAR_NONE_NONE"),
                                            labels =c("MODIS 8DAY", "MODIS DAILY", "MODIS MONTHLY", "NO PAR"))

# RT_COLOR_scale = 

#super simple
# dim(ModCol)
# sub=subset(ModCol,PAR_chosen  %in% c("PAR_MODIS_MON","PAR_NONE_NONE"))
# dim(sub)
sub=ModCol
dim(ModCol) #210 x 23 
table(ModCol$RTduration)

# sub=subset(ModCol,HR.PAR.VOL.MON.REG %in% c("BEN XhrX XparX XvolX XmonX XregX", "BEN HR XparX XvolX XmonX XregX", "BEN HR PAR XvolX XmonX XregX",
#                                             "BEN HR PAR VOL XmonX XregX", "BEN HR PAR VOL MON XregX","BEN HR PAR VOL MON REG"))

p=6
sub=ModCol
# sub=subset(ModCol,RTduration=='3_days')

sub=subset(sub,PrettyResponse %in% c('DIC','TA'))
# ModPlot = ggplot(sub,aes(x=dAIC,y=Adj.Rsq,shape=HR.PAR.VOL.DEP.MON.TEMP.REG),color='black')+#,fill=PAR_chosen))+
ModPlot = ggplot(sub,aes(x=dAIC,y=Adj.Rsq,color=RTduration,shape=HR.PAR.VOL.DEP.MON.TEMP.REG))+#,fill=PAR_chosen))+
  # ModPlot = ggplot(sub,aes(x=dAIC,y=Adj.Rsq,shape=HR.PAR.VOL.MON.REG))+#,fill=PAR_chosen))+
  # ModPlot = ggplot(sub,aes(x=dAIC,y=Adj.Rsq,color=RTduration,shape=HR.PAR.VOL))+#,fill=PAR_chosen))+
  # facet_wrap(~PrettyResponse,scales="free")+
  facet_wrap(~PrettyResponse)+
  # facet_grid(PAR_chosen~PrettyResponse,scales='free')+
  # geom_point(data=markers,aes(x=xstar,y=ystar),color='gold',shape=8,fill='gold',size=4.2,stroke=1)+
  # geom_point(data=markers,aes(x=xcross,y=ycross),color='red',shape=4,fill='gold',size=4.2,stroke=1)+
  # geom_point(data=markers,aes(x=xstar,y=ystar),color='white',shape=8,fill='white',size=.1,stroke=.1)+
  # geom_point(data=markers,aes(x=xcross,y=ycross),color='white',shape=4,fill='white',size=.1,stroke=.1)+
  geom_point(data=sub,stroke=.77,size=p)+
  # geom_point(data=subset(sub,REG_inclLU=='YES'),stroke=.77,size=3,alpha=1,aes(fill=REG_included,color=MON_included))+
  model_SHAPE_scale+
  # par_COLOR_scale+
  # par_FILL_scale+
  theme_bw()+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=18),
        strip.text= element_text(size=16))
  # theme(legend.position = "bottom",legend.box='vertical')

ModPlot


# ggsave(plot = ModPlot,filename = '/Users/heidihirsh/Desktop/flkMOD.png',units="in",width=12,height=9)
# # ggsave(plot = ModPlot,filename = '/Users/heidihirsh/Desktop/flkMOD.svg',units="in",width=12,height=9)
# ggsave(plot = ModPlot,filename = '/Users/heidihirsh/Desktop/flkMOD_inshore.svg',units="in",width=12,height=9)
# ggsave(plot = ModPlot,filename = '/Users/heidihirsh/Desktop/flkMOD_inshore+S.svg',units="in",width=12,height=9)
# ggsave(plot = ModPlot,filename = '/Users/heidihirsh/Desktop/flkMOD_inshore+S+ocean.svg',units="in",width=12,height=9)
# ggsave(plot = ModPlot,filename = '/Users/heidihirsh/Desktop/flkMOD_inshore+S+ocean_all.png',units="in",width=12,height=9)
# ggsave(plot = ModPlot,filename = '/Users/heidihirsh/Desktop/flkMOD_inshore+S+ocean_allRTs.svg',units="in",width=12,height=9)
# ggsave(plot = ModPlot,filename = '/Users/heidihirsh/Desktop/flkMOD_inshore+S+ocean_3dside.svg',units="in",width=12,height=9)

#ggsave(plot = ModPlot,filename = '/Users/heidihirsh/Desktop/flkMOD_inshore+S+ocean_3dside_CONCAVEv1_allRTS.png',units="in",width=14,height=9)


##end NASA

 
ModPlot2 = ggplot(sub,aes(x=dAIC,y=Adj.Rsq,color=RTduration,shape=HR.PAR.VOL.DEP.MON.TEMP.REG))+#,fill=PAR_chosen))+
# ModPlot = ggplot(sub,aes(x=dAIC,y=Adj.Rsq,shape=HR.PAR.VOL.MON.REG))+#,fill=PAR_chosen))+
# ModPlot = ggplot(sub,aes(x=dAIC,y=Adj.Rsq,color=RTduration,shape=HR.PAR.VOL))+#,fill=PAR_chosen))+
  # facet_wrap(~PrettyResponse,scales="free")+
  facet_wrap(~PrettyResponse)+
  # facet_grid(PAR_chosen~PrettyResponse,scales='free')+
  # geom_point(data=markers,aes(x=xstar,y=ystar),color='gold',shape=8,fill='gold',size=4.2,stroke=1)+
  # geom_point(data=markers,aes(x=xcross,y=ycross),color='red',shape=4,fill='gold',size=4.2,stroke=1)+
  # geom_point(data=markers,aes(x=xstar,y=ystar),color='white',shape=8,fill='white',size=.1,stroke=.1)+
  # geom_point(data=markers,aes(x=xcross,y=ycross),color='white',shape=4,fill='white',size=.1,stroke=.1)+
  geom_point(data=sub,stroke=.77,size=3)+
  # geom_point(data=subset(sub,REG_inclLU=='YES'),stroke=.77,size=3,alpha=1,aes(fill=REG_included,color=MON_included))+
  model_SHAPE_scale+
  # par_COLOR_scale+
  # par_FILL_scale+
  theme_bw()
ModPlot2

# ggsave(plot =ModPlot,filename = '/Users/heidihirsh/Desktop/PrelimModelSkill.png',units="in",width=14,height=6)
# ggsave(plot =ModPlot,filename = '/Users/heidihirsh/Desktop/PrelimModelSkill_dur.png',units="in",width=14,height=6)
# ggsave(plot =ModPlot,filename = '/Users/heidihirsh/Desktop/PrelimModelSkill_region.png',units="in",width=14,height=6)
# ggsave(plot =ModPlot,filename = '/Users/heidihirsh/Desktop/PrelimModelSkill_monthRegion-all.png',units="in",width=14,height=6)
# ggsave(plot =ModPlot,filename = '/Users/heidihirsh/Desktop/PrelimModelSkill_monthRegion-only7.png',units="in",width=14,height=6)
# ggsave(plot =ModPlot,filename = '/Users/heidihirsh/Desktop/PrelimModelSkill_distance.png',units="in",width=14,height=6)
# ggsave(plot =ModPlot,filename = '/Users/heidihirsh/Desktop/PrelimModelSkill_regionInt.png',units="in",width=14,height=6)

# ggsave(plot =ModPlot,filename = '/Users/heidihirsh/Desktop/PrelimModelSkill_DistanceInt.png',units="in",width=14,height=6)

# ggsave(plot =ModPlot,filename = '/Users/heidihirsh/Desktop/PrelimModelSkill_addTandD_inshore.png',units="in",width=18,height=8)
# ggsave(plot =ModPlot,filename = '/Users/heidihirsh/Desktop/PrelimModelSkill_addTandD_midChannel.png',units="in",width=18,height=8)
# ggsave(plot =ModPlot,filename = '/Users/heidihirsh/Desktop/PrelimModelSkill_addTandD_inMidOff.png',units="in",width=18,height=8)
# ggsave(plot =ModPlot,filename = '/Users/heidihirsh/Desktop/PrelimModelSkill_inMidOff_inverseVOL.png',units="in",width=18,height=8)
# ggsave(plot =ModPlot,filename = '/Users/heidihirsh/Desktop/PrelimModelSkill_inshore_inverseVOL.png',units="in",width=18,height=8)
# ggsave(plot =ModPlot,filename = '/Users/heidihirsh/Desktop/PrelimModelSkill_inshore_fixedDF.png',units="in",width=18,height=8)

# ggsave(plot =ModPlot,filename = '/Users/heidihirsh/Desktop/PrelimModelSkill_inshore_fixedDF_7.png',units="in",width=18,height=8)
# ggsave(plot =ModPlot,filename = '/Users/heidihirsh/Desktop/PrelimModelSkill_mid_fixedDF_7.png',units="in",width=18,height=8)
# ggsave(plot =ModPlot,filename = '/Users/heidihirsh/Desktop/PrelimModelSkill_all_fixedDF_7.png',units="in",width=18,height=8)


# ggsave(plot =ModPlot,filename = '/Users/heidihirsh/Desktop/PrelimModelSkill_allYears_allDays_inshore.png',units="in",width=18,height=8)

# ggsave(plot =ModPlot,filename = '/Users/heidihirsh/Desktop/PrelimModelSkill_allYears_allDays_allExceptOce.png',units="in",width=18,height=8)
