#make a dumb change

# 10/5/2023 I don't think this is current code. It has zero context. 

#___________________________#
# Add satellite PAR data --------------------------------------------------
CC$PAR_MODIS_DAILY=NA
CC$PAR_MODIS_8DAY=NA
CC$PAR_MODIS_MON=NA
plat=CC$LATITUDE_DEC
plon=CC$LONGITUDE_DEC
ptime=as.POSIXct(CC$TIMESTAMP_UTC)
for(i in 1:length(ptime)){
  par_g<-griddap("erdMH1par01day",
                 time=c(ptime[i],ptime[i]),
                 latitude=c(plat[i],plat[i]),
                 longitude=c(plon[i],plon[i]))
  CC$PAR_MODIS_DAILY[i]=par_g$data$par
  
  par_g8<-griddap("erdMH1par08day",
                  time=c(ptime[i],ptime[i]),
                  latitude=c(plat[i],plat[i]),
                  longitude=c(plon[i],plon[i]))
  CC$PAR_MODIS_8DAY[i]=par_g8$data$par
  
  par_gmon<-griddap("erdMH1par0mday",
                    time=c(ptime[i],ptime[i]),
                    latitude=c(plat[i],plat[i]),
                    longitude=c(plon[i],plon[i]))
  CC$PAR_MODIS_MON[i]=par_gmon$data$par
  
  print(i)
  print(t(CC[,c("PAR_MODIS_DAILY","PAR_MODIS_8DAY","PAR_MODIS_MON")]))
}