# RT calculations (preliminary)
## 10/9/2023 most runs, except geobuffer_pts doesn't work (screws up the level plot)


rm(list=ls())
library(raster)
library(ncdf4)
library(rasterVis)
library(gridExtra)
library(patchwork)
library(grid)
library(latticeExtra)
library(tidyverse)
library(sf)
library(rgeos)
library(ggplot2)


# # Oahu ROMS Data
# r_nc=nc_open("C:/Users/Thomas.Oliver/WORK/Projects/Carbonate_Synthesis/ROMS/roms_hiig_reanalysis_9fe2_c5d4_612e.nc")
# rT=raster(x = "C:/Users/Thomas.Oliver/WORK/Projects/Carbonate_Synthesis/ROMS/roms_hiig_reanalysis_9fe2_c5d4_612e.nc",varname="temp")
# rS=raster(x = "C:/Users/Thomas.Oliver/WORK/Projects/Carbonate_Synthesis/ROMS/roms_hiig_reanalysis_9fe2_c5d4_612e.nc",varname="salt")
# rU=raster(x = "C:/Users/Thomas.Oliver/WORK/Projects/Carbonate_Synthesis/ROMS/roms_hiig_reanalysis_9fe2_c5d4_612e.nc",varname="u") #eastward flow
# rV=raster(x = "C:/Users/Thomas.Oliver/WORK/Projects/Carbonate_Synthesis/ROMS/roms_hiig_reanalysis_9fe2_c5d4_612e.nc",varname="v") #northward flow

# #GOM Reanalysis (2012)
# r_nc=nc_open("C:/Users/Thomas.Oliver/WORK/Projects/Carbonate_Synthesis/ROMS/netcdf_2012.nc4")
# rT=raster(x = "C:/Users/Thomas.Oliver/WORK/Projects/Carbonate_Synthesis/ROMS/netcdf_2012.nc4",varname="water_temp")
# rS=raster(x = "C:/Users/Thomas.Oliver/WORK/Projects/Carbonate_Synthesis/ROMS/netcdf_2012.nc4",varname="salinity",)
# rU=raster(x = "C:/Users/Thomas.Oliver/WORK/Projects/Carbonate_Synthesis/ROMS/netcdf_2012.nc4",varname="water_u") #eastward flow
# rV=raster(x = "C:/Users/Thomas.Oliver/WORK/Projects/Carbonate_Synthesis/ROMS/netcdf_2012.nc4",varname="water_v") #northward flow
# SSH=raster(x = "C:/Users/Thomas.Oliver/WORK/Projects/Carbonate_Synthesis/ROMS/netcdf_2012.nc4",varname="surf_el") #ssh

#GOM Reanalysis (2012) - Keys Only # Many dates
#24.2:25.8 -79.7:-83.2
# r_nc=nc_open("C:/Users/Thomas.Oliver/WORK/Projects/Carbonate_Synthesis/ROMS/netcdf_2012_keysonly.nc4")
# rT=raster(x = "C:/Users/Thomas.Oliver/WORK/Projects/Carbonate_Synthesis/ROMS/netcdf_2012_keysonly.nc4",varname="water_temp")
# rS=raster(x = "C:/Users/Thomas.Oliver/WORK/Projects/Carbonate_Synthesis/ROMS/netcdf_2012_keysonly.nc4",varname="salinity",)
# rU=raster(x = "C:/Users/Thomas.Oliver/WORK/Projects/Carbonate_Synthesis/ROMS/netcdf_2012_keysonly.nc4",varname="water_u") #eastward flow
# rV=raster(x = "C:/Users/Thomas.Oliver/WORK/Projects/Carbonate_Synthesis/ROMS/netcdf_2012_keysonly.nc4",varname="water_v") #northward flow
# SSH=raster(x = "C:/Users/Thomas.Oliver/WORK/Projects/Carbonate_Synthesis/ROMS/netcdf_2012_keysonly.nc4",varname="surf_el") #ssh


r_nc=nc_open("/Users/heidi.k.hirsh/Desktop/FLK_data/netcdf_2012_keysonly.nc4")
file ="/Users/heidi.k.hirsh/Desktop/FLK_data/netcdf_2012_keysonly.nc4"

rT=raster(x = file,varname="water_temp")
rS=raster(x = file,varname="salinity",)
rU=raster(x = file,varname="water_u") #eastward flow
rV=raster(x = file,varname="water_v") #northward flow
SSH=raster(x = file,varname="surf_el") #ssh

dim(rU)
#GoM Keys Onlys: 39x85 spatial, x1 depth, x1 time

#cell cross section area in m^2
# Au=(1000*(1000/9)*ncatt_get(nc = r_nc,varid=0,attname = "geospatial_lat_resolution")$value)*ncatt_get(nc = r_nc,varid=0,attname = "geospatial_vertical_max")$value
# Av=(1000*(1000/9)*ncatt_get(nc = r_nc,varid=0,attname = "geospatial_lon_resolution")$value)*ncatt_get(nc = r_nc,varid=0,attname = "geospatial_vertical_max")$value
Lat_res_dd=(ncatt_get(nc = r_nc,varid=0,attname = "geospatial_lat_max")$value-
           ncatt_get(nc = r_nc,varid=0,attname = "geospatial_lat_min")$value)/length(ncvar_get(nc = r_nc,varid="lat"))
Lon_res_dd=(ncatt_get(nc = r_nc,varid=0,attname = "geospatial_lon_max")$value-
           ncatt_get(nc = r_nc,varid=0,attname = "geospatial_lon_min")$value)/length(ncvar_get(nc = r_nc,varid="lon"))
Lat_res_m=(1000*(1000/9)*(Lat_res_dd))
Lon_res_m=(1000*(1000/9)*(Lon_res_dd))
Times=ncvar_get(nc=r_nc,varid="time")
Depths=ncvar_get(nc=r_nc,varid="depth")
Au=Lat_res_m*diff(Depths[1:2])
Av=Lon_res_m*diff(Depths[1:2])
V1=Lat_res_m*Lon_res_m*diff(Depths[1:2]) #m3
Hrs=3#3 hourly
#xsec planar area
TestToClarifyPosition=matrix(data=c(1,2,3,4,5,6,7,8,9),nrow=3,byrow = F)

N=3
FluxMat_U3=Au*matrix(data=c(rep(1,N),rep(rep(0,N),N-2),rep(-1,N)),nrow=N,byrow = F)#m2 with In/Out reference sign
FluxMat_V3=Av*matrix(data=c(rep(-1,N),rep(rep(0,N),N-2),rep(1,N)),nrow=N,byrow = T)#m2

N=5
FluxMat_U5=Au*matrix(data=c(rep(1,N),rep(rep(0,N),N-2),rep(-1,N)),nrow=N,byrow = F)#m2 with In/Out reference sign
FluxMat_V5=Av*matrix(data=c(rep(-1,N),rep(rep(0,N),N-2),rep(1,N)),nrow=N,byrow = T)#m2

N=7
FluxMat_U7=Au*matrix(data=c(rep(1,N),rep(rep(0,N),N-2),rep(-1,N)),nrow=N,byrow = F)#m2 with In/Out reference sign
FluxMat_V7=Av*matrix(data=c(rep(-1,N),rep(rep(0,N),N-2),rep(1,N)),nrow=N,byrow = T)#m2

#Only sum Ins or Outs
SumPos=function(x){s=sign(x);x[s<=0]=0;return(sum(x,na.rm=T))}
SumNeg=function(x){s=sign(x);x[s>=0]=0;return(-sum(x,na.rm=T))}

#Define Volume (w/o SSH)
FocalVol_1=V1 #m3
FocalVol_3=V1*prod(dim(FluxMat_U3)) #m3
FocalVol_5=V1*prod(dim(FluxMat_U5)) #m3
FocalVol_7=V1*prod(dim(FluxMat_U7)) #m3

#Define Volume (w/ SSH)
FocalVol_SSH_1=Lat_res_m*Lon_res_m*SSH
FocalVol_SSH_3=focal(x = SSH,w=matrix(rep(1,3^2),nr=3),fun="sum",na.rm=T)
FocalVol_SSH_5=focal(x = SSH,w=matrix(rep(1,5^2),nr=5),fun="sum",na.rm=T)
FocalVol_SSH_7=focal(x = SSH,w=matrix(rep(1,7^2),nr=7),fun="sum",na.rm=T)

#Total Volume
FocalVol_Tot_1=FocalVol_SSH_1+FocalVol_1
FocalVol_Tot_3=FocalVol_SSH_3+FocalVol_3
FocalVol_Tot_5=FocalVol_SSH_5+FocalVol_5
FocalVol_Tot_7=FocalVol_SSH_7+FocalVol_7

#Max RT to Consider
RTcap=24*3

#Calc m3/sec fluxes in U and V; D=1 # should be relative to the D3 or higher, but may be off by a factor of 2 (RT too long)
PtFlux_U1=abs(rU)*Au*2 #(assume flux across two "sides" of a cube)
PtFlux_V1=abs(rV)*Av*2 #m3/sec
PtFlux_UV1=PtFlux_U1+PtFlux_V1
PtFlux_UV1[is.na(rU)]=NA # Apply Land Mask

RTpt_1=FocalVol_Tot_1/(PtFlux_UV1*(60*60)) #(m3)/(m3/s) * (1 Hr / 3600 s) = s * (1 Hr / 3600 s) = Hr
RTpt_1[RTpt_1>RTcap]=RTcap
RTpt_1[is.na(rU)]=NA # Apply Land Mask

#Calc m3/sec fluxes in U and V; D=3
InFlux_U3=focal(x = rU,w=FluxMat_U3,fun=SumPos)  #m3/sec
InFlux_V3=focal(x = rV,w=FluxMat_V3,fun=SumPos)  #m3/sec
InFlux_UV3=InFlux_U3+InFlux_V3
InFlux_UV3[is.na(rU)]=NA # Apply Land Mask

OutFlux_U3=focal(x = rU,w=FluxMat_U3,fun=SumNeg) #m3/sec
OutFlux_V3=focal(x = rV,w=FluxMat_V3,fun=SumNeg) #m3/sec
OutFlux_UV3=OutFlux_U3+OutFlux_V3
OutFlux_UV3[is.na(rU)]=NA # Apply Land Mask

RTin_3=FocalVol_Tot_3/(InFlux_UV3*(60*60)) #(m3)/(m3/s) * (1 Hr / 3600 s) = s * (1 Hr / 3600 s) = Hr
RTout_3=FocalVol_Tot_3/(OutFlux_UV3*(60*60)) #(m3)/(m3/s) = s * 1 Hr / (3 * 60  * 60 seconds) = Hr
RTin_3[RTin_3>RTcap]=RTcap
RTout_3[RTout_3>RTcap]=RTcap
RTin_3[is.na(rU)]=NA # Apply Land Mask
RTout_3[is.na(rU)]=NA # Apply Land Mask

#Calc m3 fluxes in U and V; D=5
InFlux_U5=focal(x = rU,w=FluxMat_U5,fun=SumPos)  #m3/sec
InFlux_V5=focal(x = rV,w=FluxMat_V5,fun=SumPos)  #m3/sec
InFlux_UV5=InFlux_U5+InFlux_V5
InFlux_UV5[is.na(rU)]=NA # Apply Land Mask

OutFlux_U5=focal(x = rU,w=FluxMat_U5,fun=SumNeg) #m3/sec
OutFlux_V5=focal(x = rV,w=FluxMat_V5,fun=SumNeg) #m3/sec
OutFlux_UV5=OutFlux_U5+OutFlux_V5
OutFlux_UV5[is.na(rU)]=NA # Apply Land Mask

RTin_5=FocalVol_Tot_5/(InFlux_UV5*(60*60)) #(m3)/(m3/s) * (1 Hr / 3600 s) = s * (1 Hr / 3600 s) = Hr
RTout_5=FocalVol_Tot_5/(OutFlux_UV5*(60*60)) #(m3)/(m3/s) = s * 1 Hr / (3 * 60  * 60 seconds) = Hr
RTin_5[RTin_5>RTcap]=RTcap
RTout_5[RTout_5>RTcap]=RTcap
RTin_5[is.na(rU)]=NA # Apply Land Mask
RTout_5[is.na(rU)]=NA # Apply Land Mask

#Calc m3 fluxes in U and V; D=7
InFlux_U7=focal(x = rU,w=FluxMat_U7,fun=SumPos)  #m3/sec
InFlux_V7=focal(x = rV,w=FluxMat_V7,fun=SumPos)  #m3/sec
InFlux_UV7=InFlux_U7+InFlux_V7
InFlux_UV7[is.na(rU)]=NA # Apply Land Mask

OutFlux_U7=focal(x = rU,w=FluxMat_U7,fun=SumNeg) #m3/sec
OutFlux_V7=focal(x = rV,w=FluxMat_V7,fun=SumNeg) #m3/sec
OutFlux_UV7=OutFlux_U7+OutFlux_V7
OutFlux_UV7[is.na(rU)]=NA  # Apply Land Mask

RTin_7=FocalVol_Tot_7/(InFlux_UV7*(60*60)) #(m3)/(m3/s) * (1 Hr / 3600 s) = s * (1 Hr / 3600 s) = Hr
RTout_7=FocalVol_Tot_7/(OutFlux_UV7*(60*60)) #(m3)/(m3/s) = s * 1 Hr / (3 * 60  * 60 seconds) = Hr
RTin_7[RTin_7>RTcap]=RTcap
RTout_7[RTout_7>RTcap]=RTcap
RTin_7[is.na(rU)]=NA  # Apply Land Mask
RTout_7[is.na(rU)]=NA # Apply Land Mask


par(mfrow=c(4,4),mar=c(3,3,3,4))
plot(rT,main="Temperature",col=topo.colors(20,rev = F),colNA="gray75")
#plot(rS,main="Salinity",col=topo.colors(20,rev = F),colNA="gray75")
plot(rU,main="U = Eastward",col=topo.colors(20,rev = F),colNA="gray75")
plot(rV,main="V = Northward",col=topo.colors(20,rev = F),colNA="gray75")
plot(FocalVol_Tot_3,main="FocalVol_Tot_3",col=topo.colors(20,rev = F),colNA="gray75")
# plot(FocalVol_Tot_5,main="FocalVol_Tot_5",col=topo.colors(20,rev = F),colNA="gray75")
# plot(FocalVol_Tot_7,main="FocalVol_Tot_7",col=topo.colors(20,rev = F),colNA="gray75")

plot(InFlux_UV3,main="InFlux_UV3",zlim=c(10e-10,max(getValues(InFlux_UV3),na.rm=T)),col=topo.colors(20,rev = F),colNA="gray75")
plot(OutFlux_UV3,main="OutFlux_UV3",zlim=c(10e-10,max(getValues(OutFlux_UV3),na.rm=T)),col=topo.colors(20,rev = F),colNA="gray75")
plot(RTin_3,zlim=c(0,RTcap+6),main="RTin D=3",col=topo.colors(20,rev = F),colNA="gray75")
plot(RTout_3,zlim=c(0,RTcap),main="RTout D=3",col=topo.colors(20,rev = F),colNA="gray75")
#hist(RTin_3,breaks=seq(0,RTcap,6),main="RTin D=3",col=topo.colors(20,rev = F),colNA="gray75")
#hist(RTout_3,breaks=seq(0,RTcap,6),main="RTout D=3",col=topo.colors(20,rev = F),colNA="gray75")

plot(InFlux_UV5,main="InFlux_UV5",zlim=c(10e-10,max(getValues(InFlux_UV5),na.rm=T)),col=topo.colors(20,rev = F),colNA="gray75")
plot(OutFlux_UV5,main="OutFlux_UV5",zlim=c(10e-10,max(getValues(OutFlux_UV5),na.rm=T)),col=topo.colors(20,rev = F),colNA="gray75")
plot(RTin_5,zlim=c(0,RTcap+6),main="RTin D=5",col=topo.colors(20,rev = F),colNA="gray75")
plot(RTout_5,zlim=c(0,RTcap),main="RTout D=5",col=topo.colors(20,rev = F),colNA="gray75")
#hist(RTin_5,breaks=seq(0,RTcap,6),main="RTin D=5",col=topo.colors(20,rev = F),colNA="gray75")
#hist(RTout_5,breaks=seq(0,RTcap,6),main="RTout D=5",col=topo.colors(20,rev = F),colNA="gray75")

plot(InFlux_UV7,main="InFlux_UV7",zlim=c(10e-10,max(getValues(InFlux_UV7),na.rm=T)),col=topo.colors(20,rev = F),colNA="gray75")
plot(OutFlux_UV7,main="OutFlux_UV7",zlim=c(10e-10,max(getValues(OutFlux_UV7),na.rm=T)),col=topo.colors(20,rev = F),colNA="gray75")
plot(RTin_7,zlim=c(0,RTcap+6),main="RTin D=7",col=topo.colors(20,rev = F),colNA="gray75")
plot(RTout_7,zlim=c(0,RTcap),main="RTout D=7",col=topo.colors(20,rev = F),colNA="gray75")
#hist(RTin_7,breaks=seq(0,RTcap,6),main="RTin D=7",col=topo.colors(20,rev = F),colNA="gray75")
#hist(RTout_7,breaks=seq(0,RTcap,6),main="RTout D=7",col=topo.colors(20,rev = F),colNA="gray75")

par(mfrow=c(2,2),mar=c(3,3,3,4))
RTcap=24*3
UVfield=stack(rU,rV)
names(UVfield)
vp=vectorplot(UVfield,isField="dXY",lwd.arrows=.2,col.arrows="white",length=unit(1e-2, 'npc'),zscaleLog=T)
lp1=levelplot(RTpt_1,zscaleLog=T)
lp3=levelplot(RTin_3,zscaleLog=T,)
grid.arrange(vp,lp1)
grid.arrange(vp,lp3)
grid.arrange(lp1,lp3)

vp

#______
#make a spatial points data frame of sample locations and layer on top of levelplot and vectorplot
CCflk <- read.csv('/Users/heidi.k.hirsh/Desktop/FLK_data/FLK_filtered_ve4.csv')
# View(CCflk) # 1612   52

CCflk19 = CCflk[which(CCflk$Year=='2019'),]
# CCflkSub = CCflk[which(CCflk$UTCDate=='19-05-05'),]
CCflk19may = CCflk[which(CCflk$UTCDate=='19-04-29' | CCflk$UTCDate=='19-04-30'),] #4/29 is north part, 4/30 is south part

pts.df <- data.frame(lat = CCflk19may$Latitude, lon= CCflk19may$Longitude, zone= CCflk19may$Zone, day= CCflk19may$UTCDate) #actual coords

# pts.df <- data.frame(lat = CCflk$Latitude, lon= CCflk$Longitude, zone= CCflk$Zone) #actual coords
# pts.df <- data.frame(lat = CCflk$dec.lat, lon= CCflk$dec.lon, zone= CCflk$Zone)

#remove any NAs
row.has.na <- apply(pts.df, 1, function(x){any(is.na(x))})
sum(row.has.na) #1
pts.df.filt <- pts.df[!row.has.na,]
summary(pts.df.filt)
dim(pts.df.filt) # 1611    3

coordinates(pts.df.filt) <- ~lon+lat
crs(pts.df.filt) <- projection(UVfield)

#buffer points with circles (6km diameter)
#start with subset for 5/5/2019
View(pts.df)
class(pts.df) #data.frame
#use pts.df.filt (it is already sp)

crs(pts.df.filt) #+proj=longlat +datum=WGS84 +no_defs 

RadInMeters = 3000 #3km radius for 6km diameter
# circ3km = gBuffer(pts.df.filt,width=100*distInMeters, byid=TRUE)

#https://stackoverflow.com/questions/25411251/buffer-geospatial-points-in-r-with-gbuffer
# install.packages("devtools") # if you do not have devtools, then install it
devtools::install_github("valentinitnelav/geobuffer")
library(geobuffer)

# pts <- data.frame(lon = c(-53.20198, -52.81218),
#                   lat = c(47.05564, 47.31741))

pts_buf_3km <- geobuffer_pts(xy = pts.df.filt, dist_m = RadInMeters) #!!geobuffer_pts doesn't work
plot(pts_buf_3km)
class(pts_buf_3km)

# shapefile(pts_buf_3km, filename="/Users/heidihirsh/Desktop/FLKhabitat/pts_buf_3km.shp")




# vectorplot(UVfield,isField="dXY",lwd.arrows=.2,col.arrows="white",length=unit(1e-2, 'npc'),zscaleLog=T) +
# trellis.par.set(canonical.theme(color = FALSE)) 
# trellis.par.set(regions=list(col=topo.colors(100)))

levelplot(RTout_5,zscaleLog=T,colorkey=list(at=log10(seq(10,70,by=10)),labels=as.character(seq(10,70,by=10))))+
  latticeExtra::layer(sp.points(pts.df.filt, pch=16,col='cyan'))+
  latticeExtra::layer(sp.polygons(pts_buf_3km,col='gray'))

# fname = paste0("/Users/heidihirsh/Desktop/RT+circles_logTEST.png")
# trellis.device(device="png", file=fname)
p1 = as.ggplot(plotRT)

dev.off()

# keysLand = shapefile('/Users/heidihirsh/Desktop/florida_shape/FKEYS_NCRMP_SampFrame_50m.shp')
keysLand = shapefile('/Users/heidi.k.hirsh/Desktop/FLK_data/Florida_Shoreline_(1_to_40%2C000_Scale)/Florida_Shoreline_(1_to_40%2C000_Scale).shp')
plot(keysLand,col='gray')
#need to translate to spatial data to plot with currents etc.

# guamLand = shapefile('/Users/heidihirsh/Desktop/guam_shoreline/guam_shoreline.shp')
# plot(guamLand,col='gray')
# guamLand = spTransform(guamLand, crs(sectors))
# crs(guamLand)
# guamLand@data$FID = rownames(guamLand@data)
# guamPoints = fortify(guamLand)
# guamDF = merge(guamLand,guamLand@data)

# myPoints= SpatialPoints(pts.df.filt)
# crs(myPoints)=crs(pts.df.filt) 
# p1= levelplot(RTin_3,zscaleLog=T,)
# p2= spplot(myPoints,pch=16,size=12)
# p=p1+p2
# p








plot(RTpt_1,zlim=c(0,RTcap+6),main="RTin D=1",col=topo.colors(20,rev = F),colNA="gray75")
plot(RTin_3,zlim=c(0,RTcap+6),main="RTin D=3",col=topo.colors(20,rev = F),colNA="gray75")
#plot(RTin_5,zlim=c(0,RTcap+6),main="RTin D=5",col=topo.colors(20,rev = F),colNA="gray75")
plot(RTout_3,zlim=c(0,RTcap),main="RTout D=3",col=topo.colors(20,rev = F),colNA="gray75")
#plot(RTout_7,zlim=c(0,RTcap),main="RTout D=7",col=topo.colors(20,rev = F),colNA="gray75")

par(mfrow=c(2,2),mar=c(3,3,3,4))
plot(RTin_3-RTout_3,main="IN OUT DIFF",col=rainbow(20,rev = F),colNA="gray75")
plot(RTin_7-RTin_3,main="D7-D3 DIFF",col=rainbow(20,rev = F),colNA="gray75")
hist(RTin_3-RTout_3,main="IN OUT DIFF",col=rainbow(20,rev = F),breaks=1000)
hist(RTin_7-RTin_3,main="D7-D3 DIFF",col=rainbow(20,rev = F),breaks=1000)
# plot(getValues(RTin_3),getValues(RTout_3),main="IN OUT DIFF",col=rainbow(20,rev = F))
# plot(getValues(RTin_7),getValues(RTin_3),main="D7-D3 DIFF",col=rainbow(20,rev = F))

