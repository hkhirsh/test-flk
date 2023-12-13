#Purpose: I think this is a script to troubleshoot/develop the method through looping through bows to cut benthic habitat data and determine habitat indices


## 10/6/2023 Test running script
#remove commented code if duplicate from other scripts or not useful
#non commented code all seems to run fine. 
#pause on working with this script for now because I think there are other scripts that pick it up/clean it up


rm(list=ls())

library(sf)
library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(tidyverse)
library(tigris)
library(rgeos)
library(ggmap)
library(rstudioapi)
library(patchwork)
library(mapview)
register_google(key = "AIzaSyB3mq0nJtcNWvQUNuqH_t-MLxEUmwDDmGk") #new api OCT2023

#sf
bow_01sf=st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_7days",layer="bow_ties_1day")
bow_02sf=st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_7days",layer="bow_ties_2days")
bow_03sf=st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_7days",layer="bow_ties_3days")
bow_04sf=st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_7days",layer="bow_ties_4days")
bow_05sf=st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_7days",layer="bow_ties_5days")
bow_06sf=st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_7days",layer="bow_ties_6days")
bow_07sf=st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_7days",layer="bow_ties_7days")

#read in FL unified reef map habitat data:
coral_sf =  st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/Final_UnifiedReefMap_Version2.0/FWC_UnifiedFloridaReefMap_v2.0.gdb",
                     layer="UnifiedReefMap")

back_bow_01sf = subset(bow_01sf,simu=='backward')

#circle sectors
# shapefile(pts_buf_3km, filename="/Users/heidihirsh/Desktop/FLKhabitat/pts_buf_3km.shp")
circles= st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/FLKhabitat",layer="pts_buf_3km")

#load lookup table:
benthicLU = read.csv("/Users/heidi.k.hirsh/Desktop/FLK_data/FLKhabitat/LookUp/ClassLv4_BenthicLookup_V1_cleaner.csv")


## PLOTTING
# test=back_bow_01sf[1,]
dim(circles) #35 points
test=circles[35,]

coral_sf.t = st_transform(coral_sf,st_crs(test))
st_crs(test) == st_crs(coral_sf)

coral_sf.tv = coral_sf.t[which(st_is_valid(coral_sf.t)),]
# table(st_is_valid(coral_sf.tv))

over_bow = st_intersection(coral_sf.tv,test)

bowExt = st_bbox(test)
coral_crop = st_crop(coral_sf.tv,bowExt)


## Plotting
FLK_map=get_map(location=c(mean(st_bbox(test)[c(1,3)]),mean(st_bbox(test)[c(2,4)])),zoom=8,maptype = "satellite")
# ggmap::ggmap(FLK_map)

a= ggmap(FLK_map)+
  geom_sf(data=coral_crop,aes(fill=ClassLv4),alpha=.5,inherit.aes = FALSE)+
  geom_sf(data=test,color='white',lwd=2,fill=NA,inherit.aes = FALSE)+
  scale_x_continuous(limits=st_bbox(coral_crop)[c(1,3)])+
  scale_y_continuous(limits=st_bbox(coral_crop)[c(2,4)])+
  theme(axis.text.x =element_blank(), axis.text.y =element_blank(),
        axis.ticks.x =element_blank(), axis.ticks.y =element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())+
  theme(legend.position="bottom")

b= ggmap(FLK_map)+
  geom_sf(data=coral_crop,aes(fill=PercentBio),alpha=.5,inherit.aes = FALSE)+
  geom_sf(data=test,color='white',lwd=2,fill=NA,inherit.aes = FALSE)+
  scale_x_continuous(limits=st_bbox(coral_crop)[c(1,3)])+
  scale_y_continuous(limits=st_bbox(coral_crop)[c(2,4)])+
  theme(axis.text.x =element_blank(), axis.text.y =element_blank(),
        axis.ticks.x =element_blank(), axis.ticks.y =element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

c= ggmap(FLK_map)+
  geom_sf(data=coral_crop,aes(fill=PercentCor),alpha=.5,inherit.aes = FALSE)+
  geom_sf(data=test,color='white',lwd=2,fill=NA,inherit.aes = FALSE)+
  scale_x_continuous(limits=st_bbox(coral_crop)[c(1,3)])+
  scale_y_continuous(limits=st_bbox(coral_crop)[c(2,4)])+
  theme(axis.text.x =element_blank(), axis.text.y =element_blank(),
        axis.ticks.x =element_blank(), axis.ticks.y =element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

# cropTest = b/c/a #stacking isn't working
cropTest = a
a
# fname = paste0("/Users/heidihirsh/Desktop/FLKhabitat/Circle20_ClassLv4.png")
# ggsave(file=fname,cropTest,width=14,height=9)
fname = paste0("/Users/heidi.k.hirsh/Desktop/FLK Figures/Circle20_ClassLv4.png")
# ggsave(file=fname,cropTest,width=14,height=9)

### end plotting


# #comment out index tables for now
# #level 1
# Pcor1=table(coral_sf$ClassLv1,coral_sf$PercentCor)
# Coral_Index1 = (0.05*Pcor1[,2] + .3*Pcor1[,3] + .7*Pcor1[,4])/rowSums(Pcor1[,2:4])
#
# Pbio1=table(coral_sf$ClassLv1,coral_sf$PercentBio)
# Bio_Index1 = (0.05*Pbio1[,2] + .3*Pbio1[,3] + .7*Pbio1[,4] + .95*Pbio1[,5])/rowSums(Pbio1[,2:5])
#
# sort(Coral_Index1)
# sort(Bio_Index1)
# sort(Bio_Index1-Coral_Index1)
#
# ind1 = data.frame(class=names(Coral_Index1), coral=Coral_Index1, Bio=Bio_Index1, BioNoCoral= Bio_Index1-Coral_Index1)
# # write.csv(ind1,file=paste0("/Users/heidihirsh/Desktop/FLKhabitat/Benthic_dfs/1day_BackBows_IndexClassLv1.csv"), row.names = FALSE)
#
#
# #level 2
# Pcor2=table(coral_sf$ClassLv2,coral_sf$PercentCor)
# Coral_Index2 = (0.05*Pcor2[,2] + .3*Pcor2[,3] + .7*Pcor2[,4])/rowSums(Pcor2[,2:4])
#
# Pbio2=table(coral_sf$ClassLv2,coral_sf$PercentBio)
# Bio_Index2 = (0.05*Pbio2[,2] + .3*Pbio2[,3] + .7*Pbio2[,4] + .95*Pbio2[,5])/rowSums(Pbio2[,2:5])
#
# sort(Coral_Index2)
# sort(Bio_Index2)
# sort(Bio_Index2-Coral_Index2)
#
# ind2 = data.frame(class=names(Coral_Index2), coral=Coral_Index2, Bio=Bio_Index2, BioNoCoral= Bio_Index2-Coral_Index2)
# # write.csv(ind2,file=paste0("/Users/heidihirsh/Desktop/FLKhabitat/Benthic_dfs/1day_BackBows_IndexClassLv2.csv"), row.names = FALSE)
#
#
# #level 3
# Pcor3=table(coral_sf$ClassLv3,coral_sf$PercentCor)
# Coral_Index3 = (0.05*Pcor3[,2] + .3*Pcor3[,3] + .7*Pcor3[,4])/rowSums(Pcor3[,2:4])
#
# Pbio3=table(coral_sf$ClassLv3,coral_sf$PercentBio)
# Bio_Index3 = (0.05*Pbio3[,2] + .3*Pbio3[,3] + .7*Pbio3[,4] + .95*Pbio3[,5])/rowSums(Pbio3[,2:5])
#
# sort(Coral_Index3)
# sort(Bio_Index3)
# sort(Bio_Index3-Coral_Index3)
#
# ind3 = data.frame(coral=Coral_Index3, Bio=Bio_Index3, BioNoCoral= Bio_Index3-Coral_Index3)
# # write.csv(ind3,file=paste0("/Users/heidihirsh/Desktop/FLKhabitat/Benthic_dfs/1day_BackBows_IndexClassLv3.csv"), row.names = FALSE)
#
#
# #level 4
# Pcor4=table(coral_sf$ClassLv4,coral_sf$PercentCor)
# nCor4=rowSums(Pcor4[,2:4])
# Coral_Index4 = (0.05*Pcor4[,2] + .3*Pcor4[,3] + .7*Pcor4[,4])/nCor4
#
# Pbio4=table(coral_sf$ClassLv4,coral_sf$PercentBio)
# nBio4=rowSums(Pbio4[,2:5])
# Bio_Index4 = (0.05*Pbio4[,2] + .3*Pbio4[,3] + .7*Pbio4[,4] + .95*Pbio4[,5])/nBio4
#
# sort(Coral_Index4)
# sort(Bio_Index4)
# sort(Bio_Index4-Coral_Index4)
#
# ind4 = data.frame(coral=Coral_Index4, Bio=Bio_Index4, BioNoCoral= Bio_Index4-Coral_Index4, nCor=nCor4, nBio=nBio4)
# # write.csv(ind4,file=paste0("/Users/heidihirsh/Desktop/FLKhabitat/Benthic_dfs/1day_BackBows_IndexClassLv4.csv"), row.names = FALSE)



#loop through different overlaps to get habitat area:
# for (bi in 1:20) {
# bi=20
# test=back_bow_01sf[bi,]
test=circles[1,]

coral_sf.t = st_transform(coral_sf,st_crs(test))
st_crs(test) == st_crs(coral_sf)

coral_sf.tv = coral_sf.t[which(st_is_valid(coral_sf.t)),]

over_bow = st_intersection(coral_sf.tv,test)

#rank benthic classifications
priority = length(unique(over_bow$ClassLv4)):1
names(priority)=(unique(over_bow$ClassLv4))
names(priority)

# over_bow_ranked = st_difference(over_bow)
# over_bow_int = st_intersection(over_bow)
# over_bow_int = st_make_valid(over_bow_int)
# over_bow_intA = st_area(over_bow_int)
# sum(over_bow_intA) #13225011
# sum(bowComp$a) # 40739995

# nclasses= length(unique(over_bow$ClassLv4))
classes=(unique(over_bow$ClassLv4))
nclasses= length(classes)
classes=classes[order(priority[classes])]

over_bow_ranked = NULL
# i=1
# i=2

#i=1 (aggregate reef) versus i=14 (Scattered Coral/Rock in Unconsolidated Sediment) is good for troubleshooting
#also 7 (Seagrass (Continuous)) versus 9 (Seagrass (Discontinuous)) - but more complicated (and when I zoom in I am not convinced there is overlap)


# for (i in 1:2){
#loop fails at i=5 (aggregate reef (inner))
for (i in 1:nclasses){
  if (i==1) {over_bow_ranked=over_bow[over_bow$ClassLv4 == classes[i],]  #if first loop, select first class
  } else {
    class_i = over_bow[over_bow$ClassLv4 == classes[i],]                 #if not first loop, class_i is the ith class
    class_i_intersecting =  st_intersects(class_i,over_bow_ranked)       #determine where this (i) class intersects with the ranked classes so far
    if (is_empty(class_i_intersecting[[1]])) {class_i_cropped=class_i    #if there are no intersecting polygons then class_i_cropped is just class_i
    } else {
      # class_i_cropped = st_difference(class_i,over_bow_ranked)         #if there ARE intersecting polygons then take the difference (save only polygons that do not overlap with the previously ranked classes)
      # class_i_overlap = st_overlaps(over_bow_ranked,class_i)
      class_i_int =  st_intersection(over_bow_ranked,class_i)            #This is how we WOULD do it, but it doesn't do anything because nothing overlaps... (i)
      class_i_cropped = st_difference(class_i,st_union(st_geometry(class_i_int)))
    }

    over_bow_ranked = rbind(over_bow_ranked,class_i_cropped)

  }
  print(i)
}
mapview(over_bow_ranked,zcol="ClassLv4")
mapview(over_bow,zcol="ClassLv4")




length(classes) #14
dim(over_bow)
dim(over_bow_ranked)


# test.int = st_intersection(class_i,class_i)
# poly_diff <- st_difference(class_i, st_union(st_geometry(test.int)))

# class_i_overlap = st_overlaps(over_bow_ranked,class_i)
# over <- class_i[st_overlaps(over_bow_ranked,class_i)[[1]],]

# plot(over_bow_ranked[,7])
# plot(class_i[,7])
mapview(over_bow_ranked)
mapview(class_i)
mapview(list(over_bow_ranked,class_i),col.regions=list("green","blue"),col=list("green","blue"),lwd=1,alpha.regions=.2)
mapview(class_i_cropped)
mapview(list(class_i,class_i_cropped),col.regions=list("blue","red"),col=list("blue","red"),lwd=1,alpha.regions=.2)
mapview(list(over_bow_ranked,class_i,class_i_cropped),col.regions=list("green","blue","red"),col=list("green","blue","red"),lwd=1,alpha.regions=.2)


#Troubleshoot to find overlap (did not find evidence of overlap)
i=1
i2=14
over_bow_ranked=over_bow[over_bow$ClassLv4 == classes[i],]
class_i = over_bow[over_bow$ClassLv4 == classes[i2],]
class_i_intersecting =  st_intersects(class_i,over_bow_ranked)
mapview(list(over_bow_ranked,class_i),col.regions=list("green","blue"),col=list("green","blue"),lwd=1,alpha.regions=.2)

mapview(over_bow,zcol="ClassLv4",alpha=.1)




# rank_bowComp = over_bow_ranked %>% st_drop_geometry() %>% dplyr::select(ClassLv4,Shape_Area) %>%  group_by(ClassLv4) %>% summarize(a=sum(Shape_Area,na.rm=TRUE))
# rank_bowComp$p = rank_bowComp$a/st_area(test)
# rank_bowComp %>% arrange(desc(p))
# sum(rank_bowComp$p)
# View(rank_bowComp)

# allComp= coral_sf.tv %>% st_drop_geometry() %>% dplyr::select(ClassLv4,Shape_Area) %>%  group_by(ClassLv4) %>% summarize(a=sum(Shape_Area,na.rm=TRUE))
bowComp= over_bow %>% st_drop_geometry() %>% dplyr::select(ClassLv4,Shape_Area) %>% group_by(ClassLv4) %>% summarize(a=sum(Shape_Area,na.rm=TRUE))
bowComp$p = bowComp$a/st_area(test)
bowComp %>% arrange(desc(p))
View(bowComp)

# write.csv(bowComp,file=paste0("/Users/heidihirsh/Desktop/FLKhabitat/Benthic_dfs/1day_BackBow_",bi,"_BowCompositionByArea_ClassLv4.csv"), row.names = FALSE)
# }

# library(rmapshaper)
# #https://r-spatial.github.io/sf/reference/geos_binary_ops.html
# # over_ints = st_intersection(over_bow)
# over_ints = st_intersects(over_bow)
#
# over_ints[,c('OBJECTID','ClassLv4',"ID",'n.overlaps')]
# over_diff=st_difference(over_bow)


# BioCov4=table(coral_sf.tv$ClassLv4,coral_sf.tv$BioCover)
# BioCov4[which(rownames(BioCov4) == 'Aggregate Reef (Inner)'),]


# ind4$ClassLv4 = rownames(ind4)
#
# BowWeight = left_join(bowComp,ind4,by='ClassLv4')
# View(BowWeight)
#
# AllWeight = left_join(allComp,ind4,by='ClassLv4')
# View(AllWeight)
#
# write.csv(ind4, file='/Users/heidihirsh/Desktop/FLKhabitat/LookUp/ClassLv4_BenthicLookup_V0.csv')    #use this to define lookup table



#leftjoin look up table to a "sector"
class(bowComp) #"tbl_df"     "tbl"        "data.frame"
class(benthicLU) #"data.frame"

BowWeight = left_join(bowComp,benthicLU,by='ClassLv4')
# View(BowWeight)
# sum(BowWeight$p)

BowWeight$NotBio = 1- BowWeight$Bio

# should these be weight * a or * p?
BowWeight$Ci= BowWeight$a*BowWeight$coral
BowWeight$Ai= BowWeight$a*BowWeight$Alage #fix spelling!
BowWeight$Si= BowWeight$a*BowWeight$Seagrass
BowWeight$NBi= BowWeight$a*BowWeight$NotBio

# BowWeight$Ci= as.numeric(BowWeight$p)*BowWeight$coral
# BowWeight$Ai= as.numeric(BowWeight$p)*BowWeight$Alage #fix spelling!
# BowWeight$Si= as.numeric(BowWeight$p)*BowWeight$Seagrass
# BowWeight$NBi= as.numeric(BowWeight$p)*BowWeight$NotBio

BowWeight$tIndex =BowWeight$Ci + BowWeight$Ai + BowWeight$Si + BowWeight$NBi  #should this add to 1? what is this number? Should this end up being the total area?

bow_area = st_area(test) #circle 1= 28186521 [m^2]

sum(BowWeight$a)/bow_area #1.445372
sum(BowWeight$p) #1.445372


#plot circles with labels

FLK_map=get_map(location=c(mean(st_bbox(circles)[c(1,3)]),mean(st_bbox(circles)[c(2,4)])),zoom=8,maptype = "satellite")

ggmap(FLK_map)+
  geom_sf(color='yellow',alpha=.25,data=circles[1,], inherit.aes = FALSE)+
  geom_sf(color='cyan',alpha=.25,data=circles[2:34,], inherit.aes = FALSE)+
  geom_sf(color='yellow',alpha=.25,data=circles[35,], inherit.aes = FALSE)+
  # geom_text_repel(aes(x=x_from,y=y_from,label=1:35),color="white",data=circles)+
  scale_x_continuous(limits=st_bbox(circles)[c(1,3)])+
  scale_y_continuous(limits=st_bbox(circles)[c(2,4)])+
  theme_bw()










#Transpose first: (so habitat types are column names)
#
# t.ind4 = as.data.frame(t(ind4))
# class(t.ind4)
# dim(t.ind4) #5 127
# names(t.ind4)
#
# class(bowComp)
# rownames(bowComp)= bowComp$ClassLv4
# t.bowComp = as.data.frame(t(bowComp))
# class(t.bowComp)
# dim(t.bowComp) #3 31
# names(t.bowComp)
#
# sapply(t.ind4,class) #numeric
# sapply(t.bowComp,class) #character
# t.ind4[1:length(t.ind4)] = sapply(t.ind4[1:length(t.ind4)],as.character)
# sapply(t.ind4,class) #character now
# BowWeight = left_join(t.ind4,t.bowComp)
# dim(BowWeight)
# View(BowWeight)





# cropTest= ggmap(FLK_map)+
#   geom_sf(data=coral_crop,aes(fill=ClassLv1),alpha=.5,inherit.aes = FALSE)+
#   # geom_sf(data=subset(coral_crop,ClassLv1 %in% c( "Individual or Aggregated Patch Reef","Pavement","Reef Rubble")),
#   #         aes(fill=PercentBio),alpha=.5,inherit.aes = FALSE)+
#   geom_sf(data=test,color='white',lwd=2,fill=NA,inherit.aes = FALSE)+
#   scale_x_continuous(limits=st_bbox(coral_crop)[c(1,3)])+
#   scale_y_continuous(limits=st_bbox(coral_crop)[c(2,4)])+
#   theme_bw()
# theme(legend.position="bottom")
