# Most of this is commented out. I'm not going to try to understand this one now because it looks like an earlier version that I developed further elsewhere
## 10/9/2023 left join to make bowWeight does not work

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
# bow_01sf=st_read(dsn = "/Users/heidihirsh/Desktop/bow_ties_7days",layer="bow_ties_1day")
# bow_02sf=st_read(dsn = "/Users/heidihirsh/Desktop/bow_ties_7days",layer="bow_ties_2days")
# bow_03sf=st_read(dsn = "/Users/heidihirsh/Desktop/bow_ties_7days",layer="bow_ties_3days")
# bow_04sf=st_read(dsn = "/Users/heidihirsh/Desktop/bow_ties_7days",layer="bow_ties_4days")
# bow_05sf=st_read(dsn = "/Users/heidihirsh/Desktop/bow_ties_7days",layer="bow_ties_5days")
# bow_06sf=st_read(dsn = "/Users/heidihirsh/Desktop/bow_ties_7days",layer="bow_ties_6days")
# bow_07sf=st_read(dsn = "/Users/heidihirsh/Desktop/bow_ties_7days",layer="bow_ties_7days")
# back_bow_01sf = subset(bow_01sf,simu=='backward')

#read in FL unified reef map habitat data: 
coral_sf =  st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/Final_UnifiedReefMap_Version2.0/FWC_UnifiedFloridaReefMap_v2.0.gdb",
                    layer="UnifiedReefMap")

#circle sectors
circles= st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/FLKhabitat",layer="pts_buf_3km")

#load benthic composition lookup table: 
benthicLU = read.csv("/Users/heidi.k.hirsh/Desktop/FLK_data/FLKhabitat/LookUp/ClassLv4_BenthicLookup_V1_cleaner.csv")



#loop through different overlaps to get habitat area: 
# for (bi in 1:20) {

test=circles[3,]
# st_crs(test) #"World Geodetic System 1984"
# st_crs(coral_sf) #"NAD_1983_Albers"

# coral_sf.t = st_transform(coral_sf,st_crs(test))
# # st_crs(coral_sf.t) #"World Geodetic System 1984"
# # st_crs(test) == st_crs(coral_sf) #false
# # st_crs(test) == st_crs(coral_sf.t) #true
 
#what if I transform test instead of coral_sf?
coral_sf.t=coral_sf #don't transform coral_sf
test.t=st_transform(test,st_crs(coral_sf))
st_crs(test.t) #"NAD_1983_Albers"
test=test.t

coral_sf.tv = coral_sf.t[which(st_is_valid(coral_sf.t)),]
class(coral_sf.tv)

over_bow = st_intersection(coral_sf.tv,test)
st_crs(over_bow)

# #rank benthic classifications
# priority = length(unique(over_bow$ClassLv4)):1
# names(priority)=(unique(over_bow$ClassLv4))
# names(priority)
# 
# classes=(unique(over_bow$ClassLv4))
# nclasses= length(classes)
# classes=classes[order(priority[classes])]

# over_bow_ranked = NULL
# i=1
# i=2
# for (i in 1:2){
#loop fails at i=5 (aggregate reef (inner))

# for (i in 1:nclasses){
#   if (i==1) {over_bow_ranked=over_bow[over_bow$ClassLv4 == classes[i],]  #if first loop, select first class
#   } else {  
#     class_i = over_bow[over_bow$ClassLv4 == classes[i],]                 #if not first loop, class_i is the ith class
#     class_i_intersecting =  st_intersects(class_i,over_bow_ranked)       #determine where this (i) class intersects with the ranked classes so far
#     if (is_empty(class_i_intersecting[[1]])) {class_i_cropped=class_i    #if there are no intersecting polygons then class_i_cropped is just class_i
#     } else {
#       #if there ARE intersecting polygons then take the difference (save only polygons that do not overlap with the previously ranked classes)
#       class_i_int =  st_intersection(over_bow_ranked,class_i)            #This is how we WOULD do it, but it doesn't do anything because nothing overlaps... (i)
#       class_i_cropped = st_difference(class_i,st_union(st_geometry(class_i_int)))                   
#     }
#     
#     over_bow_ranked = rbind(over_bow_ranked,class_i_cropped)
#     
#   }
#   
#   print(i)
# }
# mapview(over_bow_ranked,zcol="ClassLv4")
# mapview(over_bow,zcol="ClassLv4")

# length(classes) #14
# dim(over_bow)
# dim(over_bow_ranked)


# plot(over_bow_ranked[,7])
# plot(class_i[,7])
# mapview(over_bow_ranked)
# mapview(class_i)
# mapview(list(over_bow_ranked,class_i),col.regions=list("green","blue"),col=list("green","blue"),lwd=1,alpha.regions=.2)
# mapview(class_i_cropped)
# mapview(list(class_i,class_i_cropped),col.regions=list("blue","red"),col=list("blue","red"),lwd=1,alpha.regions=.2)
# mapview(list(over_bow_ranked,class_i,class_i_cropped),col.regions=list("green","blue","red"),col=list("green","blue","red"),lwd=1,alpha.regions=.2)


# #Troubleshoot to find overlap (did not find evidence of overlap)
# i=1
# i2=14
# over_bow_ranked=over_bow[over_bow$ClassLv4 == classes[i],]
# class_i = over_bow[over_bow$ClassLv4 == classes[i2],]              
# class_i_intersecting =  st_intersects(class_i,over_bow_ranked)   
# mapview(list(over_bow_ranked,class_i),col.regions=list("green","blue"),col=list("green","blue"),lwd=1,alpha.regions=.2)
# 
# mapview(over_bow,zcol="ClassLv4",alpha=.1)





# allComp= coral_sf.tv %>% st_drop_geometry() %>% dplyr::select(ClassLv4,Shape_Area) %>%  group_by(ClassLv4) %>% summarize(a=sum(Shape_Area,na.rm=TRUE))
# bowComp= over_bow %>% st_drop_geometry() %>% dplyr::select(ClassLv4,Shape_Area) %>% group_by(ClassLv4) %>% summarize(a=sum(Shape_Area,na.rm=TRUE))


# over_bow %>% dplyr::mutate(class_area=st_area(over_bow))
over_bow$class_area = as.numeric(st_area(over_bow)) #m2
# class(over_bow$Shape_Area)
# class(over_bow$class_area)
over_bow$Shape_Area-over_bow$class_area

# allComp= coral_sf.tv %>% st_drop_geometry() %>% dplyr::select(ClassLv4,Shape_Area) %>%  group_by(ClassLv4) %>% summarize(a=sum(Shape_Area,na.rm=TRUE))
# bowComp= over_bow %>% st_drop_geometry() %>% dplyr::select(ClassLv4,Shape_Area) %>% group_by(ClassLv4) %>% summarize(a=sum(Shape_Area,na.rm=TRUE))
bowComp= over_bow %>% st_drop_geometry() %>% dplyr::select(ClassLv4,Shape_Area,class_area) %>% group_by(ClassLv4) %>% summarize(a=sum(class_area,na.rm=TRUE))
bowComp$p = bowComp$a/st_area(test)
bowComp %>% arrange(desc(p))
# write.csv(bowComp,file=paste0("/Users/heidihirsh/Desktop/FLKhabitat/Benthic_dfs/1day_BackBow_",bi,"_BowCompositionByArea_ClassLv4.csv"), row.names = FALSE)
# View(bowComp)
sum(bowComp$p)



# }z


#leftjoin look up table to a "sector"
class(bowComp) #"tbl_df"     "tbl"        "data.frame"
class(benthicLU) #"data.frame"

BowWeight = left_join(bowComp,benthicLU,by='ClassLv4') # not working
# View(BowWeight)
# sum(BowWeight$p)

BowWeight$NotBio = 1-BowWeight$Bio

isit1 = BowWeight$NotBio + BowWeight$coral + BowWeight$BioNoCoral
isit1 #something funky about discontinuous seagrass?

# should these be weight * a or * p?
BowWeight$Ci= BowWeight$a*BowWeight$coral
BowWeight$Ai= BowWeight$a*BowWeight$Alage #fix spelling!
BowWeight$Si= BowWeight$a*BowWeight$Seagrass
BowWeight$NBi= BowWeight$a*BowWeight$NotBio

# BowWeight$Ci= as.numeric(BowWeight$p)*BowWeight$coral
# BowWeight$Ai= as.numeric(BowWeight$p)*BowWeight$Alage #fix spelling!
# BowWeight$Si= as.numeric(BowWeight$p)*BowWeight$Seagrass 
# BowWeight$NBi= as.numeric(BowWeight$p)*BowWeight$NotBio

BowWeight$tIndex =BowWeight$Ci + BowWeight$Ai + BowWeight$Si + BowWeight$NBi  #should this end up being the total area?
BowWeight$tIndex - BowWeight$a #why isn't this always 0?

bow_area = st_area(test) 

sum(BowWeight$a)/bow_area #1.445372 FIXED! Now is it 0.4691965
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
