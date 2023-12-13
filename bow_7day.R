#This is good code to illustrate how bowties work and relate to sampling points
#additional exploration at the end (code from Tom) - I can't get it all to run, but I'm not sure if we will be moving forward with those analyses.

## 10/9/2023


rm(list=ls())
# invisible(lapply(paste0('package:',names(sessionInfo()$otherPkgs)),
#                  detach,character.only=TRUE, unload=TRUE))

library(sf)
library(ggplot2)
library(tidyverse)
library(ggmap)
library(ggrepel)
register_google(key = "AIzaSyB3mq0nJtcNWvQUNuqH_t-MLxEUmwDDmGk") #api updated OCT23


# install.packages('rstudioapi') 
#get new google api https://www.youtube.com/watch?v=Of_M4kcE9yM
library(rstudioapi)
library(influential) #needed for "graph_from_data_frame" function
library(igraph)

# setwd("C:/Users/Thomas.Oliver/WORK/Projects/Carbonate_Synthesis/Keys/")

bow_01=st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_7days",layer="bow_ties_1day")
bow_02=st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_7days",layer="bow_ties_2days")
bow_03=st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_7days",layer="bow_ties_3days")
bow_04=st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_7days",layer="bow_ties_4days")
bow_05=st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_7days",layer="bow_ties_5days")
bow_06=st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_7days",layer="bow_ties_6days")
bow_07=st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/bow_ties_7days",layer="bow_ties_7days")
sites_i=c(3,4,9:20)#1:20#

FLK_map=get_map(location=c(-80.99306,25.27732),zoom=8,maptype = "satellite")
FLK_map=get_map(location=c(mean(st_bbox(bow_01)[c(1,3)]),mean(st_bbox(bow_01)[c(2,4)])),zoom=8,maptype = "satellite")
# ggmap(FLK_map)
inshore=c(20,16,13,12,9,5,1)
midchannel=c(17,14,11,10,6,3)
offshore=c(19,15,7,4,2)
oceanic=c(18,8)
sites_i=inshore
direc="backward"
sites_i_2=offshore
direc_2="forward"

ggmap(FLK_map)+
  geom_sf(data=bow_01, inherit.aes = FALSE)+
  scale_x_continuous(limits=st_bbox(bow_01)[c(1,3)])+
  scale_y_continuous(limits=st_bbox(bow_01)[c(2,4)])+
  theme_bw()

#this works! Oct 2023
ggmap(FLK_map)+
  geom_sf(fill="gray",alpha=.25,data=subset(bow_07,simu==direc)[sites_i,], inherit.aes = FALSE)+
  geom_sf(fill="lightskyblue4",alpha=.25,data=subset(bow_06,simu==direc)[sites_i,], inherit.aes = FALSE)+
  geom_sf(fill="lightskyblue3",alpha=.25,data=subset(bow_05,simu==direc)[sites_i,], inherit.aes = FALSE)+
  geom_sf(fill="lightskyblue2",alpha=.25,data=subset(bow_04,simu==direc)[sites_i,], inherit.aes = FALSE)+
  geom_sf(fill="lightskyblue1",alpha=.33,data=subset(bow_03,simu==direc)[sites_i,], inherit.aes = FALSE)+
  geom_sf(fill="lightskyblue",alpha=.4,data=subset(bow_02,simu==direc)[sites_i,], inherit.aes = FALSE)+
  geom_sf(fill="blue",alpha=.5,data=subset(bow_01,simu==direc)[sites_i,], inherit.aes = FALSE)+
  geom_sf(fill="gray",alpha=.25,data=subset(bow_07,simu==direc)[sites_i,], inherit.aes = FALSE)+
  geom_sf(fill="tomato4",alpha=.25,data=subset(bow_06,simu==direc_2)[sites_i_2,], inherit.aes = FALSE)+
  geom_sf(fill="tomato3",alpha=.25,data=subset(bow_05,simu==direc_2)[sites_i_2,], inherit.aes = FALSE)+
  geom_sf(fill="tomato2",alpha=.25,data=subset(bow_04,simu==direc_2)[sites_i_2,], inherit.aes = FALSE)+
  geom_sf(fill="tomato1",alpha=.33,data=subset(bow_03,simu==direc_2)[sites_i_2,], inherit.aes = FALSE)+
  geom_sf(fill="tomato",alpha=.4,data=subset(bow_02,simu==direc_2)[sites_i_2,], inherit.aes = FALSE)+
  geom_sf(fill="red",alpha=.5,data=subset(bow_01,simu==direc_2)[sites_i_2,], inherit.aes = FALSE)+
  geom_point(aes(x=x_from,y=y_from),color="yellow",data=subset(bow_01,simu==direc))+
  geom_text_repel(aes(x=x_from,y=y_from,label=1:20),color="black",data=subset(bow_01,simu==direc))+
  scale_x_continuous(limits=st_bbox(bow_01)[c(1,3)])+
  scale_y_continuous(limits=st_bbox(bow_01)[c(2,4)])+
  theme_bw()


back = ggmap(FLK_map)+
  geom_sf(color='white',lwd=1,fill=NA,data=subset(bow_01,simu==direc), inherit.aes = FALSE)+
  geom_point(aes(x=x_from,y=y_from),color="yellow",data=subset(bow_01,simu==direc))+
  geom_text_repel(aes(x=x_from,y=y_from,label=1:20),color="black",data=subset(bow_01,simu==direc))+
  scale_x_continuous(limits=st_bbox(bow_01)[c(1,3)])+
  scale_y_continuous(limits=st_bbox(bow_01)[c(2,4)])+
  theme_bw()
back
# ggsave(file="/Users/heidihirsh/Desktop/FLKhabitat/backbow1.png",back)


forw = ggmap(FLK_map)+
  geom_sf(color='white',lwd=1,fill=NA,data=subset(bow_01,simu==direc_2), inherit.aes = FALSE)+
  geom_point(aes(x=x_from,y=y_from),color="yellow",data=subset(bow_01,simu==direc))+
  geom_text_repel(aes(x=x_from,y=y_from,label=1:20),color="black",data=subset(bow_01,simu==direc))+
  scale_x_continuous(limits=st_bbox(bow_01)[c(1,3)])+
  scale_y_continuous(limits=st_bbox(bow_01)[c(2,4)])+
  theme_bw()
forw
# ggsave(file="/Users/heidihirsh/Desktop/FLKhabitat/forwardbow1.png",forw)



#what is bow? #oct9
#try defining bow as one of the bow files I read in
bow=bow_01

pts=data.frame(unique(st_drop_geometry(bow[,c("x_from","y_from")])))
pts_sf=st_multipoint(matrix(apply(pts,2,as.numeric),ncol=2))

st_distance(pts_sf)

hist(st_distance(bow)/1000,n=100)
st_crs(bow)
plot(bow)
table(bow$simu)

bow$ID_LL=factor(paste0(bow$x_from,"_",bow$y_from))
bow=bow[order(bow$ID_LL),]
bow$ID=as.numeric(bow$ID_LL)
backout=data.frame(ID_focal=unique(bow$ID),backarea_m2=as.numeric(st_area(bow[bow$simu=="backward",])))
backout=backout[sort(backout$ID_focal),]
overlap=NULL
for(id in backout$ID_focal){
  #plot(bow[bow$ID==id,"simu"])
  focalback=bow[bow$ID==id&bow$simu=="backward",]
  foreset=bow[bow$ID!=id&bow$simu=="forward","ID"]
  ints=st_intersection(x = focalback,y=foreset)
  IDints=ints$ID.1
  AREAints=st_area(ints)
  Noverlaps=length(IDints)
  if(length(IDints)<1){
    thisoverlap=data.frame(subset(backout,ID_focal==id),ID_int=NA,Overlap_m2=0)
    thisoverlap$SourceProp=0
  }else{
    thisoverlap=data.frame(subset(backout,ID_focal==id)[rep(1,Noverlaps),],ID_int=IDints,Overlap_m2=as.numeric(AREAints))
    thisoverlap$SourceProp=thisoverlap$Overlap_m2/thisoverlap$backarea_m2
  }
  overlap=rbind(overlap,thisoverlap)
  print(id)
}

ggplot()+geom_tile(aes(x=ID_focal,y=ID_int,fill=SourceProp),data=overlap)+
  coord_equal()+theme_bw()+xlab("Focal Station")+ylab("Source Station")+
  ggtitle("Proportion of Hydrodynamically Upstream Water Originating at a Source Station")

connected_nodes=na.omit(overlap[,c("ID_focal","ID_int","SourceProp")])
all_nodes=st_drop_geometry(bow[bow$simu=="backward",c("ID","x_from","y_from")])
names(all_nodes)=c("ID","x","y")
stations=graph_from_data_frame(connected_nodes, directed = TRUE,
                               vertices = all_nodes)

plot.igraph(x = stations,layout=as.matrix(all_nodes[,c("x","y")]),
            vertex.size=6,vertex.label.cex=.65,main="Arrows Point to Source")

library(igraph)
library(ggnetwork)
library(intergraph)
library(sna)
library(network)

#Code below here does not work 

e <- network.edgecount(stations)
set.edge.attribute(stations, "type", sample(letters[24:26], e, replace = TRUE))
set.edge.attribute(stations, "day", sample(1:3, e, replace = TRUE))

ggplot(stations, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(aes(linetype = type), color = "grey50") +
  geom_nodes(aes(color = family, size = importance)) +
  theme_blank()

PctLink=matrix(0,nrow = 20,ncol=20)
for (i in 1:nrow(overlap)){
  PctLink[overlap$ID_focal[i],overlap$ID_int[i]]=overlap$SourceProp[i]
}
heatmap(PctLink)

