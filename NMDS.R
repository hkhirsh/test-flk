#nmds from john morris
#NMDS plot-------------------------------------
nMDS <- metaMDS(cover, distance = "bray", k = 3, maxit = 999, trymax = 250, wascores = TRUE)
nMDS
stressplot(nMDS)

#extract NMDS scores (x and y coordinates)
data.scores = as.data.frame(scores(nMDS)$sites)

#add columns to data frame
data.scores$Time = dat$TIME
data.scores$Treatment = dat$TREATMENT
data.scores$Habitat = dat$HABITAT

head(data.scores)

levels(data.scores$Time)
levels(data.scores$Time)<-c("2022", "2010") #rename by year

#Species Fit
nMDS.spp.fit <- envfit(nMDS, cover, permutations = 999)
head(nMDS.spp.fit)

spp.scrs <- as.data.frame(scores(nMDS.spp.fit, display = "vectors"))
spp.scrs <- cbind(spp.scrs, Species = rownames(spp.scrs))  

xx = ggplot() +
  geom_point(data.scores, mapping = aes(x = NMDS1, y = NMDS2, shape = Time, color = Treatment), size = 4, stroke = 1.5)+
  scale_shape_manual(values=c(19,8))+ #pick the shapes for the symbols
  stat_ellipse(data.scores, mapping = aes(x = NMDS1, y = NMDS2, shape = Time, color = Treatment), linewidth = 1.2)+
  geom_segment(data = spp.scrs, aes(x=0, xend = NMDS1, y=0, yend = NMDS2),
               arrow = arrow(length = unit(0.1, "cm")), colour = "grey")+
  geom_text(data = spp.scrs, aes(x = NMDS1, y = NMDS2, label = Species), size = 4, colour = "grey")+
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold", size = 12),
        legend.text = element_text(size = 12, face ="bold", colour ="black"),
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"),
        legend.title = element_text(size = 14, colour = "black", face = "bold"),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank()) +
  labs(x = "NMDS1", colour = "Treatment", y = "NMDS2", shape = "Time")  +
  scale_colour_manual(values = c("#F0A87B", "#33A0A3")) #adjust colors to your yellow and purple