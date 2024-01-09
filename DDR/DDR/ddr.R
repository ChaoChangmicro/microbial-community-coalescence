library(vegan)


env<-read.csv("group_s_bio.csv",row.names=1)
abund<-read.csv("summer_biofilm.csv",row.names=1)

env<-read.csv("group_s_s.csv",row.names=1)
abund<-read.csv("summer_soil.csv",row.names=1)

env<-read.csv("group_s_se.csv",row.names=1)
abund<-read.csv("summer_sediment.csv",row.names=1)

env<-read.csv("group_s_w.csv",row.names=1)
abund<-read.csv("summer_water.csv",row.names=1)

env<-read.csv("group_w_bio.csv",row.names=1)
abund<-read.csv("winter_biofilm.csv",row.names=1)

env<-read.csv("group_w_s.csv",row.names=1)
abund<-read.csv("winter_soil.csv",row.names=1)

env<-read.csv("group_w_se.csv",row.names=1)
abund<-read.csv("winter_sediment.csv",row.names=1)

env<-read.csv("group_w_w.csv",row.names=1)
abund<-read.csv("winter_water.csv",row.names=1)

abund<-t(abund)
dist.abund <- vegdist(abund, method = 'bray')

#geographical distance
library(geosphere)
geo <- data.frame(env$lon, env$lat)
d.geo <- distm(geo, fun = distHaversine)      
dist.geo <- as.dist(d.geo)

##Mantel tests

#
abund_geo <- mantel(dist.abund, dist.geo, method = 'spearman', permutations = 9999, na.rm = TRUE)
abund_geo



library(ggplot2)
library(ggpubr)

aa <- as.vector(dist.abund)
gg <- as.vector(dist.geo)
mat <- data.frame(aa, gg)


mm <- ggplot(mat, mapping = aes(y = (1-aa)* 100, x = gg/1000)) + 
  geom_point(size =0.3) + 
  geom_smooth(method = "lm", colour = "red",alpha = 0.2) + 
  stat_cor(aes(y = (1-aa)* 100, x = gg/1000), method = "pearson")+#添加检验结果，
  labs(x = "Geographic Distance (100Km)", y = "Community Similarity (%)", fill = "Difference in Temperature (C)") + 
  theme( axis.text.x = element_text(face = "bold",colour = "black", size = 12), 
         axis.text.y = element_text(face = "bold", size = 11, colour = "black"), 
         axis.title= element_text(face = "bold", size = 14, colour = "black"), 
         panel.background = element_blank(), 
         panel.border = element_rect(fill = NA, colour = "black"),
         legend.direction = "vertical",
         legend.text = element_text(size = 10, face = "bold"),
         legend.title = element_text(size = 11, face = "bold",angle = 90),
         aspect.ratio = 0.75) +
  labs(col="Environmental distance")

mm

ggsave("summer_biofilm_ddr.pdf",plot = mm,width = 200,height = 100,units = "mm")
ggsave("summer_soil_ddr.pdf",plot = mm,width = 200,height = 100,units = "mm")
ggsave("summer_sediment_ddr.pdf",plot = mm,width = 200,height = 100,units = "mm")
ggsave("summer_water_ddr.pdf",plot = mm,width = 200,height = 100,units = "mm")
ggsave("winter_biofilm_ddr.pdf",plot = mm,width = 200,height = 100,units = "mm")
ggsave("winter_soil_ddr.pdf",plot = mm,width = 200,height = 100,units = "mm")
ggsave("winter_sediment_ddr.pdf",plot = mm,width = 200,height = 100,units = "mm")
ggsave("winter_water_ddr.pdf",plot = mm,width = 200,height = 100,units = "mm")
