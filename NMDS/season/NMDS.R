library(vegan)

otu <- read.csv('asv_rarefied.csv', row.names = 1, sep = ',', stringsAsFactors = FALSE, check.names = FALSE)
otu <- data.frame(t(otu))


bray_dis <- vegdist(otu, method = 'bray')     


write.csv(as.matrix(bray_dis), file = "bray_distance.csv")


dis <- read.csv('bray_distance.csv', row.names = 1, sep = ',', stringsAsFactors = FALSE, check.names = FALSE)
bray_dis <- as.dist(dis)  


nmds_dis <- metaMDS(bray_dis, k = 2)
nmds_dis$stress

nmds_dis_site <- data.frame(nmds_dis$points)
write.table(nmds_dis_site, 'nmds_dis_site.txt', sep = '\t', col.names = NA, quote = FALSE)


nmds_dis_site <- read.delim('nmds_cluster.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
library(ggplot2)
library(plyr)

cluster_border <- ddply(nmds_dis_site, 'cluster', function(df) df[chull(df[[1]], df[[2]]), ])

p <- ggplot(data = nmds_dis_site, aes(x = MDS1, y = MDS2)) +
  geom_hline(yintercept =  0, colour = "grey80") +
  geom_vline(xintercept = 0, colour = "grey80") +
  geom_polygon(data = cluster_border,aes(fill = cluster), alpha = 0.3, show.legend = FALSE) +
  geom_point(aes(fill=cluster, size = 1, shape = Habitat)) +
  scale_shape_manual(values = c(21,24,23,25))+
   scale_fill_manual(values = c(  '#82AA73','#b75369'), name = "Season") +
  theme(axis.text = element_text(size = 12,color = "#000000"),panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent'), legend.key = element_rect(fill = 'transparent'))+
  guides(shape = guide_legend(order = 2, override.aes=list(size = 3)),
         fill = guide_legend(order = 1, override.aes=list(shape=21, size = 3)),
         size = FALSE)+ annotate(geom="text", x=-0.3, y=0.45, label="Stress = 0.13")
p
ggsave("season_nmds.pdf",plot = p,width = 200,height = 200,units = "mm")
