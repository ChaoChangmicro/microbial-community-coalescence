## rarefaction curve

library(ggplot2)
library(reshape2)
library(ggalt)
library(vegan)
library(patchwork)

# summer
otu = read.csv("summer.csv",sep=',',header=T,row.names = 1)
g = read.csv(file = "summer_group.csv",sep=",",header=T)

#winter
otu = read.csv("winter.csv",sep=',',header=T,row.names = 1)
g = read.csv(file = "winter_group.csv",sep=",",header=T)

## 1 single habitat
x = t(otu)


## 2 five habitats-----
nn = unique(g$habitat)
otu.sum = c()
for (i in 1:4){ # i = 1
  mat = match(g[g$habitat==nn[i],]$sample,colnames(otu))
  otu.sum = cbind(otu.sum,rowSums(otu[,mat]))
}
colnames(otu.sum) = nn
otu.sum = as.data.frame(otu.sum)
x = t(otu.sum)
##-----


step = 1000 # single habitat
 step = 50000 # five habitats
read = max(rowSums(x))
all.alpha = c()
for (i in seq(1,read,step)){  
  rare.otu = rrarefy(x,i)  
  
  alpha.div <- function(x){
    result <- rowSums(x > 0)
  }
  rare.alpha <- alpha.div(rare.otu)
  all.alpha <- rbind(all.alpha,rare.alpha)
  print(i)
}
all.alpha <-  as.data.frame(all.alpha)
all.alpha$id <- seq(1,read,step)
rare.alpha <- all.alpha

gg.rare = melt(rare.alpha,
               id.vars = "id")

#- do not do this for five habitats
mat = match(gg.rare$variable,g$sample)
rare = cbind(gg.rare,g[mat,])
#-

p1 = ggplot(rare,aes(id,value,group = sample,color = habitat))+
  stat_smooth(geom="line",alpha=.5,size=1.2)+
  labs(x = 'Sequence number', y = "Richness", color = NULL)+
  scale_fill_manual(values = c("#339933","#990000","#CC9900","#99CCFF"))+
  scale_color_manual(values = c("#339933","#990000","#CC9900","#99CCFF"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 15),
        axis.text = element_text(size = 30,colour = "black"),axis.title = element_text(size = 35),
        legend.text = element_text(size = 30,colour = "black"),legend.title = element_text(size = 35))+
  theme(axis.ticks = element_line(size = 0.6))+ 
  theme(axis.ticks.length = unit(0.6,"lines"))+  
  theme(panel.grid=element_blank()) 
p1

p2 = ggplot(gg.rare,aes(id,value,group = variable,color = variable))+
  stat_smooth(geom="line",alpha=.5,size=1.2)+
  labs(x = 'Sequence number', y = "Richness", color = NULL)+
  scale_fill_manual(values = c("#339933","#990000","#CC9900","#99CCFF"))+
  scale_color_manual(values = c("#339933","#990000","#CC9900","#99CCFF"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 15),
        axis.text = element_text(size = 30,colour = "black"),axis.title = element_text(size = 35),
        legend.text = element_text(size = 30,colour = "black"),legend.title = element_text(size = 35))+
  theme(axis.ticks = element_line(size = 0.6))+ 
  theme(axis.ticks.length = unit(0.6,"lines"))+ 
  theme(panel.grid=element_blank()) 
p2

#-
summer.p = p1
summer.rare = rare

summer.sum.p = p2
summer.rare.sum = rare

winter.p = p1
winter.rare = rare

winter.sum.p = p2
winter.rare.sum = rare
#-

(summer.p | summer.sum.p) / (winter.p | winter.sum.p)

ggsave("summer.p.pdf",plot = summer.p,width = 500,height = 300,units = "mm")
ggsave("summer.sum.p.pdf",plot = summer.sum.p,width = 500,height = 300,units = "mm")
ggsave("winter.p.pdf",plot = winter.p,width = 500,height = 300,units = "mm")
ggsave("winter.sum.p.pdf",plot = winter.sum.p,width = 500,height = 300,units = "mm")
