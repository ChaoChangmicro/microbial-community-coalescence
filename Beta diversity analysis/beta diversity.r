## dissimilarity and betapart

library(tidyverse)
library(ggsignif)
library(vegan)
library(NST)

#  AB dissimilarity
# summer
b = read.csv(file = "summer_biofilm.csv",header=T,row.names = 1)
a.dis = vegdist(t(b),method = "bray")
b = dist.3col(a.dis)

se = read.csv(file = "summer_sediment.csv",header=T,row.names = 1)
a.dis = vegdist(t(se),method = "bray")
se = dist.3col(a.dis)

s = read.csv(file = "summer_soil.csv",header=T,row.names = 1)
a.dis = vegdist(t(s),method = "bray")
s = dist.3col(a.dis)

w = read.csv(file = "summer_water.csv",header=T,row.names = 1)
a.dis = vegdist(t(w),method = "bray")
w = dist.3col(a.dis)


summer.r = cbind(b$dis,se$dis,s$dis,w$dis)
colnames(summer.r) = c("Biofilm","Sediment","Soil","Water")
summer.r = as.data.frame(summer.r)

##winter
b = read.csv(file = "winter_biofilm.csv",header=T,row.names = 1)
a.dis = vegdist(t(b),method = "bray")
b = dist.3col(a.dis)

se = read.csv(file = "winter_sediment.csv",header=T,row.names = 1)
a.dis = vegdist(t(se),method = "bray")
se = dist.3col(a.dis)

s = read.csv(file = "winter_soil.csv",header=T,row.names = 1)
a.dis = vegdist(t(s),method = "bray")
s = dist.3col(a.dis)

w = read.csv(file = "winter_water.csv",header=T,row.names = 1)
a.dis = vegdist(t(w),method = "bray")
w = dist.3col(a.dis)


winter.r = cbind(b$dis,se$dis,s$dis,w$dis)
colnames(winter.r) = c("Biofilm","Sediment","Soil","Water")
winter.r = as.data.frame(winter.r)

summer.r$Region = "Summer";winter.r$Region = "Winter"
summer.r.p = summer.r %>% pivot_longer(cols = -Region)
winter.r.p = winter.r %>% pivot_longer(cols = -Region)

all.r = rbind(summer.r,winter.r)

all.r.p = all.r %>% pivot_longer(cols = -Region)

p3=ggplot(data = all.r.p, aes(x = name, y = value,fill = Region)) + 
  geom_boxplot(outlier.size = 1, 
               outlier.color = "grey", 
               outlier.fill = "grey")+ 
  labs(x="Habitat", y="Dissimilarity") +
  scale_fill_manual(values = c("#99CCFF","#339933"))+
  scale_color_manual(values = c("#99CCFF","#339933"))+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 15),
        axis.text = element_text(size = 18),axis.title = element_text(size = 18),
        legend.text = element_text(size = 18),legend.title = element_text(size = 20))
p3

p.summer=ggplot(data = summer.r.p, aes(x = name, y = value,fill = name,color = name)) + 
  geom_boxplot(alpha=.5,outlier.alpha=.3)+ 
  labs(x="Habitat", y="Bray-Curtis Dissimilarity") +
   scale_fill_manual(values = c("#66CC33","#996600","#FFCC33","#0099FF"))+
   scale_color_manual(values = c("#66CC33","#996600","#FFCC33","#0099FF"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 25),
        axis.text = element_text(size = 30),axis.title = element_text(size = 35),
        legend.text = element_text(size = 30),legend.title = element_text(size = 35),
        legend.position = "none")+
  scale_y_continuous(limits = c(0,1),breaks = seq(0,1,0.25))
p.summer


p.winter=ggplot(data = winter.r.p, aes(x = name, y = value,fill = name,color = name)) + 
  geom_boxplot(alpha=.5,outlier.alpha=.3)+ 
  labs(x="Habitat", y="Bray-Curtis Dissimilarity") +
   scale_fill_manual(values = c("#66CC33","#996600","#FFCC33","#0099FF"))+
   scale_color_manual(values = c("#66CC33","#996600","#FFCC33","#0099FF"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 25),
        axis.text = element_text(size = 30),axis.title = element_text(size = 35),
        legend.text = element_text(size = 30),legend.title = element_text(size = 35),
        legend.position = "none")+
  scale_y_continuous(limits = c(0,1),breaks = seq(0,1,0.25))
p.winter

Total <- ggpubr::ggarrange(p.summer,p.winter,ncol = 2,nrow = 1,align = "hv")
Total

library(patchwork)
p = (p.summer | p.winter) + plot_layout(guides='collect') &
  theme(legend.position="NULL")
p
getwd()
ggsave("summer_winter_dis.pdf",plot = p3,width = 500,height = 200,units = "mm")
ggsave("dis.pdf",plot = Total,width = 500,height = 200,units = "mm")

# betapart
library(betapart)
library(NST)
library(tidyverse)

otu.summer = read.csv(file = "summer.csv",sep=",",row.names=1,header=T,check.names=F)
g.summer = read.csv(file = "summer_group.csv",sep=",",header=T,check.names=F)
title = "Summer"

otu.winter = read.csv(file = "winter.csv",sep=",",row.names=1,header=T,check.names=F)
g.winter = read.csv(file = "winter_group.csv",sep=",",header=T,check.names=F)
title = "Winter"


otu.summer = ifelse(otu.summer>0,1,0)
otu.winter = ifelse(otu.winter>0,1,0)

 
b.summer<-beta.pair(t(otu.summer))
b.winter<-beta.pair(t(otu.winter))


gc()


r.summer.repl = dist.3col(b.summer$beta.sim)
r.summer.nest = dist.3col(b.summer$beta.sne)
r.summer.tota = dist.3col(b.summer$beta.sor)

r.winter.repl = dist.3col(b.winter$beta.sim)
r.winter.nest = dist.3col(b.winter$beta.sne)
r.winter.tota = dist.3col(b.winter$beta.sor)

gc()


r.summer.repl$name1 = g.summer[match(r.summer.repl$name1,g.summer$sample),]$habitat
r.summer.repl$name2 = g.summer[match(r.summer.repl$name2,g.summer$sample),]$habitat
r.summer.nest$name1 = g.summer[match(r.summer.nest$name1,g.summer$sample),]$habitat
r.summer.nest$name2 = g.summer[match(r.summer.nest$name2,g.summer$sample),]$habitat
r.summer.tota$name1 = g.summer[match(r.summer.tota$name1,g.summer$sample),]$habitat
r.summer.tota$name2 = g.summer[match(r.summer.tota$name2,g.summer$sample),]$habitat


r.winter.repl$name1 = g.winter[match(r.winter.repl$name1,g.winter$sample),]$habitat
r.winter.repl$name2 = g.winter[match(r.winter.repl$name2,g.winter$sample),]$habitat
r.winter.nest$name1 = g.winter[match(r.winter.nest$name1,g.winter$sample),]$habitat
r.winter.nest$name2 = g.winter[match(r.winter.nest$name2,g.winter$sample),]$habitat
r.winter.tota$name1 = g.winter[match(r.winter.tota$name1,g.winter$sample),]$habitat
r.winter.tota$name2 = g.winter[match(r.winter.tota$name2,g.winter$sample),]$habitat


gc()


r.summer = cbind(r.summer.repl[,1:2],replace = r.summer.repl[,3]/r.summer.tota[,3])
r.summer$nest = 1-r.summer$replace

r.winter = cbind(r.winter.repl[,1:2],replace = r.winter.repl[,3]/r.winter.tota[,3])
r.winter$nest = 1-r.winter$replace



res.summer = r.summer %>% group_by(name1,name2) %>% 
  summarise(replace.m = mean(replace)) %>% 
  mutate(nest.m = 1-replace.m);res.summer

res.winter = r.winter %>% group_by(name1,name2) %>% 
  summarise(replace.m = mean(replace)) %>% 
  mutate(nest.m = 1-replace.m);res.winter

# output, Remove duplicate results
getwd()
write.csv(res.summer, file = "betapart.summer.csv")
write.csv(res.winter, file = "betapart.winter.csv")

#### and then input
r.summer = read.table(file = "betapart.summer.txt",header=T,sep="\t")
r.winter = read.table(file = "betapart.winter.txt",header=T,sep="\t")

r.summer$g = paste(r.summer$name1," and ",r.summer$name2,sep="")
r.winter$g = paste(r.winter$name1," and ",r.winter$name2,sep="")

r.summer$nest.per = paste("(",round(r.summer$nest.m *100,2),"%)", sep = '')
r.winter$nest.per = paste("(",round(r.winter$nest.m *100,2),"%)", sep = '')

p.summer = r.summer %>% pivot_longer(cols = -c(name1,name2,g,nest.per))
p.winter = r.winter %>% pivot_longer(cols = -c(name1,name2,g,nest.per))

library(ggmap)
summer = ggplot(data = p.summer,aes(x=g,y=value,fill=name,group=g))+
  geom_bar(stat = 'identity', position = 'stack', width = 0.9)+
  coord_polar(theta = 'y')+ labs(x = '', y = '', title = '')+
  geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]/100), 
                x = cumsum(value), label = nest.per)) +
  theme_nothing() 
summer
winter = ggplot(data = p.winter,aes(x=g,y=value,fill=name,group=g))+
  geom_bar(stat = 'identity', position = 'stack', width = 0.9)+
  coord_polar(theta = 'y')+ labs(x = '', y = '', title = '')+
  geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]/100), 
                x = cumsum(value), label = nest.per)) +
  theme_nothing() 
winter
write.csv(p.summer,file = "p.summer.csv")
ggsave("summer.pdf",plot = summer,width = 500,height = 200,units = "mm")

write.csv(p.winter,file = "p.winter.csv")
ggsave("winter.pdf",plot = winter,width = 500,height = 200,units = "mm")