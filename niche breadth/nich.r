#  niche breadth
library(EcolUtils)
library(vegan)
library(tidyverse)
library(ggsignif)

gc()
summer.biofilm = read.csv(file = "summer_biofilm.csv",header=T,row.names = 1)
summer.sediment = read.csv(file = "summer_sediment.csv",header=T,row.names = 1)
summer.soil = read.csv(file = "summer_soil.csv",header=T,row.names = 1)
summer.water = read.csv(file = "summer_water.csv",header=T,row.names = 1)

winter.biofilm = read.csv(file = "winter_biofilm.csv",header=T,row.names = 1)
winter.sediment = read.csv(file = "winter_sediment.csv",header=T,row.names = 1)
winter.soil = read.csv(file = "winter_soil.csv",header=T,row.names = 1)
winter.water = read.csv(file = "winter_water.csv",header=T,row.names = 1)


summer.biofilm.n <- spec.gen(t(summer.biofilm), niche.width.method = 'levins', perm.method = 'quasiswap', n = 99, probs = c(0.025, 0.975))
summer.sediment.n <- spec.gen(t(summer.sediment), niche.width.method = 'levins', perm.method = 'quasiswap', n = 99, probs = c(0.025, 0.975))
summer.soil.n <- spec.gen(t(summer.soil), niche.width.method = 'levins', perm.method = 'quasiswap', n = 99, probs = c(0.025, 0.975))
summer.water.n<- spec.gen(t(summer.water), niche.width.method = 'levins', perm.method = 'quasiswap', n = 99, probs = c(0.025, 0.975))

winter.biofilm.n <- spec.gen(t(winter.biofilm), niche.width.method = 'levins', perm.method = 'quasiswap', n = 99, probs = c(0.025, 0.975))
winter.sediment.n <- spec.gen(t(winter.sediment), niche.width.method = 'levins', perm.method = 'quasiswap', n = 99, probs = c(0.025, 0.975))
winter.soil.n <- spec.gen(t(winter.soil), niche.width.method = 'levins', perm.method = 'quasiswap', n = 99, probs = c(0.025, 0.975))
winter.water.n<- spec.gen(t(winter.water), niche.width.method = 'levins', perm.method = 'quasiswap', n = 99, probs = c(0.025, 0.975))


summer.n = list(summer.biofilm.n$observed,summer.sediment.n$observed,
            summer.soil.n$observed,summer.water.n$observed)

summer <- do.call(cbind.data.frame,
              lapply(lapply(summer.n, unlist), `length<-`,
                            max(lengths(summer.n))))
colnames(summer) = c("Biofilm","Sediment","Soil","Water")
summer$Region = "summer"
summer.p = summer %>% pivot_longer(cols = -Region) %>% na.omit()


winter.n = list(winter.biofilm.n$observed,winter.sediment.n$observed,
                winter.soil.n$observed,winter.water.n$observed)

winter <- do.call(cbind.data.frame,
                  lapply(lapply(winter.n, unlist), `length<-`,
                         max(lengths(winter.n))))
colnames(winter) = c("Biofilm","Sediment","Soil","Water")
winter$Region = "winter"
winter.p = winter %>% pivot_longer(cols = -Region) %>% na.omit()

p.summer = ggplot(data = summer.p,aes(x = name,y = value,fill = name,color=name))+
  geom_boxplot(alpha=.5,outlier.alpha = .1)+ 
  labs(x = "Summer",y = "Niche breadth")+
  scale_fill_manual(values = c("#339933","#990000","#CC9900","#99CCFF"))+
  scale_color_manual(values = c("#339933","#990000","#CC9900","#99CCFF"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 15),
        axis.text = element_text(size = 22),axis.title = element_text(size = 25),
        legend.text = element_text(size = 22),legend.title = element_text(size = 25))
p.summer


p.winter = ggplot(data = winter.p,aes(x = name,y = value,fill = name,color=name))+
  geom_boxplot(alpha=.5,outlier.alpha = .1)+
  labs(x = "Winter",y = "Niche breadth")+
  scale_fill_manual(values = c("#339933","#990000","#CC9900","#663366","#99CCFF"))+
  scale_color_manual(values = c("#339933","#990000","#CC9900","#663366","#99CCFF"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 15),
        axis.text = element_text(size = 22),axis.title = element_text(size = 25),
        legend.text = element_text(size = 22),legend.title = element_text(size = 25))
p.winter

write.csv(summer.p,file = "nich.summer.csv")
write.csv(winter.p,file = "nich.winter.csv")
library(patchwork)
p = (p.summer | p.winter) + plot_layout(guides='collect') &
  theme(legend.position="NULL")
p

ggsave("nich.pdf",plot = p,width = 500,height = 200,units = "mm")

