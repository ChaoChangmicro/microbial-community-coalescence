library(tidyverse)
library(ggsignif)
library(ggsci)
library(ggprism)
feast = read.csv(file = "summer.csv",sep=',',header=T)
colnames(feast)
feast.p = feast %>% pivot_longer(cols = -Habitat)

compared = list(c("Mainstream","Tributary"))
feast.p$g = paste(feast.p$Habitat,feast.p$name,sep=".")


p = ggplot(data = feast.p,aes(x=Habitat,y=value,fill=name)) +
  geom_boxplot(alpha=0.75,outlier.size = 1, 
               outlier.color = "grey", 
               outlier.fill = "grey",width=0.4)+
  geom_point(pch=21,size=0.7,
             position = position_dodge(0.4))+
  labs(x = NULL,y = "Richness")

p

p.values <- sapply(split(feast.p, feast.p$Habitat), function(x){wilcox.test(value~name, x)$p.value})
labels <- symnum(p.values, corr = FALSE, cutpoints = c(0,  .001,.01,.05, 1), symbols = c("***","**","*","n.s."))
y.values <- sapply(split(feast.p, feast.p$Habitat), function(x){max(sapply(split(x, x$name), function(xx){boxplot(x$value, plot=F)$stats[5, ]}))})-200

p <- p + geom_signif(y_position = y.values, 
                     xmin = levels(factor(feast.p$Habitat)), 
                     xmax = levels(factor(feast.p$Habitat)), 
                     annotations = labels,tip_length = 0,
                     textsize = 9,color="tomato3")+
  scale_fill_manual(values = c("#339933", "#990000"))+
  scale_color_manual(values = c("#339933", "#990000"))+
   theme_bw()+
  theme(axis.text = element_text(size = 15),axis.title = element_text(size = 20),
        legend.text = element_text(size =10),legend.title = element_text(size = 15))+
  
  theme(axis.ticks = element_line(size = 0.4))+  
  theme(axis.ticks.length = unit(0.6,"lines"))+  
  theme(panel.grid=element_blank())
  
  p
  
  
  
  theme(axis.text = element_text(size = 30),axis.title = element_text(size = 35),
        legend.text = element_text(size = 30),legend.title = element_text(size = 35),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),units=,"cm"),
       # axis.line = element_line(color = "black",size = 0.4),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.2,color = "#e5e5e5"),
        axis.text.y = element_text(color="black",size=10),
        axis.text.x = element_text(margin = margin(t = -5),color="black",size=10),
        legend.position = "none",
        panel.spacing = unit(0,"lines"))+
  coord_cartesian()

p
ggsave("summer.pdf",plot = p,width = 600,height = 300,units = "mm")





feast.p %>%
  ggplot(aes(x=g,y=value)) +
  stat_boxplot(geom="errorbar",position=position_dodge(width=0.2),width=0.1)+
  geom_boxplot(position=position_dodge(width =0.2),width=0.4)+
  geom_point(aes(fill=name,size=value,alpha=value),pch=21,
             position = position_dodge(0.2))+
  scale_size_continuous(range=c(1,3))+
  geom_signif(comparisons = list(c("Summer","Winter")),
              map_signif_level=T,vjust=0.5,color="black",
              textsize=5,test=wilcox.test)+

  scale_fill_npg()+
  scale_x_discrete(guide = "prism_bracket")+
  scale_y_continuous(limits = c(0,90),minor_breaks = seq(0,90,5),guide = "prism_offset_minor")+
  labs(x=NULL,y=NULL)+
  theme_prism(base_line_size =0.5)+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),units=,"cm"),
        axis.line = element_line(color = "black",size = 0.4),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.2,color = "#e5e5e5"),
        axis.text.y = element_text(color="black",size=10),
        axis.text.x = element_text(margin = margin(t = -5),color="black",size=10),
        legend.position = "none",
        panel.spacing = unit(0,"lines"))+
  coord_cartesian()
