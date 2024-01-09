library(tidyverse)
library(ggsignif)
library(ggsci)
library(ggprism)
feast = read.csv(file = "multisource.csv",sep=',',header=T)
colnames(feast)
feast.p = feast %>% pivot_longer(cols = -Sink)

compared = list(c("Summer","Winter"))
feast.p$g = paste(feast.p$Sink,feast.p$name,sep=".")


p = ggplot(data = feast.p,aes(x=Sink,y=value,fill=name)) +
  geom_boxplot(alpha=0.75,outlier.size = 1, 
               outlier.color = "grey", 
               outlier.fill = "grey",width=0.8)+
  geom_point(pch=21,
             position = position_dodge(0.8))+
  labs(x = NULL,y = "Community coalescence")

p

p.values <- sapply(split(feast.p, feast.p$Sink), function(x){wilcox.test(value~name, x)$p.value})
labels <- symnum(p.values, corr = FALSE, cutpoints = c(0,  .001,.01,.05, 1), symbols = c("***","**","*","n.s."))
y.values <- sapply(split(feast.p, feast.p$Sink), function(x){max(sapply(split(x, x$name), function(xx){boxplot(x$value, plot=F)$stats[5, ]}))})

p <- p + geom_signif(y_position = y.values, 
                     xmin = levels(factor(feast.p$Sink)), 
                     xmax = levels(factor(feast.p$Sink)), 
                     annotations = labels,tip_length = 0,
                     textsize = 4,color="tomato3")+
  scale_fill_manual(values = c("#339933", "#990000"))+
  scale_color_manual(values = c("#339933", "#990000"))+
   theme_bw()+
  theme(axis.text = element_text(size = 15),axis.title = element_text(size = 20),
        legend.text = element_text(size =10),legend.title = element_text(size = 15),
        axis.text.x = element_text(angle = 60, hjust = 1))+
  
  theme(axis.ticks = element_line(size = 0.4))+  
  theme(axis.ticks.length = unit(0.6,"lines"))+  
  theme(panel.grid=element_blank())
  
  p
  
  
  
  
