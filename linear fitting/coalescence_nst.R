library(ggplot2)

library(ggpmisc)
df <- read.csv("summer_coalescence_nst.csv")
df <- read.csv("winter_coalescence_nst.csv")
p <- ggplot(df,aes(Community.coalescence,Stochastic.ratio))+
  geom_point(shape=21,fill = "black",size=5,alpha=0.5)+

  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 2, raw = TRUE),
              linetype=1,alpha=0.5,colour="red")+

  stat_poly_eq(formula = y ~ poly(x, 2, raw = TRUE), 
               aes(label = paste(after_stat(eq.label),
                                 after_stat(adj.rr.label),
                                 after_stat(p.value.label),sep = "~~~")), 
               parse = TRUE,label.x = "right") +
  scale_fill_manual(values = col)+
  scale_color_manual(values = col)+
  theme_bw()+
  theme(panel.grid=element_blank(),
        axis.text=element_text(color='#333c41',size=12),
        legend.text = element_text(color='#333c41',size=12),
        legend.title = element_blank(),
        legend.position = c(0.9,0.9))+
  labs(x="Multi-source community coalescence",y="Stochastic ratio")
p
ggsave("summer_coalescence_nst.pdf",plot = p,width = 250,height = 200,units = "mm")
ggsave("winter_coalescence_nst.pdf",plot = p,width = 250,height = 200,units = "mm")
