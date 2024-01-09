
rm(list=ls())

df <- read.csv("summer_βnti.csv")
df <- read.csv("winter_βnti.csv")

library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(ggpmisc) # Miscellaneous Extensions to 'ggplot2'
library(RColorBrewer) # ColorBrewer Palettes
library(grid) # The Grid Graphics Package
library(scales) # Scale Functions for Visualization



#col<-c("#be0027", "#cf8d2e")  "#AAD875"

#color1 <- colorRampPalette(brewer.pal(11,"PiYG"))(30)
#color2 <- colorRampPalette(brewer.pal(11,"PuOr"))(30)
col <- c("#F2BADC", "#F4F6F0", "#BCE38D")
p2 <- ggplot(df,aes(Coalescence,NTI))+
  geom_point(shape=21,fill = "black",size=3,alpha=0.5)+

  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 3, raw = TRUE),
              linetype=1,alpha=0.5,colour="red")+
  geom_hline(yintercept = 2, linetype = "dashed", color = "black",linewidth=1) +
  geom_hline(yintercept = -2, linetype = "dashed", color = "black",linewidth=1) +

  stat_poly_eq(formula = y ~ poly(x, 3, raw = TRUE), 
               aes(label = paste(after_stat(eq.label),
                                             after_stat(adj.rr.label),
                                             ..p.value.label..,sep = "~~~")), 
               parse = TRUE,label.x = "right") +
  scale_fill_manual(values = col)+
  scale_color_manual(values = col)+
  theme_bw()+
  theme(panel.grid=element_blank(),
        axis.text=element_text(color='#333c41',size=12),
        legend.text = element_text(color='#333c41',size=12),
        legend.title = element_blank(),
        legend.position = c(0.9,0.9))+
  labs(x="Community coalescence",y="βNTI")+
  annotate("rect",
           xmin = -Inf,
           xmax = Inf,
           ymin = c(-Inf,-2,2), ymax = c(-2,2,Inf), alpha = 0.5, fill = col)





  

p2

