
rm(list=ls())
df <- read.csv("summer_diff.csv")
df <- read.csv("winter_diff.csv")


library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(ggpmisc) # Miscellaneous Extensions to 'ggplot2'
library(RColorBrewer) # ColorBrewer Palettes
library(grid) # The Grid Graphics Package
library(scales) # Scale Functions for Visualization
library(tidyverse)
library(magrittr)
library(ggstatsplot)

p <- ggplot(df,aes(coalescence,Richness_diff))+
  geom_point(shape=21,fill = "black",size=3,alpha=0.7)+
  theme_bw()+
  theme(panel.grid=element_blank(),
        axis.text=element_text(color='#333c41',size=12),
        legend.text = element_text(color='#333c41',size=12),
        legend.title = element_blank(),
        legend.position = c(0.9,0.9))+
  labs(x="Community coalescence",y="Absolute Richness difference in pairwise sample")


ggscatterstats(data = df,coalescence,Richness_diff,
               centrality.para = "mean",
               margins = "both",                                         
               xfill = "#CC79A7", 
               yfill = "#009E73", 
               marginal.type = "histogram",
               xlab="Community coalescence",
               ylab="Absolute Richness difference in pairwise sample",
               point.args = list(size = 5, alpha = 0.4))
  

