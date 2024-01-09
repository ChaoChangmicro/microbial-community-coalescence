library(tidyverse)
library(magrittr)
library(ggstatsplot)
df <- read.csv("summer_water.csv")
df <- read.csv("winter_water.csv")
df <- read.csv("summer_soil.csv")
df <- read.csv("winter_soil.csv")
df <- read.csv("summer_biofilm.csv")
df <- read.csv("winter_biofilm.csv")
df <- read.csv("summer_sediment.csv")
df <- read.csv("winter_sediment.csv")
ggscatterstats(data = df,y = diversity,x=coalescence,
               centrality.para = "mean",
               margins = "both",                                         
               xfill = "#CC79A7", 
               yfill = "#009E73", 
               marginal.type = "histogram",
               xlab="Community coalescence",
               ylab="Stochastic ratio",
               point.args = list(size = 10, alpha = 0.4))

ggsave("summer_water.pdf",width = 400,height = 300,units = "mm")
ggsave("winter_water.pdf",width = 400,height = 300,units = "mm")
ggsave("summer_soil.pdf",width = 400,height = 300,units = "mm")
ggsave("winter_soil.pdf",width = 400,height = 300,units = "mm")
ggsave("summer_biofilm.pdf",width = 400,height = 300,units = "mm")
ggsave("winter_biofilm.pdf",width = 400,height = 300,units = "mm")
ggsave("summer_sediment.pdf",width = 400,height = 300,units = "mm")
ggsave("winter_sediment.pdf",width = 400,height = 300,units = "mm")
