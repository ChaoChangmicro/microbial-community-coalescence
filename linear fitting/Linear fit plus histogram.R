library(tidyverse)
library(magrittr)
library(ggstatsplot)
df <- read.csv("summer_coalescence_nst.csv")
df <- read.csv("winter_coalescence_nst.csv")

ggscatterstats(data = df,y = Stochastic.ratio,x=Community.coalescence,
               centrality.para = "mean",
               margins = "both",                                         
               xfill = "#CC79A7", 
               yfill = "#009E73", 
               marginal.type = "histogram",
               xlab="Community coalescence",
               ylab="Stochastic ratio",
               point.args = list(size = 5, alpha = 0.4))

ggsave("summer.coalescence.nst.pdf",width = 400,height = 300,units = "mm")

ggsave("winter.coalescence.nst.pdf",width = 400,height = 300,units = "mm")
