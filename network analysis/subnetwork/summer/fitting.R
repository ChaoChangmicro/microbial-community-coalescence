####----load R Package----####
library(tidyverse)
library(ggfun)
library(ggpmisc)
library(broom)
library(ggpubr)

df <- read.csv("summer_water.csv")
df <- read.csv("summer_biofilm.csv")
df <- read.csv("summer_soil.csv")
df <- read.csv("summer_sediment.csv")
df=df[,-1]
data <- gather(df, key = "type", value = "Value", -Coalescence)
####----Plot----####

# linear fitting
# set formula
formula <- y ~ x


# correlation  linear fitting
plot_correlation_linearfitting <- ggplot(data = data, aes(x = Coalescence, y = Value, group = type, color = type)) + 
  geom_point(aes(fill = type, shape = type), size = 4, color = "#000000", alpha = 0.7) + 
  geom_smooth(aes(color = type), method = "lm", formula = formula, se = F) +
  stat_cor(method = "pearson",parse = TRUE,size=4) + 
 # stat_poly_eq(use_label(c("eq", "adj.R2", "P")),formula = formula, size = 4) +
  scale_shape_manual(values = c(21:25,23)) + 
  scale_fill_manual(values =  c("Node.number" = "#7fc97f",
                                "Edge.number" = "#beaed4",
                                "Degree" = "#fdc086",
                                "Average.path.length" = "#fb9a99",
                                "Betweenness.centralization" = "#386cb0",
                                "Modularity" = "#BEF714",
                                "Vlnerability" = "#B1B336")) +  
  scale_color_manual(values = c("Node.number" = "#7fc97f",
                                "Edge.number" = "#beaed4",
                                "Degree" = "#fdc086",
                                "Average.path.length" = "#fb9a99",
                                "Betweenness.centralization" = "#386cb0",
                                "Modularity" = "#BEF714",
                                "Vlnerability" = "#B1B336")) +  
  # ggtitle(label = "Correlation and linear fitting ",
  #         subtitle = "Correlation and linear fitting") + 
  theme_bw() + 
  theme(panel.grid = element_blank(),
        legend.background = element_rect(fill = "white", color = "white"), 
        legend.position = "none",  
        axis.text = element_text(size = 13, color = "#000000"),
        axis.title = element_text(size = 15),  
        plot.title = element_text(hjust = 0.5, size = 20),
        legend.title = element_blank(), 
        legend.text = element_text(size = 10),
        plot.subtitle = element_text( size = 15), 
        strip.text = element_text(size = 15)  
  ) + 
  facet_wrap(~type, scales = "free_y")+
  labs(x = "Multi-source community coalescence", y = "Network topological properties", subtitle = "Summer-Water")
plot_correlation_linearfitting
p=plot_correlation_linearfitting

ggsave("summer_water.pdf",plot = p,width = 400,height = 300,units = "mm")
ggsave("summer_biofilm.pdf",plot = p,width = 400,height = 300,units = "mm")
ggsave("summer_soil.pdf",plot = p,width = 400,height = 300,units = "mm")
ggsave("summer_sediment.pdf",plot = p,width = 400,height = 300,units = "mm")

