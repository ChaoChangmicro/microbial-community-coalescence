rm(list = ls())

getwd()
library(ggplot2,car,palmerpenguins)
library(ggpubr)
aa<-read.csv("summer_div_dis.csv",row.names = 1)


dat_plot <- reshape2::melt(aa, id = 'Distance')

p1<-ggplot(dat_plot %>% na.omit()) +
  geom_smooth(aes(Distance, value, colour = variable), method = lm) +
  geom_point(aes(Distance, value, fill = variable), shape = 21, size = 2, colour = "black") +
  stat_cor(aes(Distance, value, colour = variable), method = "pearson", show.legend = FALSE) +
  
#  scale_colour_manual(values = c("cyl" = "#DD7694",
  #                               "disp"="#1f78b4",
  #                               "hp"="#00B76D",
  #                               "drat"="#FF5900",
  #                               "wt"="#ECBF4D",
  #                               "qsec"="#0D0DBB",
 #                                "vs"="#e31a1c"))+
  scale_fill_manual(values = c("Water" = "#DD7694",
                                 "Biofilm"="#1f78b4",
                                 "Soil"="#00B76D",
                       
                                 "Sediment"="#ECBF4D"))+
  scale_colour_manual(values = c("Water" = "#DD7694",
                               "Biofilm"="#1f78b4",
                               "Soil"="#00B76D",
                               
                               "Sediment"="#ECBF4D"))+
  theme(panel.background = element_rect(fill='white', colour='black'),
        panel.grid=element_blank())+
  theme_bw()+
  theme(axis.ticks.length = unit(0.4,"lines"), axis.ticks = element_line(color='black'),
        axis.line = element_line(colour = "black"), 
        axis.title.x=element_text(colour='black', size=18,face = "bold",vjust = 1.5),
        axis.title.y=element_text(colour='black', size=18,face = "bold",vjust = 1.5),
        axis.text.y=element_text(colour='black',size=15),
        axis.text.x=element_text(colour = "black",size = 15,
                                 hjust = 1,vjust = 0.5))+
  scale_x_reverse()

p1


gr <- c("cyl" ,
        "disp",
        "hp",
        "drat",
        "wt",
        "qsec",
        "vs")
R2 <- c()
R2_adj <- c()
p_value<- c()


for (i in gr) {
  fit_stat <- summary(lm(aa[['mpg']]~aa[[i]])) 
  R2 <- c(R2, fit_stat$r.squared)
  R2_adj<- c(R2_adj, fit_stat$adj.r.squared)
  p_value <- c(p_value, fit_stat$coefficients[2,4]) 
}

gr_result <- data.frame(row.names = gr, R2, R2_adj, p_value)
gr_result 
write.csv(gr_result ,"1.csv")



ggplot(dat_plot %>% na.omit()) +
  geom_smooth(aes(Distance, value, colour = variable), method = lm) +
  geom_point(aes(Distance, value, fill = variable), shape = 21, size = 2, colour = "black") +
  stat_cor(aes(Distance, value, colour = variable), method = "pearson", show.legend = FALSE) +
  scale_fill_manual(values = c("Water" = "#DD7694", "Biofilm" = "#1f78b4", "Soil" = "#00B76D", "Sediment" = "#ECBF4D")) +
  theme(
    panel.background = element_rect(fill = 'white', colour = 'black'),
    panel.grid = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    axis.ticks.length = unit(0.4, "lines"),
    axis.ticks = element_line(color = 'black'),
    axis.line = element_line(colour = "black"),
    axis.title.x = element_text(colour = 'black', size = 18, face = "bold", vjust = 1.5),
    axis.title.y = element_text(colour = 'black', size = 18, face = "bold", vjust = 1.5),
    axis.text.y = element_text(colour = 'black', size = 15),
    axis.text.x = element_text(colour = "black", size = 15, hjust = 1, vjust = 0.5)
  ) +
  theme_bw() +
  scale_x_reverse()