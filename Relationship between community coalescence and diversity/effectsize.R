
dat <- read.csv('biofilm.csv', sep = ',', row.names = 1)
dat <- read.csv('sediment.csv', sep = ',', row.names = 1)
dat <- read.csv('water.csv', sep = ',', row.names = 1)
dat <- read.csv('soil.csv', sep = ',', row.names = 1)
dat <- dat[,-1]

dat <- data.frame(scale(dat))


fit.lm <- lm(div~Water+Sediment+Biofilm+Soil, data = dat)
summary(fit.lm) 
stat.lm <- summary(fit.lm)$coefficients

stat.lm <- data.frame(stat.lm, check.names = FALSE)
stat.lm$env <- rownames(stat.lm)
stat.lm$sig <- ifelse(stat.lm$`Pr(>|t|)` < 0.001, '***',
                      ifelse(stat.lm$`Pr(>|t|)` < 0.01, '**',
                             ifelse(stat.lm$`Pr(>|t|)` < 0.05, '*', ' ')))

stat.lm$min=stat.lm$Estimate-stat.lm$`Std. Error`
stat.lm$max=stat.lm$Estimate+stat.lm$`Std. Error`
stat.lm$med=stat.lm$Estimate
stat.lm <-stat.lm[-1,]
stat.lm$group_col <- c("#e7a40e", "#78bee5", "#1c6891","#a59d70")

library(ggplot2)

ggplot(stat.lm)+

  geom_hline(yintercept = 0, linewidth = 0.3)+

  geom_linerange(aes(env, ymin = min, ymax = max, color = env,size=1), show.legend = F)+

  geom_point(aes(env, med, color = env,size=2)) +

  geom_text(aes(env, y = max + 0.05, label = sig, color = env), show.legend = F)+

  scale_color_manual(name = "",
                     values = c("Water" = "#d55e00",
                                "Sediment" = "#ffbd88",
                                "Biofilm" = "#0072b2",
                                "Soil" = "#7acfff"))+

  annotate("rect",
           xmin = c(0.5,1.5,2.5,3.5),
           xmax = c(1.5,2.5,3.5,4.5),
           ymin = -1, ymax = 1, alpha = 0.2, fill = rev(unique(stat.lm$group_col))) +

  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(expand = c(0,0))+
  xlab("Habitat type")+
  ylab("Coalescence effect size")+
  theme_bw()+
  theme(axis.text.y = element_text(color = rev(stat.lm$group_col)))+
  coord_flip()
ggsave("Biofilm.pdf", height = 6, width =6)
ggsave("Sediment.pdf", height = 6, width =6)
ggsave("Water.pdf", height = 6, width =6)
ggsave("Soil.pdf", height = 6, width =6)
