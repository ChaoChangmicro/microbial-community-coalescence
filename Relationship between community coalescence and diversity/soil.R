
dat <- read.csv('soil.csv', sep = ',', row.names = 1)
dat <- dat[,-1]

dat <- data.frame(scale(dat))


fit.lm <- lm(div~w+s+b+c, data = dat)
summary(fit.lm)  
stat.lm <- summary(fit.lm)$coefficients
stat.lm


library(ggplot2)

stat.lm <- data.frame(stat.lm, check.names = FALSE)
stat.lm$env <- rownames(stat.lm)
stat.lm$sig <- ifelse(stat.lm$'Pr(>|t|)'<0.05, '0', '1')

ggplot(stat.lm[-1, ], aes(x = env, y = Estimate)) +  
  geom_point(aes(shape = sig), size = 2, show.legend = FALSE) + 
  scale_shape_manual(values = c(15, 0)) +
  geom_errorbar(aes(ymin = Estimate - `Std. Error`, ymax = Estimate + `Std. Error`), width = 0.25) + 
  geom_hline(yintercept = 0, linetype = 2) +  
  labs(x = '', y = 'Standardized Mean', title = expression(R^{2}*'.adj = 0.21, P < 0.05')) +  
  theme(panel.grid = element_blank(), panel.background = element_blank(), plot.title = element_text(size = 9), 
        axis.ticks.y = element_blank(), axis.line.x = element_line(color = 'black')) +
  coord_flip()  
