
phylum_top10 <- read.csv('summer_top10.csv', row.names = 1, stringsAsFactors = FALSE, check.names = FALSE)

library(reshape2)
library(ggplot2)


phylum_top10$Taxonomy <- factor(rownames(phylum_top10), levels = rev(rownames(phylum_top10)))
phylum_top10 <- melt(phylum_top10, id = 'Taxonomy')


group <- read.csv('summer_group.csv', stringsAsFactors = FALSE, check.names = FALSE)
names(group)[1] <- 'variable'
phylum_top10 <- merge(phylum_top10, group, by = 'variable')
phylum_top10$group<- factor(phylum_top10$group, levels = c("Water","Soil","Biofilm","Sediment"))


p <- ggplot(phylum_top10, aes(variable, 100 * value, fill = Taxonomy)) +
  geom_col(position = 'stack', width = 0.7) +
  facet_wrap(~group, scales = 'free_x', ncol = 2) +
  #scale_fill_brewer(palette = 'BrBG',direction = -1) +
  scale_fill_manual(values =  rev(c('#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3', '#FDB462', '#B3DE69', '#FCCDE5', '#BC80BD', '#CCEBC5', 'gray')))  +
  labs(x = '', y = 'Relative Abundance(%)') +
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent'), strip.text = element_text(size = 12)) +
  theme(axis.text = element_text(size = 10,color = "#000000"), axis.title = element_text(size = 13), legend.title = element_blank(), legend.text = element_text(size = 11),axis.text.x = element_text(angle=90, hjust=1, vjust=.5))
p
#ggsave('ggplot2_plot.pdf', p, width = 8, height = 6)
ggsave('ggplot2_plot.png', p, width = 8, height = 6)
theme_get()$text
