
library(Hmisc)


genus<-read.csv("summer.csv",row.names = 1)
genus_t<-t(genus)
rel <- genus_t/rowSums(genus_t)
genus <- t(rel)

genus <- genus[which(rowSums(genus) >= 0.005), ]    
genus <- as.data.frame(genus)
genus1 <- genus
genus1[genus1>0] <- 1
genus <- genus[which(rowSums(genus1) >= 5), ]  


genus_corr <- rcorr(t(genus), type = 'spearman')


r <- genus_corr$r
r[abs(r) < 0.7] <- 0


p <- genus_corr$P
p <- p.adjust(p, method = 'fdr')    
p[p>=0.001] <- -1
p[p<0.001 & p>=0] <- 1
p[p==-1] <- 0


z <- r * p
diag(z) <- 0   
head(z)[1:6,1:6]
p=data.frame(z, check.names = FALSE)

write.csv(z,file="summer_network.csv")