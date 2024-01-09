#read table
spe <- read.csv('summer.csv', row.names = 1, sep = ',', check.names = FALSE)
genus_t<-t(spe)
rel <- genus_t/rowSums(genus_t)
spe<- as.data.frame(t(rel))

#Network adjacency matrix
g <- read.csv('summer_network.csv', row.names = 1, sep = ',', check.names = FALSE)
g[g== 0] <- NA
spe <- spe[row.names(g),]
# positive and negative connectivity
r_pos <- c()
r_neg <- c()
spe_i <- c()

for (i in names(g)) {
  co <- na.omit(g[[i]])
  r_pos <- c(r_pos, mean(co[co>0]))
  r_neg <- c(r_neg, mean(co[co<0]))
  spe_i <- c(spe_i, i)
}
r <- data.frame(spe_i, r_pos, r_neg)

r <- na.omit(r)
#The positive and negative cohesion
C_pos <- c()
C_neg <- c()
sample <- c()

for (j in names(spe)) {
  C_pos <- c(C_pos, sum(spe[[j]]*r$r_pos))
  C_neg <- c(C_neg, sum(spe[[j]]*r$r_neg))
  sample <- c(sample, j)
}
C <- data.frame(sample, C_pos, C_neg)
C

#write
write.table(r, 'summer_connectedness.txt', sep = '\t', row.names = FALSE, quote = FALSE)
write.table(C, 'summer_cohesion.txt', sep = '\t', row.names = FALSE, quote = FALSE)
