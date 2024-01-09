
library(Hmisc)


genus<-read.csv("winter.csv",row.names = 1)
genus_t<-t(genus)
rel <- genus_t/rowSums(genus_t)
genus <- t(rel)

genus <- genus[which(rowSums(genus) >= 0.005), ]   
genus <- as.data.frame(genus)
genus1 <- genus
genus1[genus1>0] <- 1
genus <- genus[which(rowSums(genus1) >= 5), ]   




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




library(igraph)


g <- graph.adjacency(z, weighted = TRUE, mode = 'undirected')
g


g <- simplify(g)


g <- delete.vertices(g, names(degree(g)[degree(g) == 0]))



E(g)$correlation <- E(g)$weight
E(g)$weight <- abs(E(g)$weight)




edge <- data.frame(as_edgelist(g))   

edge_list <- data.frame(
  source = edge[[1]],
  target = edge[[2]],
  weight = E(g)$weight,
  correlation = E(g)$correlation
)
head(edge_list)




df1 <- genus
df2<-edge_list[,-3]




result_df <- data.frame(Sample1 = numeric(), Sample2 = numeric(), Positive_Count = numeric(),Negative_Count = numeric())

for (i in 1:length(df1)) {
  for (j in 1:length(df1)) {
    
    zero_rows1 <- rownames(df1[df1[[i]] == 0,])
    zero_rows2 <- rownames(df1[df1[[j]] == 0,])
 
    edge <- df2[!(df2$source %in% zero_rows1) & !(df2$target %in% zero_rows2), ]
    

    positive_count <- sum(edge$correlation > 0)
    negative_count <- sum(edge$correlation < 0)
 
    result_df <- rbind(result_df, data.frame(Sample1 = colnames(df1[i]),Sample2 = colnames(df1[j]), Positive_Count = positive_count, Negative_Count = negative_count))
  }
}

print(result_df)

write.csv(result_df,file = "winter_paired_cor.csv")

