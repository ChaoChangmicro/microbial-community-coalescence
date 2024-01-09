##计算微生物丰度间的相关系数
library(Hmisc)

#以属水平丰度为例，“genus_table.txt” 是一个属水平的微生物丰度表
genus<-read.csv("winter.csv",row.names = 1)
genus_t<-t(genus)
rel <- genus_t/rowSums(genus_t)
genus <- t(rel)
#可选事先过滤一些低丰度或低频的类群
genus <- genus[which(rowSums(genus) >= 0.005), ]    #例如只保留相对丰度总和高于 0.005 的属
genus <- as.data.frame(genus)
genus1 <- genus
genus1[genus1>0] <- 1
genus <- genus[which(rowSums(genus1) >= 5), ]    #例如只保留在 5 个及以上样本中出现的属

#计算两属之间是否存在丰度变化的相关性，以 spearman 相关系数为例
genus_corr <- rcorr(t(genus), type = 'spearman')

#阈值筛选
#将 spearman 相关系数低于 0.7 的关系剔除，即 r>=0.7
r <- genus_corr$r
r[abs(r) < 0.7] <- 0

#选取显著性 p 值小于 0.05 的相关系数，即 p<0.05
p <- genus_corr$P
p <- p.adjust(p, method = 'fdr')    #可选 p 值校正，这里使用 BH 法校正 p 值
p[p>=0.001] <- -1
p[p<0.001 & p>=0] <- 1
p[p==-1] <- 0

#根据上述筛选的 r 值和 p 值保留数据
z <- r * p
diag(z) <- 0    #将相关矩阵中对角线中的值（代表了自相关）转为 0
head(z)[1:6,1:6]
p=data.frame(z, check.names = FALSE)



##获得网络
library(igraph)

#将邻接矩阵转化为 igraph 网络的邻接列表
#构建含权的无向网络，权重代表了微生物属间丰度的 spearman 相关系数
g <- graph.adjacency(z, weighted = TRUE, mode = 'undirected')
g

#自相关也可以通过该式去除
g <- simplify(g)

#孤立节点的删除（删除度为 0 的节点）
g <- delete.vertices(g, names(degree(g)[degree(g) == 0]))

#plot(g)
#该模式下，边权重代表了相关系数
#由于权重通常为正值，因此最好取个绝对值，相关系数重新复制一列
E(g)$correlation <- E(g)$weight
E(g)$weight <- abs(E(g)$weight)



#边列表
edge <- data.frame(as_edgelist(g))    #igraph 的邻接列表转为边列表

edge_list <- data.frame(
  source = edge[[1]],
  target = edge[[2]],
  weight = E(g)$weight,
  correlation = E(g)$correlation
)
head(edge_list)




##样本正相关负相关数量计算
#单个计算
df1 <- genus
df2<-edge_list[,-3]



# 创建一个带有指定列名的空dataframe
result_df <- data.frame(Sample1 = numeric(), Sample2 = numeric(), Positive_Count = numeric(),Negative_Count = numeric())

for (i in 1:length(df1)) {
  for (j in 1:length(df1)) {
    
    zero_rows1 <- rownames(df1[df1[[i]] == 0,])
    zero_rows2 <- rownames(df1[df1[[j]] == 0,])
    # 删除前两列包含这些行名的行
    edge <- df2[!(df2$source %in% zero_rows1) & !(df2$target %in% zero_rows2), ]
    
    # 计算相关性中正值和负值的数量
    positive_count <- sum(edge$correlation > 0)
    negative_count <- sum(edge$correlation < 0)
    
    # 将结果添加到结果数据框中
    result_df <- rbind(result_df, data.frame(Sample1 = colnames(df1[i]),Sample2 = colnames(df1[j]), Positive_Count = positive_count, Negative_Count = negative_count))
  }
}

print(result_df)

write.csv(result_df,file = "winter_paired_cor.csv")

