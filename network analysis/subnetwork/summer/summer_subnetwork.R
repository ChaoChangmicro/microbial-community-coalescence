
library(igraph)
source("info.centrality.R")

adjacency_unweight <- read.csv('summer_network.csv', row.names = 1, check.names = FALSE)
adjacency_unweight[adjacency_unweight != 0] <- 1
head(adjacency_unweight)[1:6]    


g <- graph_from_adjacency_matrix(as.matrix(adjacency_unweight), mode = 'undirected', weighted = NULL, diag = FALSE)
g    




otu <- read.csv('summer.csv', row.names = 1, check.names = FALSE)
otu =otu[row.names(adjacency_unweight),]

sub_graph <- list()
for (i in names(otu)) {
  sample_i <- otu[i]
  select_node <- rownames(sample_i)[which(sample_i != 0)]
  sub_graph[[i]] <- subgraph(g, select_node)
}



sample_name <- c()
nodes_num <- c()
edges_num<- c()
degree <- c()
average_path_length <- c()
betweenness_centralization <- c()
modularity <- c()
vlnerability <- c()
for(i in 1:length(sub_graph)){
  sample_name <- c(sample_name, names(sub_graph[i]))
  nodes_num <- c(nodes_num, length(V(sub_graph[[i]])))  
  edges_num<- c(edges_num, length(E(sub_graph[[i]])))
  degree <- c(degree, mean(degree(sub_graph[[i]])))  
  average_path_length <- c(average_path_length, average.path.length(sub_graph[[i]], directed = FALSE))  #平均路径长度
  betweenness_centralization <- c(betweenness_centralization, centralization.betweenness(sub_graph[[i]])$centralization)  #介数中心性
  
  greedy <- cluster_fast_greedy(sub_graph[[i]])
  greedy2 <- modularity(sub_graph[[i]], membership( greedy))
  modularity <-c(modularity,greedy2) 
  
  
  node.vul<-info.centrality.vertex(sub_graph[[i]])
  vlnerability <- c(vlnerability,max(node.vul))
  
  
  }

sub_graph_stat <- data.frame(nodes_num, edges_num,degree, average_path_length, betweenness_centralization,modularity,vlnerability)
rownames(sub_graph_stat) <- sample_name
head(sub_graph_stat)  
mean(node.vul)
write.csv(sub_graph_stat, 'summer_subnet_top_new1.csv', quote = FALSE)



