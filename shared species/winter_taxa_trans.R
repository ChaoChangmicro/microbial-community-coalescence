

library(tidyverse)
library(ggsci)
library(ggplot2)
library(scales)
library(EcolUtils)


tax = read.csv(file = "tax_rarefied.csv", header=T,sep=",")
otu.all <- read.csv(file = "winter.csv", sep=',',header=T,row.names = 1)
colSums(otu.all)

g = read.csv(file = "winter_group.csv",sep=",",header=T)

# bio.s
metadata <- read.csv(file = "winter_biotos.csv", header=T, sep = ",", row.names = 1)
otus <- read.csv(file = "winter_biofilm_soil.csv", header=T, sep = ",", row.names = 1)
otus <- otus[which(rowSums(otus)>0),]
# bio.se
metadata <- read.csv(file = "winter_biotosed.csv", header=T, sep = ",", row.names = 1)
otus <- read.csv(file = "winter_biofilm_sediment.csv", header=T, sep = ",", row.names = 1)

# bio.w
metadata <- read.csv(file = "winter_biotow.csv", header=T, sep = ",", row.names = 1)
otus <- read.csv(file = "winter_biofilm_water.csv", header=T, sep = ",", row.names = 1)

# se.w
metadata <- read.csv(file = "winter_sedtow.csv", header=T, sep = ",", row.names = 1)
otus <- read.csv(file = "winter_sediment_water.csv", header=T, sep = ",", row.names = 1)

# se.s
metadata <- read.csv(file = "winter_sedtos.csv", header=T, sep = ",", row.names = 1)
otus <- read.csv(file = "winter_sediment_soil.csv", header=T, sep = ",", row.names = 1)

# s.w
metadata <- read.csv(file = "winter_stow.csv", header=T, sep = ",", row.names = 1)
otus <- read.csv(file = "winter_soil_water.csv", header=T, sep = ",", row.names = 1)


if(TRUE){
  otus = otus[rowSums(otus)>0,]
  a = intersect(colnames(otus),rownames(metadata))
  
  otus2 = otus[,a]
  g = metadata[a,]
  
  
  otus3 = c()
  for (k in 1:nrow(otus2)){ 
    g1 = otus2[k,][g$SourceSink=="Source"];n1 = length(g1)
    g2 = otus2[k,][g$SourceSink=="Sink"];n2 = length(g2)
    if( (sum(g1>0) >= n1/2 ) & (sum(g2>0) >= n2/2 ) ){
      otus3 = rbind(otus3, otus2[k,])
    }
  }
  
  res = c()
  for(i in 1:nrow(otus3)){  
    aa = data.frame(t(otus3[i,]) )
    colnames(aa) = "d"
    aa$g = g$SourceSink
    ra = wilcox.test(aa[aa$g=="Source",1],aa[aa$g=="Sink",1])
    
    r = c(ra$p.value,ra$estimate)
    res = rbind(res,r)
  }
  
  res = as.data.frame(res)
  colnames(res) = "p"
  res$otu = rownames(otus3)
  res = na.omit(res)
  
  aa = match(res$otu,tax[,1])
  tax2 = tax[aa,]
  res2 = cbind(res,tax2[,2:8])
  table(res2$D)
  
  res3 = res2[res2$p > 0.05,];
  r = as.data.frame(table(res3$D))
  per = nrow(res3)/nrow(res2)
  
  # niche width
  sour = otus3[,match(rownames(g[g$SourceSink=="Source",]),colnames(otus3))]
  sink = otus3[,match(rownames(g[g$SourceSink=="Sink",]),colnames(otus3))]
  
  sour.niche <- spec.gen(t(sour), niche.width.method = 'levins', perm.method = 'quasiswap', n = 99, probs = c(0.025, 0.975))
  sink.niche <- spec.gen(t(sink), niche.width.method = 'levins', perm.method = 'quasiswap', n = 99, probs = c(0.025, 0.975))
  nich = data.frame(source = sour.niche$observed,sink = sink.niche$observed)
}

#- run the above code 10 times and get each result
bio.s = res3;per.bio.s = per;niche.bio.s = nich
bio.se = res3;per.bio.se = per;niche.bio.se = nich
bio.w = res3;per.bio.w = per;niche.bio.w = nich

se.w = res3;per.se.w = per;niche.se.w = nich
se.s = res3;per.se.s = per;niche.se.s = nich

s.w = res3;per.s.w = per;niche.s.w = nich

#-

bio.s$g = "bio.s"
bio.se$g =  "bio.se"
bio.w$g =  "bio.w"
se.w$g =  "se.w"
se.s$g =  "se.s"
s.w$g =  "s.w"


all = rbind(select(bio.s,c(p,otu,Phylum,g)),
            select(bio.se,c(p,otu,Phylum,g)),
            select(bio.w,c(p,otu,Phylum,g)),
            select(se.w,c(p,otu,Phylum,g)),
            select(se.s,c(p,otu,Phylum,g)),
            select(s.w,c(p,otu,Phylum,g)))

lab = c("Biofilm.Soil","Biofilm.Sediment","Biofilm.Water",
        "Sediment.Water","Sediment.Soil","Soil.Water")
all$D = ifelse(all$Phylum == "","Unidentified",all$Phylum)


#custom_colors <- c("#fb9a99", "#fdbf6f", "#cab2d6", "#ff7f00", "#6a3d9a", "#b15928", "#a6cee3", "#b2df8a",   "#1f78b4", "#33a02c", "#e31a1c",   "#ffff99", "#cccccc", "#8dd3c7", "#bebada", "#fb8072", "#80b1d3", "#fdb462", "#d9d9d9")
#custom_colors <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#b15928", "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f", "#cab2d6", "#ffff99", "#cccccc", "#8dd3c7", "#bebada", "#fb8072", "#80b1d3", "#fdb462", "#d9d9d9")
#value=pal_npg("nrc", alpha = 1)(11)
custom_colors <- c("#B09C85FF","#7E6148FF","#DC0000FF","#91D1C2FF","#8491B4FF","#F39B7FFF","#3C5488FF","#00A087FF","#4DBBD5FF","#E64B35FF" ,"#33a02c")
#custom_colors <- c("#2c7bb6", "#abd9e9", "#a65628","#31a354", "#74c476", "#e41a1c", "#377eb8", "#4daf4a", "#ffff33","#b2df8a", "#984ea3", "#ff7f00", "#377eb8",  "#f781bf", "#999999", "#66c2a5", "#fc8d62", "#8da0cb", "#a6d854", "#ffd92f")
p1 = ggplot(all, aes(g,fill=Phylum)) +
  geom_bar(stat="count",width=.5) +
  guides(fill=guide_legend(reverse=F)) +
  labs(x = NULL,y = "Taxa numbers")+
  scale_fill_manual(values = custom_colors )+
  scale_color_manual( values =custom_colors )+
  scale_x_discrete(labels  = lab)+  
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 15),
        axis.text = element_text(size = 30),axis.title = element_text(size = 35),
        legend.text = element_text(size = 30),legend.title = element_text(size = 35),
        legend.position="right",axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(axis.ticks = element_line(size = 0.4))+  
  theme(axis.ticks.length = unit(0.6,"lines"))+  
  theme(panel.grid=element_blank())+
  theme(legend.position="none")
p1
write.csv(all,file = "winter_taxa_trans.csv")
ggsave("winter_taxa_trans.pdf",plot = p1,width = 400,height = 200,units = "mm")



p2 = ggplot(all, aes(g,fill=Phylum)) +
  geom_bar(position = "fill",width=.5) +
  guides(fill=guide_legend(reverse=F)) +
  scale_y_continuous(labels = percent)+  
  labs(x = NULL,y = "Taxa percentage(%)")+
  scale_fill_manual(values = custom_colors)+
  scale_color_manual(values = custom_colors)+
  scale_x_discrete(labels  = lab)+  
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 15),
        axis.text = element_text(size = 30),axis.title = element_text(size = 35),
        legend.text = element_text(size = 30),legend.title = element_text(size = 35),
        legend.position="right",axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(axis.ticks = element_line(size = 0.4))+  
  theme(axis.ticks.length = unit(0.6,"lines"))+  
  theme(panel.grid=element_blank())+
  theme(legend.position="none")
  
p2
ggsave("winter_relative_trans.pdf",plot = p2,width = 400,height = 200,units = "mm")

##
g = read.csv(file = "winter_group.csv",sep=",",header=T)

# bio.s
N1 = "Biofilm";N2 = "Soil";N3 = "bio.s"
a = g[g$habitat==N1|g$habitat==N2,] 
m.otu = otu.all[,intersect(a$sample,colnames(otu.all))] 
m = intersect(rownames(m.otu),all[all$g == N3,]$otu) 
m.otu2 =  m.otu[m,]
m.all = all[all$g == N3,] 
u = unique(m.all$Phylum)

m.r = c()
for(i in 1:length(u)){ 
  ot = m.all[m.all$Phylum == u[i],]$otu
  m.otu.per = m.otu2[intersect(ot,rownames(m.otu2)),]
  m.r.per = colSums(m.otu.per)
  m.r = rbind(m.r,m.r.per)
}
m.r  = as.data.frame(m.r)
m.r$Phylum = u
m.r.t = m.r %>% pivot_longer(cols = -Phylum)
m.g = g[match(unique(m.r.t$name),g$sample),] 


bio.s.r = cbind(m.r.t,g[match(m.r.t$name,g$sample),])
bio.s.r$g = paste(N1,N2,sep=".")
bio.s.r$g2 = ifelse(bio.s.r$habitat==N1,1,2) 


# bio.se
N1 = "Biofilm";N2 = "Sediment";N3 = "bio.se"
a = g[g$habitat==N1|g$habitat==N2,] 
m.otu = otu.all[,intersect(a$sample,colnames(otu.all))] 
m = intersect(rownames(m.otu),all[all$g == N3,]$otu) 
m.otu2 =  m.otu[m,]
m.all = all[all$g == N3,] 
u = unique(m.all$Phylum)

m.r = c()
for(i in 1:length(u)){ 
  ot = m.all[m.all$Phylum == u[i],]$otu
  m.otu.per = m.otu2[intersect(ot,rownames(m.otu2)),]
  m.r.per = colSums(m.otu.per)
  m.r = rbind(m.r,m.r.per)
}
m.r  = as.data.frame(m.r)
m.r$Phylum = u
m.r.t = m.r %>% pivot_longer(cols = -Phylum)
m.g = g[match(unique(m.r.t$name),g$sample),] 


bio.se.r = cbind(m.r.t,g[match(m.r.t$name,g$sample),])
bio.se.r$g = paste(N1,N2,sep=".")
bio.se.r$g2 = ifelse(bio.se.r$habitat==N1,1,2) 

# bio.w
N1 = "Biofilm";N2 = "Water";N3 = "bio.w"
a = g[g$habitat==N1|g$habitat==N2,] 
m.otu = otu.all[,intersect(a$sample,colnames(otu.all))] 
m = intersect(rownames(m.otu),all[all$g == N3,]$otu) 
m.otu2 =  m.otu[m,]
m.all = all[all$g == N3,] 
u = unique(m.all$Phylum)

m.r = c()
for(i in 1:length(u)){ 
  ot = m.all[m.all$Phylum == u[i],]$otu
  m.otu.per = m.otu2[intersect(ot,rownames(m.otu2)),]
  m.r.per = colSums(m.otu.per)
  m.r = rbind(m.r,m.r.per)
}
m.r  = as.data.frame(m.r)
m.r$Phylum = u
m.r.t = m.r %>% pivot_longer(cols = -Phylum)
m.g = g[match(unique(m.r.t$name),g$sample),] 


bio.w.r = cbind(m.r.t,g[match(m.r.t$name,g$sample),])
bio.w.r$g = paste(N1,N2,sep=".")
bio.w.r$g2 = ifelse(bio.w.r$habitat==N1,1,2) 


# se.w
N1 = "Sediment";N2 = "Water";N3 = "se.w"
a = g[g$habitat==N1|g$habitat==N2,] 
m.otu = otu.all[,intersect(a$sample,colnames(otu.all))] 
m = intersect(rownames(m.otu),all[all$g == N3,]$otu) 
m.otu2 =  m.otu[m,]
m.all = all[all$g == N3,] 
u = unique(m.all$Phylum)

m.r = c()
for(i in 1:length(u)){ 
  ot = m.all[m.all$Phylum == u[i],]$otu
  m.otu.per = m.otu2[intersect(ot,rownames(m.otu2)),]
  m.r.per = colSums(m.otu.per)
  m.r = rbind(m.r,m.r.per)
}
m.r  = as.data.frame(m.r)
m.r$Phylum = u
m.r.t = m.r %>% pivot_longer(cols = -Phylum)
m.g = g[match(unique(m.r.t$name),g$sample),] 


se.w.r = cbind(m.r.t,g[match(m.r.t$name,g$sample),])
se.w.r$g = paste(N1,N2,sep=".")
se.w.r$g2 = ifelse(se.w.r$habitat==N1,1,2) 

# se.s
N1 = "Sediment";N2 = "Soil";N3 = "se.s"
a = g[g$habitat==N1|g$habitat==N2,] 
m.otu = otu.all[,intersect(a$sample,colnames(otu.all))] 
m = intersect(rownames(m.otu),all[all$g == N3,]$otu) 
m.otu2 =  m.otu[m,]
m.all = all[all$g == N3,] 
u = unique(m.all$Phylum)

m.r = c()
for(i in 1:length(u)){ 
  ot = m.all[m.all$Phylum == u[i],]$otu
  m.otu.per = m.otu2[intersect(ot,rownames(m.otu2)),]
  m.r.per = colSums(m.otu.per)
  m.r = rbind(m.r,m.r.per)
}
m.r  = as.data.frame(m.r)
m.r$Phylum = u
m.r.t = m.r %>% pivot_longer(cols = -Phylum)
m.g = g[match(unique(m.r.t$name),g$sample),] 


se.s.r = cbind(m.r.t,g[match(m.r.t$name,g$sample),])
se.s.r$g = paste(N1,N2,sep=".")
se.s.r$g2 = ifelse(se.s.r$habitat==N1,1,2)

# se.w
N1 = "Sediment";N2 = "Water";N3 = "se.w"
a = g[g$habitat==N1|g$habitat==N2,] 
m.otu = otu.all[,intersect(a$sample,colnames(otu.all))] 
m = intersect(rownames(m.otu),all[all$g == N3,]$otu) 
m.otu2 =  m.otu[m,]
m.all = all[all$g == N3,] 
u = unique(m.all$Phylum)

m.r = c()
for(i in 1:length(u)){ 
  ot = m.all[m.all$Phylum == u[i],]$otu
  m.otu.per = m.otu2[intersect(ot,rownames(m.otu2)),]
  m.r.per = colSums(m.otu.per)
  m.r = rbind(m.r,m.r.per)
}
m.r  = as.data.frame(m.r)
m.r$Phylum = u
m.r.t = m.r %>% pivot_longer(cols = -Phylum)
m.g = g[match(unique(m.r.t$name),g$sample),] 


se.w.r = cbind(m.r.t,g[match(m.r.t$name,g$sample),])
se.w.r$g = paste(N1,N2,sep=".")
se.w.r$g2 = ifelse(se.w.r$habitat==N1,1,2)





all.per = rbind(bio.s.r,bio.se.r,bio.w.r,se.w.r,
                se.s.r,se.w.r)


all.per$g = factor(all.per$g,levels = lab)

p3 = ggplot(all.per,aes(x = g2, y = value/1000,fill=Phylum,color=Phylum))+
  geom_bar(stat="identity",position = "stack",width=.75) +
  facet_wrap(.~g,nrow=1)+
  labs(x = "Habitats for bacterial intermigration",y = "Average abundance (× 10^3)")+
  scale_fill_manual(values = custom_colors)+
  scale_color_manual(values =custom_colors)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 15),
        axis.text = element_text(size = 30,color="black"),axis.title = element_text(size = 35),
        legend.text = element_text(size = 20,color="black"),legend.title = element_text(size = 25),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(axis.ticks = element_line(size = 0.4))+  
  theme(axis.ticks.length = unit(0.6,"lines"))+  
  theme(panel.grid=element_blank())+  
  theme(legend.position="NULL") 

p3

ggsave("winter_abundance.pdf",plot = p3,width = 600,height = 400,units = "mm")

library(patchwork)
layout <- "
1111122222
333333333#
"
p = (p1 | p2)/(p3) + 
  theme(legend.position="right")

p = p1 + p2 + p3 + plot_layout(design = layout)
p
getwd()
ggsave("winter_all.pdf",plot = p,width = 600,height = 400,units = "mm")


bio.s$g = "bio.s"
bio.se$g =  "bio.se"
bio.w$g =  "bio.w"
se.w$g =  "se.w"
se.s$g =  "se.s"
s.w$g =  "s.w"
##
niche.bio.s2 = niche.bio.s %>% rename(Biofilm=source,Soil=sink) %>% 
  mutate(h = "bio.s") %>% pivot_longer(col=-h)
niche.bio.se2 = niche.bio.se %>% rename(Biofilm=source,Sediment=sink) %>% 
  mutate(h = "bio.se") %>% pivot_longer(col=-h)
niche.bio.w2 = niche.bio.w %>% rename(Biofilm=source,Water=sink) %>% 
  mutate(h = "bio.w") %>% pivot_longer(col=-h)
niche.se.w2 = niche.se.w %>% rename(Sediment=source,Water=sink) %>% 
  mutate(h = "se.w") %>% pivot_longer(col=-h)
niche.se.s2 = niche.se.s %>% rename(Sediment=source,Soil=sink) %>% 
  mutate(h = "se.s") %>% pivot_longer(col=-h)
niche.s.w2 = niche.s.w %>% rename(Soil=source,Water=sink) %>% 
  mutate(h = "s.w") %>% pivot_longer(col=-h)

niche.bio.s3=niche.bio.s%>% mutate(h = "bio.s") 
niche.bio.se3=niche.bio.se%>% mutate(h = "bio.se") 
niche.bio.w3=niche.bio.w%>% mutate(h = "bio.w") 
niche.se.w3=niche.se.w%>% mutate(h = "se.w") 
niche.se.s3=niche.se.s%>% mutate(h = "se.s") 
niche.s.w3=niche.s.w%>% mutate(h = "s.w") 

all <- rbind(niche.bio.s3,niche.bio.se3,niche.bio.w3,niche.se.w3,niche.se.s3,niche.s.w3)
feast.p = all %>% pivot_longer(cols = -h)
compared = list(c("source","sink"))
feast.p$g = paste(feast.p$h,feast.p$name,sep=".")


p = ggplot(data = feast.p,aes(x=h,y=value,fill=g)) +
  geom_boxplot(alpha=0.75,outlier.size = 1, 
               outlier.color = "grey", 
               outlier.fill = "grey")+ 
  labs(x = "Shared species in habitat pair",y = "Niche breadth")

p

p.values <- sapply(split(feast.p, feast.p$h), function(x){wilcox.test(value~name, x)$p.value})
labels <- symnum(p.values, corr = FALSE, cutpoints = c(0,  .001,.01,.05, 1), symbols = c("***","**","*","n.s."))
y.values <- sapply(split(feast.p, feast.p$h), function(x){max(sapply(split(x, x$name), function(xx){boxplot(x$value, plot=F)$stats[5, ]}))})-0.5
library(ggsignif)
p <- p + geom_signif(y_position = y.values, 
                     xmin = levels(factor(feast.p$h)), 
                     xmax = levels(factor(feast.p$h)), 
                     annotations = labels,tip_length = 0,
                     textsize = 9,color="tomato3")+
  scale_fill_manual(values = c("#339933","#84c184",
                               "#990000","#c16666",
                               "#CC9900","#e0c166",
                               "#663366","#a384a3",
                               "#99CCFF","#c1e0ff",
                               "#B09C85FF","#7E6148FF"
                               ))+
  scale_color_manual(values = c("#339933","#84c184",
                                "#990000","#c16666",
                                "#CC9900","#e0c166",
                                "#663366","#a384a3",
                                "#99CCFF","#c1e0ff",
                                "#B09C85FF","#7E6148FF"))+
  theme_bw()+
  theme(axis.text = element_text(size = 30),axis.title = element_text(size = 35),
        legend.text = element_text(size = 30),legend.title = element_text(size = 35),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  
  theme(axis.ticks = element_line(size = 0.4))+  
  theme(axis.ticks.length = unit(0.6,"lines"))+  
  theme(panel.grid=element_blank())+  
  theme(legend.position = "NULL")
p

ggsave("winter_nich.pdf",plot = p,width = 400,height = 200,units = "mm")




p4 = ggplot()+
  geom_boxplot(data=niche.bio.s2,aes(x=h,y=value,group=name,fill = h))+
  geom_boxplot(data=niche.bio.se2,aes(x=h,y=value,group=name,fill = h))+
  geom_boxplot(data=niche.bio.w2,aes(x=h,y=value,group=name,fill = h))+
  geom_boxplot(data=niche.se.w2,aes(x=h,y=value,group=name,fill = h))+
  geom_boxplot(data=niche.se.s2,aes(x=h,y=value,group=name,fill = h))+
  geom_boxplot(data=niche.s.w2,aes(x=h,y=value,group=name,fill = h))+
  labs(x = "Shared species in habitat pair",y = "Niche breadth")+
  scale_fill_manual(values = custom_colors)+
  scale_color_manual(values = custom_colors)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 15),
        axis.text = element_text(size = 30,color="black"),axis.title = element_text(size = 35),
        legend.text = element_text(size = 20,color="black"),legend.title = element_text(size = 25),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(axis.ticks = element_line(size = 0.4))+  
  theme(axis.ticks.length = unit(0.6,"lines"))+  
  theme(panel.grid=element_blank())+  
  theme(legend.position="NULL") 
p4 



getwd()
ggsave("winter_nich.pdf",plot = p4,width = 400,height = 200,units = "mm")
