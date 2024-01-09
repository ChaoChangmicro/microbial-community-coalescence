# loan model

library(Hmisc)
library(minpack.lm)
library(stats4)

#- run 8 times
sp<-read.csv("summer_biofilm.csv",head=T,stringsAsFactors=F,row.names=1)
sp<-read.csv("summer_sediment.csv",head=T,stringsAsFactors=F,row.names=1)
sp<-read.csv("summer_soil.csv",head=T,stringsAsFactors=F,row.names=1)
sp<-read.csv("summer_water.csv",head=T,stringsAsFactors=F,row.names=1)


sp<-read.csv("winter_biofilm.csv",head=T,stringsAsFactors=F,row.names=1)
sp<-read.csv("winter_sediment.csv",head=T,stringsAsFactors=F,row.names=1)
sp<-read.csv("winter_soil.csv",head=T,stringsAsFactors=F,row.names=1)
sp<-read.csv("winter_water.csv",head=T,stringsAsFactors=F,row.names=1)
#-


sp = as.data.frame(t(sp))
N <- mean(apply(sp, 1, sum))
p.m<- apply(sp, 2, mean)
p.m<- p.m[p.m != 0]
p <- p.m/N
sp.bi <- 1*(sp>0)
freq<- apply(sp.bi, 2, mean)
freq<- freq[freq != 0]
C <- merge(p, freq, by=0)
C <- C[order(C[,2]),]
C <- as.data.frame(C)
C.0 <- C[!(apply(C, 1, function(y) any(y == 0))),]
p <- C.0[,2]
freq<- C.0[,3]
names(p) <- C.0[,1]
names(freq) <- C.0[,1]
d = 1/N
m.fit<- nlsLM(freq ~ pbeta(d, N*m*p, N*m*(1 -p), lower.tail=FALSE),start=list(m=0.1))
m.ci <- confint(m.fit, 'm', level=0.95)
freq.pred<- pbeta(d, N*coef(m.fit)*p, N*coef(m.fit)*(1 -p), lower.tail=FALSE)
Rsqr<- 1 - (sum((freq - freq.pred)^2))/(sum((freq - mean(freq))^2))


Rsqr
Nm=round(coef(m.fit)*N)
Nm
