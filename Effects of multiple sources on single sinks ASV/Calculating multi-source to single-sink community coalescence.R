S.biofilm = read.csv(file = "summer_biofilm.csv",header=T,row.names = 1)
S.sediment = read.csv(file = "summer_sediment.csv",header=T,row.names = 1)
S.soil = read.csv(file = "summer_soil.csv",header=T,row.names = 1)
S.water = read.csv(file = "summer_water.csv",header=T,row.names = 1)



W.biofilm = read.csv(file = "winter_biofilm.csv",header=T,row.names = 1)
W.sediment = read.csv(file = "winter_sediment.csv",header=T,row.names = 1)
W.soil = read.csv(file = "winter_soil.csv",header=T,row.names = 1)
W.water = read.csv(file = "winter_water.csv",header=T,row.names = 1)



x = list(S.biofilm = ifelse(rowSums(S.biofilm)>0,rownames(S.biofilm),NULL),
              S.sediment = ifelse(rowSums(S.sediment)>0,rownames(S.sediment),NULL),
              S.soil= ifelse(rowSums(S.soil)>0,rownames(S.soil),NULL),
              S.water= ifelse(rowSums(S.water)>0,rownames(S.water),NULL))
  A <- x[[1]]
  B <- x[[2]]
  C <- x[[3]]
  D <- x[[4]]
  #list.names <- category.names
  n12 <- intersect(A, B)
  n13 <- intersect(A, C)
  n14 <- intersect(A, D)
  n23 <- intersect(B, C)
  n24 <- intersect(B, D)
  n34 <- intersect(C, D)
  n123 <- intersect(n12, C)
  n124 <- intersect(n12, D)
  n134 <- intersect(n13, D)
  n234 <- intersect(n23, D)
  n1234 <- intersect(n123, D)
area1 = length(A); area2 = length(B); area3 = length(C); area4 = length(D); 
n12 = length(n12); n13 = length(n13); n14 = length(n14); 
n23 = length(n23); n24 = length(n24); n34 = length(n34); 
n123 = length(n123); n124 = length(n124); n134 = length(n134); 
n234 = length(n234); n1234 = length(n1234)
a6 <- n1234
a12 <- n123 - a6
a11 <- n124 - a6
a5 <- n134 - a6
a7 <- n234 - a6
a15 <- n12 - a6 - a11 - a12
a4 <- n13 - a6 - a5 - a12
a10 <- n14 - a6 - a5 - a11
a13 <- n23 - a6 - a7 - a12
a8 <- n24 - a6 - a7 - a11
a2 <- n34 - a6 - a5 - a7
a9 <- area1 - a4 - a5 - a6 - a10 - a11 - a12 - a15
a14 <- area2 - a6 - a7 - a8 - a11 - a12 - a13 - a15
a1 <- area3 - a2 - a4 - a5 - a6 - a7 - a12 - a13
a3 <- area4 - a2 - a5 - a6 - a7 - a8 - a10 - a11
areas <- c(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, 
           a11, a12, a13, a14, a15)

water=(a10+a11+a5+a6+a2+a7+a8)/(a1+a2+a3+a4+a5+a6+a7+a8+a9+a10+a11+a12+a13+a14+a15)
biofilm=(a10+a11+a15+a4+a5+a6+a12)/(a1+a2+a3+a4+a5+a6+a7+a8+a9+a10+a11+a12+a13+a14+a15)
soil=(a4+a5+a6+a12+a2+a7+a13)/(a1+a2+a3+a4+a5+a6+a7+a8+a9+a10+a11+a12+a13+a14+a15)
sediment=(a11+a15+a6+a12+a7+a13+a8)/(a1+a2+a3+a4+a5+a6+a7+a8+a9+a10+a11+a12+a13+a14+a15)


total<- length(Reduce(union, x))
A <- x[[1]]
B <- x[[2]]
C <- x[[3]]
D <- x[[4]]
AD=intersect(A, D)
BD=intersect(B, D)
CD=intersect(C, D)
length(Reduce(union, list(AD,BD,CD)))

