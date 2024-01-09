S.biofilm = read.csv(file = "summer_biofilm.csv",header=T,row.names = 1)
S.sediment = read.csv(file = "summer_sediment.csv",header=T,row.names = 1)
S.soil = read.csv(file = "summer_soil.csv",header=T,row.names = 1)
S.water = read.csv(file = "summer_water.csv",header=T,row.names = 1)

library(dplyr)

##1 summer_C_M2


sediment=S.sediment%>%select("summer_S_M1") 
biofilm=S.biofilm%>%select("summer_B_M1") 
water=S.water%>%select("summer_W_M1") 
soil.source=S.soil %>%select("summer_C_M1") 
soil.sink=S.soil %>%select("summer_C_M2") 

x = list(sediment = rownames(sediment)[sediment > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         soil.source= rownames(soil.source)[soil.source> 0],
         soil.sink=  rownames(soil.sink)[soil.sink > 0])

A <- x[[1]]
B <- x[[2]]
C <- x[[3]]
D <- x[[4]]
E <- x[[5]]
total<- length(Reduce(union, x))
AE=intersect(A, E)
BE=intersect(B, E)
CE=intersect(C, E)
DE=intersect(D, E)
length(Reduce(union, list(AE,BE,CE,DE)))
soil_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
soil_sink

##2 summer_C_M3


sediment=S.sediment%>%select("summer_S_M2") 
biofilm=S.biofilm%>%select("summer_B_M2") 
water=S.water%>%select("summer_W_M2") 
soil.source=S.soil %>%select("summer_C_M2") 
soil.sink=S.soil %>%select("summer_C_M3") 

x = list(sediment = rownames(sediment)[sediment > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         soil.source= rownames(soil.source)[soil.source> 0],
         soil.sink=  rownames(soil.sink)[soil.sink > 0])

A <- x[[1]]
B <- x[[2]]
C <- x[[3]]
D <- x[[4]]
E <- x[[5]]
total<- length(Reduce(union, x))
AE=intersect(A, E)
BE=intersect(B, E)
CE=intersect(C, E)
DE=intersect(D, E)
length(Reduce(union, list(AE,BE,CE,DE)))
soil_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
soil_sink

##3 summer_C_M4


sediment=S.sediment%>%select("summer_S_M3") 
biofilm=S.biofilm%>%select("summer_B_M3") 
water=S.water%>%select("summer_W_M3") 
soil.source=S.soil %>%select("summer_C_M3") 
soil.sink=S.soil %>%select("summer_C_M4") 

x = list(sediment = rownames(sediment)[sediment > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         soil.source= rownames(soil.source)[soil.source> 0],
         soil.sink=  rownames(soil.sink)[soil.sink > 0])

A <- x[[1]]
B <- x[[2]]
C <- x[[3]]
D <- x[[4]]
E <- x[[5]]
total<- length(Reduce(union, x))
AE=intersect(A, E)
BE=intersect(B, E)
CE=intersect(C, E)
DE=intersect(D, E)
length(Reduce(union, list(AE,BE,CE,DE)))
soil_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
soil_sink


##4 summer_C_M5


sediment=S.sediment%>%select("summer_S_M4") 
biofilm=S.biofilm%>%select("summer_B_M4") 
water=S.water%>%select("summer_W_M4") 
soil.source=S.soil %>%select("summer_C_M4") 
soil.sink=S.soil %>%select("summer_C_M5") 

x = list(sediment = rownames(sediment)[sediment > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         soil.source= rownames(soil.source)[soil.source> 0],
         soil.sink=  rownames(soil.sink)[soil.sink > 0])

A <- x[[1]]
B <- x[[2]]
C <- x[[3]]
D <- x[[4]]
E <- x[[5]]
total<- length(Reduce(union, x))
AE=intersect(A, E)
BE=intersect(B, E)
CE=intersect(C, E)
DE=intersect(D, E)
length(Reduce(union, list(AE,BE,CE,DE)))
soil_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
soil_sink


##5 summer_C_M6


sediment=S.sediment%>%select("summer_S_M5") 
biofilm=S.biofilm%>%select("summer_B_M5") 
water=S.water%>%select("summer_W_M5") 
soil.source=S.soil %>%select("summer_C_M5") 
soil.sink=S.soil %>%select("summer_C_M6") 

x = list(sediment = rownames(sediment)[sediment > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         soil.source= rownames(soil.source)[soil.source> 0],
         soil.sink=  rownames(soil.sink)[soil.sink > 0])

A <- x[[1]]
B <- x[[2]]
C <- x[[3]]
D <- x[[4]]
E <- x[[5]]
total<- length(Reduce(union, x))
AE=intersect(A, E)
BE=intersect(B, E)
CE=intersect(C, E)
DE=intersect(D, E)
length(Reduce(union, list(AE,BE,CE,DE)))
soil_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
soil_sink



##6 summer_C_M7


sediment=S.sediment%>%select("summer_S_M6") 
biofilm=S.biofilm%>%select("summer_B_M6") 
water=S.water%>%select("summer_W_M6") 
soil.source=S.soil %>%select("summer_C_M6") 
soil.sink=S.soil %>%select("summer_C_M7") 

x = list(sediment = rownames(sediment)[sediment > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         soil.source= rownames(soil.source)[soil.source> 0],
         soil.sink=  rownames(soil.sink)[soil.sink > 0])

A <- x[[1]]
B <- x[[2]]
C <- x[[3]]
D <- x[[4]]
E <- x[[5]]
total<- length(Reduce(union, x))
AE=intersect(A, E)
BE=intersect(B, E)
CE=intersect(C, E)
DE=intersect(D, E)
length(Reduce(union, list(AE,BE,CE,DE)))
soil_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
soil_sink


##7 summer_C_M8


sediment=S.sediment%>%select("summer_S_M7") 
biofilm=S.biofilm%>%select("summer_B_M7") 
water=S.water%>%select("summer_W_M7") 
soil.source=S.soil %>%select("summer_C_M7") 
soil.sink=S.soil %>%select("summer_C_M8") 

x = list(sediment = rownames(sediment)[sediment > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         soil.source= rownames(soil.source)[soil.source> 0],
         soil.sink=  rownames(soil.sink)[soil.sink > 0])

A <- x[[1]]
B <- x[[2]]
C <- x[[3]]
D <- x[[4]]
E <- x[[5]]
total<- length(Reduce(union, x))
AE=intersect(A, E)
BE=intersect(B, E)
CE=intersect(C, E)
DE=intersect(D, E)
length(Reduce(union, list(AE,BE,CE,DE)))
soil_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
soil_sink





sediment=S.sediment%>%select("summer_S_JH4") 
biofilm=S.biofilm%>%select("summer_B_JH4") 
water=S.water%>%select("summer_W_JH4") 
soil.source=S.soil %>%select("summer_C_JH4") 
soil.sink=S.soil %>%select("summer_C_M8") 

x = list(sediment = rownames(sediment)[sediment > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         soil.source= rownames(soil.source)[soil.source> 0],
         soil.sink=  rownames(soil.sink)[soil.sink > 0])

A <- x[[1]]
B <- x[[2]]
C <- x[[3]]
D <- x[[4]]
E <- x[[5]]
total<- length(Reduce(union, x))
AE=intersect(A, E)
BE=intersect(B, E)
CE=intersect(C, E)
DE=intersect(D, E)
length(Reduce(union, list(AE,BE,CE,DE)))
soil_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
soil_sink

## summer_C_M9


sediment=S.sediment%>%select("summer_S_M8") 
biofilm=S.biofilm%>%select("summer_B_M8") 
water=S.water%>%select("summer_W_M8") 
soil.source=S.soil %>%select("summer_C_M8") 
soil.sink=S.soil %>%select("summer_C_M9") 

x = list(sediment = rownames(sediment)[sediment > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         soil.source= rownames(soil.source)[soil.source> 0],
         soil.sink=  rownames(soil.sink)[soil.sink > 0])

A <- x[[1]]
B <- x[[2]]
C <- x[[3]]
D <- x[[4]]
E <- x[[5]]
total<- length(Reduce(union, x))
AE=intersect(A, E)
BE=intersect(B, E)
CE=intersect(C, E)
DE=intersect(D, E)
length(Reduce(union, list(AE,BE,CE,DE)))
soil_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
soil_sink


## summer_C_M9


sediment=S.sediment%>%select("summer_S_ZSH") 
biofilm=S.biofilm%>%select("summer_B_ZSH") 
water=S.water%>%select("summer_W_ZSH") 
soil.source=S.soil %>%select("summer_C_ZSH") 
soil.sink=S.soil %>%select("summer_C_M9") 

x = list(sediment = rownames(sediment)[sediment > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         soil.source= rownames(soil.source)[soil.source> 0],
         soil.sink=  rownames(soil.sink)[soil.sink > 0])

A <- x[[1]]
B <- x[[2]]
C <- x[[3]]
D <- x[[4]]
E <- x[[5]]
total<- length(Reduce(union, x))
AE=intersect(A, E)
BE=intersect(B, E)
CE=intersect(C, E)
DE=intersect(D, E)
length(Reduce(union, list(AE,BE,CE,DE)))
soil_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
soil_sink


## summer_C_M10


sediment=S.sediment%>%select("summer_S_M9") 
biofilm=S.biofilm%>%select("summer_B_M9") 
water=S.water%>%select("summer_W_M9") 
soil.source=S.soil %>%select("summer_C_M9") 
soil.sink=S.soil %>%select("summer_C_M10") 

x = list(sediment = rownames(sediment)[sediment > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         soil.source= rownames(soil.source)[soil.source> 0],
         soil.sink=  rownames(soil.sink)[soil.sink > 0])

A <- x[[1]]
B <- x[[2]]
C <- x[[3]]
D <- x[[4]]
E <- x[[5]]
total<- length(Reduce(union, x))
AE=intersect(A, E)
BE=intersect(B, E)
CE=intersect(C, E)
DE=intersect(D, E)
length(Reduce(union, list(AE,BE,CE,DE)))
soil_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
soil_sink

## summer_C_M11


sediment=S.sediment%>%select("summer_S_M10") 
biofilm=S.biofilm%>%select("summer_B_M10") 
water=S.water%>%select("summer_W_M10") 
soil.source=S.soil %>%select("summer_C_M10") 
soil.sink=S.soil %>%select("summer_C_M11") 

x = list(sediment = rownames(sediment)[sediment > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         soil.source= rownames(soil.source)[soil.source> 0],
         soil.sink=  rownames(soil.sink)[soil.sink > 0])

A <- x[[1]]
B <- x[[2]]
C <- x[[3]]
D <- x[[4]]
E <- x[[5]]
total<- length(Reduce(union, x))
AE=intersect(A, E)
BE=intersect(B, E)
CE=intersect(C, E)
DE=intersect(D, E)
length(Reduce(union, list(AE,BE,CE,DE)))
soil_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
soil_sink

## summer_C_M12


sediment=S.sediment%>%select("summer_S_M11") 
biofilm=S.biofilm%>%select("summer_B_M11") 
water=S.water%>%select("summer_W_M11") 
soil.source=S.soil %>%select("summer_C_M11") 
soil.sink=S.soil %>%select("summer_C_M12") 

x = list(sediment = rownames(sediment)[sediment > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         soil.source= rownames(soil.source)[soil.source> 0],
         soil.sink=  rownames(soil.sink)[soil.sink > 0])

A <- x[[1]]
B <- x[[2]]
C <- x[[3]]
D <- x[[4]]
E <- x[[5]]
total<- length(Reduce(union, x))
AE=intersect(A, E)
BE=intersect(B, E)
CE=intersect(C, E)
DE=intersect(D, E)
length(Reduce(union, list(AE,BE,CE,DE)))
soil_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
soil_sink


## summer_C_M13


sediment=S.sediment%>%select("summer_S_M12") 
biofilm=S.biofilm%>%select("summer_B_M12") 
water=S.water%>%select("summer_W_M12") 
soil.source=S.soil %>%select("summer_C_M12") 
soil.sink=S.soil %>%select("summer_C_M13") 

x = list(sediment = rownames(sediment)[sediment > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         soil.source= rownames(soil.source)[soil.source> 0],
         soil.sink=  rownames(soil.sink)[soil.sink > 0])

A <- x[[1]]
B <- x[[2]]
C <- x[[3]]
D <- x[[4]]
E <- x[[5]]
total<- length(Reduce(union, x))
AE=intersect(A, E)
BE=intersect(B, E)
CE=intersect(C, E)
DE=intersect(D, E)
length(Reduce(union, list(AE,BE,CE,DE)))
soil_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
soil_sink



sediment=S.sediment%>%select("summer_S_QYH4") 
biofilm=S.biofilm%>%select("summer_B_QYH4") 
water=S.water%>%select("summer_W_QYH4") 
soil.source=S.soil %>%select("summer_C_QYH4") 
soil.sink=S.soil %>%select("summer_C_M13") 

x = list(sediment = rownames(sediment)[sediment > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         soil.source= rownames(soil.source)[soil.source> 0],
         soil.sink=  rownames(soil.sink)[soil.sink > 0])

A <- x[[1]]
B <- x[[2]]
C <- x[[3]]
D <- x[[4]]
E <- x[[5]]
total<- length(Reduce(union, x))
AE=intersect(A, E)
BE=intersect(B, E)
CE=intersect(C, E)
DE=intersect(D, E)
length(Reduce(union, list(AE,BE,CE,DE)))
soil_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
soil_sink


## summer_C_JH2


sediment=S.sediment%>%select("summer_S_JH1") 
biofilm=S.biofilm%>%select("summer_B_JH1") 
water=S.water%>%select("summer_W_JH1") 
soil.source=S.soil %>%select("summer_C_JH1") 
soil.sink=S.soil %>%select("summer_C_JH2") 

x = list(sediment = rownames(sediment)[sediment > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         soil.source= rownames(soil.source)[soil.source> 0],
         soil.sink=  rownames(soil.sink)[soil.sink > 0])

A <- x[[1]]
B <- x[[2]]
C <- x[[3]]
D <- x[[4]]
E <- x[[5]]
total<- length(Reduce(union, x))
AE=intersect(A, E)
BE=intersect(B, E)
CE=intersect(C, E)
DE=intersect(D, E)
length(Reduce(union, list(AE,BE,CE,DE)))
soil_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
soil_sink

## summer_C_JH3


sediment=S.sediment%>%select("summer_S_JH2") 
biofilm=S.biofilm%>%select("summer_B_JH2") 
water=S.water%>%select("summer_W_JH2") 
soil.source=S.soil %>%select("summer_C_JH2") 
soil.sink=S.soil %>%select("summer_C_JH3") 

x = list(sediment = rownames(sediment)[sediment > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         soil.source= rownames(soil.source)[soil.source> 0],
         soil.sink=  rownames(soil.sink)[soil.sink > 0])

A <- x[[1]]
B <- x[[2]]
C <- x[[3]]
D <- x[[4]]
E <- x[[5]]
total<- length(Reduce(union, x))
AE=intersect(A, E)
BE=intersect(B, E)
CE=intersect(C, E)
DE=intersect(D, E)
length(Reduce(union, list(AE,BE,CE,DE)))
soil_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
soil_sink

## summer_C_JH4


sediment=S.sediment%>%select("summer_S_JH3") 
biofilm=S.biofilm%>%select("summer_B_JH3") 
water=S.water%>%select("summer_W_JH3") 
soil.source=S.soil %>%select("summer_C_JH3") 
soil.sink=S.soil %>%select("summer_C_JH4") 

x = list(sediment = rownames(sediment)[sediment > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         soil.source= rownames(soil.source)[soil.source> 0],
         soil.sink=  rownames(soil.sink)[soil.sink > 0])

A <- x[[1]]
B <- x[[2]]
C <- x[[3]]
D <- x[[4]]
E <- x[[5]]
total<- length(Reduce(union, x))
AE=intersect(A, E)
BE=intersect(B, E)
CE=intersect(C, E)
DE=intersect(D, E)
length(Reduce(union, list(AE,BE,CE,DE)))
soil_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
soil_sink


## summer_C_JH4


sediment=S.sediment%>%select("summer_S_JH3") 
biofilm=S.biofilm%>%select("summer_B_JH3") 
water=S.water%>%select("summer_W_JH3") 
soil.source=S.soil %>%select("summer_C_JH3") 
soil.sink=S.soil %>%select("summer_C_JH4") 

x = list(sediment = rownames(sediment)[sediment > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         soil.source= rownames(soil.source)[soil.source> 0],
         soil.sink=  rownames(soil.sink)[soil.sink > 0])

A <- x[[1]]
B <- x[[2]]
C <- x[[3]]
D <- x[[4]]
E <- x[[5]]
total<- length(Reduce(union, x))
AE=intersect(A, E)
BE=intersect(B, E)
CE=intersect(C, E)
DE=intersect(D, E)
length(Reduce(union, list(AE,BE,CE,DE)))
soil_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
soil_sink


## summer_C_TQP

sediment=S.sediment%>%select("summer_S_JH3") 
biofilm=S.biofilm%>%select("summer_B_JH3") 
water=S.water%>%select("summer_W_JH3") 
soil.source=S.soil %>%select("summer_C_JH3") 
soil.sink=S.soil %>%select("summer_C_TQP") 

x = list(sediment = rownames(sediment)[sediment > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         soil.source= rownames(soil.source)[soil.source> 0],
         soil.sink=  rownames(soil.sink)[soil.sink > 0])

A <- x[[1]]
B <- x[[2]]
C <- x[[3]]
D <- x[[4]]
E <- x[[5]]
total<- length(Reduce(union, x))
AE=intersect(A, E)
BE=intersect(B, E)
CE=intersect(C, E)
DE=intersect(D, E)
length(Reduce(union, list(AE,BE,CE,DE)))
soil_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
soil_sink

## summer_C_QYH2


sediment=S.sediment%>%select("summer_S_QYH1") 
biofilm=S.biofilm%>%select("summer_B_QYH1") 
water=S.water%>%select("summer_W_QYH1") 
soil.source=S.soil %>%select("summer_C_QYH1") 
soil.sink=S.soil %>%select("summer_C_QYH2") 

x = list(sediment = rownames(sediment)[sediment > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         soil.source= rownames(soil.source)[soil.source> 0],
         soil.sink=  rownames(soil.sink)[soil.sink > 0])

A <- x[[1]]
B <- x[[2]]
C <- x[[3]]
D <- x[[4]]
E <- x[[5]]
total<- length(Reduce(union, x))
AE=intersect(A, E)
BE=intersect(B, E)
CE=intersect(C, E)
DE=intersect(D, E)
length(Reduce(union, list(AE,BE,CE,DE)))
soil_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
soil_sink

## summer_C_QYH3


sediment=S.sediment%>%select("summer_S_QYH2") 
biofilm=S.biofilm%>%select("summer_B_QYH2") 
water=S.water%>%select("summer_W_QYH2") 
soil.source=S.soil %>%select("summer_C_QYH2") 
soil.sink=S.soil %>%select("summer_C_QYH3") 

x = list(sediment = rownames(sediment)[sediment > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         soil.source= rownames(soil.source)[soil.source> 0],
         soil.sink=  rownames(soil.sink)[soil.sink > 0])

A <- x[[1]]
B <- x[[2]]
C <- x[[3]]
D <- x[[4]]
E <- x[[5]]
total<- length(Reduce(union, x))
AE=intersect(A, E)
BE=intersect(B, E)
CE=intersect(C, E)
DE=intersect(D, E)
length(Reduce(union, list(AE,BE,CE,DE)))
soil_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
soil_sink

## summer_C_QYH4


sediment=S.sediment%>%select("summer_S_QYH3") 
biofilm=S.biofilm%>%select("summer_B_QYH3") 
water=S.water%>%select("summer_W_QYH3") 
soil.source=S.soil %>%select("summer_C_QYH3") 
soil.sink=S.soil %>%select("summer_C_QYH4") 

x = list(sediment = rownames(sediment)[sediment > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         soil.source= rownames(soil.source)[soil.source> 0],
         soil.sink=  rownames(soil.sink)[soil.sink > 0])

A <- x[[1]]
B <- x[[2]]
C <- x[[3]]
D <- x[[4]]
E <- x[[5]]
total<- length(Reduce(union, x))
AE=intersect(A, E)
BE=intersect(B, E)
CE=intersect(C, E)
DE=intersect(D, E)
length(Reduce(union, list(AE,BE,CE,DE)))
soil_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
soil_sink