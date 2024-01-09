S.biofilm = read.csv(file = "winter_biofilm.csv",header=T,row.names = 1)
S.sediment = read.csv(file = "winter_sediment.csv",header=T,row.names = 1)
S.soil = read.csv(file = "winter_soil.csv",header=T,row.names = 1)
S.water = read.csv(file = "winter_water.csv",header=T,row.names = 1)

library(dplyr)

##1 winter_B_M2


sediment=S.sediment%>%select("winter_S_M1") 
soil=S.soil%>%select("winter_C_M1") 
water=S.water%>%select("winter_W_M1") 
biofilm.source=S.biofilm %>%select("winter_B_M1") 
biofilm.sink=S.biofilm %>%select("winter_B_M2") 

x = list(sediment = rownames(sediment)[sediment > 0],
         soil=rownames(soil)[soil > 0] ,
         water= rownames(water)[water > 0],
         biofilm.source= rownames(biofilm.source)[biofilm.source> 0],
         biofilm.sink=  rownames(biofilm.sink)[biofilm.sink > 0])

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
biofilm_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
biofilm_sink

##2 winter_B_M3


sediment=S.sediment%>%select("winter_S_M2") 
soil=S.soil%>%select("winter_C_M2") 
water=S.water%>%select("winter_W_M2") 
biofilm.source=S.biofilm %>%select("winter_B_M2") 
biofilm.sink=S.biofilm %>%select("winter_B_M3") 

x = list(sediment = rownames(sediment)[sediment > 0],
         soil=rownames(soil)[soil > 0] ,
         water= rownames(water)[water > 0],
         biofilm.source= rownames(biofilm.source)[biofilm.source> 0],
         biofilm.sink=  rownames(biofilm.sink)[biofilm.sink > 0])

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
biofilm_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
biofilm_sink


##3 winter_B_M4


sediment=S.sediment%>%select("winter_S_M3") 
soil=S.soil%>%select("winter_C_M3") 
water=S.water%>%select("winter_W_M3") 
biofilm.source=S.biofilm %>%select("winter_B_M3") 
biofilm.sink=S.biofilm %>%select("winter_B_M4") 

x = list(sediment = rownames(sediment)[sediment > 0],
         soil=rownames(soil)[soil > 0] ,
         water= rownames(water)[water > 0],
         biofilm.source= rownames(biofilm.source)[biofilm.source> 0],
         biofilm.sink=  rownames(biofilm.sink)[biofilm.sink > 0])

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
biofilm_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
biofilm_sink

##4 winter_B_M5


sediment=S.sediment%>%select("winter_S_M4") 
soil=S.soil%>%select("winter_C_M4") 
water=S.water%>%select("winter_W_M4") 
biofilm.source=S.biofilm %>%select("winter_B_M4") 
biofilm.sink=S.biofilm %>%select("winter_B_M5") 

x = list(sediment = rownames(sediment)[sediment > 0],
         soil=rownames(soil)[soil > 0] ,
         water= rownames(water)[water > 0],
         biofilm.source= rownames(biofilm.source)[biofilm.source> 0],
         biofilm.sink=  rownames(biofilm.sink)[biofilm.sink > 0])

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
biofilm_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
biofilm_sink

##5 winter_B_M6


sediment=S.sediment%>%select("winter_S_M5") 
soil=S.soil%>%select("winter_C_M5") 
water=S.water%>%select("winter_W_M5") 
biofilm.source=S.biofilm %>%select("winter_B_M5") 
biofilm.sink=S.biofilm %>%select("winter_B_M6") 

x = list(sediment = rownames(sediment)[sediment > 0],
         soil=rownames(soil)[soil > 0] ,
         water= rownames(water)[water > 0],
         biofilm.source= rownames(biofilm.source)[biofilm.source> 0],
         biofilm.sink=  rownames(biofilm.sink)[biofilm.sink > 0])

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
biofilm_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
biofilm_sink


##6 winter_B_M7


sediment=S.sediment%>%select("winter_S_M6") 
soil=S.soil%>%select("winter_C_M6") 
water=S.water%>%select("winter_W_M6") 
biofilm.source=S.biofilm %>%select("winter_B_M6") 
biofilm.sink=S.biofilm %>%select("winter_B_M7") 

x = list(sediment = rownames(sediment)[sediment > 0],
         soil=rownames(soil)[soil > 0] ,
         water= rownames(water)[water > 0],
         biofilm.source= rownames(biofilm.source)[biofilm.source> 0],
         biofilm.sink=  rownames(biofilm.sink)[biofilm.sink > 0])

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
biofilm_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
biofilm_sink


##7 winter_B_M8


sediment=S.sediment%>%select("winter_S_M7") 
soil=S.soil%>%select("winter_C_M7") 
water=S.water%>%select("winter_W_M7") 
biofilm.source=S.biofilm %>%select("winter_B_M7") 
biofilm.sink=S.biofilm %>%select("winter_B_M8") 

x = list(sediment = rownames(sediment)[sediment > 0],
         soil=rownames(soil)[soil > 0] ,
         water= rownames(water)[water > 0],
         biofilm.source= rownames(biofilm.source)[biofilm.source> 0],
         biofilm.sink=  rownames(biofilm.sink)[biofilm.sink > 0])

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
biofilm_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
biofilm_sink



sediment=S.sediment%>%select("winter_S_JH4") 
soil=S.soil%>%select("winter_C_JH4") 
water=S.water%>%select("winter_W_JH4") 
biofilm.source=S.biofilm %>%select("winter_B_JH4") 
biofilm.sink=S.biofilm %>%select("winter_B_M8") 

x = list(sediment = rownames(sediment)[sediment > 0],
         soil=rownames(soil)[soil > 0] ,
         water= rownames(water)[water > 0],
         biofilm.source= rownames(biofilm.source)[biofilm.source> 0],
         biofilm.sink=  rownames(biofilm.sink)[biofilm.sink > 0])

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
biofilm_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
biofilm_sink

##8 winter_B_M9


sediment=S.sediment%>%select("winter_S_M8") 
soil=S.soil%>%select("winter_C_M8") 
water=S.water%>%select("winter_W_M8") 
biofilm.source=S.biofilm %>%select("winter_B_M8") 
biofilm.sink=S.biofilm %>%select("winter_B_M9") 

x = list(sediment = rownames(sediment)[sediment > 0],
         soil=rownames(soil)[soil > 0] ,
         water= rownames(water)[water > 0],
         biofilm.source= rownames(biofilm.source)[biofilm.source> 0],
         biofilm.sink=  rownames(biofilm.sink)[biofilm.sink > 0])

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
biofilm_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
biofilm_sink


sediment=S.sediment%>%select("winter_S_ZSH") 
soil=S.soil%>%select("winter_C_ZSH") 
water=S.water%>%select("winter_W_ZSH") 
biofilm.source=S.biofilm %>%select("winter_B_ZSH") 
biofilm.sink=S.biofilm %>%select("winter_B_M9") 

x = list(sediment = rownames(sediment)[sediment > 0],
         soil=rownames(soil)[soil > 0] ,
         water= rownames(water)[water > 0],
         biofilm.source= rownames(biofilm.source)[biofilm.source> 0],
         biofilm.sink=  rownames(biofilm.sink)[biofilm.sink > 0])

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
biofilm_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
biofilm_sink



##9 winter_B_M10


sediment=S.sediment%>%select("winter_S_M9") 
soil=S.soil%>%select("winter_C_M9") 
water=S.water%>%select("winter_W_M9") 
biofilm.source=S.biofilm %>%select("winter_B_M9") 
biofilm.sink=S.biofilm %>%select("winter_B_M10") 

x = list(sediment = rownames(sediment)[sediment > 0],
         soil=rownames(soil)[soil > 0] ,
         water= rownames(water)[water > 0],
         biofilm.source= rownames(biofilm.source)[biofilm.source> 0],
         biofilm.sink=  rownames(biofilm.sink)[biofilm.sink > 0])

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
biofilm_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
biofilm_sink



##10 winter_B_M11


sediment=S.sediment%>%select("winter_S_M10") 
soil=S.soil%>%select("winter_C_M10") 
water=S.water%>%select("winter_W_M10") 
biofilm.source=S.biofilm %>%select("winter_B_M10") 
biofilm.sink=S.biofilm %>%select("winter_B_M11") 

x = list(sediment = rownames(sediment)[sediment > 0],
         soil=rownames(soil)[soil > 0] ,
         water= rownames(water)[water > 0],
         biofilm.source= rownames(biofilm.source)[biofilm.source> 0],
         biofilm.sink=  rownames(biofilm.sink)[biofilm.sink > 0])

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
biofilm_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
biofilm_sink


##11 winter_B_M12


sediment=S.sediment%>%select("winter_S_M11") 
soil=S.soil%>%select("winter_C_M11") 
water=S.water%>%select("winter_W_M11") 
biofilm.source=S.biofilm %>%select("winter_B_M11") 
biofilm.sink=S.biofilm %>%select("winter_B_M12") 

x = list(sediment = rownames(sediment)[sediment > 0],
         soil=rownames(soil)[soil > 0] ,
         water= rownames(water)[water > 0],
         biofilm.source= rownames(biofilm.source)[biofilm.source> 0],
         biofilm.sink=  rownames(biofilm.sink)[biofilm.sink > 0])

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
biofilm_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
biofilm_sink


##12 winter_B_M13


sediment=S.sediment%>%select("winter_S_M12") 
soil=S.soil%>%select("winter_C_M12") 
water=S.water%>%select("winter_W_M12") 
biofilm.source=S.biofilm %>%select("winter_B_M12") 
biofilm.sink=S.biofilm %>%select("winter_B_M13") 

x = list(sediment = rownames(sediment)[sediment > 0],
         soil=rownames(soil)[soil > 0] ,
         water= rownames(water)[water > 0],
         biofilm.source= rownames(biofilm.source)[biofilm.source> 0],
         biofilm.sink=  rownames(biofilm.sink)[biofilm.sink > 0])

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
biofilm_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
biofilm_sink


sediment=S.sediment%>%select("winter_S_QYH4") 
soil=S.soil%>%select("winter_C_QYH4") 
water=S.water%>%select("winter_W_QYH4") 
biofilm.source=S.biofilm %>%select("winter_B_QYH4") 
biofilm.sink=S.biofilm %>%select("winter_B_M13") 

x = list(sediment = rownames(sediment)[sediment > 0],
         soil=rownames(soil)[soil > 0] ,
         water= rownames(water)[water > 0],
         biofilm.source= rownames(biofilm.source)[biofilm.source> 0],
         biofilm.sink=  rownames(biofilm.sink)[biofilm.sink > 0])

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
biofilm_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
biofilm_sink



##13 winter_B_JH2



sediment=S.sediment%>%select("winter_S_JH1") 
soil=S.soil%>%select("winter_C_JH1") 
water=S.water%>%select("winter_W_JH1") 
biofilm.source=S.biofilm %>%select("winter_B_JH1") 
biofilm.sink=S.biofilm %>%select("winter_B_JH2") 

x = list(sediment = rownames(sediment)[sediment > 0],
         soil=rownames(soil)[soil > 0] ,
         water= rownames(water)[water > 0],
         biofilm.source= rownames(biofilm.source)[biofilm.source> 0],
         biofilm.sink=  rownames(biofilm.sink)[biofilm.sink > 0])

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
biofilm_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
biofilm_sink




##16 winter_B_QYH2


sediment=S.sediment%>%select("winter_S_QYH1") 
soil=S.soil%>%select("winter_C_QYH1") 
water=S.water%>%select("winter_W_QYH1") 
biofilm.source=S.biofilm %>%select("winter_B_QYH1") 
biofilm.sink=S.biofilm %>%select("winter_B_QYH2") 

x = list(sediment = rownames(sediment)[sediment > 0],
         soil=rownames(soil)[soil > 0] ,
         water= rownames(water)[water > 0],
         biofilm.source= rownames(biofilm.source)[biofilm.source> 0],
         biofilm.sink=  rownames(biofilm.sink)[biofilm.sink > 0])

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
biofilm_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
biofilm_sink


##17 winter_B_QYH3


sediment=S.sediment%>%select("winter_S_QYH2") 
soil=S.soil%>%select("winter_C_QYH2") 
water=S.water%>%select("winter_W_QYH2") 
biofilm.source=S.biofilm %>%select("winter_B_QYH2") 
biofilm.sink=S.biofilm %>%select("winter_B_QYH3") 

x = list(sediment = rownames(sediment)[sediment > 0],
         soil=rownames(soil)[soil > 0] ,
         water= rownames(water)[water > 0],
         biofilm.source= rownames(biofilm.source)[biofilm.source> 0],
         biofilm.sink=  rownames(biofilm.sink)[biofilm.sink > 0])

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
biofilm_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
biofilm_sink



##18 winter_B_QYH4


sediment=S.sediment%>%select("winter_S_QYH3") 
soil=S.soil%>%select("winter_C_QYH3") 
water=S.water%>%select("winter_W_QYH3") 
biofilm.source=S.biofilm %>%select("winter_B_QYH3") 
biofilm.sink=S.biofilm %>%select("winter_B_QYH4") 

x = list(sediment = rownames(sediment)[sediment > 0],
         soil=rownames(soil)[soil > 0] ,
         water= rownames(water)[water > 0],
         biofilm.source= rownames(biofilm.source)[biofilm.source> 0],
         biofilm.sink=  rownames(biofilm.sink)[biofilm.sink > 0])

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
biofilm_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
biofilm_sink