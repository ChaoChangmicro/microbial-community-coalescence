S.biofilm = read.csv(file = "summer_biofilm.csv",header=T,row.names = 1)
S.sediment = read.csv(file = "summer_sediment.csv",header=T,row.names = 1)
S.soil = read.csv(file = "summer_soil.csv",header=T,row.names = 1)
S.water = read.csv(file = "summer_water.csv",header=T,row.names = 1)

library(dplyr)

##1 summer_C_M2


soil=S.soil%>%select("summer_C_M1") 
biofilm=S.biofilm%>%select("summer_B_M1") 
water=S.water%>%select("summer_W_M1") 
sediment.source=S.sediment %>%select("summer_S_M1") 
sediment.sink=S.sediment %>%select("summer_S_M2") 

x = list(soil = rownames(soil)[soil > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         sediment.source= rownames(sediment.source)[sediment.source> 0],
         sediment.sink=  rownames(sediment.sink)[sediment.sink > 0])

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
sediment_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
sediment_sink

##2 summer_C_M3


soil=S.soil%>%select("summer_C_M2") 
biofilm=S.biofilm%>%select("summer_B_M2") 
water=S.water%>%select("summer_W_M2") 
sediment.source=S.sediment %>%select("summer_S_M2") 
sediment.sink=S.sediment %>%select("summer_S_M3") 

x = list(soil = rownames(soil)[soil > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         sediment.source= rownames(sediment.source)[sediment.source> 0],
         sediment.sink=  rownames(sediment.sink)[sediment.sink > 0])

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
sediment_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
sediment_sink

##  summer_S_M4


soil=S.soil%>%select("summer_C_M3") 
biofilm=S.biofilm%>%select("summer_B_M3") 
water=S.water%>%select("summer_W_M3") 
sediment.source=S.sediment %>%select("summer_S_M3") 
sediment.sink=S.sediment %>%select("summer_S_M4") 

x = list(soil = rownames(soil)[soil > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         sediment.source= rownames(sediment.source)[sediment.source> 0],
         sediment.sink=  rownames(sediment.sink)[sediment.sink > 0])

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
sediment_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
sediment_sink

##  summer_S_M5


soil=S.soil%>%select("summer_C_M4") 
biofilm=S.biofilm%>%select("summer_B_M4") 
water=S.water%>%select("summer_W_M4") 
sediment.source=S.sediment %>%select("summer_S_M4") 
sediment.sink=S.sediment %>%select("summer_S_M5") 

x = list(soil = rownames(soil)[soil > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         sediment.source= rownames(sediment.source)[sediment.source> 0],
         sediment.sink=  rownames(sediment.sink)[sediment.sink > 0])

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
sediment_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
sediment_sink


##  summer_S_M6


soil=S.soil%>%select("summer_C_M5") 
biofilm=S.biofilm%>%select("summer_B_M5") 
water=S.water%>%select("summer_W_M5") 
sediment.source=S.sediment %>%select("summer_S_M5") 
sediment.sink=S.sediment %>%select("summer_S_M6") 

x = list(soil = rownames(soil)[soil > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         sediment.source= rownames(sediment.source)[sediment.source> 0],
         sediment.sink=  rownames(sediment.sink)[sediment.sink > 0])

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
sediment_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
sediment_sink

##  summer_S_M7


soil=S.soil%>%select("summer_C_M6") 
biofilm=S.biofilm%>%select("summer_B_M6") 
water=S.water%>%select("summer_W_M6") 
sediment.source=S.sediment %>%select("summer_S_M6") 
sediment.sink=S.sediment %>%select("summer_S_M7") 

x = list(soil = rownames(soil)[soil > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         sediment.source= rownames(sediment.source)[sediment.source> 0],
         sediment.sink=  rownames(sediment.sink)[sediment.sink > 0])

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
sediment_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
sediment_sink


##  summer_S_M8


soil=S.soil%>%select("summer_C_M7") 
biofilm=S.biofilm%>%select("summer_B_M7") 
water=S.water%>%select("summer_W_M7") 
sediment.source=S.sediment %>%select("summer_S_M7") 
sediment.sink=S.sediment %>%select("summer_S_M8") 

x = list(soil = rownames(soil)[soil > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         sediment.source= rownames(sediment.source)[sediment.source> 0],
         sediment.sink=  rownames(sediment.sink)[sediment.sink > 0])

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
sediment_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
sediment_sink


soil=S.soil%>%select("summer_C_JH4") 
biofilm=S.biofilm%>%select("summer_B_JH4") 
water=S.water%>%select("summer_W_JH4") 
sediment.source=S.sediment %>%select("summer_S_JH4") 
sediment.sink=S.sediment %>%select("summer_S_M8") 

x = list(soil = rownames(soil)[soil > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         sediment.source= rownames(sediment.source)[sediment.source> 0],
         sediment.sink=  rownames(sediment.sink)[sediment.sink > 0])

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
sediment_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
sediment_sink


##  summer_S_M9


soil=S.soil%>%select("summer_C_M8") 
biofilm=S.biofilm%>%select("summer_B_M8") 
water=S.water%>%select("summer_W_M8") 
sediment.source=S.sediment %>%select("summer_S_M8") 
sediment.sink=S.sediment %>%select("summer_S_M9") 

x = list(soil = rownames(soil)[soil > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         sediment.source= rownames(sediment.source)[sediment.source> 0],
         sediment.sink=  rownames(sediment.sink)[sediment.sink > 0])

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
sediment_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
sediment_sink



soil=S.soil%>%select("summer_C_ZSH") 
biofilm=S.biofilm%>%select("summer_B_ZSH") 
water=S.water%>%select("summer_W_ZSH") 
sediment.source=S.sediment %>%select("summer_S_ZSH") 
sediment.sink=S.sediment %>%select("summer_S_M9") 

x = list(soil = rownames(soil)[soil > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         sediment.source= rownames(sediment.source)[sediment.source> 0],
         sediment.sink=  rownames(sediment.sink)[sediment.sink > 0])

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
sediment_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
sediment_sink



##  summer_S_M10


soil=S.soil%>%select("summer_C_M9") 
biofilm=S.biofilm%>%select("summer_B_M9") 
water=S.water%>%select("summer_W_M9") 
sediment.source=S.sediment %>%select("summer_S_M9") 
sediment.sink=S.sediment %>%select("summer_S_M10") 

x = list(soil = rownames(soil)[soil > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         sediment.source= rownames(sediment.source)[sediment.source> 0],
         sediment.sink=  rownames(sediment.sink)[sediment.sink > 0])

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
sediment_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
sediment_sink


##  summer_S_M11


soil=S.soil%>%select("summer_C_M10") 
biofilm=S.biofilm%>%select("summer_B_M10") 
water=S.water%>%select("summer_W_M10") 
sediment.source=S.sediment %>%select("summer_S_M10") 
sediment.sink=S.sediment %>%select("summer_S_M11") 

x = list(soil = rownames(soil)[soil > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         sediment.source= rownames(sediment.source)[sediment.source> 0],
         sediment.sink=  rownames(sediment.sink)[sediment.sink > 0])

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
sediment_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
sediment_sink


##  summer_S_M12


soil=S.soil%>%select("summer_C_M11") 
biofilm=S.biofilm%>%select("summer_B_M11") 
water=S.water%>%select("summer_W_M11") 
sediment.source=S.sediment %>%select("summer_S_M11") 
sediment.sink=S.sediment %>%select("summer_S_M12") 

x = list(soil = rownames(soil)[soil > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         sediment.source= rownames(sediment.source)[sediment.source> 0],
         sediment.sink=  rownames(sediment.sink)[sediment.sink > 0])

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
sediment_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
sediment_sink


##  summer_S_M13


soil=S.soil%>%select("summer_C_M12") 
biofilm=S.biofilm%>%select("summer_B_M12") 
water=S.water%>%select("summer_W_M12") 
sediment.source=S.sediment %>%select("summer_S_M12") 
sediment.sink=S.sediment %>%select("summer_S_M13") 

x = list(soil = rownames(soil)[soil > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         sediment.source= rownames(sediment.source)[sediment.source> 0],
         sediment.sink=  rownames(sediment.sink)[sediment.sink > 0])

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
sediment_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
sediment_sink




soil=S.soil%>%select("summer_C_QYH4") 
biofilm=S.biofilm%>%select("summer_B_QYH4") 
water=S.water%>%select("summer_W_QYH4") 
sediment.source=S.sediment %>%select("summer_S_QYH4") 
sediment.sink=S.sediment %>%select("summer_S_M13") 

x = list(soil = rownames(soil)[soil > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         sediment.source= rownames(sediment.source)[sediment.source> 0],
         sediment.sink=  rownames(sediment.sink)[sediment.sink > 0])

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
sediment_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
sediment_sink

##  summer_S_JH2


soil=S.soil%>%select("summer_C_JH1") 
biofilm=S.biofilm%>%select("summer_B_JH1") 
water=S.water%>%select("summer_W_JH1") 
sediment.source=S.sediment %>%select("summer_S_JH1") 
sediment.sink=S.sediment %>%select("summer_S_JH2") 

x = list(soil = rownames(soil)[soil > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         sediment.source= rownames(sediment.source)[sediment.source> 0],
         sediment.sink=  rownames(sediment.sink)[sediment.sink > 0])

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
sediment_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
sediment_sink


##  summer_S_JH3


soil=S.soil%>%select("summer_C_JH2") 
biofilm=S.biofilm%>%select("summer_B_JH2") 
water=S.water%>%select("summer_W_JH2") 
sediment.source=S.sediment %>%select("summer_S_JH2") 
sediment.sink=S.sediment %>%select("summer_S_JH3") 

x = list(soil = rownames(soil)[soil > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         sediment.source= rownames(sediment.source)[sediment.source> 0],
         sediment.sink=  rownames(sediment.sink)[sediment.sink > 0])

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
sediment_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
sediment_sink


##  summer_S_JH4


soil=S.soil%>%select("summer_C_JH3") 
biofilm=S.biofilm%>%select("summer_B_JH3") 
water=S.water%>%select("summer_W_JH3") 
sediment.source=S.sediment %>%select("summer_S_JH3") 
sediment.sink=S.sediment %>%select("summer_S_JH4") 

x = list(soil = rownames(soil)[soil > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         sediment.source= rownames(sediment.source)[sediment.source> 0],
         sediment.sink=  rownames(sediment.sink)[sediment.sink > 0])

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
sediment_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
sediment_sink


## summer_S_TQP


soil=S.soil%>%select("summer_C_JH3") 
biofilm=S.biofilm%>%select("summer_B_JH3") 
water=S.water%>%select("summer_W_JH3") 
sediment.source=S.sediment %>%select("summer_S_JH3") 
sediment.sink=S.sediment %>%select("summer_S_TQP") 

x = list(soil = rownames(soil)[soil > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         sediment.source= rownames(sediment.source)[sediment.source> 0],
         sediment.sink=  rownames(sediment.sink)[sediment.sink > 0])

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
sediment_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
sediment_sink


## summer_S_QYH2


soil=S.soil%>%select("summer_C_QYH1") 
biofilm=S.biofilm%>%select("summer_B_QYH1") 
water=S.water%>%select("summer_W_QYH1") 
sediment.source=S.sediment %>%select("summer_S_QYH1") 
sediment.sink=S.sediment %>%select("summer_S_QYH2") 

x = list(soil = rownames(soil)[soil > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         sediment.source= rownames(sediment.source)[sediment.source> 0],
         sediment.sink=  rownames(sediment.sink)[sediment.sink > 0])

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
sediment_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
sediment_sink

## summer_S_QYH3


soil=S.soil%>%select("summer_C_QYH2") 
biofilm=S.biofilm%>%select("summer_B_QYH2") 
water=S.water%>%select("summer_W_QYH2") 
sediment.source=S.sediment %>%select("summer_S_QYH2") 
sediment.sink=S.sediment %>%select("summer_S_QYH3") 

x = list(soil = rownames(soil)[soil > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         sediment.source= rownames(sediment.source)[sediment.source> 0],
         sediment.sink=  rownames(sediment.sink)[sediment.sink > 0])

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
sediment_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
sediment_sink


## summer_S_QYH4


soil=S.soil%>%select("summer_C_QYH3") 
biofilm=S.biofilm%>%select("summer_B_QYH3") 
water=S.water%>%select("summer_W_QYH3") 
sediment.source=S.sediment %>%select("summer_S_QYH3") 
sediment.sink=S.sediment %>%select("summer_S_QYH4") 

x = list(soil = rownames(soil)[soil > 0],
         biofilm=rownames(biofilm)[biofilm > 0] ,
         water= rownames(water)[water > 0],
         sediment.source= rownames(sediment.source)[sediment.source> 0],
         sediment.sink=  rownames(sediment.sink)[sediment.sink > 0])

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
sediment_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
sediment_sink