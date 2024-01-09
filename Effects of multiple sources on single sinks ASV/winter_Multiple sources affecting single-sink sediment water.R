S.biofilm = read.csv(file = "winter_biofilm.csv",header=T,row.names = 1)
S.sediment = read.csv(file = "winter_sediment.csv",header=T,row.names = 1)
S.soil = read.csv(file = "winter_soil.csv",header=T,row.names = 1)
S.water = read.csv(file = "winter_water.csv",header=T,row.names = 1)

library(dplyr)

##1 winter_W_M2

biofilm=S.biofilm %>%select("winter_B_M1") 
sediment=S.sediment%>%select("winter_S_M1") 
soil=S.soil%>%select("winter_C_M1") 
water.source=S.water  %>%select("winter_W_M1") 
water.sink=S.water  %>%select("winter_W_M2")

x = list(biofilm = rownames(biofilm)[biofilm > 0],
         sediment =rownames(sediment)[sediment > 0] ,
         soil= rownames(soil)[soil > 0],
         water.source= rownames(water.source)[water.source > 0],
         water.sink=  rownames(water.sink)[water.sink > 0])

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
water_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
water_sink

##2 winter_W_M3

biofilm=S.biofilm %>%select("winter_B_M2") 
sediment=S.sediment%>%select("winter_S_M2") 
soil=S.soil%>%select("winter_C_M2") 
water.source=S.water  %>%select("winter_W_M2") 
water.sink=S.water  %>%select("winter_W_M3")

x = list(biofilm = rownames(biofilm)[biofilm > 0],
         sediment =rownames(sediment)[sediment > 0] ,
         soil= rownames(soil)[soil > 0],
         water.source= rownames(water.source)[water.source > 0],
         water.sink=  rownames(water.sink)[water.sink > 0])

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
water_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
water_sink



##3 winter_W_M4
biofilm=S.biofilm %>%select("winter_B_M3") 
sediment=S.sediment%>%select("winter_S_M3") 
soil=S.soil%>%select("winter_C_M3") 
water.source=S.water  %>%select("winter_W_M3") 
water.sink=S.water  %>%select("winter_W_M4")

x = list(biofilm = rownames(biofilm)[biofilm > 0],
         sediment =rownames(sediment)[sediment > 0] ,
         soil= rownames(soil)[soil > 0],
         water.source= rownames(water.source)[water.source > 0],
         water.sink=  rownames(water.sink)[water.sink > 0])

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
water_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
water_sink



##4 winter_W_M5
biofilm=S.biofilm %>%select("winter_B_M4") 
sediment=S.sediment%>%select("winter_S_M4") 
soil=S.soil%>%select("winter_C_M4") 
water.source=S.water  %>%select("winter_W_M4") 
water.sink=S.water  %>%select("winter_W_M5")

x = list(biofilm = rownames(biofilm)[biofilm > 0],
         sediment =rownames(sediment)[sediment > 0] ,
         soil= rownames(soil)[soil > 0],
         water.source= rownames(water.source)[water.source > 0],
         water.sink=  rownames(water.sink)[water.sink > 0])

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
water_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
water_sink


##5 winter_W_M6
biofilm=S.biofilm %>%select("winter_B_M5") 
sediment=S.sediment%>%select("winter_S_M5") 
soil=S.soil%>%select("winter_C_M5") 
water.source=S.water  %>%select("winter_W_M5") 
water.sink=S.water  %>%select("winter_W_M6")

x = list(biofilm = rownames(biofilm)[biofilm > 0],
         sediment =rownames(sediment)[sediment > 0] ,
         soil= rownames(soil)[soil > 0],
         water.source= rownames(water.source)[water.source > 0],
         water.sink=  rownames(water.sink)[water.sink > 0])

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
water_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
water_sink


##6 winter_W_M7
biofilm=S.biofilm %>%select("winter_B_M6") 
sediment=S.sediment%>%select("winter_S_M6") 
soil=S.soil%>%select("winter_C_M6") 
water.source=S.water  %>%select("winter_W_M6") 
water.sink=S.water  %>%select("winter_W_M7")

x = list(biofilm = rownames(biofilm)[biofilm > 0],
         sediment =rownames(sediment)[sediment > 0] ,
         soil= rownames(soil)[soil > 0],
         water.source= rownames(water.source)[water.source > 0],
         water.sink=  rownames(water.sink)[water.sink > 0])

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
water_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
water_sink

##7 winter_W_M8
biofilm=S.biofilm %>%select("winter_B_M7") 
sediment=S.sediment%>%select("winter_S_M7") 
soil=S.soil%>%select("winter_C_M7") 
water.source=S.water  %>%select("winter_W_M7") 
water.sink=S.water  %>%select("winter_W_M8")

x = list(biofilm = rownames(biofilm)[biofilm > 0],
         sediment =rownames(sediment)[sediment > 0] ,
         soil= rownames(soil)[soil > 0],
         water.source= rownames(water.source)[water.source > 0],
         water.sink=  rownames(water.sink)[water.sink > 0])

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
water_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
water_sink





##9 winter_W_M9
biofilm=S.biofilm %>%select("winter_B_M8") 
sediment=S.sediment%>%select("winter_S_M8") 
soil=S.soil%>%select("winter_C_M8") 
water.source=S.water  %>%select("winter_W_M8") 
water.sink=S.water  %>%select("winter_W_M9")

x = list(biofilm = rownames(biofilm)[biofilm > 0],
         sediment =rownames(sediment)[sediment > 0] ,
         soil= rownames(soil)[soil > 0],
         water.source= rownames(water.source)[water.source > 0],
         water.sink=  rownames(water.sink)[water.sink > 0])

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
water_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
water_sink





##11 winter_W_M10 
biofilm=S.biofilm %>%select("winter_B_M9") 
sediment=S.sediment%>%select("winter_S_M9") 
soil=S.soil%>%select("winter_C_M9") 
water.source=S.water  %>%select("winter_W_M9") 
water.sink=S.water  %>%select("winter_W_M10")

x = list(biofilm = rownames(biofilm)[biofilm > 0],
         sediment =rownames(sediment)[sediment > 0] ,
         soil= rownames(soil)[soil > 0],
         water.source= rownames(water.source)[water.source > 0],
         water.sink=  rownames(water.sink)[water.sink > 0])

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
water_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
water_sink


##12 winter_W_M11
biofilm=S.biofilm %>%select("winter_B_M10") 
sediment=S.sediment%>%select("winter_S_M10") 
soil=S.soil%>%select("winter_C_M10") 
water.source=S.water  %>%select("winter_W_M10") 
water.sink=S.water  %>%select("winter_W_M11")

x = list(biofilm = rownames(biofilm)[biofilm > 0],
         sediment =rownames(sediment)[sediment > 0] ,
         soil= rownames(soil)[soil > 0],
         water.source= rownames(water.source)[water.source > 0],
         water.sink=  rownames(water.sink)[water.sink > 0])

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
water_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
water_sink

##13 winter_W_M12
biofilm=S.biofilm %>%select("winter_B_M11") 
sediment=S.sediment%>%select("winter_S_M11") 
soil=S.soil%>%select("winter_C_M11") 
water.source=S.water  %>%select("winter_W_M11") 
water.sink=S.water  %>%select("winter_W_M12")

x = list(biofilm = rownames(biofilm)[biofilm > 0],
         sediment =rownames(sediment)[sediment > 0] ,
         soil= rownames(soil)[soil > 0],
         water.source= rownames(water.source)[water.source > 0],
         water.sink=  rownames(water.sink)[water.sink > 0])

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
water_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
water_sink



##14 winter_W_M13
biofilm=S.biofilm %>%select("winter_B_QYH4") 
sediment=S.sediment%>%select("winter_S_QYH4") 
soil=S.soil%>%select("winter_C_QYH4") 
water.source=S.water  %>%select("winter_W_QYH4") 
water.sink=S.water  %>%select("winter_W_M13")

x = list(biofilm = rownames(biofilm)[biofilm > 0],
         sediment =rownames(sediment)[sediment > 0] ,
         soil= rownames(soil)[soil > 0],
         water.source= rownames(water.source)[water.source > 0],
         water.sink=  rownames(water.sink)[water.sink > 0])

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
water_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
water_sink


##13 winter_W_M12
biofilm=S.biofilm %>%select("winter_B_M12") 
sediment=S.sediment%>%select("winter_S_M12") 
soil=S.soil%>%select("winter_C_M12") 
water.source=S.water  %>%select("winter_W_M12") 
water.sink=S.water  %>%select("winter_W_M13")

x = list(biofilm = rownames(biofilm)[biofilm > 0],
         sediment =rownames(sediment)[sediment > 0] ,
         soil= rownames(soil)[soil > 0],
         water.source= rownames(water.source)[water.source > 0],
         water.sink=  rownames(water.sink)[water.sink > 0])

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
water_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
water_sink




##15 winter_W_JH2
biofilm=S.biofilm %>%select("winter_B_JH1") 
sediment=S.sediment%>%select("winter_S_JH1") 
soil=S.soil%>%select("winter_C_JH1") 
water.source=S.water  %>%select("winter_W_JH1") 
water.sink=S.water  %>%select("winter_W_JH2")

x = list(biofilm = rownames(biofilm)[biofilm > 0],
         sediment =rownames(sediment)[sediment > 0] ,
         soil= rownames(soil)[soil > 0],
         water.source= rownames(water.source)[water.source > 0],
         water.sink=  rownames(water.sink)[water.sink > 0])

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
water_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
water_sink












##19 winter_W_QYH2
biofilm=S.biofilm %>%select("winter_B_QYH1") 
sediment=S.sediment%>%select("winter_S_QYH1") 
soil=S.soil%>%select("winter_C_QYH1") 
water.source=S.water  %>%select("winter_W_QYH1") 
water.sink=S.water  %>%select("winter_W_QYH2")

x = list(biofilm = rownames(biofilm)[biofilm > 0],
         sediment =rownames(sediment)[sediment > 0] ,
         soil= rownames(soil)[soil > 0],
         water.source= rownames(water.source)[water.source > 0],
         water.sink=  rownames(water.sink)[water.sink > 0])

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
water_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
water_sink

##19 winter_W_QYH3
biofilm=S.biofilm %>%select("winter_B_QYH2") 
sediment=S.sediment%>%select("winter_S_QYH2") 
soil=S.soil%>%select("winter_C_QYH2") 
water.source=S.water  %>%select("winter_W_QYH2") 
water.sink=S.water  %>%select("winter_W_QYH3")

x = list(biofilm = rownames(biofilm)[biofilm > 0],
         sediment =rownames(sediment)[sediment > 0] ,
         soil= rownames(soil)[soil > 0],
         water.source= rownames(water.source)[water.source > 0],
         water.sink=  rownames(water.sink)[water.sink > 0])

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
water_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
water_sink



##19 winter_W_QYH4
biofilm=S.biofilm %>%select("winter_B_QYH3") 
sediment=S.sediment%>%select("winter_S_QYH3") 
soil=S.soil%>%select("winter_C_QYH3") 
water.source=S.water  %>%select("winter_W_QYH3") 
water.sink=S.water  %>%select("winter_W_QYH4")

x = list(biofilm = rownames(biofilm)[biofilm > 0],
         sediment =rownames(sediment)[sediment > 0] ,
         soil= rownames(soil)[soil > 0],
         water.source= rownames(water.source)[water.source > 0],
         water.sink=  rownames(water.sink)[water.sink > 0])

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
water_sink <- length(Reduce(union, list(AE,BE,CE,DE)))/total
water_sink