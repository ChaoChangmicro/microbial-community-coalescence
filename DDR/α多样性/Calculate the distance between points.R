library(vegan)

env<-read.csv("lonlat.csv")



library(geosphere)
geo <- data.frame(env$longitude, env$latitude)
d.geo <- distm(geo, fun = distHaversine)      

distance_df <- as.data.frame(as.matrix(d.geo))


rownames(distance_df) <-  env$site
colnames(distance_df)<-  env$site
distance_df
write.csv(distance_df,file = "distance.csv")
