lmedata=read.csv("winter_moving.csv",row.name=1,head=T,sep = ",")
head(lmedata)
lm <-lm(Positive_Count~jaccard,data=lmedata)
summary(lm)

vectorVAR<-as.matrix(lmedata$assembly)

result_df <- data.frame(mean= numeric(),coef = numeric())
for(i in 1:(nrow(lmedata)-200))
{
  dfi<-lmedata[as.matrix(lmedata$assembly) %in% vectorVAR[i:(i+200)],]
  fit <-lm(Positive_Count~jaccard,data=dfi)
  coef_value <- coef(fit)[[2]]
  meanvalue <- mean(vectorVAR[i:(i+200)])
  result_df <- rbind(result_df, data.frame(mean = meanvalue,coef = coef_value))
}

aa <- lm(coef~mean ,data=result_df)
summary(aa)
write.csv(result_df,file = "winter_Positive_moving.csv")
 