rm(list = ls())
#gc()

#print("Change directory path")
#dir_path = paste("~/FEAST/")
#setwd(paste0(dir_path, "FEAST_src"))
source("src.R")

#Set the arguments of your data
# metadata_file = "your_metadata_file_name"
# count_matrix = "your_taxa_count_matrix"
EM_iterations = 1000 #default value
##if you use different sources for each sink, different_sources_flag = 1, otherwise = 0
different_sources_flag = 0

# setwd(paste0(dir_path, "Data_files"))
# Load sample metadata

#  1
metadata <- read.csv("../V9/group/summer_sedtos.csv",header = T, sep = ",", row.names = 1)
# Load OTU table
otus <- read.table("../V9/input/summer_soil_sediment.csv",header = T, sep = ",", row.names = 1)
otus <- t(as.matrix(otus))





# Extract only those samples in common between the two tables
common.sample.ids <- intersect(rownames(metadata), rownames(otus))
otus <- otus[common.sample.ids,]
metadata <- metadata[common.sample.ids,]
# Double-check that the mapping file and otu table
# had overlapping samples
if(length(common.sample.ids) <= 1) {
  message <- paste(sprintf('Error: there are %d sample ids in common '),
                   'between the metadata file and data table')
  stop(message)
}


if(different_sources_flag == 0){
  
  metadata$id[metadata$SourceSink == 'Source'] = NA
  metadata$id[metadata$SourceSink == 'Sink'] = c(1:length(which(metadata$SourceSink == 'Sink')))
}


envs <- metadata$Env
Ids <- na.omit(unique(metadata$id))
Proportions_est <- list()

for(it in 1:length(Ids)){
  
  if(different_sources_flag == 1){
    
    train.ix <- which(metadata$SourceSink=='Source' & metadata$id == Ids[it])
    test.ix <- which(metadata$SourceSink=='Sink' & metadata$id == Ids[it])
  } else {
    train.ix <- which(metadata$SourceSink=='Source')
    test.ix <- which(metadata$SourceSink=='Sink' & metadata$id == Ids[it])
  }
  
  num_sources <- length(train.ix)
  COVERAGE =  min(rowSums(otus[c(train.ix, test.ix),])) 
  str(COVERAGE)
  
  sources <- as.data.frame(as.matrix(rarefy(as.data.frame(otus[train.ix,]), COVERAGE)))
  sinks <- as.data.frame(as.matrix(rarefy(as.data.frame(t(as.matrix(otus[test.ix,]))), COVERAGE)))
  
  
  print(paste("Number of OTUs in the sink sample = ",length(which(sinks > 0))))
  print(paste("Seq depth in the sources and sink samples = ",COVERAGE))
  print(paste("The sink is:", envs[test.ix]))
  
  # Estimate source proportions for each sink
  FEAST_output<-FEAST(source=sources, sinks = t(sinks), env = envs[train.ix], em_itr = EM_iterations, COVERAGE = COVERAGE)
  Proportions_est[[it]] <- FEAST_output$data_prop[,1]
  
  
  names(Proportions_est[[it]]) <- c(as.character(envs[train.ix]), "unknown")
  
  if(length(Proportions_est[[it]]) < num_sources +1){
    
    tmp = Proportions_est[[it]]
    Proportions_est[[it]][num_sources] = NA
    Proportions_est[[it]][num_sources+1] = tmp[num_sources]
  }
  
  print("Source mixing proportions")
  print(Proportions_est[[it]])
  
  
}


print(Proportions_est)
write.csv(Proportions_est,file = "summer_sedtos.res.csv",quote=F)