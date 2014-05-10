pollutantmean <- function(directory, pollutant, id = 1:332){
  setwd(directory)
  
  dataset <- data.frame()
  
  for(file in id){
    
    if (!exists("dataset")){
      dataset <- read.csv(paste(sprintf("%03d",file),".csv",sep=""), header=T, sep=",")
    }
    
    if(exists("dataset")){
      temp_dataset <- read.csv(paste(sprintf("%03d",file),".csv",sep=""), header=T, sep=",")
      dataset <- rbind(dataset, temp_dataset)
      rm(temp_dataset)
    }
  }
  mean(na.omit(paste("dataset$",pollutant,sep=""))
}