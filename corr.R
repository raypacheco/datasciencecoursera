corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  cc <- complete(directory, id = 1:332)
  ccthresh <- cc[["id"]][cc$nobs > threshold]
  c <- as.numeric(c())
  if (length(ccthresh) !=0){
    files <- paste(directory, sprintf("%03d.csv", ccthresh), sep="/")
      for(i in 1:length(ccthresh)){
        data <- na.omit(read.csv(files[i], header=TRUE))
        c[i] <- cor(data$nitrate,data$sulfate)
    }
  }
  return(c)
}