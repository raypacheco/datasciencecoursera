complete <- function(directory, id = 1:332){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  files <- paste(directory, sprintf("%03d.csv", id), sep="/")
  
  nobs <- c()

  for(i in 1:length(files)){
  
    data <- read.csv(files[i], header=TRUE)
    nobs[i] <- sum(complete.cases(data))
  }
  return(data.frame(id,nobs))  
}
