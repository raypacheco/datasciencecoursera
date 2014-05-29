best <- function(state, outcome) {
  
  ## outcome types and the referring column number in the table
  types <- c("heart attack", "heart failure", "pneumonia")
  colNum <- c(11, 17, 23)
  names(colNum) <- types
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings=c("Not Available", "NA"))
  
  ## Check that state and outcome are valid
  if(!(state %in% data[,7])){
    stop("invalid state")}
  if(!(outcome %in% types)){
    stop("invalid outcome")}
  
  ## change needed column to numeric data first
  data[,colNum[[outcome]]] <- as.numeric(data[,colNum[[outcome]]])
  
  ## Subset data to specified state and outcome
  sub <- subset(data[c(2,colNum[[outcome]])], data$State == state)
  
  ## Alphabatize subset in case of ties for min
  ordered <- sub[do.call(order, sub),]
  
  ## Return hospital name in that state with lowest 30-day
  ## death rate
  ordered[which.min(ordered[,2]),1]

}