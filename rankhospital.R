rankhospital <- function(state, outcome, num = "best") {
  
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
  ordered <- sub[order(sub[,2], sub[,1]),]
  
  ## Remove rows with NA values
  ordered <- ordered[complete.cases(ordered),]
  
  ## Add column with ranking for that state
  ordered$Rank <- 1:nrow(ordered)
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  if(num == "best"){
    ordered[1,1]}
  else {if(num == "worst"){
    ordered[nrow(ordered),1]}
  else {if(num > nrow(ordered)){
    NA}
  else {ordered[num,1]}}}
}