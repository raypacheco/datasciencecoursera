rankall <- function(outcome, num = "best") {
  
  ## outcome types and the referring column number in the table
  types <- c("heart attack", "heart failure", "pneumonia")
  colNum <- c(11, 17, 23)
  names(colNum) <- types
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings=c("Not Available", "NA"))
  
  ## Check that outcome is valid
  if(!(outcome %in% types)){
    stop("invalid outcome")}
  
  ## change needed column to numeric data first
  data[,colNum[[outcome]]] <- as.numeric(data[,colNum[[outcome]]])
  
  ## create list for the data frame hospital outcomes
  hospitals <- c()
  
  ## create a list of all unique states
  states <- sort(unique(data$State))
  
  ## loop through all states (probably could use apply here)
  for(s in states){
    num <- num
    ## subset each state for specified outcome
    sub <- subset(data[c(2,colNum[[outcome]])], data$State == s)
    
    ## Alphabatize subset in case of ties for min
    ordered <- sub[order(sub[,2], sub[,1]),]

    ## Remove rows with NA values
    ordered <- ordered[complete.cases(ordered),]

    ## Return hospital name in that state with the given rank
    ## 30-day death rate

    if(num == "best"){
      hospitals <- append(hospitals, ordered[1,1])}
    else {if(num == "worst"){
      hospitals <- append(hospitals, ordered[nrow(ordered),1])}
    else {if(num > nrow(ordered)){
      hospitals <- append(hospitals, "<NA>")}
    else {hospitals <- append(hospitals, ordered[num,1])}}}

  } 
  ## create output dataframe
  data.frame(hospital=hospitals, state=states)
}