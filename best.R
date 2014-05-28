best <- function(state, outcome) {
  ## Read outcome data
  read.csv("outcome-of-care-measures.csv", colClasses = "character")
  sub <- subset(outcome[c(7,11)], outcome$State == "TX")
  ## Check that state and outcome are valid
  
  ## Return hospital name in that state with lowest 30-day
  ## death rate
  
  
}