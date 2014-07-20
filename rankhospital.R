## =====
## R. Peng Programming Course, Jul 2014
## Assignment 3, Part 3; Ranking hospitals by outcome in a state
## =====

library(datasets)

state <- "MD"
outcome <- "heart attack"
num <- "worst"

rankhospital <- function(state, outcome, num = "best") {
     ## Read outcome data
     
     mastFrame <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.string = "Not Available")
     
     ## in read above, need to strip out the 'Not Available' entries in the
     ## that need to contain only numeric data, else it will throw a warning
     ## message when calling the function
     
     ## convert columns from character to numeric for correct processing
     
     mastFrame[ , 11] <- as.numeric(mastFrame[ , 11], na.rm = TRUE)
     mastFrame[ , 17] <- as.numeric(mastFrame[ , 17], na.rm = TRUE)
     mastFrame[ , 23] <- as.numeric(mastFrame[ , 23], na.rm = TRUE)
     
     ## Check that state and outcome are valid
     
     valState <- state == state.abb  ## sum of vector will be one (1) if valid, else zero
     if (sum(valState) != 1) stop("invalid state")
     
     valOutcomeVec <- c("heart attack", "heart failure", "pneumonia")
     valOutcome <- outcome == valOutcomeVec
     
     if (sum(valOutcome) != 1) stop("invalid outcome")
     
     ## Return hospital name in that state with the given rank
     ## 30-day death rate
     
     ## extract subset for state
     
     subsetFrame <- subset(mastFrame, mastFrame[ ,7] == state)

     ## sort the subset frame on the requested outcome
     #valOutcome
     if (valOutcome[1]) {
          colNr <- 11
     } else {
          if (valOutcome[2]) {
               colNr <- 17
          } else colNr <- 23
          ## Have already determined that the 'outome' input parameter is valid,
          ## so if not one of the first two, then has to be the third column
     }
     
     ## sort the subset by outcome and hospital name
     
     #subsetOrdered <- subsetFrame[order(subsetFrame[colNr], subsetFrame[2], na.last = NA)]
     subsetOrdered <- subsetFrame[order(subsetFrame[colNr], subsetFrame[2]),]
     subsetOrdered <- na.omit(subsetOrdered)
     
     #subsetOrdered1 <- na.omit(subsetOrdered)
     #help("na.omit")
     #help("order")
     
     ## check for validity of 'num' (has to be between 1 and number of rows in
     ## the subset), and set row to the desired rank position
     #num <- 371
     if (num == "best") {
          row <- 1
     } else {
          if(num == "worst") {
               row <- nrow(subsetOrdered)  ## set the row to the last entry in df
          } else {
               if (num > nrow(subsetOrdered)) {
                    return ("NA")
               } else {
               row <- num   
               }
          
          }
     }
     #head(subsetOrdered)
     #tail(subsetOrdered1)
     return(subsetOrdered[row, 2])
}

