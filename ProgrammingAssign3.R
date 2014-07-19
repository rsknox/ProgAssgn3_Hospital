## =====
## R. Peng Programming Class, Jul 2014
## Programming Assignment 3:  Hospital Comparison analysis
## =====

## =====
## Part 1: Plot the 30-day mortality rates for heart attack
## =====
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])

## =====
## Part 2: Finding the best hospital in a state
## =====
library(datasets)
#str(state.abb)
best <- function(state, outcome) {
     #state <- "MD"
     #outcome <- "pneumonia"
     
     ## Read outcome data
     
     mastFrame <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.string = "Not Available")
     
     ## convert columns from character to numeric for correct processing

     mastFrame[ , 11] <- as.numeric(mastFrame[ , 11], na.rm = TRUE)
     mastFrame[ , 17] <- as.numeric(mastFrame[ , 17])
     mastFrame[ , 23] <- as.numeric(mastFrame[ , 23])
     
     ## Check that state and outcome are valid
     valState <- state == state.abb  ## sum of vector will be one (1) if valid, else zero
     if (sum(valState) != 1) stop("invalid state")
     
     valOutcomeVec <- c("heart attack", "heart failure", "pneumonia")
     valOutcome <- outcome == valOutcomeVec

     if (sum(valOutcome) != 1) stop("invalid outcome")

          ## It looks like:
               ##        "heart attack" is outcome[, 11],
               ##        "heart failure" is outcome[, 17],
               ##        "pneumonia" is outcome[, 23]
               ##        Hospital.Name is outcome[ , 2]
     
     ## Return hospital name in that state with lowest 30-day death
     ## rate
     
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
     }

     subsetOrdered <- subsetFrame[order(subsetFrame[colNr], subsetFrame[2]),]
     
     return(subsetOrdered[1, 2])
}
options(warn = -1)  ## surpress warning message In best("TX", "heart attack") : 
##                       NAs introduced by coercion
best("MD", "pneumonia")
options(warn = 0)  ## enable warning messages
