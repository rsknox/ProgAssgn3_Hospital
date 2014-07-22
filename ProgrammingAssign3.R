## =====
## R. Peng Programming Class, Jul 2014
## Programming Assignment 3:  Hospital Comparison analysis
## First commit to GitHub on 19 Jul with Parts 1 and 2 tested and functional; added a ## few comments below
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
     
     ## in read above, need to strip out the 'Not Available' entries in the
     ## that need to contain only numeric data, else it will throw a warning
     ## message when calling the function
     
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
          ## Have already determined that the 'outome' input parameter is valid,
          ## so if not one of the first two, then has to be the third column
     }
     
     ## grab the hospital name and return to the function
     
     subsetOrdered <- subsetFrame[order(subsetFrame[colNr], subsetFrame[2]),]
     
     return(subsetOrdered[1, 2])
}
##options(warn = -1)  ## surpress warning message In best("TX", "heart attack") : 
##                       NAs introduced by coercion
best("MD", "pneumonia")
##options(warn = 0)  ## enable warning messages
source(best)
source("http://d396qusza40orc.cloudfront.net/rprog%2Fscripts%2Fsubmitscript3.R")
submit()

source("rankhospital.R")
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)

num <- "best"
#typeof(num)

row <- NA
print("top")
# first check if num is a character or a number
if (typeof(num) != "character") {
  # check to see if num is a valid number
  if (num <= 0) {
    print("if 1")
    stop ("invalid num")
  } else {
    if (num > 0) {
      print("if 2")
      row <- num
} else {  # if it gets here, it is not a number
  if (num == "best") {
    print("if 3")
    row <- 1
  } else {
    if (num == "worst") {
      print("if 4")
      row <- -1  # set row negative for check later on
    } else {
      stop("invalid num")
    }
  }
}
}
}
row

# first check to see if num is a valid number
if (num <= 0) {
  print("if 1")
  stop ("invalid num")
} else {
  if (num > 0) {
    print("if 2")
    row <- num
  } else {  # it gets to this point num is not a number
    if (num == "best") {
      print("if 3")
      row <- 1
    } else {
      if (num == "worst") {
        print("if 4")
        row <- -1  # set row negative for check later on
      } else {
        print("final else")
        stop("invalid num")
      }
    }
  }
}
row

if (num == "best") {
  row <- 1
} else {
  if (num == "worst") {
    row = -1
  } else {
    if (num <= 0) {
      stop("invalid num")
    }
    row <- num
  }
}
row