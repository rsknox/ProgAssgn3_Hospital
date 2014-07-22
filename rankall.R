## =====
## R. Peng Programming Class, Jul 2014
## Programming Assignment 4:   Ranking hospitals in all states
## a function calledrankallthat takes two arguments: an outcome name (outcome) 
## and a hospital ranking (num).  The function reads the
## outcome-of-care-measures.csv fille and returns a 2-column data frame
## containing the hospital in each state that has the ranking speciied in num.
## =====

#@outcome <-"heart attack"
#@outcome <- "pneumonia"
#@num <- "worst"
#@num <- 1
#@rm(mastFrame)
#@rm(return.data)
#@rm(subsetFrame)
#@rm(subsetOrdered1)

rankall <- function(outcome, num = "best") {
  
  library(datasets)  # to pick up state abbreviations
  # initialze the return data frame
  #@return.data <- data.frame(hospital = character(50), state = character(50))
  return.data <- data.frame(hospital = NA, state = NA)
  #@str(return.data)
  ## Read outcome data
  
  mastFrame <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.string = c("Not Available", "<Not Available>"))
  #@str(mastFrame)
  ## in read above, need to strip out the 'Not Available' entries in the
  ## that need to contain only numeric data, else it will throw a warning
  ## message when calling the function
  
  ## convert columns from character to numeric for correct processing
  
  mastFrame[ , 11] <- as.numeric(mastFrame[ , 11], na.rm = TRUE)
  mastFrame[ , 17] <- as.numeric(mastFrame[ , 17], na.rm = TRUE)
  mastFrame[ , 23] <- as.numeric(mastFrame[ , 23], na.rm = TRUE)
  
  ## Check that outcome parameter is valid
  
  valOutcomeVec <- c("heart attack", "heart failure", "pneumonia")
  valOutcome <- outcome == valOutcomeVec
  
  if (sum(valOutcome) != 1) stop("invalid outcome")
  
  ## setup for the sort on proper column in the loop below
  
  if (valOutcome[1]) {
    colNr <- 11
  } else {
    if (valOutcome[2]) {
      colNr <- 17
    } else colNr <- 23
    ## Have already determined that the 'outome' input parameter is valid,
    ## so if not one of the first two, then has to be the third column
  }
  
  ## Check that num parameter is valid
  
  if (typeof(num) != "character") {
    ## then it has to be a number, check to see if neg
    if (num <= 0) {
      stop("invalid num")
    } else {
      row <- num
    }
  } else {   ## if gets to this point then not a numeric input
    ## has to be "best" or "worst"
    if (num == "best") {
      row <- 1
    } else {
      if (num == "worst") {
        row <- -1  ## then set to neg flag to check later on
      } else {
        stop("invalid num")
      }
    }
  }
  
  ## For each state, find the hospital of the given rank
  state.abbrev <- c(state.abb, "DC")
  state.abbrev <- sort(state.abbrev)
  
  for (i in 1:51) {
    #@i <- 51
    stat <- state.abbrev[i]

  ## extract subset for state
  subsetFrame <- subset(mastFrame, mastFrame[ ,7] == stat)
  #@row <- -1
  ## sort the subset frame on the requested outcome and hosptial name
  subsetOrdered <- subsetFrame[order(subsetFrame[colNr], subsetFrame[2]),]
  #@head(subsetOrdered, 12)
  #@help("break")
  #@tail(subsetOrdered, 10)
  ## handle the case for "worst" rank
  if (row == -1) {
    ## remove any NAs at the bottom of the sorted list
    leng <- nrow(subsetOrdered)
    for (j in 1:leng) {
      n <- leng + 1 - j
      if (is.na(subsetOrdered[n, colNr])) {
        
      } else {
        break
      }
    }
    row <- n

    #@subsetOrdered <- na.omit(subsetOrdered[, 23])
    #@row <- nrow(subsetOrdered)  ## set row pointer to the last row in df
  }
  
  ## move the hostipal name and state into return df
  #@i <- 1
  #@t1 <- as.character(subsetOrdered[row, 2)
  #@t1
  #@class(t1)
  #@t2 <- subsetOrdered[row, 7]
  #@str(t2)
  #@return.data[1, 2] <- t1
  return.data[i, 1] <- as.character(subsetOrdered[row, 2])
  return.data[i, 2] <- as.character(state.abbrev[i])
  #@tail(return.data)
  #@str(return.data)
  #@class(return.data$hospital)
  #@head(return.data$hospital)
  }
  #@head(return.data, 50)
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  return(return.data)
}