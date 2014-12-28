## best.R

best <- function(state, outcome) {
    ## Read outcome data
    dataTable <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
        
    ## Check that state is valid
    stateNames <- unique(dataTable$State)
    validState <- stateNames[stateNames==state]
    if(!length(validState) > 0) stop("invalid state")
    
    ## Check that outcome is valid
    outcomeArgs <- c("heart attack", "heart failure", "pneumonia")
    validOutcomeName <- outcomeArgs[outcomeArgs==outcome]
    if(!length(validOutcomeName) > 0) stop("invalid outcome")
    
    ## Get relevant data records
    outcomeStem <- "Hospital.30.Day.Death..Mortality..Rates.from."
    colNames <- c(paste(outcomeStem, "Heart.Attack", sep=""),
                  paste(outcomeStem, "Heart.Failure", sep=""),
                  paste(outcomeStem, "Pneumonia", sep=""))
    names(colNames) <- outcomeArgs  
    outcomeCol <- colNames[outcome]
    data <- dataTable[dataTable$State==state, c("Hospital.Name", outcomeCol)]
    dataNotNA <- data[data[outcomeCol] != "Not Available", c("Hospital.Name", outcomeCol)]
        
    ## Return hospital name in that state with lowest 30-day death rate
    orderedOutcomeIndex <- order(as.numeric(dataNotNA[,2]))
    orderedByOutcome <- dataNotNA[orderedOutcomeIndex,]
    bestOutcome <- orderedByOutcome[1,2]
    bestHospital <- orderedByOutcome[orderedByOutcome[,2] == bestOutcome, 1]
    if(length(bestHospital) > 1) {
        sortedByAlpha <- sort(bestHospital)
        bestHospital <- sortedByAlpha[1]
    }
    bestHospital
}



