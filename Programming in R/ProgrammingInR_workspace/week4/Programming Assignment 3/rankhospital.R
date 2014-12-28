rankhospital <- function(state, outcome, num = "best") {
    targetHospital <- "NA"
    
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
    
    ## Return hospital name in that state with the given rank for 30-day death rate
    
    ## Sort records by outcome
    orderedOutcomeIndex <- order(as.numeric(dataNotNA[,2]), dataNotNA[,1])
    orderedByOutcome <- dataNotNA[orderedOutcomeIndex,]
    
    ## Determine and validate num argument
    # Select hospital by rank
    if(class(num) == "numeric") {
        if(length(orderedByOutcome) <= num && nrow(orderedByOutcome) > 0) {
            targetHospital <- orderedByOutcome[num, 1]
        }
    }
    # Select best or worst hospital
    else if(class(num) == "character") {
        if(num == "best") {
            targetHospital <- orderedByOutcome[1, 1]
        }
        else if(num == "worst") {
            targetHospital <- orderedByOutcome[nrow(orderedByOutcome), 1]
        }
    }
    targetHospital
}





