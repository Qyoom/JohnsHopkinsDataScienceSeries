rankall <- function(outcome, num = "best") {
    ## Read outcome data
    dataTable <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that outcome is valid
    outcomeArgs <- c("heart attack", "heart failure", "pneumonia")
    validOutcomeName <- outcomeArgs[outcomeArgs==outcome]
    if(!length(validOutcomeName) > 0) stop("invalid outcome")
    
    ## Prepare state vector
    stateNames <- unique(dataTable$State)
    
    ## Get relevant data records
    outcomeStem <- "Hospital.30.Day.Death..Mortality..Rates.from."
    colNames <- c(paste(outcomeStem, "Heart.Attack", sep=""),
                  paste(outcomeStem, "Heart.Failure", sep=""),
                  paste(outcomeStem, "Pneumonia", sep=""))
    names(colNames) <- outcomeArgs  
    outcomeCol <- colNames[outcome]
    
}