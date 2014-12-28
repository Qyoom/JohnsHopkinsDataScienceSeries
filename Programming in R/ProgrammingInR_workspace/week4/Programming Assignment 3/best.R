## best.R

best <- function(state, outcome) {
    ## Read outcome data
    dataTable <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state is valid
    stateNames <- unique(dataTable$State)
    validState <- stateNames[stateNames==state]
    if(!length(validState) > 0) stop("invalid state")
    
    ## Check that outcome valid
    outcomeArgs <- c("heart attack", "heart failure", "pneumonia")
    validOutcomeName <- outcomeArgs[outcomeArgs==outcome]
    if(!length(validOutcomeName) > 0) stop("invalid outcome")
    
    ## Get relevant data records
    outcomeStem <- "Hospital.30.Day.Death..Mortality..Rates.from."
    colNames <- c(paste(outcomeStem, "Heart.Attack", sep=""),
                  paste(outcomeStem, "Heart.Failure", sep=""),
                  paste(outcomeStem, "Pneumonia", sep=""))
    names(colNames) <- outcomeArgs       
    data <- dataTable[dataTable$State==state, c("Hospital.Name", colNames[outcome])]
    dataNotNA <- data[data[colNames[outcome]] != "Not Available", c("Hospital.Name", colNames[outcome])]
    
    ## Return hospital name in that state with lowest 30-day death rate
    minOutcome <- min(as.numeric(dataNotNA[,2]))
    bestHospital <- dataNotNA[dataNotNA[,2]==minOutcome,]
    
    
    
}