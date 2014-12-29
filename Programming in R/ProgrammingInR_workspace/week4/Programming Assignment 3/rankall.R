rankall <- function(outcome, num = "best") {
    targetsAllStates <- matrix(, nrow = 0, ncol = 2)
    
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
    
    ## For each state, produce a vector with the 
    ## hospital names and the (abbreviated) state name
    for(state in stateNames) {
        targetHospital <- "NA"
        data <- dataTable[dataTable$State==state, c("Hospital.Name", outcomeCol)]
        dataNotNA <- data[data[outcomeCol] != "Not Available", c("Hospital.Name", outcomeCol)]
        
        ## Sort records by outcome
        orderedOutcomeIndex <- order(as.numeric(dataNotNA[,2]), dataNotNA[,1])
        orderedByOutcome <- dataNotNA[orderedOutcomeIndex,]
        
        ## Determine and validate num argument
        # Select hospital by rank
        if(class(num) == "numeric") {
            if(nrow(orderedByOutcome) <= num && nrow(orderedByOutcome) > 0) {
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
        
        ## Assemble matrix
        targetsAllStates <- rbind(targetsAllStates, c(targetHospital, state))
    } # END for(state in stateNames)
    
    ## Convert matrix to data.frame
    df <- as.data.frame(targetsAllStates)
    colnames(df) <- c("hospital", "state")
    df # return
}



