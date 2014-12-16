readFile <- function(directory, id) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## ------------------------------------------------------- ##
    
    fileName <- if(id < 10) {
        paste("00", id, ".csv", sep="")
    } else if(id < 100) {
        paste("0", id, ".csv", sep="")
    } else {
        paste(id, ".csv", sep="")
    }
    
    filePath <- paste(directory, "/", fileName, sep="")
    file <- read.csv(filePath, header=TRUE)
    file
}