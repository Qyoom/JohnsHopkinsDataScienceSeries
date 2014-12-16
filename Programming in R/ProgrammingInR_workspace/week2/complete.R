complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    ## -------------------------------------------------------- ##
    
    source("readFile.R")
    
    aggregate <- matrix(, nrow = 0, ncol = 2, dimnames=list(NULL, c("id", "nobs")))
    
    for(i in seq_along(id)) { 
        file <- readFile(directory, id[i])
        isNotNA <- complete.cases(file)
        notNA <- file[isNotNA,]
        aggregate <- rbind(aggregate, c(id[i], nrow(notNA)))
    }
    
    as.data.frame(aggregate) ## return
}






