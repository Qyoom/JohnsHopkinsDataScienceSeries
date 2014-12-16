corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    ## ------------------------------------------------------- ##
    
    source("readFile.R")
    
    aggregate <- numeric()
    
    id <- 1:332
    for(i in seq_along(id)) { 
        file <- readFile(directory, id[i])
        isNotNA <- complete.cases(file)
        notNA <- file[isNotNA,]
        if(nrow(notNA) > threshold) {
            thisCorr <- cor(as.vector(notNA[,2]), as.vector(notNA[,3]))
            aggregate <- c(aggregate, thisCorr)
        }
    }

    aggregate ## return
}




