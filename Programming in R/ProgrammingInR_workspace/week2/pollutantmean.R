pollutantmean <- function(directory, pollutant, id = 1:332) {
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files

	## 'pollutant' is a character vector of length 1 indicating
	## the name of the pollutant for which we will calculate the
	## mean; either "sulfate" or "nitrate".

	## 'id' is an integer vector indicating the monitor ID numbers
	## to be used

	## Return the mean of the pollutant across all monitors listed
	## in the 'id' vector (ignoring NA values)

	##-------------------------------------------------------##

    source("readFile.R")
    
    aggregate <- numeric()

	for(i in seq_along(id)) { 
	    file <- readFile(directory, id[i])
		isNotNA <- !is.na(file[,pollutant])
		notNA <- file[isNotNA, pollutant]
		aggregate <- c(aggregate, notNA)
	}
	aggregateMean <- mean(aggregate)
	round(aggregateMean, 3)
}

