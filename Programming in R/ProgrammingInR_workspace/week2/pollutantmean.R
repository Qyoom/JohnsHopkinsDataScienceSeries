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

	aggregatedData <- numeric()

	for(i in seq_along(id)) { 
		fileName <- if(id[i] < 10) {
			paste("00", id[i], ".csv", sep="")
		} else if(id[i] < 100) {
			paste("0", id[i], ".csv", sep="")
		} else {
			paste(id[i], ".csv", sep="")
		}

		filePath <- paste(directory, "/", fileName, sep="")

		file <- read.csv(filePath, header=TRUE)

		isNotNA <- !is.na(file[,pollutant])
		notNA <- file[isNotNA, pollutant]

		##print(paste("notNA length: ", length(notNA)))

		aggregatedData <- c(aggregatedData, notNA)
	}
	##print(paste("aggregatedData: ", length(aggregatedData)))
	aggregateMean <- mean(aggregatedData)
	##print(paste("aggregateMean: ", aggregateMean))
	format(round(aggregateMean, 3), nsmall = 3)
}

