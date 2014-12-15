add2 <- function(x, y) {
	x + y
}

above10 <- function(x) {
	use <- x > 10
	x[use]
}

above10v2 <- function(x) {
	x[x > 10]
}

above <- function(x, n = 0) {
	x[x > n]
}

meanCols <- function(matx, removeNA = TRUE) {
	numCols <- ncol(matx)
	means <- numeric(numCols)
	for(i in 1:numCols) {
		means[i] = mean(matx[,i], na.rm = removeNA)
	}
	means
}

make.power <- function(p) {
	function(x) {
		x^p
	}
}

f <- function(x) {
        g <- function(y) {
                y + z
        }
        z <- 4
        x + g(x)
}














