complete <- function(directory, id = 1:332){
	
	numVec <- vector()

	for(num in id){
		idChar <- as.character(num)
		len <- nchar(idChar)

		#Formatting the id num
		if(len == 1) currID <- paste("00", idChar, sep="")
		else if(len == 2) currID <- paste("0", idChar, sep="")
		else currID <- idChar
		#reading in one file per loop
		file <- paste(directory, "/", currID,".csv", sep="")
		data <- read.csv(file)

		good <- complete.cases(data)
		compl <- data[good, ][,]
		numCompl <- nrow(compl)
		numVec <- append(numVec, numCompl)
	}
	data_frame <- data.frame("id" = id, "nobs" = numVec)
	print(data_frame)
	
}