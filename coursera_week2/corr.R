getmonitor <- function(id, directory, summarize = FALSE){
	idChar <- as.character(id)
	len <- nchar(idChar)

	if(len == 1) num <- paste("00", idChar, sep="")
	else if(len == 2) num <- paste("0", idChar, sep="")
	else num <- idChar

	file <- paste(directory, "/", num,".csv", sep="")
	data <- read.csv(file)
	if(summarize == TRUE){
		print(summary(data))
		data
	}
	else{
		data
	}	
}

complete <- function(directory, id = 1:332){
	
	numVec <- vector()

	for(num in id){
		data <- getmonitor(num, directory)

		good <- complete.cases(data)
		compl <- data[good, ][,]
		numCompl <- nrow(compl)
		numVec <- append(numVec, numCompl)
	}
	data_frame <- data.frame("id" = id, "nobs" = numVec)
	#print(data_frame)
	
}

corr <- function(directory, threshold = 0){
	data <- complete(directory)

	data2 <- data[data[,2] > threshold,]
	#print(data2)
	vec <- vector()
	if(nrow(data2) > 0){
		for(num in 1:nrow(data2)){
			id <- data2[[num, 1]]

			origData <- getmonitor(id, directory)

			corData <- cor(origData[,2], origData[,3], use="complete.obs")
			#print(corData)
			vec <- append(vec, corData)
		}	
	}
	
	vec
	#print(vec)
}
