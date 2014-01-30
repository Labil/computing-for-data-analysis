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