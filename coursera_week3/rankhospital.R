rankhospital <- function(state, outcome, num="best"){
	data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
	diseaseNum <- 0
	# First checks if a valid state name is given
	if(nrow(data[data$State == state, ]) == 0) stop("invalid state")
	#coerce death rate data to numeric. If not valid outcome, stop program
	if(outcome == "heart attack"){
		suppressWarnings(data[, 11] <- as.numeric(data[, 11]))
		diseaseNum <- 11
	} 
	else if(outcome == "heart failure"){
		suppressWarnings(data[, 17] <- as.numeric(data[,17]))
		diseaseNum <- 17
	} 
	else if(outcome =="pneumonia"){
		suppressWarnings(data[, 23] <- as.numeric(data[,23]))
		diseaseNum <- 23
	} 
	else stop("invalid outcome")

	#Data containing only the rows relevant for the given state:
	state_data <- data[data$State == state, ]

	# finds the hospitals with lowest mortality rate for the given disease
	# using order (sort is for vectors, order for data frames) to sort 
	# them in ascending order 
	# column 2 is name of hospital
	hospitals <- state_data[order(state_data[, diseaseNum], state_data[,2]),]

	if(num=="best") return(hospitals[1,2])
	else if(num=="worst"){
		hospitals <- hospitals[order(-hospitals[, diseaseNum], hospitals[,2]),]
		return(hospitals[1,2])
	} 
	else return(hospitals[num, 2])

}