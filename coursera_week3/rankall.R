rankall <- function(outcome, num="best"){
	data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
	diseaseNum <- 0
	vec <- vector()
	#unique returns list of all the state names in data. No duplicates
	states <- sort(unique(data$State))

	for(state in states){
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

		hospitals <- state_data[order(state_data[, diseaseNum], state_data[,2]),]

		if(num=="best") vec <- c(vec, hospitals[1,2])
		else if(num=="worst"){
			hospitals <- hospitals[order(-hospitals[, diseaseNum], hospitals[,2]),]
			vec <- c(vec, hospitals[1,2])
		} 
		else vec <- c(vec, hospitals[num, 2])

	}
	dataframe <- data.frame(hospital = vec, state = states)
}