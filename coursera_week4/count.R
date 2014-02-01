count <- function(cause = NULL){
	# Can not test for if(cause == NULL), this results in error, no matter
	# if cause is specified. Unsure why
	if(cause == "") stop("No cause specified.")

	causes <- c("asphyxiation", "blunt force", "other", "shooting", "stabbing", "unknown")
	if(!(cause %in% causes)) stop("Please choose a valid cause of death") 
	#reads any type of text file
	homicides <- readLines("homicides.txt")
	
	#Better to use the paste function with ignore.case than check with ifZ
	num <- length(grep(paste("cause: ", cause, "", sep=""), homicides, ignore.case = TRUE))
	
	return(num)
}




	#first try
	#if(cause == "asphyxiation")
	#	num <- length(grep("[Cc]ause: [Aa]sphyxiation", homicides))
	#else if(cause == "blunt force")
	#	num <- length(grep("[Cc]ause: [Bb]lunt [Ff]orce", homicides))
	#else if(cause == "other")
	#	num <- length(grep("[Cc]ause: [Oo]ther", homicides))
	#else if(cause == "shooting")
#		num <- length(grep("[Cc]ause: [Ss]hooting", homicides))
#	else if(cause == "stabbing")
#		num <- length(grep("[Cc]ause: [Ss]tabbing", homicides))
#	else if(cause == "unknown")
#		num <- length(grep("[Cc]ause: [Uu]nknown", homicides))
#	else
#		stop("Please choose a valid cause of death")