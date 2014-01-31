data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
#head(data)

#coerce into numeric, as I imported as character and this field should be num
#suppresses warnings about introducing NAs
suppressWarnings(data[, 11] <- as.numeric(data[,11]))
suppressWarnings(data[, 17] <- as.numeric(data[,17]))
suppressWarnings(data[, 23] <- as.numeric(data[,23]))

#get the count of observations in each state
t <- table(data$State)
#Subset the data to only contain states with num observations >= 20
#Note: t is a table with one dimension (?) (Had some trouble subsetting)
data2 <- data[t[data$State] >= 20, ]

states <- sort(unique(data2$State))
#pre-allocate an empty list of length num states
vec <- vector()
for(state in states){
	m <- median(data2[data2$State == state, 11], na.rm=TRUE)
	vec <- c(vec, m)
}

dataframe <- data.frame(state = states, median = vec)
#Sort data frame by column
dataframe <- dataframe[order(dataframe[,2]), ]
#This way works too, if sorting by name. And multiple things to sort can
#be added //dd[with(dd, order(-z, b)), ]  OR dd[ order(-dd[,4], dd[,1]), ]
#dataframe <- dataframe[with(dataframe, order(median)), ]
#print(dataframe)

#Basic boxplot:
#death <- data2[, 11]
#state <- data2$State
#Sets the axis lables to be perpendicular to x-axis so that they will fit
#las = 0 = parallell (? uh, look it up)
#par(las=2)
# set xaxt="n" so that the boxplot won't print the x-axis 
# as I will make some changes to it using the axis() fnc
#boxplot(death ~ state, col=rainbow(5), main="Heart Attack 30-day Death Rate by State", 
#	ylab="30-day Death Rate", cex.axis=0.7)

#table(data2$State)
#max(data[,11], na.rm=TRUE)
#to be able to see several plots in the same window: 3 rows, 1 col
#par(mfrow = c(3, 1))
#makes histogram of the 11th column
#hist(data[, 11], main="Heart Attack", xlab="30-day Death Rate", xlim=range(0, 30))
#hist(data[, 17], main="Heart Failure", xlab="30-day Death Rate", xlim=range(0,30))
#hist(data[, 23], main="Pneumonia", xlab="30-day Death Rate", xlim=range(0, 30))


