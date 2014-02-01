agecount <- function(age = NULL){
	if(age == "") stop("Please enter an age")

	homicides <- readLines("homicides.txt")

	# Example excerpt from file that I wanna match:
	# <<dd>black male, 26 years old</dd>
	# NEI! Var to forskjellige: "Age: 45 years old"-format også
	#r <- regexec("<dd>[a-zA-Z]+ [a-zA-Z]+, ([0-9]+) years old</dd>", homicides)
	r <- regexec("([0-9]+) years old", homicides)
	#applies the regular expression on homicides and returns 
	#the matches in a list containing the strings (two, since regexec
	# gave us back one string containing the whole match, and one with
	# only the parenthesized sub-expression)
	m <- regmatches(homicides, r) 

	# iterates through every object in m and extracts the
	# second element of each object, which will be the age
	ages <- sapply(m, function(x) x[2])
	#print(table(ages))
	#Sums up the number of cases where the age is the same as the input
	num <- sum(ages[] == age, na.rm=TRUE)
	#print(num)
}