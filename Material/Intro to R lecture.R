# BASIC PRINCIPLES OF R ----------------

#1) Setup of RStudio

#SCRIPT is a text editor that lets you create sets of commands to send to R.
#CONSOLE lets you enter commands line-by-line.
#ENVIRONMENT is the collection of all the stuff that R can see and use
#FILES, PLOTS, PACKAGES, and HELP are displayed in the 4th window

#Shortcuts: Ctrl/Cmd+1 switches to script, Ctrl/Cmd+2 switches to console

#First off: Go to TOOLS > GLOBAL OPTIONS > CODE
#Check 'Soft-wrap R source files'

#2) Basic R syntax

# Hashtags/pound signs represent COMMENTS - these are bits of text that R will not read
# Lines are run using Ctrl/Cmd+Return
# Assignment operator <- . This tells R to write object to memory. You can use whatever names you want (with a couple exceptions)
# ? - Help on commands
?mean #Get help on the MEAN command
?plot #Get help on the PLOT command

# R is case sensitive: 'Cats' is different from 'cats'
# Whitespace is not interpreted in R. If a function is missing the end, it will keep looking for it (unless you press Escape).

#Functions and Objects - In R, everything is either a function or an object

#OBJECTS are things that you store in memory.
#FUNCTIONS do things to objects. They take objects as ARGUMENTS (input) and return objects as output.

#Example:
# Basic math
a <- 1 #Create new object called 'a', set value to 1. 
b <- 2
c <- a + b #Add a and b together in a new object called 'c'
c #This doesn't change anything, but shows the value of 'c'

(b+c)/2 #Mean of b and c - remember order of operations! (BEDMAS)

#TASK: Ask the people at your table how many years they have been in University. 
# If there are less than 3 people, go make some friends!
# Using basic arithmetic, make a new object called 'yearMean' that is the mean (average) of everyone's time in university



#3) Classes of data

#There are many classes of data, but we'll learn only a few important ones:
#LOGICAL - TRUE/FALSE (Boolean value)
#NUMERIC - Integers or Floating Point (numbers with a decimal place)
#CHARACTER - Character strings, using alphanumerics.
#FACTOR - Special type of character string, used for statistical models. Example: 'Control', 'Treatment A', 'Treatment B'.
# To find out what class an object is in, use the CLASS command

#Logicals - TRUE/FALSE
logi <- T
class(logi)

#Numerics - integers (whole numbers) or numeric/double floating point (has a decimal point)
int <- as.integer(3)
dbl <- 3.0 #This is stored differently than the above

class(int)
class(dbl)

newnum <- int + dbl #Adds 2 numbers together
newnum # 3 + 3.0 = 6
class(newnum) #Int has been RECAST to a double. R does this automatically.

#Characters - collections of characters. Must be quoted with ' or "
string <- 'abc'
string2 <- 'def'
class(string)

string+string2 #This throws an ERROR. Why doesn't this work?

#To join strings together, use PASTE function 
paste(string,string2,sep='-') #What are these arguments doing?

#TASK: Get everyone's name in your group, and make a single string called 'group' that is separated by commas.
#Hint: Copy the command from above and modify it
group <- paste('Anne','Bubba',string,'Doug',sep=', ')
group



#4) Data structures

#Vectors - joins similar objects together into a row. (The single values that you had above are called a SCALAR)

#Joins strings into a vector using c() command - stands for concatenate
stringArr <- c(string,string2) 
stringArr
length(stringArr) #Array is 2 long

#This can be done with any basic data type

#Numeric array
numArr <- c(1,2,3)
numArr2 <- c(4,5,6)
numArr+numArr2 #What did this just do?

#Logical array
logiArr <- c(T,T,F)
logiArr2 <- c(F,T,F)
logiArr+logiArr2 #What did this just do?

#TASK: Using c(), make a vector called 'years', containing the years that your table mates have been in University.
#Use MEAN to make a new object called 'yearMean' that is the mean of everyone's time.
#Use SD to to make a new object called 'yearSD' that is the standard deviation of everyone's time
#Use c() to make a new object called 'yearStats' that contains yearMean and yearSD

years <- c(2,3,1.1,4,8)
yearMean <- mean(years)
yearSD <- sd(years)
yearStats <- c(yearMean,yearSD)


#Indexing - get specific parts from an array, using square brackets []

numArr[1] #Get the 1st value from numArr
numArr[c(2,3)] #Get the 2nd and 3rd values
numArr[logiArr] #What does this do? 

#Matrix - vector with more than 1 dimension
# [] indexing alway goes [row, column]
numMat <- matrix(1:9,nrow=3) #Create a 3x3 matrix
numMat
#To get specific parts from a matrix, you need to specify 2 coordinates (row and column).
numMat[2,3] #2nd row, 3rd column
numMat[,3] #All rows, 3rd column

#TASK: Add the 2nd column of the numMat array to numArr2
numMat[,2]+numArr2

#5) Functions
#Some common lower-level math functions in R:
?Arithmetic #All basic arithmetic
#mean
#sum
#length - length of an array/number of matrix elements
#dim - length of each dimension of a matrix
#sqrt - square root
#log - natural logarithm
#exp - e to the x
#abs - absolute value

#ls - objects in your environment
#rm - removes objects from environment
#getwd / setwd - get/set working directory

#There are thousands of premade functions, but you can also make your own.

#Name of the new function goes on the left-hand side of the <-
#The word 'function' is a reserved word (you aren't allowed to name things 'function' in R).
#Arguments for the function go in parentheses.
#Stuff that the function does goes below in curly braces {}
#'return' (another reserved word) outputs an object from the function.

#Example: 
functionName <- function(x) { #x is the input to the function (first argument)
  x <- sum(x)-1 #Adds x, subtracts 1 from x,
	return(x) #returns x (output from the function)
}

#What are these doing?
functionName(3) 
functionName(numArr) 
functionName(numMat) 

#TASK: make a standard error function named SE using SD, LENGTH, and SQRT. 
#Use SE to make a new object called 'yearSE' that is the standard error of the number of years in university.
#Standard error = standard deviation / square root of number of samples
#To get started, copy the function from above, and then change it below.

SE <- function(x) { 
  x <- sd(x)/sqrt(length(x))
  return(x) #returns x
}
SE(1:1000)


# LOOPING and IF statments -----------------------------------------------------------------

#FOR loops are an important part of any programming language. In R, they look like:
# for(index in vector) {
#   do something using index
# }
#An index is the value that the FOR loop takes on every time it runs the loop. It takes on all values of the vector you provide.
#Useful for going through data to find errors, or for running simulations.

#Example:
for(i in 1:10){ #Goes through a vector of values (1 through 10)
  print(paste('This is iteration',i,sep=' ')) #Every time it goes through the loop, prints "This is iteration i"
}

#Good for getting R to do repetitive things. e.g:
for(i in 99:1){
  print(paste(i,'bottles of beer on the wall.',sep=' '))
  print(paste(i,'bottles of beer.',sep=' '))
  print('Take one down, pass it around.')
  print(paste(i-1,'bottles of beer on the wall.',sep=' '))
  print('-------------------') 
}

#What about when i is 1? "One bottles of beer" doesn't make sense. We need an IF statment (also see ifelse(), which works with arrays)
#IF statements look like:
#if(condition is true){ 
#   do something 
# } else { 
#   do another thing
# }

#Example:
a <- 3
b <- 1

if(a>b){
  print('A is bigger than B')
} else {
  print('A is not bigger than B')
}

#You can also string together IF ELSE statements to capture many conditions
if(a>b){
  print('A is bigger than B')
} else if(a==b){ #== means 'equal to'
  print('A is the same size as B')
} else{
  print('A is smaller than B')
}

#Other logical statements include:
?base::Logic # List of logical operators

#TASK: Using an IF statment, modify the FOR loop above so that the last verse is correct (there are many ways to do this).
#Hint: Try making a character string ("bottle"/"bottles") that is assigned inside an IF statement, then pasted into the print command along with 'i'.














# GET DATA INTO R ----------------

#In order to manage data we've loaded into R, we need to know how DATA FRAMES work:

# Data frames - special data structure that looks similar to spreadsheets in Excel
# Each column is the same class, and each column must be the same length

# Example:
#Makes data frame with 3 columns
df <- data.frame(firstCol=c(1,2,3,4),secondCol=c('a','b','c','d'),thirdCol=c(T,T,F,T))
df #Inspect dataframe

str(df) #Structure of dataframe. What is the class of each column?

#In order to access columns in a dataframe, use the $ operator, or [] operators

# $ operator - name of the column
df$firstCol
df$secondCol

# [] square bracket operator - similar to matrices
df[1,1] #First row, first column
df[2,1] #Second row, first column
df[2,] #Second row, all columns

#If you use the $ operator, it turns it into a vector:
df$firstCol[1] #First index in first column.

#Now we can actually load in the data files.

# Load in csv file - these are better for working with than .txt files.
# setwd('../Desktop') #Sets working directory to my Desktop. If you can't remember where something is, use file.choose().
testData <- read.csv("C:\\Users\\Samuel\\Desktop\\test_results.csv") #Reads in csv file as data frame.

# Inspect data
View(testData)
head(testData) #Shows first 6 entries - TAIL also works
str(testData) #Structure of dataframe

# Given what we've seen, what's wrong with this data?
# How do we fix this? 
# 1) Fix in Excel (easy for small datasets)
# 2) Fix in R (harder, but scales better for large datasets) 

# Change column names
names(testData) #R replaces spaces (or other complex characters) with periods
names(testData)[3] #Third column name
names(testData)[3] <- 'LabMember' #Sets third column name to new name
names(testData)[3] #New third column name

# Get rid of unnecessary columns

#Time of day column doesn't really add anything, so let's get rid of it
testData$Time.of.Day <- NULL

#Look at Concentration data
testData$Concentration #30 looks weird. Decimal place error?

rowPos <- which.max(testData$Concentration) #Row position of maximum value
rowPos #Maximum value is in row 11
testData$Concentration[rowPos] <- testData$Concentration[rowPos]/10 #Divides concentration in row 11 by 10
testData$Concentration #30 is now 3.0

#How do we fix the "Bee" in Treatment?
rowPos <- which(testData$Treatment=='Bee') #Row 17 of Treatment is "Bee" instead of "B"
testData$Treatment[rowPos] <- 'B' #Overwrite "Bee" with "B"
testData$Treatment #However, we still have "Bee" as a level in the factor. R assigns factor levels when the data is loaded in, but doesn't keep track of them after that.
testData$Treatment <- droplevels(testData$Treatment) #DROPLEVELS gets rid of unused factor levels

#TASK: Change level from "Same" to "Sam"
rowPos <- which(testData$LabMember=='Same') 
testData$LabMember[rowPos] <- 'Sam'
testData$LabMember <- droplevels(testData$LabMember) 

#R orders factors alphabetically, but what if we wanted "Control" to be first? 
levels(testData$Treatment) #Levels: A, B, Control
testData$Treatment <- factor(testData$Treatment,levels=c('Control','A','B')) #Manually assign levels of treatment using FACTOR
levels(testData$Treatment) #Levels: Control, A, B

#What about the NA value? NA represents a missing value that not recorded for some reason. 
noNAs <- complete.cases(testData) #Rows that are complete (no NAs)
noNAs 
#SUBSET data - removes rows from dataframe that match logical expression
testData <- subset(testData,noNAs)
testData #No more NAs

# BASIC PLOTTING -------------------

#Take a look at the data we read in
hist(testData$Concentration)
#Let's make a better-looking plot
hist(testData$Concentration,xlab='Concentration (mg/L)',main='',breaks=10)

#Basic boxplot - plot(xdata, ydata)
plot(testData$Treatment,testData$Concentration,xlab='Treatments',ylab='Concentration (mg/L)')

#Basic scatterplot 
BOD #Biochemical oxygen demand dataset (comes installed with R)
plot(BOD$Time,BOD$demand)

#Let's make a better-looking plot
plot(BOD$Time,BOD$demand,xlab='Time',ylab='Demand',pch=19)

#Say we needed to make 2 plots in one window:
par(mfrow=c(2,1)) #Sets up plotting area to have 2 rows, 1 column
plot(testData$Treatment,testData$Concentration,xlab='Treatments',ylab='Concentration (mg/L)') #Plot first figure
plot(BOD$Time,BOD$demand,xlab='Time',ylab='Demand',pch=19) #Plot second figure
par(mfrow=c(1,1)) #Change it back to single window.

#TASK: make a boxplot of Lab Member and Concentration (to see if Sam and Will got different concentrations) 
#NOTE: GOT TO HERE IN ABOUT 1.5 HRS, WITHOUT DOING SECTION ON LOOPS AND IF STATEMENTS


#Oscar will be showing you other plotting methods using the ggplot2 package.

# BASIC STATISTICS ----------------

#Common statistical tests can all be done in R
?t.test #Student's T test
?chisq.test #Chi-square test
?fisher.test #Fisher's exact test
#Summary statistics from data
?table #Contingency tables (for count/categorical data)
?aggregate #Summary statistics using the function your choice (mean, sum, etc.)

#Fitting an ANOVA (ANalysis Of VAriance) model

#Question: do treatments have different concentrations?
#AOV fits an ANOVA model of Concentration by Treatment.
#Uses a formula as the first argument. '~' in a formula stands for for 'dependent on'.
mod1 <- aov(Concentration~Treatment,data=testData)

#Is the model appropriate? Check residuals:
par(mfrow=c(2,2)) #Changes plotting window to 2x2 grid
plot(mod1) #Plots residuals from model. Looks OK.

summary(mod1) #Interpret results of model
#Results: Treatment has a strong effect on Concentration.

#Follow-up question: 
#Given that we found a difference, which treatment groups differ from each other?
#Requires a Post-Hoc test (Tukey's Honestly Significant Difference)
TukeyHSD(mod1) #Performs a Tukey test. B isn't different from Control.

#Plots of results
par(mfrow=c(1,1)) #Changes plotting window back to 1x1
plot(testData$Treatment,testData$Concentration,xlab='Treatment',ylab='Concentration (mg/L)')
text(x=c(1,2,3)+0.4,y=c(3.5,7,4),labels=c('A','B','A'),col='red') #Displays grouping letters


#Fitting a linear regression model, once again using the BOD data
mod2 <- lm(demand~Time,data=BOD) #Fits linear model of Demand on Time

par(mfrow=c(2,2)) #Changes plotting window to 2x2 grid
plot(mod2) #Plots residuals from model. 

#Doesn't look good, so let's fit a polynomial model
mod2a <- lm(demand~poly(Time,2),data=BOD) #Fits 2nd-order polynomial model of Demand on Time
plot(mod2a) #Plots residuals from model. Looks better

summary(mod2a) #Interpret results of model
#Results: marginal effect of Time on demand, but this requires a nonlinear model, so let's keep this and move on

#Plots model results (red line) on top of data
par(mfrow=c(1,1)) #Changes plotting window back to 1x1
plot(BOD$Time,BOD$demand,xlab='Time',ylab='Oxygen Demand',pch=19) #Plots data points
predBOD <- predict(mod2a,interval='confidence') #Gets all predicted values from the model
lines(BOD$Time,predBOD[,1],col='red') #Adds model results in red
lines(BOD$Time,predBOD[,2],col='red',lty='dashed') #Adds lower confidence interval
lines(BOD$Time,predBOD[,3],col='red',lty='dashed') #Adds upper confidence interval

# CHALLENGE SECTION -------------------
#Challenge 1:
#Using 'iris' dataset, show us how Sepal.Length differs between Iris species. 
#Make a graph and perform an ANOVA model.
#Hint: look at the concentration example above.

data(iris)
head(iris)













#Challenge 2:
#Using 'mtcars' dataset, show us how mpg varies with hp. 
#Make a graph and perform a linear regression. 
#Hint: you'll need to re-order predictions once you've fit the model, or feed in a new dataframe of values into predict.
#Hint: you'll need to log-transform the x-variable. This can be done inside the formula. Example:  y~log(x)
#Useful functions: order()

data(mtcars)
head(mtcars)
















#Challenge 3 (hard):
#Using a FOR loop, model a population of rabbits after getting onto a deserted island with no predators.
#Show how the population changes over a 20-year period.
#Use the logistic growth equation:
#P_[t+1] = P_t + rmax*P_t*(1-P_t/K) #Discrete form
#i.e. Rabbits in Year 2 = Rabbits in Year 1 + Max Reproduction Rate x Rabbits in Year 1 x (1 - Rabbits in Year 1/Carrying capacity)

#Assumptions:
#Starting population of 2 (2 rabbits)
#Island can support 100 rabbits (K=100)
#If food is plentiful, each rabbit can replace itself and add 1 more to the population each year (rmax=1)
#Now do the same thing, but with highly reproductive rabbits that can replace themselves and add 2.5 to the population (rmax=2.5). What happens?

#Hint: make a dataframe to store the number of rabbits in each year, then use the FOR loop to go through all the rows and make new values based on the last row. Look at the FOR loop examples above to see how they work. This model can be written and displayed using about 12 lines of code. Bonus points if you use a function!

































#End of Section