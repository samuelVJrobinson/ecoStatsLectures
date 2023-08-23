#Solutions to Lecture 1 Problems

# Concentration data processing ----------------------------

setwd("~/ecoStatsLectures/01 Intro to R") #Set the working directory

testData <- read.csv('test_results.csv') #Reads in csv file from that working directory

# Inspect data
View(testData)
head(testData) #Shows first 6 entries
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
rowPos <- which(testData$LabMember=='Same') #Row 17 of Treatment is "Bee" instead of "B"
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
testData #No more NAs. 
#It is a good idea to clean out any NAs in your data before running it through models or plotting functions! They will (usually) still run, but this will avoid problems further down the road.

#Note: this seems like a lot of work for just changing a few problem numbers! However:
# - This is a very small dataset. Large datasets (>1000 rows) are harder to look through by eye, and Excel has trouble opening very large datasets (>10000 rows)
# - You now have a record of exactly how you cleaned up your data! You can now refer to this when writing papers (e.g. "All values <0 were excluded from further analysis"), or can give this script to other colleagues so that they can clean up their data in the same way. 

# Basic plots from concentration data

#Take a look at the data we read in
hist(testData$Concentration) #Histogram of Concentration
#Let's make a better-looking plot
hist(testData$Concentration,xlab='Concentration (mg/L)',main='',breaks=10)

#Say we wanted to have 2 figures on the same plot:
par(mfrow=c(1,2)) #2 rows, 1 column

#Make a boxplot of Treatment and Concentration 
plot(testData$Treatment,testData$Concentration,xlab='Treatments',ylab='Concentration (mg/L)')

#Make a boxplot of Lab Member and Concentration
plot(testData$LabMember,testData$Concentration,xlab='Lab Member',ylab='Concentration (mg/L)') 

#Reset plotting parameters
par(mfrow=c(1,1))

# Growth models -------------------------------

#Exponential growth

t <- 100 #100 time points
n0 <- 5 #Starting number of critters
r <- 0.1 #10% growth rate

n <- rep(NA,t) #Empty vector
n[1] <- n0 #Fill in the first number

for(i in 2:t){
  n[i] <- n[i-1] + n[i-1]*r
}

plot(1:t,n,xlab='Time',ylab='Number of Critters',main='Exponential growth',pch=19)

# Logistic growth

t <- 100 #100 time points
m0 <- 5 #Starting number of critters
r <- 0.1 #10% growth rate
k <- 100 #Carrying capacity

m <- rep(NA,t) #Empty vector
m[1] <- m0 #Fill in the first number

for(i in 2:t){
  m[i] <- m[i-1] + m[i-1]*r*(1-(m[i-1]/k))
}

plot(1:t,m,xlab='Time',ylab='Number of Critters',main='Logistic growth',pch=19)
abline(h=k,col='red')

#Predator-prey (Lotka-Volterra model)

t <- 100 #100 time points
pred0 <- 3 #Starting number of predators
prey0 <- 10 #Starting number of prey
r <- 0.1 # 10% growth rate for prey
a1 <- 0.02 #Effect of predators on prey growth
a2 <- 0.02 #Effect of prey on predator growth
d <- 0.2 #Death rate of predators

pred <- prey <- rep(NA,t) #Empty vectors
pred[1] <- pred0; prey[1] <- prey0 #Set first value

for(i in 2:t){
  prey[i] <- prey[i-1] + prey[i-1]*r - a1*prey[i-1]*pred[i-1]
  pred[i] <- pred[i-1] + a2*prey[i-1]*pred[i-1] - pred[i-1]*d
  
  #If either prey or predator count is negative, set to 0 (extinction)
  prey[i] <- pmax(0,prey[i]); pred[i] <- pmax(0,pred[i]) 
}

par(mfrow=c(2,1)) #Split plot into 2 windows

#Time series plot
plot(1:t,prey,pch=19,ylim=range(c(prey,pred)),xlab='Time',ylab='Number of Critters')
points(1:t,pred,col='red',pch=19)
legend('topright',c('Prey','Pred'),fill=c('black','red'))

#Phase plane diagram, showing dynamic spirals
plot(pred,prey,pch=19,xlab='Predators',ylab='Prey',main='Phase-plane')
text(pred[1]*1.1,prey[1],'Start',col='red')

