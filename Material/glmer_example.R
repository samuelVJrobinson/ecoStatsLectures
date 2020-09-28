#Example of fitting process for generalized linear mixed effects models
#Written by Sam Robinson, winter 2020
library(lme4)
library(tidyverse)
library(effects)
theme_set(theme_classic())

rm(list=ls())

# General linear model (glm) ------------------------------------------------

#First, generate some data:
#Number of fish babies in some population are driven by temperature (temp) and feeding (low, med, high)

set.seed(123) #Make sure we get the same random numbers

N <- 150
temp <- runif(N,10,30) 
feeding <- rep(c('low','med','high'),length.out=N)
feeding <- factor(feeding,levels=c('low','med','high'))

slopeTemp <- -0.05 #For each extra degree, fish have less babies
int <- 2.4
slopeMed <- 0.5
slopeHigh <- 1
allCoefs <- c(int,slopeMed,slopeHigh,slopeTemp)
# noise <- 5 #Error term (SD of residuals)
mu <- model.matrix(~feeding+temp) %*% allCoefs #+ rnorm(N,0,noise) 
fishCount <- rpois(length(mu),exp(mu))

#Put everything together in a data frame
fishDat <- data.frame(fishCount=fishCount,temp=temp,feeding=feeding)

#Take a look at the data
ggplot(fishDat,aes(x=temp,y=fishCount,col=feeding))+
  geom_point()+geom_smooth(method='lm',formula=y~log(x),se=F)+
  scale_colour_manual(values=c('blue','purple','red'))+
  labs(x='Temperature',y='Number of offsping')

#Generalized linear models are just like linear models, but the predictions go through a "link function", and are derived using (usually) Poisson or Binomial distributions, which describe discrete counts or yes/no outcomes, respectively. Example:

#LINEAR MODEL
# y = Intercept + Slope * variable #This is the "linear" part
# mu = y # The link function for a linear model is the identity (i.e. times 1)
# size ~ Normal(mu,SD) #This is the likelihood function

# GENERALIZED LINEAR MODEL
# y = Intercept + Slope * variable #This is the "linear" part of a GLM
# mu = exp(y) # This is the link function
# counts ~ Poisson(mu) #This is the likelihood function. Notice there is no SD term for a Poisson distribution

#Fit a Poisson GLM

mod1 <- glm(fishCount ~ feeding + temp, family='poisson',data=fishDat)
#These are not "regular" residuals, but are "deviance" residuals. The same rules apply to interpreting them (normal distribution, equal variance), but if your data has many zeros, the graph will look ugly.
par(mfrow=c(2,1)); plot(mod1,which=c(1,2)); par(mfrow=c(1,1))
summary(mod1) #Estimates look good. Note that residual deviance is roughly equal to df, meaning that there is no evidence for overdispersion.

drop1(mod1,test='Chisq') #Both terms are highly significant.




