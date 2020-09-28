#Example of fitting process for linear mixed effects models
#Written by Sam Robinson, winter 2020
library(lme4)
library(tidyverse)
library(effects)
theme_set(theme_classic())

rm(list=ls())
# Simple linear model (lm) ------------------------------------------------

#First, generate some data:
#Fish sizes in some population are driven by temperature (temp) and feeding (low, med, high)

set.seed(123) #Make sure we get the same random numbers

N <- 90
temp <- runif(N,10,30) 
feeding <- rep(c('low','med','high'),length.out=N)
feeding <- factor(feeding,levels=c('low','med','high'))

slopeTemp <- -0.5 #For each extra degree, fish are 0.5cm smaller
int <- 30
slopeMed <- 10
slopeHigh <- 20
allCoefs <- c(int,slopeMed,slopeHigh,slopeTemp)
noise <- 5 #Error term (SD of residuals)
fishSize <- model.matrix(~feeding+temp) %*% allCoefs + rnorm(N,0,noise) 

#Put everything together in a data frame
fishDat <- data.frame(fishSize=fishSize,temp=temp,feeding=feeding)

#Take a look at the data
ggplot(fishDat,aes(x=temp,y=fishSize,col=feeding))+
  geom_point()+geom_smooth(method='lm',se=F)+
  scale_colour_manual(values=c('blue','purple','red'))+
  labs(x='Temperature',y='Fish size (cm)')

#Fit model
mod1 <- lm(fishSize~feeding+temp,data=fishDat)
summary(mod1) #Get a summary of model

#Check residuals
par(mfrow=c(2,1))
#These look OK. Can also do formal tests (Shapiro-Wilk, Kolmogorov-Smirnoff),
#but these often don't work well.
plot(mod1,which=c(1,2)) 
par(mfrow=c(1,1))

#Check strength of terms

#Beware of using this. This is a Type-I or "sequential" ANOVA that drops terms
#in order they are specified. This means that the p-values can change depending
#on the order the terms were entered in the model (try switching feeding/temp
#and run this again)
anova(mod1) 

#Better to use this. This is a Type-III ANOVA that drops terms individually.
drop1(mod1,test='F')

#Plot the model results

#If we want to show only the effect of one term, we need to "subtract out" the
#effect of the others. This is called a partial effects plot. It's fairly
#straightforward to do this in base R, but there's a package called Effects that
#is handy for this.

#Say we were only interested in the effect of feeding
(feedEff <- effect('feeding',mod1))
#This next plot is ugly, but informative. Use ?plot.eff and change plotting
#parameters to make it less ugly.
plot(feedEff,partial.residuals = TRUE) 

#Effect of temperature (same as above)
effect('temp',mod1)
plot(effect('temp',mod1),partial.residuals = TRUE)


# Mixed effects model (lmer) -----------------------------------------------------

#NOTE: mixed effects models, random effects models, and heirarchical models are all the same thing

#Let's use the same data as above, but now, all the fish come from a set of 15
#different lakes
Nlakes <- 15
lakes <- factor(rep(letters[1:Nlakes],length.out=N))

#Each lake, for some reason, has bigger or smaller fish, and this is from a
#normal distribution with a mean = 0, SD = 8
lakeNoise <- 8 #SD for lakesv
lakeEffect <- rnorm(Nlakes,0,lakeNoise) #Effect of each lake

fishDat2 <- fishDat #Copy first dataframe
fishDat2$lakes <- lakes
fishDat2$fishSize <- model.matrix(~feeding+temp) %*% allCoefs + 
  model.matrix(~lakes) %*% lakeEffect + 
  rnorm(N,0,noise) 

#If you don't account for lakes... weird things may happen. In this case, it
#looks like there's a temp:feeding interaction, but this is caused by
#differences between lakes

ggplot(fishDat2,aes(x=temp,y=fishSize,col=feeding))+
  geom_point()+geom_smooth(method='lm',se=F)+
  scale_colour_manual(values=c('blue','purple','red'))+
  labs(x='Temperature',y='Fish size (cm)')

#This is a plot of each lake (a to o). Some lakes are weirdly higher than
#others.
ggplot(fishDat2,aes(x=temp,y=fishSize,col=feeding))+
  geom_point()+geom_smooth(method='lm',se=F)+
  facet_wrap(~lakes)+
  scale_colour_manual(values=c('blue','purple','red'))+
  labs(x='Temperature',y='Fish size (cm)')

#Fits "lakes" as a random effect. In essence, this is fitting the same model as
#above, but assuming that the size of the fish in each lake is randomly higher
#or lower than the "average" lake, and that these random changes between lakes
#are normally distributed.

mod2 <- lmer(fishSize~temp+feeding+(1|lakes),data=fishDat2)

#This is similar to fitting a fixed effect (e.g. in the model above, feeding was
#a fixed effect with 3 levels -- low, med, high), but with a conjugate prior set
#to zero with a SD that is to be estimated. This can be hard to understand, but
#here are the basic differences between random and fixed effects:

#1) Random effects "pool" variance between groups. This means that the means of
#lakes with few samples are actually pulled towards the mean of the other lakes
#(called "shrinkage"). 
#2) Random effects in lmer rely on the distribution of lake effects to be
#normal. This may or may not be true, but it's often a good starting point.
#3) Because the function is estimating a normal distribution of
#lake effects, this means that you need a larger number of lakes in order to be
#confident about this estimate. Usually about 6 is the bare minimum. If you have
#less than 6 lakes (sites/subjects/ect) then it's probably better to just fit
#them as a fixed effect.

#Check the model assumptions:
par(mfrow=c(3,1)) 
#Distribution of residuals
plot(fitted(mod2),resid(mod2)); abline(h=0)
qqnorm(resid(mod2)); qqline(resid(mod2)) 
#Distribution of random effects
qqnorm(unlist(ranef(mod2))); qqline(unlist(ranef(mod2))) 
par(mfrow=c(1,1))
#These all look OK

#lmer doesn't provide p-values because one of the package authors doesn't believe they can be calculated (easily) for mixed-effects models.
summary(mod2)




