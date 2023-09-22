setwd('./03 Linear models/')

#Simulated bat data within a causal modeling framework
set.seed(123)
N <- 100
temp <- runif(N,10,30) #Temperature
humidity <- temp+40+rnorm(N,0,5) #Humidity
clouds <- rbeta(N,6,3) #Cloud cover
light <- 3000*exp(clouds*-10)+rexp(N,0.3) #Light levels
# bugs <- round(runif(N,0,100))
bugs <- rpois(N,abs(-30+temp*0.1+0.3*temp^2)+rgamma(N,10,0.1)) #Bugs per night (negbin)

#In this model, bats care about temperature, light, and bugs

#True causal model: 
# bats <- poly(temp,2) + log(light) + bugs
# bugs <- poly(temp,2)
# light <- clouds
# humidity <- temp
  
tempEff <- 0.01*(-temp+5)*(temp - 40)
lightEff <- 10*log(light)*(-0.05)
bugEff <- bugs*0.01

par(mfrow=c(3,1)) #Partial effects
plot(temp,tempEff); plot(light,lightEff); plot(bugs,bugEff)

ybats <- tempEff + bugEff + lightEff
bats <- rpois(N,exp(ybats))
dat <- data.frame(bats,temp,humidity,clouds,light,bugs)

pairs(dat)

write.csv(dat,file='batDat.csv',row.names = FALSE)

dat <- dat %>% mutate(logBats=log(bats+1)) #Pre-transform

mod1 <- lm(logBats~poly(temp,2)+log(light)+bugs,data=dat)
par(mfrow=c(2,1))
plot(mod1,which=c(1,2)) #Couple outliers, but not horrible
summary(mod1) #Pretty good. Coeffs for poly are from othogonal polynomials

library(tidyverse)
theme_set(theme_classic())
library(ggeffects)

ggpredict(mod1,terms='temp') %>% plot(residuals=TRUE)
ggpredict(mod1,terms='light') %>% plot(residuals=TRUE)
ggpredict(mod1,terms='bugs') %>% plot(residuals=TRUE)
