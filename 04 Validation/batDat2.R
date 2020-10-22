#More simulated bat data, this time within a causal modeling framework

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
# bats <- poly(temp,2) + light + bugs
# bugs <- poly(temp,2)
# light <- clouds
# humidity <- temp
  
tempEff <- 0.01*(-temp+5)*(temp - 40)
lightEff <- light*(-0.05)
bugEff <- bugs*0.01
ybats <- tempEff + bugEff + lightEff
bats <- rpois(N,exp(ybats))
dat <- data.frame(bats,temp,humidity,clouds,light,bugs)

mod1 <- lm(log(bats+0.1)~poly(temp,2)+light+bugs,data=dat)
par(mfrow=c(2,1))
plot(mod1,which=c(1,2)) #Couple outliers, but not horrible

summary(mod1) #Pretty good. Coeffs for poly are from othogonal polynomials

library(tidyverse)
theme_set(theme_classic())
library(ggeffects)

data.frame(ggpredict(mod1,terms='temp',back.transform = TRUE)) %>% 
  ggplot(aes(x,predicted))+
  geom_ribbon(aes(ymax=conf.high,ymin=conf.low),alpha=0.3)+
  geom_line()+labs(x='Temperature',y='Bat counts')

data.frame(ggpredict(mod1,terms='bugs',back.transform = TRUE)) %>% 
  ggplot(aes(x,predicted))+
  geom_ribbon(aes(ymax=conf.high,ymin=conf.low),alpha=0.3)+
  geom_line()+labs(x='Bugs',y='Bat counts')

data.frame(ggpredict(mod1,terms='light',back.transform = TRUE)) %>% 
  ggplot(aes(x,predicted))+
  geom_ribbon(aes(ymax=conf.high,ymin=conf.low),alpha=0.3)+
  geom_line()+labs(x='Light',y='Bat counts')
