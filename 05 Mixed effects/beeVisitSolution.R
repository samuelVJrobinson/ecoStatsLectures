# Lecture 5 final exercise

library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(ggeffects)

dat <- read.csv("C:\\Users\\Samuel\\Desktop\\doi_10.5061_dryad.vhhmgqnvj__v4\\commodityVisitation.csv")

dat <- dat %>% 
  mutate(across(c(StartTime,EndTime),as.POSIXlt)) %>%
  mutate(time=as.numeric(difftime(EndTime,StartTime,units = 'mins'))/10) %>% 
  mutate(across(c(Field,Year),factor)) %>% mutate(logTime=log(time))
  

m1 <- glmmTMB(Honeybee ~ offset(logTime)+Distance*NumHives+
                (Distance|Field),family='nbinom2',
              data=dat)
summary(m1)

devRes <- resid(m1,type = 'deviance')
qqnorm(devRes); qqline(devRes)
plot(predict(m1),devRes)

plot(m1Res) #Distribution looks OK, but trend for higher qResids at higher predictions
testZeroInflation(m1Res) #OK for zero-inflation
testDispersion(m1Res) #OK for dispersion

ggpredict(m1,terms=c('Distance[0:500]','NumHives[0,20,40]'),condition = list(logTime=0))
