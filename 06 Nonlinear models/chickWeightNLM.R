#NLM example, using `ChickWeight` data

library(tidyverse)
theme_set(theme_classic())
library(lme4)
library(mgcv)

#Load data, and take a look at it
data("ChickWeight")
head(ChickWeight)
ggplot(ChickWeight,aes(x=Time,y=weight,col=Diet))+
  geom_line(aes(group=Chick))+
  geom_point()


# SSlogis (logistic growth model) -----------------------------------------

#Start with a simple nonlinear

#If models have trouble starting, this is sometimes OK for getting initial "guesses" for parameters
getInitial(weight ~ SSlogis(Time, Asym, xmid, scal), data=ChickWeight)

mLog1 <- nls(weight ~ SSlogis(Time, Diet, xmid, scal), data=ChickWeight)
summary(mLog1)

data.frame(pred=predict(mLog1),act=ChickWeight$weight) %>% 
  ggplot(aes(x=pred,y=act))+geom_point()+
  labs(x='Predicted',y='Actual')

#OK for a starter model. But clearly variation among groups and individuals
ChickWeight %>% 
  mutate(pred=predict(mLog1)) %>% 
  ggplot(aes(x=Time,y=weight,col=Diet))+
  geom_line(aes(group=Chick))+geom_point()+
  geom_line(aes(y=pred),col='black',size=2)






# SSgompertz --------------------------------------------------------------


