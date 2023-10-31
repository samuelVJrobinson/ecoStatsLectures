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

#Take a look at what the SSlogis function does
ggplot(data.frame(x=seq(0,10,by=0.1)),aes(x))+
  stat_function(fun=function(x) SSlogis(x,10,5,1))+
  stat_function(fun=function(x) SSlogis(x,10,2,1),col='red')+
  stat_function(fun=function(x) SSlogis(x,8,5,2),col='blue')+
  annotate(geom='text',x=8.5,y=8,label='Asym: 10, xmid: 5, scal: 1')+
  annotate(geom='text',x=2,y=9,col='red',label='Asym: 10, xmid: 2, scal: 1')+
  annotate(geom='text',x=8.5,y=5,col='blue',label='Asym: 10, xmid: 5, scal: 2')+
  labs(title=TeX('\\textbf{SSlogis}: $Asym/(1+e^{\\frac{xmid-x}{scal}})$'))

#Start with a simple nonlinear (no random/fixed effects)

#If models have trouble starting, this is sometimes OK for getting initial "guesses" for parameters
startVals <- getInitial(weight ~ SSlogis(Time, Asym, xmid, scal), data=ChickWeight)

mLog1 <- nls(weight ~ SSlogis(Time, Asym, xmid, scal), data=ChickWeight, start = startVals)
summary(mLog1)

#Predicted vs Actual + 1:1 line
data.frame(pred=predict(mLog1),act=ChickWeight$weight) %>% 
  ggplot(aes(x=pred,y=act))+geom_point()+
  geom_abline(intercept = 0,slope = 1,col='red')+
  labs(x='Predicted',y='Actual')

#OK for a starter model. But clearly variation among groups and individuals
ChickWeight %>% 
  mutate(pred=predict(mLog1)) %>% 
  ggplot(aes(x=Time,y=weight,col=Diet))+
  geom_line(aes(group=Chick))+geom_point()+
  geom_line(aes(y=pred),col='black',linewidth=2) #Overall predictions

#It is possible to fit fixed-effects nonlinear models in lmer or nlmer, but it requires that you specify the gradient function, which can be really annoying (see here: https://stackoverflow.com/questions/15141952/nlmer-longitudinal-data)

#Instead we'll try this using nlme. This can fit a whole range of models with both fixed and random effects (but can be finicky to start, and can sometimes take a long time)

#Starting values
startVals2 <- list(fixed=rep(startVals,c(4,1,1))) #Same starting values, but with 4 "Asym" start vals

mLog2 <- nlme(model = weight ~ SSlogis(Time, Asym, xmid, scal), #Overall model: weight follows SSlogis curve
              #Fixed effects: Asym varies with Diet
              # Single fixed effect for xmid and scal
              fixed = list( Asym ~ Diet, xmid + scal ~ 1), 
              #Random effects: Asym varies randomly by Chick
              random = Asym  ~ 1 | Chick, 
              data= ChickWeight, start = startVals2)
summary(mLog2)

#Predictions split by diet, looks OK, but some mis-fitting (especially Diet 1)
ChickWeight %>% 
  mutate(pred=predict(mLog2,level=0)) %>% #Get predictions at population level (no random effects)
  ggplot(aes(x=Time,y=weight))+
  geom_point()+ #Overall predictions
  geom_line(aes(y=pred,group=Chick))+
  facet_wrap(~Diet)

#Weird residual patterns - likely missing a parameter
ChickWeight %>% 
  mutate(res=residuals(mLog2)) %>% 
  ggplot(aes(x=Time,y=res,group=Chick))+
  geom_point()+geom_line()

ChickWeight %>% 
  mutate(res=residuals(mLog2),pred=predict(mLog2)) %>% 
  ggplot(aes(x=pred,y=res,group=Chick))+
  geom_point()+geom_line()

#Try again, letting xmid and scal vary with Diet as well
startVals2 <- list(fixed=rep(startVals,c(4,4,4))) #Same starting values, but with 4 start vals for each param

mLog3 <- nlme(model = weight ~ SSlogis(Time, Asym, xmid, scal), #Overall model: weight follows SSlogis curve
              #Fixed effects: Asym  and scal vary with Diet
              fixed = list( Asym ~ Diet, xmid ~ Diet, scal ~ Diet), 
              #Random effects: Asym and xmid vary randomly by Chick
              random = Asym + xmid ~ 1 | Chick, 
              data= ChickWeight, start = startVals2)
summary(mLog3)

#Looks like some of the individual level variance might also depend on diet (spread is larger for some groups than others)
ChickWeight %>% 
  mutate(pred=predict(mLog3,level=0)) %>% #Get predictions at population level (no random effects)
  ggplot(aes(x=Time,y=weight))+
  geom_point()+ #Overall predictions
  geom_line(aes(y=pred,group=Chick),linewidth=1,col='red',linetype='dashed')+
  facet_wrap(~Diet)

#Residuals look slightly better
ChickWeight %>% 
  mutate(res=residuals(mLog3)) %>% 
  ggplot(aes(x=Time,y=res,group=Chick))+
  geom_point()+geom_line()

#Still a bit of a weird shape to residuals, especially at lower levels
ChickWeight %>% 
  mutate(res=residuals(mLog3,type='pearson'),pred=predict(mLog3)) %>% 
  ggplot(aes(x=pred,y=res,group=Chick))+
  geom_point()+geom_line()+
  geom_hline(yintercept = 0,col='red')

# SSgompertz --------------------------------------------------------------

#Take a look at what the SSgompertz function does
ggplot(data.frame(x=seq(0,10,by=0.1)),aes(x))+
  stat_function(fun=function(x) SSgompertz(x,10,1,0.5))+
  stat_function(fun=function(x) SSgompertz(x,10,2,0.5),col='red')+
  stat_function(fun=function(x) SSgompertz(x,5,1,0.1),col='blue')+
  stat_function(fun=function(x) SSgompertz(x,5,2,0.1),col='green')+
  annotate(geom='text',x=7,y=9,label='Asym: 10, b2: 1, b3: 0.5')+
  annotate(geom='text',x=7,y=8,col='red',label='Asym: 10, b2: 2, b3: 0.5')+
  annotate(geom='text',x=7,y=4,col='blue',label='Asym: 5, b2: 1, b3: 0.1')+
  annotate(geom='text',x=3,y=3,col='green',label='Asym: 5, b2: 2, b3: 0.1')+
  labs(title=TeX('\\textbf{SSgompertz}: $(Asym)e^{-b_2b_3^x}$'))

startVals <- getInitial(weight ~ SSgompertz(Time, Asym, b2, b3), data=ChickWeight)

mGom1 <- nls(weight ~ SSgompertz(Time, Asym, b2, b3), data=ChickWeight, start = startVals)
summary(mLog1)

#Predicted vs Actual + 1:1 line
data.frame(pred=predict(mGom1),act=ChickWeight$weight) %>% 
  ggplot(aes(x=pred,y=act))+geom_point()+
  geom_abline(intercept = 0,slope = 1,col='red')+
  labs(x='Predicted',y='Actual')

#OK for a starter model. But clearly variation among groups and individuals
ChickWeight %>% 
  mutate(pred=predict(mGom1)) %>% 
  ggplot(aes(x=Time,y=weight,col=Diet))+
  geom_line(aes(group=Chick))+geom_point()+
  geom_line(aes(y=pred),col='black',linewidth=2) #Overall predictions

#Try a mixed effects nlme model:
startVals2 <- list(fixed=rep(startVals,c(4,1,1))) #Same starting values, but with 4 "Asym" start vals

#Fits, but you have to use a reduced tolerance. This isn't really a good sign; usually indicate that the model is overly complex, or otherwise mis-fit to the data
mGom2 <- nlme(model = weight ~ SSgompertz(Time, Asym, b2, b3), #Overall model: weight follows SSlogis curve
              #Fixed effects: Asym varies with Diet
              # Single fixed effect for xmid and scal
              fixed = list( Asym ~ Diet, b2 + b3 ~ 1), 
              #Random effects: Asym varies randomly by Chick
              random = Asym  ~ 1 | Chick, 
              data= ChickWeight, start = startVals2,
              control=nlmeControl(maxIter=150,tolerance=1e-1))
summary(mGom2)

#Predictions split by diet, looks OK, but some mis-fitting
ChickWeight %>% 
  mutate(pred=predict(mGom2,level=0)) %>% #Get predictions at population level (no random effects)
  ggplot(aes(x=Time,y=weight))+
  geom_line(aes(group=Chick))+  #Original data
  geom_line(aes(y=pred,group=Chick),col='red',linetype='dashed')+ #Diet-level predictions
  facet_wrap(~Diet)

startVals2 <- list(fixed=rep(startVals,c(4,4,1))) 

#Fits with "standard" tolerance: a good sign
mGom3 <- nlme(model = weight ~ SSgompertz(Time, Asym, b2, b3), #Overall model: weight follows SSlogis curve
              #Fixed effects: Asym varies with Diet
              # Single fixed effect for xmid and scal
              fixed = list( Asym + b2 ~ Diet, b3 ~ 1), 
              #Random effects: Asym varies randomly by Chick
              random = Asym + b2 ~ 1 | Chick, 
              data= ChickWeight, start = startVals2,
              control=nlmeControl(maxIter=50))
summary(mGom3)

#Looks better
ChickWeight %>% 
  mutate(pred=predict(mGom3,level=0)) %>% #Get predictions at population level (no random effects)
  ggplot(aes(x=Time,y=weight))+
  geom_line(aes(group=Chick))+  #Original data
  geom_line(aes(y=pred,group=Chick),col='red',linetype='dashed')+ #Diet-level predictions
  facet_wrap(~Diet)

#Residuals look OK-ish
ChickWeight %>% 
  mutate(res=residuals(mGom3)) %>% 
  ggplot(aes(x=Time,y=res,group=Chick))+
  geom_point()+geom_line()

#Residuals look much better
ChickWeight %>% 
  mutate(res=residuals(mGom3,type='pearson'),pred=predict(mLog3)) %>% 
  ggplot(aes(x=pred,y=res,group=Chick))+
  geom_point()+geom_line()+
  geom_hline(yintercept = 0,col='red')


# GAM ---------------------------------------------------------------------

#Fit an additive model for each diet
gam1 <- gam(weight ~ Diet + s(Time,by=Diet), 
            data=ChickWeight)
summary(gam1)

ChickWeight %>% 
  mutate(pred=predict(gam1,level=0)) %>% #Get predictions at population level (no random effects)
  ggplot(aes(x=Time,y=weight))+
  geom_line(aes(group=Chick))+  #Original data
  geom_line(aes(y=pred,group=Chick),col='red',linetype='dashed')+ #Diet-level predictions
  facet_wrap(~Diet)

#Residuals look OK-ish
ChickWeight %>% 
  mutate(res=residuals(gam1,type='scaled.pearson')) %>% 
  ggplot(aes(x=Time,y=res,group=Chick))+
  geom_point()+geom_line()

#Residuals look OK, but non-constant variance (due to individual chick)
ChickWeight %>% 
  mutate(res=residuals(gam1,type='scaled.pearson'),pred=predict(gam1)) %>% 
  ggplot(aes(x=pred,y=res,group=Chick))+
  geom_point()+geom_line()+
  geom_hline(yintercept = 0,col='red')

#Takes a while to run. Constrain the basis functions (k=8) to make sure it doesn't get "too wiggly"
# Could also use extra penalization: bs='tp' or bs='cp'
gam2 <- gam(weight ~ Diet + s(Time,by=Diet, k = 8)+ #Intercept and smoother for each diet
              s(Chick,bs='re')+ #Random intercept for chick
              #Different smoother for each chick (not technically a random effect, but best we can do)
              s(Time,by=Chick, k = 8), 
            data=ChickWeight)
summary(gam2)

#Residuals look OK-ish
ChickWeight %>% 
  mutate(res=residuals(gam2,type='scaled.pearson')) %>% 
  ggplot(aes(x=Time,y=res,group=Chick))+
  geom_point()+geom_line()

#Get predictions without the random intercept or chick-wise model predictions
gam2Preds <- predict(gam2,exclude = names(gam2$sp)[grepl('Chick',names(gam2$sp))])

#Not great predictions, especially for Diet 1 - requires further adjustments to get improved model results
ChickWeight %>% 
  mutate(pred=gam2Preds) %>% #Get predictions at population level (no random effects)
  ggplot(aes(x=Time,y=weight))+
  geom_line(aes(group=Chick))+  #Original data
  geom_line(aes(y=pred,group=Chick),col='red',linetype='dashed')+ #Diet-level predictions
  facet_wrap(~Diet)

# Compare models ----------------------------------------------------------

#Suggests that logistic model is the better overall fit to the data
AIC(mGom3,mLog3)
anova(mGom3,mLog3)

summary(mLog3) #Looks like Diet 3 is the best, in terms of overall weight gain

ChickWeight %>% 
  mutate(pred=predict(mLog3,level=0)) %>% #Get predictions at population level (no random effects)
  ggplot(aes(x=Time,y=weight,col=Diet))+
  geom_jitter()+ #Overall predictions
  geom_line(aes(y=pred,group=Chick),linewidth=2)

