#Generate some bat data

library(tidyverse)
library(broom)
theme_set(theme_classic())

setwd("~/Documents/ecoStats2020/08 GLMs 2")
set.seed(123)

#Functions
logit <- function(x) log(x/(1-x))
invLogit <- function(x) exp(x)/(1+exp(x))

#Generate some non-normal data
N <- 100
var1 <- sample(c('Edmonton','Calgary'),N,replace=TRUE) #Sampling site
var2 <- rexp(N,0.5) #Nest volume

dat <- data.frame(city=var1,size=var2)
modMat <- model.matrix(~city+size,data=dat)
coefs <- c(1,-0.2,0.3) #Same coefficients for both models

yhat <- modMat %*% coefs #Expected value (link scale)

dat$batCounts <- rpois(N,exp(yhat))  #Poisson process
dat$batPres <- rbinom(N,1,invLogit(yhat))  #Binomial (bernoulli) process

write.csv(dat,file = 'batDatGLM.csv')

#Fit models

modPois <- glm(batCounts~city+size,data=dat,family='poisson')
summary(modPois)

modBin <- glm(batPres~city+size,data=dat,family='binomial')
summary(modBin)

#Compare models
p1 <- mutate(tidy(modPois),mod='count') %>% 
  bind_rows(mutate(tidy(modBin),mod='presence')) %>% 
  select(-statistic,-p.value) %>% 
  bind_rows(data.frame(term=names(coef(modBin)),estimate=coefs,std.error=0,mod='actual')) %>% 
  mutate(term=factor(term,labels=c('Intercept','City','Size'))) %>% 
  ggplot(aes(term,estimate,col=mod))+geom_pointrange(aes(ymax=estimate+std.error*2,ymin=estimate-std.error*2),position=position_dodge(width=0.3))+
  geom_hline(yintercept=0,linetype='dashed')+
  scale_colour_manual(values=c('black','red','blue'))+
  labs(x=NULL,y='Estimate',col='Model')

ggsave(filename = 'modResults.png',plot=p1,width=6, height=4)  
