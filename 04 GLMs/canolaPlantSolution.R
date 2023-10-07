#Solution to canola plant problem from GLM lecture

library(tidyverse)
library(glmmTMB)
library(ggeffects)

dat <- read.csv('./04 GLMs/canolaPlants.csv') %>% 
  filter(Missing>=0,Pods>0) %>% #Filters out bad data
  mutate(Field=factor(Field)) %>%
  mutate(Plot=factor(paste(Year,Field,Distance,sep='_')))
head(dat)

m1 <- glm(cbind(Pods,Missing)~Year+VegMass+Distance,
          data=dat,family='binomial')
summary(m1)

#Large overdispersion

#Quasibinomial version
m2a <- glm(cbind(Pods,Missing)~Year+VegMass+Distance,data=dat,family='quasibinomial')
summary(m2a)

#Betabinomial version
m2b <- glmmTMB(cbind(Pods,Missing)~Year+VegMass+Distance,data=dat,family='betabinomial')
summary(m2b) #Similar answers to quasibinomial

list(
  data.frame(ggpredict(m2a,terms = 'VegMass[all]')),
  data.frame(ggpredict(m2b,terms = 'VegMass[all]'))
) %>% bind_rows(.id='model') %>% 
  mutate(model=factor(model,labels=c('quasibinomial','betabinomial'))) %>% 
  ggplot(aes(x=x,y=predicted*100))+
  geom_ribbon(aes(ymax=conf.high*100,ymin=conf.low*100,fill=model),alpha=0.3)+
  geom_line((aes(col=model)))+
  theme_classic()+
  scale_colour_manual(values=c('blue','red'),aesthetics = c('colour','fill'))+
  labs(x='Veg Mass (g)',y='Pod Success %')
