
# Load everything ---------------------------------------------------------

library(tidyverse)
theme_set(theme_classic())
library(ggeffects)

#Functions
logit <- function(x) -log((1/x) -1)
invLogit <- function(x) 1/(1+exp(-x))

expFun1 <- function(x,a,b) a*exp(x*b)

leq <- function(x,a,b) 1/(1+exp(-(a+b*x)))

#Truncated gamma distribution
trgamma <- function(N,a,b,lims){
  x <- rgamma(N,a,b)
  outside <- x>lims[2]|x<lims[1]
  while(any(outside)){
    x[outside] <- rgamma(sum(outside),a,b)
    outside <- any(x>lims[2]|x<lims[1])
  }
  x
}

#Distribution parameters
set.seed(1)
deerLeqPars <- c(3,-0.1)
soilExpPars <- c(250,-0.05)

Nrep <- 200
# Nsites <- 20 #Future examples: add random effects
slope <- trgamma(Nrep,1,0.05,c(0,90)) #Generate slopes
mu_propDeer <- leq(slope,deerLeqPars[1],deerLeqPars[2]) #Avg deer presence
mu_soilDepth <- expFun1(slope,soilExpPars[1],soilExpPars[2]) #Avg soil depth
plantPresPars <- c(0.5,-2,1,-1) #Parameters for generating plant presence
mu_plantPres <- plantPresPars[1] + 
  scale(mu_propDeer)*plantPresPars[2] + 
  scale(mu_soilDepth)*plantPresPars[3] + 
  scale(mu_propDeer)*scale(mu_soilDepth)*plantPresPars[4] 
mu_plantPres <- invLogit(mu_plantPres*5)
  
hist(slope)
plot(slope,mu_propDeer)
plot(slope,mu_soilDepth)
plot(slope,mu_plantPres)
#Collinear deer/soil data give bad examples in 2D
plot(mu_propDeer,mu_plantPres)
plot(mu_soilDepth,mu_plantPres)
data.frame(mu_plantPres,mu_soilDepth,mu_propDeer) %>% #Easy to see collinearity in 3D
  ggplot(aes(x=mu_soilDepth,y=mu_propDeer))+
  geom_point(aes(col=mu_plantPres))


#Add noise

#Deer
deerTheta <- 10
deerAlpha <- mu_propDeer*deerTheta
deerBeta <- (1-mu_propDeer)*deerTheta
propDeer <- rbeta(length(mu_propDeer),deerAlpha,deerBeta)
plot(slope,propDeer,ylim=c(0,1))
points(slope,mu_propDeer,col='red')

#Soil
soilTheta <- 10
soilAlpha <- mu_soilDepth/soilTheta
soilBeta <- soilTheta
soilDepth <- rgamma(length(mu_soilDepth),shape = soilAlpha, scale=soilBeta)
plot(slope,soilDepth)
points(slope,mu_soilDepth,col='red')

#Plants
plantPres <- rbinom(length(mu_plantPres),size = 1,prob = mu_plantPres)
plot(slope,plantPres)
points(slope,mu_plantPres,col='red')

#Easy to see collinearity in 3D
data.frame(plantPres,soilDepth,propDeer) %>% 
  ggplot(aes(x=soilDepth,y=propDeer))+
  geom_point(aes(col=factor(plantPres)))+
  scale_colour_manual(values=c('black','red'))+
  labs(x='SoilDepth',y='PropDeer',col='PlantPres')

#Assemble dataframe
dat <- data.frame(plantPres,propDeer,soilDepth,slope)
write.csv(dat,'./09 Writing/plantDat.csv',row.names = FALSE)
rm(list=ls()[ls()!='dat'])
mod1 <- glm(plantPres~propDeer*soilDepth,family='binomial',data=dat) 
summary(mod1)

ggpredict(mod1,terms=c('propDeer[0:1 by=0.05]','soilDepth[0,200,400]')) %>% 
  plot()

ggpredict(mod1,terms=c('soilDepth[0:400 by=10]','propDeer[0,0.5,1]')) %>% 
  plot()

ggpredict(mod1,terms=c('propDeer[0:1 by=0.1]','soilDepth[0:400 by=20]')) %>% 
  data.frame() %>% mutate(group=as.numeric(as.character(group))) %>% 
  ggplot(aes(x=x,y=group))+
  geom_raster(aes(fill=predicted))+
  geom_point(data=dat,aes(x=propDeer,y=soilDepth,col=factor(plantPres)))+
  labs(x='propDeer',y='SoilDepth',col='Plant\nPresent')+
  theme_bw()+
  scale_fill_distiller()+
  scale_colour_manual(values=c('red','black'))

data.frame(actual=plantPresPars,estMu=coef(mod1),estSE=diag(vcov(mod1))) %>% 
  mutate(upr=estMu+estSE*1.96,lwr=estMu-estSE*1.96) %>% 
  rownames_to_column('coef') %>% mutate(coef=factor(coef,levels=coef)) %>% 
  ggplot(aes(x=coef))+
  geom_pointrange(aes(y=estMu,ymax=upr,ymin=lwr),col='red')+
  geom_point(aes(y=actual))
  


