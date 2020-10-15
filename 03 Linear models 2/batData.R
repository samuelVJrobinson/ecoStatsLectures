# Generate some bat data
setwd("~/Documents/ecoStats2020/03 Linear models 2")

set.seed(1)
N <- 100 #Sample size
var1 <- sample(c('Edmonton','Calgary'),N,replace=TRUE) #Sampling site
var2 <- round(rexp(N,0.5)) #Age of bats
var3 <- sample(c('F','M'),N,replace=TRUE) #Sex
dat <- data.frame(city=var1,age=var2,sex=var3)
modMat <- model.matrix(~city+age*sex,data=dat)
coefs <- c(20,-3,3,0,-1) #Coefficients
sd <- 3
weight <- abs(rnorm(N,modMat%*%coefs,5)) #Reflecting boundary
weight[weight<5] <- weight[weight<5]+rexp(sum(weight<5),0.2) #Feed those tiny bats more food!
dat$weight <- weight
rm(var1,var2,var3,modMat,coefs,sd,N,weight) #Cleanup
write.csv(dat,file='batDat.csv')

batMod <- lm(weight~city+age*sex,data=dat)
summary(batMod)

#Partial effects plots - effects
library(effects)
batModEff <- predictorEffects(batMod,partial.residuals=TRUE) #Calculate partial effects
plot(batModEff,
     lines=list(col='red'), partial.residuals=list(pch=19,col='black',cex=0.25))

#Same thing using ggeffects
library(tidyverse)
theme_set(theme_classic())
library(ggeffects)

batModEff1 <- ggpredict(batMod,terms=c('age','sex')) %>% data.frame()
ggplot(batModEff1,aes(x=x,y=predicted))+
  geom_ribbon(aes(ymax=conf.high,ymin=conf.low,fill=group),alpha=0.3)+
  geom_line(aes(col=group))

batModEff2 <- ggpredict(batMod,terms=c('city')) %>% data.frame()
ggplot(batModEff2,aes(x=x,y=predicted))+
  geom_pointrange(aes(ymax=conf.high,ymin=conf.low))


#Bonus:
a <- c(rep('Na',16),rep(c('Batman!',rep('',7)),2))
for(i in 1:length(a)) print(a[i])
