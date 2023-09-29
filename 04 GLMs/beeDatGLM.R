#Generate some bee data

library(tidyverse)
library(broom)
theme_set(theme_classic())

set.seed(123)
setwd('./04 GLMs')

#Functions
logit <- function(x) log(x/(1-x))
invLogit <- function(x) exp(x)/(1+exp(x))

#Generate some non-normal data
N <- 100
var1 <- sample(c('Edmonton','Calgary'),N,replace=TRUE) #Sampling site
var2 <- rexp(N,0.5) #Floral resources

dat <- data.frame(city=var1,floral=var2)
modMat <- model.matrix(~city+floral,data=dat)
coefs <- c(1,-0.2,0.3) #Same coefficients for both models

yhat <- modMat %*% coefs #Expected value (link scale)

dat$beeCounts <- rnbinom(N,mu=exp(yhat),size=1) #NB process
dat$beePres <- rbinom(N,1,invLogit(yhat))  #Bernoulli process

write.csv(dat,file = 'beeDatGLM.csv',row.names = FALSE)
