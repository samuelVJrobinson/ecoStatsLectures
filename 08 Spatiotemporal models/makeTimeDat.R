# Generate temporally correlated data

library(tidyverse)
library(glmmTMB)
library(mgcv)

#Squared-exponential distance function
covFun <- function(sigma,rho,d) (sigma^2)*exp((-rho^2)*(d^2)) 

##Earlier version
# time <- runif(ngroups,0,10)
# # time <- seq(-20,20,length.out=20)
# distMat <- as.matrix(dist(time,diag=TRUE,upper=TRUE)) #Matrix of distances
# diag(distMat) <- 1e-5
# covMat <- covFun(sigmaG,0.2,distMat) #Covariance matrix
# corMat <- cov2cor(covMat) #Correlation matrix
# cholCorMat <- chol(corMat,pivot = TRUE) #Choleski matrix
# raneffs <- rnorm(ngroups,0,1) %*% cholCorMat #Induce correlation
# raneff_int <- model.matrix(~g-1) %*% raneffs[1,] #Intercept vector
# yhat <- intercept + slopeX*x + raneff_int  #Expected value
# # y <- rnorm(n,yhat,sigmaR) #Data (normal)
# y <- rnbinom(n,mu=exp(yhat/5),sigmaR) #Data (NB)
# # lat <- model.matrix(~g-1) %*% lat
# # lon <- model.matrix(~g-1) %*% lon
# time <- model.matrix(~g-1) %*% time
# dat3 <- data.frame(count=y,temperature=x+10,location=g,days=time) #Assemble into data frame

set.seed(1)
n <- 160
ngroups <- 16

x <- runif(n,0,20) #Single fixed effect predictor
g <- sample(letters[1:ngroups],n,TRUE) #Groups
intercept <- 1
slopeX <- 0.5
sigmaR <- 3 #Residual sigma 
sigmaTime <- 10 #Covariance for GP model
sigmaG <- 3 #Group intercept sigma
rho <- 0.1

# Generate temporally correlated random intercepts
time <- round(sort(runif(n,0,30)))
# time <- seq(-20,20,length.out=20)
distMat <- as.matrix(dist(time,diag=TRUE,upper=TRUE)) #Matrix of distances
diag(distMat) <- 1e-5
covMat <- covFun(sigmaTime,rho,distMat) #Covariance matrix
curve(covFun(sigmaTime,rho,x),0,10)
corMat <- cov2cor(covMat) #Correlation matrix
cholCorMat <- chol(corMat,pivot = TRUE) #Choleski matrix
raneff_time <- (rnorm(n,0,1) %*% cholCorMat)[1,] #Induce correlation
plot(time,raneff_time) #Temporal effect
raneff_site <- model.matrix(~g-1) %*% rnorm(ngroups,0,sigmaG) #Random intercept for site

yhat <- intercept + slopeX*x + raneff_time + raneff_site  #Expected value
# y <- rnorm(n,yhat,sigmaR) #Data (normal)
y <- rnbinom(n,mu=exp(yhat/5),sigmaR) #Data (NB)
# lat <- model.matrix(~g-1) %*% lat
# lon <- model.matrix(~g-1) %*% lon
# time <- model.matrix(~g-1) %*% time
dat <- data.frame(count=y,temperature=x,site=g,days=factor(time)) #Assemble into data frame
rm(x,g,y,y2,lat,lon,yhat,n)
filepath <- './08 Spatiotemporal models/timeDat.csv'

# write.csv(dat,filepath,row.names = FALSE)
# dat <- read.csv(filepath) %>% mutate(site=factor(site))

#Naive random effect model
m2 <- glmmTMB(count~temperature+(1|site),
              data=dat,
              family='nbinom2')

m2Res <- residuals(m2,'deviance')

dat %>% mutate(m2Res) %>% 
  mutate(days=as.numeric(days)) %>% 
  ggplot(aes(x=days,y=m2Res))+
  geom_point()+
  geom_smooth(method='loess',formula=y~x)+
  labs(x='Days',y='Residuals')

#Days as "numeric factor"
dat$dayF <- numFactor(dat$days)
dat$group <- factor(rep(1,nrow(dat)))

#Fit model with temporal random effect
m2_ac <- glmmTMB(count~temperature+(1|site)+
                   exp(dayF+0|group),
                 data=dat,
                 family='nbinom2')
m2acRes <- residuals(m2_ac,'deviance')

dat %>% mutate(m2acRes) %>% 
  mutate(days=as.numeric(days)) %>% 
  ggplot(aes(x=days,y=m2acRes))+
  geom_point()+
  geom_smooth(method='loess',formula=y~x)+
  labs(x='Days',y='Residuals')

data.frame(mod=c('1. Actual','2. Standard','3. Autocorr'),
           coef=c(0.5,fixef(m2)[[1]][2],fixef(m2_ac)[[1]][2]),
           se=c(0,sqrt(diag(vcov(m2)[[1]]))[1],sqrt(diag(vcov(m2_ac)[[1]]))[1])) %>% 
  mutate(upr=coef+se*1.96,lwr=coef-se*1.96) %>% 
  ggplot(aes(x=mod,y=coef))+geom_pointrange(aes(ymax=upr,ymin=lwr))+
  labs(x=NULL,y='Coefficient')

dat$days <- as.numeric(dat$days)

m3 <- gam(count~temperature+
            s(days,bs='cr'),
          data=dat,family='nb')

summary(m3)
m3Res <- residuals(m3,'deviance')

dat %>% mutate(m3Res) %>% 
  mutate(days=as.numeric(days)) %>% 
  ggplot(aes(x=days,y=m3Res))+
  geom_point()+
  geom_smooth(method='loess',formula=y~x)+
  labs(x='Days',y='Residuals')





