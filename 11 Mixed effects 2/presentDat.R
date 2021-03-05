#Make mixed effect model example data

setwd("~/Projects/Stats projects/ecoStats2020/11 Mixed effects 2")

set.seed(15)

#Generate data
n <- 200

#Fixed effects
milkAndCookies <- sample(x=c('None','Milk and Cookies','Kombucha'),size=n,
                         replace=TRUE,prob=c(0.4,0.3,0.3)) #Presence of snacks
naughtiness <- rbeta(n,2,2) #Naughtiness
chimneyWidth <- rgamma(n,2,1.5) #Chimney width
  
fixefMat <- model.matrix(~milkAndCookies+naughtiness+chimneyWidth-1) #Fixed effects matrix

#kombucha, m&k, none, naughtiness, chimney width
fixCoef <- c(0.9,0,-0.5,-1,0.2)+0.3

#Random effects

#Variance components
sigmaR <- 2 #Overdispersion
sigmaG <- 3 #Group intercept sigma
rhoG <- 0.3

#20 Sampling sites
sites <- sample(c('Wellwick', 'Falconport', 'Greenelf', 'Riverbridge', 'Highhedge', 'Mallowholt', 'Blueglass', 'Snowbeach', 'Brightdale', 'Iceapple', 'Lorwald', 'Violetborough', 'Stonepine', 'Icebarrow', 'Lincastle','Belpond',
'Buttermaple','Lightspring','Wyvernfield','Pryfort'),n,TRUE)

nsites <- length(unique(sites))
lat <- runif(nsites,49,55) #Site latitude
lon <- runif(nsites,-120,-110)

#Squared-exponential distance function
covFun <- function(sigma,rho,d) (sigma^2)*exp((-rho^2)*(d^2)) 

distMat <- as.matrix(dist(cbind(lat,lon),diag=TRUE,upper=TRUE)) #Matrix of distances
covMat <- covFun(sigmaG,rhoG,distMat) #Covariance matrix
# curve(covFun(sigmaG,rhoG,x),0,10)
corMat <- cov2cor(covMat) #Correlation matrix
cholCorMat <- chol(corMat) #Choleski matrix

raneffs <- rnorm(nsites,0,1) #Random normal vector
raneffs <- raneffs %*% cholCorMat #Induce correlation
(p1 <- data.frame(lon,lat,raneffs) %>% ggplot(aes(x=lon,y=lat,col=raneffs,size=abs(raneffs)))+geom_point()+
  scale_color_gradient2(low='blue',mid='purple',high='red')) #Look at intercepts
raneff_int <- model.matrix(~sites-1) %*% raneffs[1,] #Intercept vector

# raneff <- scale(log(lat))*2 #Non-isotropic effect (Northern effect)
# raneff_int <- model.matrix(~sites-1) %*% raneff

yhat <- fixefMat %*% fixCoef + raneff_int #Expected value (link scale)
(presents <- rnbinom(n,mu=exp(yhat),size=sigmaR)) #Data
dat <- data.frame(presents,milkAndCookies,naughtiness,chimneyWidth,sites,lat,lon) #Assemble into data frame

par(mfrow=c(2,2))
plot(raneff_int,yhat)
plot(factor(milkAndCookies),yhat)
plot(naughtiness,yhat)
plot(chimneyWidth,yhat)

write.csv(dat,file='presentDat.csv')
# dat <- read.csv(file='presentDat.csv') #Read from csv

#Check data

library(tidyverse)
theme_set(theme_classic())
library(glmmTMB)

#Prep for spatial model
dat <- dat %>% mutate(coords=numFactor(lon,lat),group=factor(rep(1,n())))

m1 <- glmmTMB(presents~milkAndCookies+naughtiness+chimneyWidth+(1|sites),family=nbinom2(),data=dat)
m2 <- glmmTMB(presents~milkAndCookies+naughtiness+chimneyWidth+gau(coords + 0|group),
              family=nbinom2(),data=dat)

#Intercepts from m1
(p2 <- data.frame(lon,lat,ran=ranef(m1)[[1]][[1]][,1]) %>% 
  ggplot(aes(x=lon,y=lat,col=ran,size=abs(ran)))+geom_point()+
  scale_color_gradient2(low='blue',mid='purple',high='red'))

#Random effect field from m2
spRanEff <- expand.grid(lon=seq(min(lon),max(lon),length.out = 12),
                        lat=seq(min(lat),max(lat),length.out = 12)) %>% 
  mutate(coords=numFactor(lon,lat),group=factor(rep(1,nrow(.))),milkAndCookies='None',
         naughtiness=0.5,chimneyWidth=2) %>% 
  mutate(pred=predict(object=m2,newdata=.,type='response',allow.new.levels = TRUE))

(p3 <- ggplot(spRanEff,aes(lon,lat,fill=pred))+geom_raster()+
  geom_point(data=data.frame(lat,lon),aes(lon,lat,fill=NULL),col='red')+
  labs(title='Spatial random effect',fill='Intercept'))

