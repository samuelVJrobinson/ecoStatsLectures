#Spatial autocorrelation example
# Modified from https://cran.r-project.org/web/packages/glmmTMB/vignettes/covstruct.html

#Load glmmTMB
library(glmmTMB)

set.seed(1234)

Nsites <- 100 #Number of sites
N <- 300 #Number of samples (3 per site)

x <- runif(N,-1,1) #Single dependent variable "x"
fixMM <- model.matrix(~x) #Model matrix for fixed effects
fixCoefs <- c(0,1) #Fixed effect coefficients
sites <- rep(factor(1:Nsites),length.out=N) #Assign to sites 

#Spatial random effect (from "volcano" elevation dataset)
volcano <- (volcano-mean(volcano))/sd(volcano) #Rescale elevation values for model
s <- data.frame(z = as.vector(volcano),
                x = as.vector(row(volcano)),
                y = as.vector(col(volcano)))
s <- s[sample(nrow(s),Nsites),] #Sample 26 sites
s$siteName <- factor(1:Nsites) #Label sites

#Plot data from sites
volcano.data <- array(NA, dim(volcano))
volcano.data[cbind(s$x, s$y)] <- s$z
image(volcano.data, main="Spatial data", useRaster=TRUE)

#Generate data from "fixed" and site effect
fixedEff <- fixMM %*% fixCoefs #Fixed effect
siteEff <- model.matrix(~sites-1) %*% s$z #Site effect
yhat <- fixedEff + siteEff #Predicted value
y <- yhat + rnorm(N,0,0.5) #Data (Residual SD = 0.5)

#Add spatial coordinates as E and N
siteE <- model.matrix(~sites-1) %*% s$x 
siteN <- model.matrix(~sites-1) %*% s$y 

#Set up spatial data for use in glmmTMB model
siteCoords <- numFactor(siteE,siteN) #Coordinates
siteGroup <- factor(rep(1,N)) #Group

#Assemble dataframe
dat <- data.frame(y,x,sites,siteE,siteN,siteCoords,siteGroup) 

#Model generated data

#Model using only site ID
mod1 <- glmmTMB(y~1+(1|sites),data=dat,family=gaussian())

#Model using site location
mod2 <- glmmTMB(y~1+exp(siteCoords + 0|siteGroup),data=dat,family=gaussian())

#How do models do at estimating fixed effects (slope = 1)

#Both are essentially identical
data.frame(NonSpatial=confint(mod1)[2,],Spatial=confint(mod2)[2,])

#How do models do at estimating spatial random effect?
checkRE <- data.frame(s,ranefM1=ranef(mod1)[[1]][[1]][,1],
                     ranefM2=unlist(ranef(mod2)[[1]][[1]]))

par(mfrow=c(1,2)) #Spatial model does a poor job
with(checkRE,plot(z,ranefM1,xlab='Actual',ylab='Estimated',main='Non-spatial RE')); abline(0,1)
with(checkRE,plot(z,ranefM2,xlab='Actual',ylab='Estimated',main='Spatial RE')); abline(0,1)
par(mfrow=c(1,1))

#Can the spatial model reconstruct "volcano" image?
predict_col <- function(i) { #Function to construct 1 column at a time (more efficient)
  newdata <- data.frame( siteCoords = numFactor(expand.grid(x=i,y=1:nrow(volcano)))) #i=cols,j=rows
  newdata$siteGroup <- factor(rep(1,nrow(newdata)))
  newdata$x <- 0 #Marginalize across x
  predict(mod2, newdata=newdata, type="response", allow.new.levels=TRUE)
}

#Takes a few minutes to predict all responses. Weirdly slow for Matern and Gaussian
pred <- sapply(1:ncol(volcano),predict_col)

pred<- predict_col(1)  #Takes a few minutes to predict all responses. Weirdly slow for Matern and Gaussian
for(i in 2:ncol(volcano)){
  pred <- cbind(pred,predict_col(i))
  print(paste('Col',i,'/',ncol(volcano),'Complete'))
}

#Plot predictions + errors
par(mfrow=c(2,2))

#Original data
image(volcano,main='Original')

#Data from sites
volcano.data <- array(NA, dim(volcano))
volcano.data[cbind(s$x, s$y)] <- s$z
image(volcano.data, main="Sampled data", useRaster=TRUE)

#Plot data from predictions
image(pred, main="Predictions from mod2", useRaster=TRUE)

#How wrong are the predictions?
error.data <- abs(volcano-pred)
image(error.data, main="abs(Error)", useRaster=TRUE)

par(mfrow=c(1,1))