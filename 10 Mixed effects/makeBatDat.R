#Make mixed effect model example data

setwd("~/Projects/Stats projects/ecoStats2020/10 Mixed effects")

set.seed(123)

#Generate data
n <- 150

#Fixed effects
species <- sample(c('Spp1','Spp2','Spp3'),n,TRUE) #Species
forest <- rbeta(n,0.5,0.5)*100 #Forest cover %
fixefMat <- model.matrix(~species*forest-1) #Fixed effects matrix

fixCoef <- c(35,20,50,0.01,0.05,-0.15)

fixefMat %*% fixCoef

#Random effects

#Sampling sites
sites <- sample(c('Wellwick', 'Falconport', 'Greenelf', 'Riverbridge', 'Highhedge', 'Mallowholt', 'Blueglass', 'Snowbeach', 'Brightdale', 'Iceapple', 'Lorwald', 'Violetborough', 'Stonepine', 'Icebarrow', 'Lincastle'),n,TRUE)
nsites <- length(unique(sites))

#Variance components
sigmaR <- 3 #Residual sigma 
sigmaG <- 5 #Group intercept sigma
sigmaG_slope <- 0.03 #Slope sigma

#Correlated intercepts and slopes, using Choleski matrices
raneffs <- matrix(rnorm(nsites*2,0,1),ncol=2) #Uncorrelated unit normals
slopeCor <- -0.7 #Intercept-slope correlation
corMat <- matrix(c(1,slopeCor,slopeCor,1),ncol=2) #Correlation matrix
cholCorMat <- chol(corMat) #Choleski transform of corMat
raneffs <- raneffs %*% cholCorMat #Induces correlation in slopes
raneffs <- raneffs * matrix(rep(c(sigmaG,sigmaG_slope),each=nsites),ncol=2,
                            dimnames=list(letters[1:nsites],c('Int','Slope'))) #Changes SD for each column
raneff_int <- model.matrix(~sites-1) %*% raneffs[,1] #Intercept vector
raneff_slope <- forest * model.matrix(~sites-1) %*% raneffs[,2]  #Slope vector

yhat <- fixefMat %*% fixCoef + raneff_int + raneff_slope  #Expected value
mass <- rnorm(n,yhat,sigmaR) #Data
dat <- data.frame(mass,species,forest,sites) #Assemble into data frame
write.csv(dat,file='batMass.csv')

#Check data

library(tidyverse)
theme_set(theme_classic())
library(lme4)

ggplot(dat,aes(x=forest,y=mass,col=species))+geom_point()+geom_smooth(method='lm')

#Fit model
dat <- dat %>% mutate(sForest=scale(forest))

m0 <- lm(mass~species*forest-1,data=dat)
m1 <- lmer(mass~species*forest-1+(1|sites),data=dat)
m2 <- lmer(mass~species*forest-1+(forest|sites),data=dat)
m3 <- lm(mass~species*forest+sites-1,data=dat) #"My supervisor told me to"
m4 <- lm(mass~species*forest+forest*sites-1,data=dat) #"My supervisor told me to 2"

# m0 <- lm(mass~species*sForest-1,data=dat)
# m1 <- lmer(mass~species*sForest-1+(1|sites),data=dat)
# m2 <- lmer(mass~species*sForest-1+(sForest|sites),data=dat)
# m3 <- lm(mass~species*sForest+sForest*sites-1,data=dat) #"My supervisor told me to"
# m4 <- lm(mass~sForest*sites+species*sForest-1,data=dat) #"My supervisor told me to 2"

with(dat,xtabs(~sites+species))
with(dat,xtabs(~sites)) #If any of the sites have 1 record only, you'll get a singularity warning

getCoefs <- function(mod,mult=1.96){ #Extract fixed effects coefficients + SEs
  if(class(mod)=='lmerMod'){
    est <- fixef(mod)
  } else {
    est <- coef(mod)
  }
  se <- sqrt(diag(vcov(mod)))
  upr <- est+se*mult
  lwr <- est-se*mult
  d <- data.frame(est,se,upr,lwr)
  return(d)
}

modList <- list(m0=m0,m1=m1,m2=m2,m3=m3,m4=m4)

modLabels <- sapply(modList,function(x){ #Get labels from model formula
  f <- as.character(formula(x))
  f <- paste(f[2],f[1],f[3],collapse='')
  f <- gsub(' - 1','',f)
  f <- gsub('\\s','',f)
})

#Plot of coefficients
p1 <- lapply(modList,getCoefs) %>% do.call('rbind',.) %>% 
  rownames_to_column('coef') %>% 
  separate(coef,c('mod','coef'),sep='\\.') %>% 
  filter(!grepl('sites',coef)) %>% 
  bind_rows(.,data.frame(mod='actual',coef=names(coef(m0)),est=fixCoef,se=0,upr=NA,lwr=NA)) %>% 
  mutate(mod=factor(mod,levels=rev(unique(mod)),labels=c('actual',rev(modLabels))),
         coef=factor(coef,levels=names(coef(m0)))) %>% 
  ggplot(aes(x=mod,y=est))+
  geom_pointrange(aes(ymax=upr,ymin=lwr,col=ifelse(mod=='actual','a','b')),
                  position=position_dodge(width=0.5),show.legend = FALSE)+
  geom_hline(yintercept=0,linetype='dashed')+
  facet_wrap(~coef,scales='free')+
  labs(y='Estimate',x='Model')+
  scale_colour_manual(values=c('red','black'))+
  coord_flip()

#Plot of predictions
library(ggeffects)

#Get dataframe of predictions from models
getPreds <- function(mod,t=c('forest','species')){ 
  ggpredict(mod,terms=t) %>% data.frame(.)
}

#Model predictions
modPreds <- do.call('rbind',lapply(modList,getPreds)) %>% 
  rownames_to_column('mod') %>% 
  separate(mod,c('mod','temp'),sep='\\.') %>% select(-temp) %>% 
  mutate(mod=factor(mod,labels=modLabels))

#Colours
colValues <- c('red','purple','blue')

#Plot of model predictions vs actual 
p2 <- ggplot(modPreds,aes(x=x,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low,fill=group),alpha=0.3)+
  geom_line(aes(col=group),size=1)+
  geom_line(data=data.frame(forest,species,pred=fixefMat %*% fixCoef),aes(x=forest,y=pred,group=species),col='black',size=1)+
  facet_wrap(~mod)+
  labs(y='mass',x='forest',col='Species',fill='Species')+
  scale_fill_manual(values=colValues)+scale_colour_manual(values=colValues)

ggsave('coefPlot.png',p1,height=8,width=12)
ggsave('predPlot.png',p2,height=8,width=12)
