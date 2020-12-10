#Make mixed effect model example data

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

#Plot of coefficients
lapply(modList,getCoefs) %>% do.call('rbind',.) %>% 
  rownames_to_column('coef') %>% 
  separate(coef,c('mod','coef'),sep='\\.') %>% 
  filter(!grepl('sites',coef)) %>% 
  bind_rows(.,data.frame(mod='actual',coef=names(coef(m0)),est=fixCoef,se=0,upr=NA,lwr=NA)) %>% 
  ggplot(aes(x=mod,y=est))+
  geom_pointrange(aes(ymax=upr,ymin=lwr,col=ifelse(mod=='actual','a','b')),
                  position=position_dodge(width=0.5),show.legend = FALSE)+
  geom_hline(yintercept=0,linetype='dashed')+
  facet_wrap(~coef,nrow=1,scales='free_y')+
  scale_colour_manual(values=c('red','black'))

meanAt <- function(mat,cols=NA){ #Take mean of only certain matrix columns
  if(length(cols==1)&&is.na(cols)) return(mat)
  cMeans <- matrix(rep(apply(mat[,cols],2,mean),each=nrow(mat)),nrow=nrow(mat))
  mat[,cols] <- cMeans
  return(mat)
}

meanAt(head(model.matrix(m3)),which(grepl('sites',names(coef(m3)))))

#Plot of predictions - would be better with standard errors
data.frame(species,forest,
  actual=fixefMat %*% fixCoef,
  m0=predict(m0),
  m1=predict(m1,re.form=~0),m2=predict(m2,re.form=~0),
  m3=meanAt(model.matrix(m3),which(grepl('sites',names(coef(m3))))) %*% coef(m3),
  m4=meanAt(model.matrix(m4),which(grepl('sites',names(coef(m4))))) %*% coef(m4)
  ) %>% 
  arrange(species,forest) %>% 
  pivot_longer(m0:m4) %>% 
  ggplot(aes(x=forest,y=value,col=species))+geom_line()+
  geom_line(aes(y=actual,group=species),col='black',linetype='dashed')+
  facet_wrap(~name)


