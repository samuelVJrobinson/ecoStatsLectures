library(tidyverse)
library(vegan)
theme_set(theme_classic())

#Function to make screeplots from prcomp
varPlot <- function(pcmod,total=FALSE){
  vars <- pcmod$sdev^2 #Eigenvalues (variance per column)
  vars <- 100*vars/sum(vars)
  if(total) vars <- cumsum(vars)
  data.frame(varPerc=vars,pc=1:length(vars)) %>% 
    ggplot(aes(x=pc,y=varPerc))+geom_point()+geom_line()+
    labs(x="PC",y=ifelse(total,"Total percent variance","Percent variance")) 
}

# Bird data -------------------

birdDat <- read.csv('./07 Multivariate models/birds.csv')
birdMat <- birdDat[,3:7] #Selects only data columns
birdPCA <- prcomp(birdMat,scale. = TRUE) #Principal components (scale each column)

var <- birdPCA$sdev^2 #Get variance for each PC (eigenvalues)
propVar <- var/sum(var) #Change to proportion total variance

#Screeplot
data.frame(n=1:length(birdPCA$sdev),var=propVar) %>% 
  ggplot(aes(x=n,y=var))+geom_point()+geom_line()+
  labs(x='PC',y='Prop Var')

birdPCA$x[,1:2] %>% data.frame() %>% 
  mutate(Survived=birdDat$Survived) %>% 
  ggplot(aes(x=PC1,y=PC2,col=Survived))+
  geom_point()+
  labs(x='PC1 (86%)',y='PC2 (11%)')

data.frame(PC1=birdPCA$x[,1])  %>% 
  mutate(Survived=birdDat$Survived) %>% 
  ggplot(aes(x=PC1))+geom_histogram()+
  facet_wrap(~Survived,ncol=1)

# Bug data ----------------------

bugdat <- read.csv('./07 Multivariate models/bugDat.csv') 
bugPreds <- bugdat %>% select(Date:Method) %>% 
  mutate(Method=gsub('Bowl Trap ','',Method)) %>% 
  mutate(Method=ifelse(Method=='Netting Control','Net',Method)) %>% 
  mutate(across(everything(),as.factor))
bugMat <- bugdat %>% select(-Date:-Method) %>% as.matrix()
zeroRows <- which(apply(bugMat,1,sum)==0)
bugMat <- bugMat[-zeroRows,]
bugPreds <- bugPreds[-zeroRows,]

bugPCA <- prcomp(bugMat,scale. = TRUE) #Fit PCA (scaled data)
bugFit <- envfit(bugPCA ~ Method, data=bugPreds)

varPlot(bugPCA) #Screeplot - high dimensionality, so 2D plot may not be the best!
bugFit #Strong difference between trapping types

#Make a plot of the group centroids + decomposed data
bugFitCent <- data.frame(names=gsub('Method','',rownames(bugFit$factors$centroids)),bugFit$factors$centroids)

var <- bugPCA$sdev^2 #Get PC variance
var <- 100*var[1:2]/sum(var) #Proportion variance of first 2 PCs
var <- paste0(c('PC1','PC2'),' (',round(var,1),'%)') #Create axis labels

data.frame(bugPCA$x[,1:2]) %>% 
  bind_cols(bugPreds) %>% 
  ggplot(aes(x=PC1,y=PC2,col=Method))+geom_point()+
  geom_text(data=bugFitCent,aes(x=PC1,y=PC2,label=names),col='black')+
  labs(x=var[1],y=var[2])

#Same thing, but plot using NMDS 
bugMDS <- metaMDS(bugMat,k=2) #Stress ~ 0.15, so OK
bugFit <- envfit(bugMDS ~ Method, data=bugPreds) #Same results, but in NMDS distance rather than PC

bugFitCent <- data.frame(names=gsub('Method','',rownames(bugFit$factors$centroids)),bugFit$factors$centroids)

#Net samples cluster away from Control/OTC methods
data.frame(bugMDS$points) %>% 
  bind_cols(bugPreds) %>% 
  ggplot(aes(x=MDS1,y=MDS2,col=Method))+geom_point()+
  geom_text(data=bugFitCent,aes(x=NMDS1,y=NMDS2,label=names),col='black')

#What species are found more in nets vs other methods?
data.frame(bugMDS$species) %>% 
  mutate(spp=gsub('.spp','',rownames(bugMDS$species))) %>% 
  ggplot(aes(x=MDS1,y=MDS2))+
  geom_text(aes(label=spp),size=3)+
  geom_label(data=bugFitCent,aes(x=NMDS1,y=NMDS2,label=names),col='red')
#Not the best figure, but shows that netting is biased towards Bombus (bumblebees) and hoverflies, while bowl traps tend to catch more flies and butterflies  
  
# Water data (courtesy of Abi) -------------------------------------

#This section uses some of the default `vegan` plotting functions

#Field water
fWatDat <- read.csv('./07 Multivariate models/fieldWater.csv')
fWatMat <- fWatDat[,4:9] #Choose only data columns
chooseThese <- complete.cases(fWatMat) #Which rows have no NAs?
fWatMat <- fWatMat[chooseThese,] #Remove NAs
fWatDat <- fWatDat[chooseThese,] #Remove NAs
fWatPCA <- prcomp(fWatMat, scale. = TRUE)
varPlot(fWatPCA)

#How does treatment change field water?
ordiplot(fWatPCA,display = 'site',type = 'point')
ordihull(fWatPCA,fWatDat$Treatment,col=1:4) #Looks like some treatments are different than others

# ordiplot(fWatPCA,display = 'species',type = 'text',ylim=c(-2,2),xlim=c(-2,2))
# ordihull(fWatPCA,fWatDat$Treatment,col=1:4) #Looks like some treatments are different than others

#How does depth change field water?
ordiplot(fWatPCA,display = 'site',type = 'point')
ordihull(fWatPCA,fWatDat$Depth,col=1:4) #Looks like both are about the same

envfit(fWatPCA ~ Treatment + Depth, data=fWatDat) #Large difference between treatments, but not depth

#Nutrient
nWatDat <- read.csv('./07 Multivariate models/nutrientWater.csv')
nWatMat <- nWatDat[,-1:-3] #Choose only data columns
chooseThese <- is.na(nWatMat) #Which data points are NA?
nWatMat[chooseThese] <- 0 #Replace NAs with zeros
nWatPCA <- prcomp(nWatMat,scale. = TRUE)
varPlot(nWatPCA)

#How does treatment change nutrient water?
ordiplot(nWatPCA,display = 'site',type = 'point')
ordihull(nWatPCA,nWatDat$Treatment,col=1:4)
#Some extreme values are present. Which points are they?

#NOTE: vegan treats columns and rows as "species" and "sites", respectively
ordiplot(nWatPCA,display = 'sites',type = 'text') #Looks like site34 (i.e. row 34) is very different
nWatDat[30:35,] #Very high K values at this site

ordiplot(nWatPCA,display = 'sites',type='none') #Blank plot
points(nWatPCA$x,cex=0.8) #Add sites (i.e. samples) to plot
#Add rotation vectors (i.e. PCs) to plot. Looks like "weird" site is very Al, PO4, or Zn heavy? Other sites in bottom-left cluster towards TOC, Na, and Ca
arrows(x0=0,y0=0,x1 = nWatPCA$rotation[,1]*30,y1=nWatPCA$rotation[,2]*30,length = 0.1,col='red') 
text(nWatPCA$rotation*33,labels=rownames(nWatPCA$rotation),col='red',cex=0.7)






