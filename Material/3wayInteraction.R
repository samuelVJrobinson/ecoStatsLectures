#DEMO FOR FITTING AND PLOTTING 3-WAY CONTINOUS INTERACTIONS

#Packages
library(MASS)
library(effects)
library(ggplot2)
library(rgl)

#Redo theme for ggplot
prestheme=theme(legend.position='right',
                legend.text=element_text(size=10),
                axis.text=element_text(size=10), 
                axis.title=element_text(size=20),
                title=element_text(size=20),
                axis.line.x=element_line(colour='black'),
                axis.line.y=element_line(colour='black'),
                strip.text=element_text(size=15))  
theme_set(theme_bw()+prestheme) #Sets graph theme to B/Ws + prestheme


#Generate fake landscape/body size data
set.seed(1)
n=5000 #Number of points to generate
bodySize=runif(n,2,8) #Uniform
landscape=rbeta(n,1.5,2) #Beta
local=rbeta(n,5,2) #Beta
date=rep(1:10,each=n/10) #Collection date (or some other term you aren't interested in)

fakedata=data.frame(bodySize,landscape,local,date)
rm(bodySize,landscape,local,date)

#Model terms
int=1 #Intercepts
bs=-0.1
ls=0.1
lo=0.5
bs_ls= 1 #Interactions
bs_lo=-0.2
ls_lo=0.2
bs_ls_lo=0.5
#Date (hump-shaped reponse)
datefun=function(date) (-0.01*(1-date)*(10-date))

#Lambda coefficients for poisson
lambda=with(fakedata,int+(bodySize*bs)+(landscape*ls)+(local*lo)+
       (bodySize*landscape*bs_lo)+(bodySize*local*bs_lo)+(landscape*local*ls_lo)+
       (bodySize*landscape*local*bs_ls_lo)+datefun(date))

#Generate fake response data
fakedata$count=rpois(n,exp(lambda))

#Fit model
mod1=glm(count~bodySize*landscape*local+poly(date,2,raw=TRUE),data=fakedata,maxit=25,family='quasipoisson')
summary(mod1)

#Extract partial effects (marginal models)
mod1eff=effect(term="bodySize:landscape:local",mod=mod1,xlevels=list(bodySize=2:8,landscape=c(0.2,0.5,0.8),local=c(0.2,0.5,0.8)))
mod1eff=data.frame(mod1eff) #Convert to df
#Convert landscape and local predictors to factors
mod1eff$landscape=cut(mod1eff$landscape,breaks=3,labels=c('Landscape:Low','Landscape:Med','Landscape:High')) 
mod1eff$local=cut(mod1eff$local,breaks=3,labels=c('Local:Low','Local:Med','Local:High'))

#Options for plotting in ggplot2:

#Straightforward faceted partial effects plot
(p=ggplot(mod1eff,aes(bodySize,fit))+
  geom_ribbon(aes(ymax=upper,ymin=lower),alpha=0.3)+
  geom_line(size=1)+facet_grid(landscape~local)+
  labs(x='Body size',y='Count',col='Landscape'))

#Version with data included (Ralph won't like this, because it displays raw data without subtracting Date effect ("marginalized" or "adjusted" data), but I like these better because "normal" people can immediately understand them)
tempdat=fakedata[seq(1,nrow(fakedata),10),] #Use 1/10th of the data
tempdat$landscape=cut(tempdat$landscape,breaks=3,labels=c('Landscape:Low','Landscape:Med','Landscape:High'))
tempdat$local=cut(tempdat$local,breaks=3,labels=c('Local:Low','Local:Med','Local:High'))
p+geom_point(data=tempdat,aes(y=count))+ylim(0,8)

#Version with "date adjusted" (marginalized) response - annoying, but do-able
modmat=model.matrix(mod1) #Model matrix
modmat[,'poly(date, 2, raw = TRUE)1']=mean(fakedata$date) #Set date to mean value
modmat[,'poly(date, 2, raw = TRUE)2']=mean(fakedata$date^2) #Set date^2 to mean value
mu=(modmat%*%coef(mod1))+resid(mod1) #Marginalized values (on link scale)
tempdat$margCount=exp(mu)[seq(1,nrow(fakedata),10),] #Marginalized count data
p+geom_point(data=tempdat,aes(y=margCount))+ylim(0,8)+labs(y='Marginalized Count')

#Version with multiple colours for different landscapes
mod1eff$landscape=factor(mod1eff$landscape,labels=c('Low','Med','High'))

ggplot(mod1eff,aes(bodySize,fit,col=landscape))+
  geom_ribbon(aes(ymax=upper,ymin=lower,fill=landscape,col=NULL),alpha=0.3)+
  geom_line(size=1)+facet_grid(~local)+
  labs(x='Body size',y='Count',col='Landscape',fill='Landscape')+
  scale_colour_manual(values=c('red','purple','blue'))+
  scale_fill_manual(values=c('red','purple','blue'))

#Plotting in 3D:
mod1eff=data.frame(effect(term="bodySize:landscape:local",mod=mod1,
                          xlevels=list(bodySize=c(2,6,8),landscape=seq(0,1,0.05),local=seq(0,1,0.05))))
with(mod1eff,plot3d(local,landscape,fit,type='n',col='black',size=0.1,xlab='Landscape',ylab='Local',zlab='Count')) #Empty plot

landscapeVals=seq(0,1,0.05)
localVals=seq(0,1,0.05)
with(subset(mod1eff,bodySize==2),surface3d(landscapeVals,localVals,matrix(fit,ncol=length(landscapeVals)),alpha=0.75,col='blue'))
with(subset(mod1eff,bodySize==6),surface3d(landscapeVals,localVals,matrix(fit,ncol=length(landscapeVals)),alpha=0.75,col='purple'))
with(subset(mod1eff,bodySize==8),surface3d(landscapeVals,localVals,matrix(fit,ncol=length(landscapeVals)),alpha=0.75,col='red'))
legend3d('topleft',c('Small','Medium','Large'),fill=c('blue','purple','red'),title='Body Size')

