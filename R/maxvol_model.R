##Aechmea mertensii maxvol estimation

mertensii<-read.csv("~/Desktop/Allometry/mertensii.csv",header=TRUE, sep=";")
##rename natural log transformed variables
mertensii$logNL<-log(mertensii$NL)
mertensii$logDiam1<-log(mertensii$Diam1)
mertensii$logVmax<-log(mertensii$Vmax)

##compare linear vs polynomial model
a0<-lm(logVmax~logNL,data=mertensii)
a1<-lm(logVmax~poly(logDiam1,1)+poly(logNL,1),data=mertensii)#linear
#second order polynomial
a2<-glm(logVmax~poly(logDiam1,2)+poly(logNL,2),data=mertensii)
a2.lm<-lm(logVmax~poly(logDiam1,2)+poly(logNL,2),data=mertensii)#to get R2
a3<-glm(logVmax~poly(logDiam1,3)+poly(logNL,3),data=mertensii)#third order polynomial
a4<-glm(logVmax~poly(logDiam1,2)+poly(logNL,2)-1,data=mertensii)#second order polynomial force intercept to 0
AIC(a0,a1,a2,a2.lm,a3,a4)##

a6<-lm(logVmax~logDiam1+logNL,data=mertensii)#linear


#model formula
anova(a1,test="F")
a1

AIC(a1,a6)#a6 better

summary(a6)
a6

##plot selected model on log-log scale
par(mfrow=c(1,1),omi=c(1,1,1,1),mar=c(5,6,6,5)*.5)
plot(mertensii$logNL,mertensii$logVmax,ylim=c(0,(max(mertensii$logVmax)*1.2)),xlab="",ylab="")
xy<-seq(min(mertensii$logNL),max(mertensii$logNL),length=100)
pNL.a6<-predict(a6,data.frame(logNL=xy,logDiam1=rep(mean(mertensii$logDiam1),length(xy))))
lines(xy,pNL.a1)
mtext("Ln Number of leaves",side=1,line=2.5,cex=1.2)
mtext("Ln Max volume (mL)",side=2,line=2.5,cex=1.2)

plot(mertensii$logDiam1,mertensii$logVmax,ylim=c(0,(max(mertensii$logVmax)*1.2)),xlab="",ylab="")
xv<-seq(min(mertensii$logDiam1),max(mertensii$logDiam1),length=100)
pDiam.a6<-predict(a1,data.frame(logDiam1=xv,logNL=rep(mean(mertensii$logNL),length(xv))))
lines(xv,pDiam.a6)
mtext("Ln Diameter (cm)",side=1,line=2.5,cex=1.2)
mtext("Ln Max volume (mL)",side=2,line=2.5,cex=1.2)


#plot selected model with actual response values
par(mfrow=c(1,1),omi=c(1,1,1,1),mar=c(5,6,6,5)*.5)
plot(exp(mertensii$logNL),exp(mertensii$logVmax),ylim=c(0,(max(exp(mertensii$logVmax))*1.2)),xlab="",ylab="")
lines(exp(xy),exp(pNL.a6))
mtext("Number of leaves",side=1,line=2.5,cex=1.2)
mtext("Max volume (mL)",side=2,line=2.5,cex=1.2)

plot(exp(mertensii$logDiam6),exp(mertensii$logVmax),ylim=c(0,(max(exp(mertensii$logVmax))*1.2)),xlab="",ylab="")
lines(exp(xv),exp(pDiam.a6))
mtext("Diameter (cm)",side=1,line=2.5,cex=1.2)
mtext("Max volume (mL)",side=2,line=2.5,cex=1.2)


#model with Diam only

a5<-lm(logVmax~logDiam1,data=mertensii)
summary(a5)
AIC(a1,a5)
a5





####Vriesea splendens maxvol estimation###
##start with 2 datasets
vriesea<-read.csv("~/Desktop/Allometry/vriesea.csv",header=TRUE, sep=";")
vriesea_prod<-read.csv("~/Desktop/Allometry/vriesea_prod.csv",header=TRUE, sep=";")
##merge datasets
vriesea$dataset<-1 #datafrom droughtexpt
vriesea_prod$dataset<-2 #data from production
vriesea_all<-rbind(vriesea,vriesea_prod)
sort(vriesea_all$BromeliadID)

##rename natural log transformed variables
vriesea_all$logNL<-log(vriesea_all$NL)
vriesea_all$logDiam1<-log(vriesea_all$Diam1)
vriesea_all$logVmax<-log(vriesea_all$Vmax)

##test allometry with Diameter1 and Number of leaves
#a1<-lm(log(Vmax)~log(Diam1)+log(NL),data=vriesea_all)
a1<-lm(logVmax~logDiam1+logNL,data=vriesea_all)
anova(a1,test="F")
summary(a1)

##compare linear vs polynomial model
a1<-glm(logVmax~poly(logDiam1,1)+poly(logNL,1),data=vriesea_all)#linear
#second order polynomial
a2<-glm(logVmax~poly(logDiam1,2)+poly(logNL,2),data=vriesea_all)
a2.lm<-lm(logVmax~poly(logDiam1,2)+poly(logNL,2),data=vriesea_all)#to get R2
a3<-glm(logVmax~poly(logDiam1,3)+poly(logNL,3),data=vriesea_all)#third order polynomial
a4<-glm(logVmax~poly(logDiam1,2)+poly(logNL,2)-1,data=vriesea_all)#second order polynomial force intercept to 0
AIC(a1,a2,a2.lm,a3,a4)


##plot selected model on log-log scale
par(mfrow=c(2,1),omi=c(1,1,1,1),mar=c(5,6,6,5)*.5)
plot(vriesea_all$logNL,vriesea_all$logVmax,ylim=c(0,(max(vriesea_all$logVmax)*1.2)),xlab="",ylab="")
xy<-seq(min(vriesea_all$logNL),max(vriesea_all$logNL),length=100)
#pNL.a1<-predict(a1,data.frame(NL=xy,Diam1=rep(mean(vriesea_all$Diam1),length(xy))),type="response")
pNL.a1<-predict(a2,data.frame(logNL=xy,logDiam1=rep(mean(vriesea_all$logDiam1),length(xy))))
lines(xy,pNL.a1)
mtext("Ln Number of leaves",side=1,line=2.5,cex=1.2)
mtext("Ln Max volume (mL)",side=2,line=2.5,cex=1.2)

plot(vriesea_all$logDiam1,vriesea_all$logVmax,ylim=c(0,(max(vriesea_all$logVmax)*1.2)),xlab="",ylab="")
xv<-seq(min(vriesea_all$logDiam1),max(vriesea_all$logDiam1),length=100)
pDiam.a1<-predict(a1,data.frame(logDiam1=xv,logNL=rep(mean(vriesea_all$logNL),length(xv))))
lines(xv,pDiam.a1)
mtext("Ln Diameter (cm)",side=1,line=2.5,cex=1.2)
mtext("Ln Max volume (mL)",side=2,line=2.5,cex=1.2)

#plot selected model with actual response values
par(mfrow=c(2,1),omi=c(1,1,1,1),mar=c(5,6,6,5)*.5)
plot(exp(vriesea_all$logNL),exp(vriesea_all$logVmax),ylim=c(0,(max(exp(vriesea_all$logVmax))*1.2)),xlab="",ylab="")
lines(exp(xy),exp(pNL.a1))
mtext("Number of leaves",side=1,line=2.5,cex=1.2)
mtext("Max volume (mL)",side=2,line=2.5,cex=1.2)

plot(exp(vriesea_all$logDiam1),exp(vriesea_all$logVmax),ylim=c(0,(max(exp(vriesea_all$logVmax))*1.2)),xlab="",ylab="")
lines(exp(xv),exp(pDiam.a1))
mtext("Diameter (cm)",side=1,line=2.5,cex=1.2)
mtext("Max volume (mL)",side=2,line=2.5,cex=1.2)


########################################################################################

####Aechmea aquilega maxvol estimation###
##start with 2 datasets
aquilega<-read.csv("~/Desktop/Allometry/aquilega_Biog.csv",header=TRUE, sep=";")
aquilegaKT<-read.csv("~/Desktop/Allometry/aquilegaKT.csv",header=TRUE, sep=";")
##merge datasets
aquilega$dataset<-1 #datafrom biogeogr expt
aquilegaKT$dataset<-2 #data KT algae expt
aquilega_all<-rbind(aquilega,aquilegaKT)
sort(aquilega_all$ID)

##rename natural log transformed variables
aquilega_all$logNL<-log(aquilega_all$NL)
aquilega_all$logDiam1<-log(aquilega_all$Diam1)
aquilega_all$logVmax<-log(aquilega_all$Vmax)

##test allometry with Diameter1 and Number of leaves
#a1<-lm(log(Vmax)~log(Diam1)+log(NL),data=aquilega_all)
a1<-lm(logVmax~poly(logDiam1,2)+poly(logNL,2),data=aquilega_all)
anova(a1,test="F")
summary(a1)

##compare linear vs polynomial model
a1<-glm(logVmax~poly(logDiam1,1)+poly(logNL,1),data=aquilega_all)#linear
a1.lm<-lm(logVmax~poly(logDiam1,1)+poly(logNL,1),data=aquilega_all)#to get R2
a2<-glm(logVmax~poly(logDiam1,2)+poly(logNL,2),data=aquilega_all)#second order polynomial
a3<-glm(logVmax~poly(logDiam1,3)+poly(logNL,3),data=aquilega_all)#third order polynomial
a4<-glm(logVmax~poly(logDiam1,2)+poly(logNL,2)-1,data=aquilega_all)#second order polynomial force intercept to 0
AIC(a1,a2,a3,a4)

##> AIC(a1,a2,a3) - example
#a1 selected >2 less than a1 (rule of the thumb)

a5<-lm(logVmax~logDiam1+logNL,data=aquilega_all)#linear
summary(a5)
AIC(a1,a5)


##plot selected model on log-log scale
par(mfrow=c(2,1),omi=c(1,1,1,1),mar=c(5,6,6,5)*.5)
plot(aquilega_all$logNL,aquilega_all$logVmax,ylim=c(0,(max(aquilega_all$logVmax)*1.2)),xlab="",ylab="")
xy<-seq(min(aquilega_all$logNL),max(aquilega_all$logNL),length=100)
#pNL.a1<-predict(a1,data.frame(NL=xy,Diam1=rep(mean(aquilega_all$Diam1),length(xy))),type="response")
pNL.a5<-predict(a5,data.frame(logNL=xy,logDiam1=rep(mean(aquilega_all$logDiam1),length(xy))))
lines(xy,pNL.a5)
mtext("Ln Number of leaves",side=1,line=2.5,cex=1.2)
mtext("Ln Max volume (mL)",side=2,line=2.5,cex=1.2)

plot(aquilega_all$logDiam1,aquilega_all$logVmax,ylim=c(0,(max(aquilega_all$logVmax)*1.2)),xlab="",ylab="")
xv<-seq(min(aquilega_all$logDiam1),max(aquilega_all$logDiam1),length=100)
pDiam.a5<-predict(a1,data.frame(logDiam1=xv,logNL=rep(mean(aquilega_all$logNL),length(xv))))
lines(xv,pDiam.a5)
mtext("Ln Diameter (cm)",side=1,line=2.5,cex=1.2)
mtext("Ln Max volume (mL)",side=2,line=2.5,cex=1.2)

#plot selected model with actual response values
par(mfrow=c(1,1),omi=c(1,1,1,1),mar=c(5,6,6,5)*.5)
plot(exp(aquilega_all$logNL),exp(aquilega_all$logVmax),ylim=c(0,(max(exp(aquilega_all$logVmax))*1.2)),xlab="",ylab="")
lines(exp(xy),exp(pNL.a5))
mtext("Number of leaves",side=1,line=2.5,cex=1.2)
mtext("Max volume (mL)",side=2,line=2.5,cex=1.2)

plot(exp(aquilega_all$logDiam1),exp(aquilega_all$logVmax),ylim=c(0,(max(exp(aquilega_all$logVmax))*1.2)),xlab="",ylab="")
lines(exp(xv),exp(pDiam.a5))
mtext("Diameter (cm)",side=1,line=2.5,cex=1.2)
mtext("Max volume (mL)",side=2,line=2.5,cex=1.2)







##notes on other models explored Vriesea splendens

######test allometry with mean diameter and Number of leaves
vriesea$meandiam<-(vriesea$Diam1+vriesea$Diam2)/2
vriesea
a2<-lm(log(Vmax)~log(meandiam)+log(NL),data=vriesea)
anova(a2,test="F")
summary(a2)
a2$residuals

##test allometry with Diameter1 only - reduction r2?
a3<-lm(log(Vmax)~log(Diam1),data=vriesea)
anova(a3,test="F")
summary(a3)

##examine residuals - model fit a1
par(mfrow=c(2,1))
plot((vriesea_all$logDiam1),a1$residuals)
abline(h=0)
plot((vriesea_all$logNL),a1$residuals)
abline(h=0)

##examine residuals - model fit a2
par(mfrow=c(2,1))
plot(log(vriesea$meandiam),a2$residuals)
abline(h=0)
plot(log(vriesea$NL),a2$residuals)
abline(h=0)

##Guzmania Puerto Rico

guzmania<-read.csv("~/Desktop/Allometry/Guzmania_PR.csv",header=TRUE, sep=";")
##rename natural log transformed variables
guzmania$logNL<-log(guzmania$NL)
guzmania$logDiam1<-log(guzmania$Diam1)
guzmania$logVmax<-log(guzmania$Vmax)

##compare linear vs polynomial model
a0<-lm(logVmax~logNL,data=guzmania)
a1<-lm(logVmax~poly(logDiam1,1)+poly(logNL,1),data=guzmania)#linear
#second order polynomial
a2<-glm(logVmax~poly(logDiam1,2)+poly(logNL,2),data=guzmania)
a2.lm<-lm(logVmax~poly(logDiam1,2)+poly(logNL,2),data=guzmania)#to get R2
a3<-glm(logVmax~poly(logDiam1,3)+poly(logNL,3),data=guzmania)#third order polynomial
a4<-glm(logVmax~poly(logDiam1,2)+poly(logNL,2)-1,data=guzmania)#second order polynomial force intercept to 0
AIC(a0,a1,a2,a2.lm,a3,a4)##a0 is selected

summary(a0)
a0
plot(logVmax~logNL,data=guzmania)
