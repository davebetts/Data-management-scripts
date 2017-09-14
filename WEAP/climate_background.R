rm(list=ls())
gc()

setwd(
dir()

#############################################################################################
###            !!! redo  this block!!! USGS stream gauge data processing for comparison with WEAP output          ###
###                 Sites:                             ###
###          All data from:            ###
###                             David J. Betts - July 2014                                ###
###                 WEAP model of climate change along the Wasatch front                  ###
###         (The Bear River, Weber River and "Provo - Jordan" River watershed)            ###
#############################################################################################

#############################################################################################
###         !!! redo this block !!! Read all gauge data for the entire study areawithin the study area            ###
###                    Available daily averages from  1985 - 2005                         ###
#############################################################################################
##
##
##
## !!! list other data processing procedures
##
##

p<-read.csv("testp.csv",header=T)
library(reshape)
p1 = melt(p)
names(p1)[2:3]<-c("Date","Precip")
str(p1)

d<-read.csv("testd.csv",header=T)
h<-read.csv("HUCS.csv",header=T)

hp=merge(h,p1,by=c("Name","Name"))

hp$HUC12<-as.factor(hp$HUC12)
hp$HUC10<-as.factor(hp$HUC10)
hp$HUC08<-as.factor(hp$HUC08)
hp$HUC06<-as.factor(hp$HUC06)
str(hp)

hpd<-merge(hp,d,by=c("Date","Date"))
hpd
str(hpd)
write.csv(hpd,file="hpd.csv",row.names=F)

y=length(unique(hpd$Year))
py<-function(x) x/y

PtotH12<-aggregate(Precip~Name,data=hpd,sum)
PtotH10<-aggregate(Precip~HUC10,data=hpd,sum)
PtotH08<-aggregate(Precip~HUC08,data=hpd,sum)
PtotH06<-aggregate(Precip~HUC06,data=hpd,sum)
PtotH12$MAP<-sapply(PtotH12[,2],py)
PtotH10$MAP<-sapply(PtotH10[,2],py)
PtotH08$MAP<-sapply(PtotH08[,2],py)
PtotH06$MAP<-sapply(PtotH06[,2],py)

write.csv(PtotH12,file="PtotH12.csv",row.names=F)
write.csv(PtotH10,file="PtotH10.csv",row.names=F)
write.csv(PtotH08,file="PtotH08.csv",row.names=F)
write.csv(PtotH06,file="PtotH06.csv",row.names=F)

plot(PtotH12$Precip~PtotH12$Name)
plot(PtotH10$Precip~PtotH10$HUC10)
plot(PtotH08$Precip~PtotH08$HUC08)
plot(PtotH06$Precip~PtotH06$HUC06)

yp<-aggregate(Precip~Year,data=hpd,sum) 
plot(yp)
ypH12<-aggregate(Precip~HUC12*Year,data=hpd,sum)
plot(ypH12)

ypH08<-aggregate(Precip~HUC08*Year,data=hpd,sum)
plot(ypH08)
barplot(yp$Precip)

write.csv(yp,file="yp.csv",row.names=F)
write.csv(ypH08,file="yp0H8.csv",row.names=F)

melt(ypH08)
library(reshape2)
y8p<-dcast(ypH08,Year~HUC08,value.var="Precip")
write.csv(y8p,file="y8p.csv",row.names=F)


gc()
t<-read.csv("testt.csv",header=T)

library(reshape)
t1=melt(t)
names(t1)[2:3]<-c("Date","Temp")
str(t1)

d<-read.csv("testd.csv",header=T)
h<-read.csv("HUCS.csv",header=T)

ht=merge(h,t1,by=c("Name","Name"))

ht$HUC12<-as.factor(ht$HUC12)
ht$HUC10<-as.factor(ht$HUC10)
ht$HUC08<-as.factor(ht$HUC08)
ht$HUC06<-as.factor(ht$HUC06)
str(ht)

htd<-merge(ht,d,by=c("Date","Date"))
str(htd)
write.csv(htd,file="htd.csv",row.names=F)

y=length(unique(htd$Year))
py<-function(x) x/y

TH12<-aggregate(Temp~Name,data=htd,mean)
TH10<-aggregate(Temp~HUC10,data=htd,mean)
TH08<-aggregate(Temp~HUC08,data=htd,mean)
TH06<-aggregate(Temp~HUC06,data=htd,mean)
TH12$MAT<-sapply(TH12[,2],py)
TH10$MAT<-sapply(TH10[,2],py)
TH08$MAT<-sapply(TH08[,2],py)
TH06$MAT<-sapply(TH06[,2],py)

write.csv(TH12,file="TH12.csv",row.names=F)
write.csv(TH10,file="TH10.csv",row.names=F)
write.csv(TH08,file="TH08.csv",row.names=F)
write.csv(TH06,file="TH06.csv",row.names=F)

plot(TH12$MAT~TH12$Name)
plot(TH10$MAT~TH10$HUC10)
plot(TH08$MAT~TH08$HUC08)
plot(TH06$MAT~TH06$HUC06)

yt<-aggregate(Temp~Year,data=htd,mean) 
plot(yt)
ytH08<-aggregate(Temp~HUC08*Year,data=htd,mean)
plot(ytH08)

write.csv(yt,file="yt.csv",row.names=F)
write.csv(ytH08,file="yt0H8.csv",row.names=F)

library(reshape2)
yt8<-dcast(ytH08,Year~HUC08,value.var="Temp")
write.csv(yt8,file="yt8.csv",row.names=F)
summary(yt)
rm(list=ls())
gc()
getwd()
setwd("C:/Users/davebetts/Dropbox/PhD/WEAP/climate_testing")
dir()
p<-read.csv("testp.csv",header=T)

library(reshape)
p1=melt(p)
names(p1)[2:3]<-c("Date","Precip")
str(p1)

d<-read.csv("testd.csv",header=T)
h<-read.csv("HUCS.csv",header=T)

hp=merge(h,p1,by=c("Name","Name"))

hp$HUC12<-as.factor(hp$HUC12)
hp$HUC10<-as.factor(hp$HUC10)
hp$HUC08<-as.factor(hp$HUC08)
hp$HUC06<-as.factor(hp$HUC06)
str(hp)

hpd<-merge(hp,d,by=c("Date","Date"))
hpd
str(hpd)
write.csv(hpd,file="hpd.csv",row.names=F)

y=length(unique(hpd$Year))
py<-function(x) x/y

QH12<-aggregate(Precip~Name,data=hpd,sum)
QH10<-aggregate(Precip~HUC10,data=hpd,sum)
QH08<-aggregate(Precip~HUC08,data=hpd,sum)
QH06<-aggregate(Precip~HUC06,data=hpd,sum)
QH12$MAF<-sapply(QH12[,2],py)
QH10$MAF<-sapply(QH10[,2],py)
QH08$MAF<-sapply(QH08[,2],py)
QH06$MAF<-sapply(QH06[,2],py)

write.csv(QH12,file="QH12.csv",row.names=F)
write.csv(QH10,file="QH10.csv",row.names=F)
write.csv(QH08,file="QH08.csv",row.names=F)
write.csv(QH06,file="QH06.csv",row.names=F)

plot(QH12$MAF~QH12$Name)
plot(QH10$MAF~QH10$HUC10)
plot(QH08$MAF~QH08$HUC08)
plot(QH06$MAF~QH06$HUC06)

yp<-aggregate(Precip~Year,data=hpd,sum) 
plot(yp)
ypH08<-aggregate(Precip~HUC08*Year,data=hpd,sum)
plot(ypH08)

write.csv(yp,file="yp.csv",row.names=F)
write.csv(ypH08,file="yp0H8.csv",row.names=F)

melt(ypH08)
library(reshape2)
y8p<-dcast(ypH08,Year~HUC08,value.var="Precip")
write.csv(y8p,file="y8p.csv",row.names=F)


gc()
t<-read.csv("testt.csv",header=T)

library(reshape)
t1=melt(t)
names(t1)[2:3]<-c("Date","Temp")
str(t1)

d<-read.csv("testd.csv",header=T)
h<-read.csv("HUCS.csv",header=T)

ht=merge(h,t1,by=c("Name","Name"))

ht$HUC12<-as.factor(ht$HUC12)
ht$HUC10<-as.factor(ht$HUC10)
ht$HUC08<-as.factor(ht$HUC08)
ht$HUC06<-as.factor(ht$HUC06)
str(ht)

htd<-merge(ht,d,by=c("Date","Date"))
str(htd)
write.csv(htd,file="htd.csv",row.names=F)

y=length(unique(htd$Year))
py<-function(x) x/y

TH12<-aggregate(Temp~Name,data=htd,mean)
TH10<-aggregate(Temp~HUC10,data=htd,mean)
TH08<-aggregate(Temp~HUC08,data=htd,mean)
TH06<-aggregate(Temp~HUC06,data=htd,mean)
TH12$MAF<-sapply(TH12[,2],py)
TH10$MAF<-sapply(TH10[,2],py)
TH08$MAF<-sapply(TH08[,2],py)
TH06$MAF<-sapply(TH06[,2],py)

write.csv(TH12,file="TH12.csv",row.names=F)
write.csv(TH10,file="TH10.csv",row.names=F)
write.csv(TH08,file="TH08.csv",row.names=F)
write.csv(TH06,file="TH06.csv",row.names=F)

plot(TH12$MAF~TH12$Name)
plot(TH10$MAF~TH10$HUC10)
plot(TH08$MAF~TH08$HUC08)
plot(TH06$MAF~TH06$HUC06)

yt<-aggregate(Temp~Year,data=htd,mean) 
plot(yt)
ytH08<-aggregate(Temp~HUC08*Year,data=htd,mean)
plot(ytH08)

write.csv(yt,file="yt.csv",row.names=F)
write.csv(ytH08,file="yt0H8.csv",row.names=F)

library(reshape2)
yt8<-dcast(ytH08,Year~HUC08,value.var="Temp")
write.csv(yt8,file="yt8.csv",row.names=F)
summary(yt)
