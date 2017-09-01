### Homework 2
### some of the code adapted from Rwork.r (Tarboton)

rm(list=ls())
s = 5.68E-08 # sigma, W/(m^2 K^4)
A = 5.1E+14 # m^2
S = 1.74E+17 # W
Qe = 4.08E+16 # W
Qh = 8.67E+15 # W
W = 1.07E+13 # W
ap = 0.300
ku = 0.180
kl = 0.075
f = 0.950 # 0 <= f <= 1
ck=273.16

fstring <- seq(0,1,0.05)

Ts=rep(NA,length(fstring))
Tl=rep(NA,length(fstring))
Tu=rep(NA,length(fstring))
TsC=rep(NA,length(fstring))
TsC=rep(NA,length(fstring))
TlC=rep(NA,length(fstring))
TuC=rep(NA,length(fstring))

for(i in seq_along(fstring)) {
  newf=fstring[i]
  Ts[[i]] = (((3-3*ap-2*ku-kl)*S+2*W-1.5*Qe-Qh)/(s*A*(3-2*newf)))^(1/4)
  Tl[[i]] = ((2*(S+W-ap*S-(1-newf)*??*Ts^4*A)-ku*S-0.5*Qe)/(??*A))^(1/4)
  Tu[[i]] = ((S+W-ap*S-(1-newf)*??*Ts^4*A)/(??*A))^(1/4)
  TsC[[i]]=Ts[i]-ck
  TsC[[i]]=Ts[i]-ck
  TlC[[i]]=Tl[i]-ck
  TuC[[i]]=Tu[i]-ck
}
tt<-cbind(fstring,Tu,Tl,Ts,TuC,TlC,TsC)

f=0
f=0.95
Ts = (((3-3*ap-2*ku-kl)*S+2*W-1.5*Qe-Qh)/(s*A*(3-2*f)))^(1/4)
Tl = ((2*(S+W-ap*S-(1-f)*??*Ts^4*A)-ku*S-0.5*Qe)/(??*A))^(1/4)
Tu = ((S+W-ap*S-(1-f)*??*Ts^4*A)/(??*A))^(1/4)
TsC[i]=Ts[i]-ck
TsC[i]=Ts[i]-ck
TlC[i]=Tl[i]-ck
TuC[i]=Tu[i]-ck
TuC[i]=Tu[i]-ck

# HW2.5
# Some of this section adapted from Rwork.r (Tarboton)
# CFS to inches/yr
CFStoin.yr<-function(x) x*12*12*12*60*60*24*365.25
CFStoin.yr(202.6)
CFStoin.yr(186.4)

# summarize streamflow data corresponding to the precipitation data summaries
library(dataRetrieval)

## retrieve site description for USGS 10011500 - BEAR RIVER NEAR UTAH-WYOMING STATE LINE 
siteNumber <- "10011500"

# select daily mean streamflow data
parameterCd <- "00060"

# Retrieve all available daily data; using default statistic code for daily mean
rawdailydata <- readNWISdv(siteNumber,parameterCd,"","")

# Rename column titles for readability
rawdailydata <- renameNWISColumns(rawdailydata) # replaces codes for parameter and statistic with names

# collecting attribute data for labelling plots
parameterInfo <- attr(rawdailydata, "variableInfo")
siteInfo <- attr(rawdailydata, "siteInfo")

Q=rawdailydata$Flow # vector of mean daily discharge
dt=rawdailydata$Date # vector of dates

Q71 <- Q[which(dt>="1971-01-01"&dt<"2001-01-01")]
summary(Q71)
Q85 <- Q[which(dt>="1985-01-01"&dt<"2011-01-01")]
summary(Q85)
dt71 <- dt[which(dt>="1971-01-01"&dt<"2001-01-01")]
summary(dt71)
dt85 <- dt[which(dt>="1985-01-01"&dt<"2011-01-01")]
summary(dt85)

q71<-as.data.frame(as.character(dt71))
q71$flow<-Q71
write.csv(q71,"flow71.csv")
write.csv(pmo71,"precip71.csv")

# Using code from Rwork.r (Tarboton)
# NOT grouped by water year
yy71=as.numeric(format.Date(dt71,"%Y"))
mo71=as.numeric(format.Date(dt71,"%m"))
#wy71=ifelse(mo71>=10,yy71+1,yy71)
#yrseq71=unique(wy71)
yrseq71=unique(yy71)

# Vector for annual flow
Q71mean=rep(NA,length(yrseq71))
for(i in 1:length(yrseq71)){
  yr=yrseq71[i]
  Q71mean[i]=mean(Q71[yy71==yr])
}
mean(Q71mean)

df71<-as.data.frame(cbind(yrseq71,Q71mean))
write.csv(df71,"yr71Q.csv")

CFStoin3.yr<-function(x) x*12*12*12*60*60*24*365.25
CFStoin3.yr(mean(Q71mean))

# Using code from Rwork.r (Tarboton) 
# NOT grouped by water year
yy85=as.numeric(format.Date(dt85,"%Y"))
mo85=as.numeric(format.Date(dt85,"%m"))
#wy85=ifelse(mo85>=10,yy85+1,yy85)
#yrseq85=unique(wy85)
yrseq85=unique(yy85)

# Vector for annual flow
Q85mean=rep(NA,length(yrseq85))
for(i in 1:length(yrseq85)){
  yr=yrseq85[i]
  Q85mean[i]=mean(Q85[yy85==yr])
}
mean(Q85mean) # mean annual runoff (CFS)

CFStoin3.yr<-function(x) x*12*12*12*60*60*24*365.25
CFStoin3.yr(mean(Q85mean))

# collecting attribute data for labelling plots
parameterInfo <- attr(rawdailydata, "variableInfo")
siteInfo <- attr(rawdailydata, "siteInfo")

# Area of watershed
A71 = 174 # square miles
A85 = 265.676896 # square miles (688.05092 km2); this is the HUC10 watershed over which the precipitation data was summarized

# inches per year
CFStoin.yr<-function(x,y) (x*12*12*12*60*60*24*365.25)/(y*12*12*5280*5280)
Q.A71<-CFStoin.yr(mean(Q71mean),A71)
Q.A85<-CFStoin.yr(mean(Q85mean),A71)

CFStomm.yr<-function(x,y) (x*12*12*12*25.4*25.4*25.4*60*60*24*365.25)/(y*12*12*5280*5280)
Q.A85<-CFStomm.yr(mean(Q85mean),A71)


# Average monthly precipitation, inches
pmo71<-c(3.5,3.37,3.92,3.95,3.44,1.83,1.64,1.86,2.26,2.77,3.39,3.02)
names(pmo71)<-month.abb
HUC10<-read.csv("C:\\Users\\a01987147\\Documents\\HUC10p_hist.csv", nrows=1,check.names = F)
H10 <- HUC10[,-c(1:8)]
dates <- names(H10)
dates <- gsub("p","",dates)
dates <- as.Date(dates, format="%Y%m%d")
mop85=as.numeric(format.Date(dates,"%m"))
yyp85=as.numeric(format.Date(dates,"%Y"))
p85=as.numeric(H10[1,]) # 
# P=sum(p85)/length(p85) # monthly varation

mp=cbind(yyp85,mop85,p85)
mpsum=aggregate(p85~yyp85+mop85,data=mp,sum)
mpmean=aggregate(p85~mop85,data=mpsum,mean)
mpm.inch<-mpmean$p85/25.4
plot(mpm.inch,type="o",xaxt="n", col="red", pch=2,
     xlab=paste("Months\n",siteInfo$agency_cd,siteInfo$site_no),
     ylab="Precipitation, inches",
     main=(paste("Average Monthly Precipitation\n",siteInfo$station_nm)))
#x-axis labels
mos<-seq(1,12,1)
axis(1,at=mos,labels=month.abb)
lines(mos,pmo71,type="o",col="blue",pch=1)
grid()
legend("bottomright", inset=c(0.02,0.04), 
       c("1971-2000", "1985-2010"), 
       col=c("blue","red"), lty=1, pch=c(1,2))

# longterm average P
mpy=cbind(yyp85,p85)
mpsum=aggregate(p85~yyp85,data=mpy,sum)
P=mean(mpsum$p85) # longterm average precip per year (mm) !!! already expressed per unit area
P.inch=(P/25.4) #precip per year (inches)

# monthly flow per unit area
CFStoin.yr<-function(x,y) (x*12*12*12*60*60*24*365.25)/(y*12*12*5280*5280)
mQ.A71<-CFStoin.yr(Q71,A71)
mQ.A85<-CFStoin.yr(Q85,A71)

# mQ71=cbind(mo71,mQ.A71)
# mQ71m=aggregate(mQ.A71~mo71,data=mQ71,mean)
# mQ85=cbind(mo85,mQ.A85)
# mQ85m=aggregate(mQ.A85~mo85,data=mQ85,mean)
# 
# plot(mQ71m$mQ.A71,type="o",xaxt="n", col="blue", pch=1,
#      xlab=paste("Months\n",siteInfo$agency_cd,siteInfo$site_no),
#      ylab="Discharge per unit area, inches",
#      main=(paste("Average Monthly Discharge per Unit Area\n",siteInfo$station_nm)))
# #x-axis labels
# mos<-seq(1,12,1)
# axis(1,at=mos,labels=month.abb)
# lines(mos,mQ85m$mQ.A85,type="o",col="red",pch=2)
# grid()
# legend("topright", inset=c(0.02,0.04), 
#        c("1971-2000", "1985-2010"), 
#        col=c("blue","red"), lty=1, pch=c(1,2))
# 
mQ71=cbind(mo71,Q71)
mQ71m=aggregate(Q71~mo71,data=mQ71,mean)
mQ85=cbind(mo85,Q85)
mQ85m=aggregate(Q85~mo85,data=mQ85,mean)

#flow in inches per unit area
mQ.A85=mQ85m$Q85*(12^3) # cfs to cubic inches/s
mQ.A85=mQ.A85*(60*60*24*365.25) # in^3/yr
mQ.A85=mQ.A85/(174*(5280*12)^2) # in/yr
dpm<-c(31,28.5,31,30,31,30,31,31,30,31,30,31) #days per month
yrfrac<-dpm/365.25 # fraction of a year for each month
mQ.A85=mQ.A85*yrfrac # inches Q for a given month

mQ.A71=mQ71m$Q71*(12^3) # cfs to cubic inches/s
mQ.A71=mQ.A71*(60*60*24*365.25) # in^3/yr
mQ.A71=mQ.A71/(174*(5280*12)^2) # in/yr
dpm<-c(31,28.5,31,30,31,30,31,31,30,31,30,31) #days per month
yrfrac<-dpm/365.25 # fraction of a year for each month
mQ.A71=mQ.A71*yrfrac # inches Q for a given month


plot(mQ.A71,type="o",xaxt="n", col="blue", pch=1,
     xlab=paste("Months\n",siteInfo$agency_cd,siteInfo$site_no),
     ylab="Inches",
     main=(paste("Average Runoff per Unit Area\n",siteInfo$station_nm)))
#x-axis labels
mos<-seq(1,12,1)
axis(1,at=mos,labels=month.abb)
lines(mos,mQ.A85,type="o",col="red",pch=2)
grid()
legend("topright", inset=c(0.02,0.04), 
       c("1971-2000", "1985-2010"), 
       col=c("blue","red"), lty=1, pch=c(1,2))

# runoff ratio
Q.A71/34.8
Q.A85/P.inch

w71<-mQ.A71/pmo71
w85<-mQ.A85/mpm.inch

ET71<-pmo71-mQ.A71
ET85<-mpm.inch-mQ.A85

plot(w71,type="o",xaxt="n", col="blue", pch=1,
     xlab=paste("Months\n",siteInfo$agency_cd,siteInfo$site_no),
     ylab="Runoff ratio, w",
     main=(paste("Runoff ratio = Q/P\n",siteInfo$station_nm)))
#x-axis labels
mos<-seq(1,12,1)
axis(1,at=mos,labels=month.abb)
lines(mos,w85,type="o",col="red",pch=2)
grid()
legend("topright", inset=c(0.02,0.04), 
       c("1971-2000", "1985-2010"), 
       col=c("blue","red"), lty=1, pch=c(1,2))

plot(ET71,type="o",xaxt="n", col="blue", pch=1,
     xlab=paste("Months\n",siteInfo$agency_cd,siteInfo$site_no),
     ylab="Runoff ratio, w",
     main=(paste("Runoff ration = Q/P\n",siteInfo$station_nm)))
#x-axis labels
mos<-seq(1,12,1)
axis(1,at=mos,labels=month.abb)
lines(mos,ET85,type="o",col="red",pch=2)
grid()
legend("topright", inset=c(0.02,0.04), 
       c("1971-2000", "1985-2010"), 
       col=c("blue","red"), lty=1, pch=c(1,2))


##HW2.6
# Average annual temperature
HUC10<-read.csv("C:\\Users\\a01987147\\Documents\\HUC10t_hist.csv", nrows=1,check.names = F)
H10 <- HUC10[,-c(1:8)]
dates <- names(H10)
dates <- gsub("t","",dates)
dates <- as.Date(dates, format="%Y%m%d")
mot85=as.numeric(format.Date(dates,"%m"))
yyt85=as.numeric(format.Date(dates,"%Y"))
t85=as.numeric(H10[1,]) # 
T_=mean(t85)+273.16

# PET
PET=(1.2*10^10)*exp(-4620/T_)
PET # 526.0726

A = 174 # square miles
Amm = A*(5280*12*25.4)^2 # mm^2

mQ=mean(Q85mean) # mean CFS
mQ.mm3 = mQ*304.8^3 # mean mm^3/s
mQ.mm = mQ.mm3/Amm # mean mm/s
mQy.mm.y = mQ.mm*60*60*24*365.25 # mean mm/yr
mQy.mm.y # 369.053123 mm/yr


w=2.21578693483154
RO=P*(1-(PET/(((P^w)+(PET^w))^(1/w))))


##
epROP<-(1-(1/(1+(P/PET)^w)^(1+(1/w))))/(1-(PET/(P^w+PET^w)^(1/w)))


dROPC=-(5.54*10^13*exp(-4620/T_))/(T_^2*P*((1+(PET/P)^w)^(1+1/w))*(1-(PET/((P^w+PET^w)^(1/w)))))
# (((2*dROPC*mQy.mm.y+mQy.mm.y)*Amm)/(25.4*25.4*25.4*12*12*12))/(60*60*24*365.25)
# result in a drop of the average CFS to 165.2 (from 186?)
##############################
##############################
# copied but not used yet
##############################
##############################
3




mt=cbind(yyt85,mot85,t85)
mtmean=aggregate(t85~yyt85+mot85,data=mt,sum) # montlhy mean temperatures
atmean=aggregate(t85~mot85,data=mtmean,mean) # annual mean temperatures
mean(atmean$t85)

plot(mtm.inch,type="o",xaxt="n", col="red", pch=2,
     xlab=paste("Months\n",siteInfo$agency_cd,siteInfo$site_no),
     ylab="Precipitation, inches",
     main=(paste("Average Monthly Precipitation\n",siteInfo$station_nm)))
#x-axis labels
mos<-seq(1,12,1)
axis(1,at=mos,labels=month.abb)
lines(mos,tmo71,type="o",col="blue",pch=1)
grid()
legend("bottomright", inset=c(0.02,0.04), 
       c("1971-2000", "1985-2010"), 
       col=c("blue","red"), lty=1, pch=c(1,2))

# longterm average t
mty=cbind(yyt85,t85)
mtmean=aggregate(t85~yyt85,data=mty,sum)
t=mean(mtmean$t85) # longterm average precip per year (mm) !!! already expressed per unit area
t.inch=(t/25.4) #precip per year (inches)

# monthly flow per unit area
CFStoin.yr<-function(x,y) (x*12*12*12*60*60*24*365.25)/(y*12*12*5280*5280)
mQ.A71<-CFStoin.yr(Q71,A71)
mQ.A85<-CFStoin.yr(Q85,A71)











##############################
##############################
# copied but not used yet
##############################
##############################
3

# plot peak, mean, and 7-day minimum flows
# y-axis uses log scale
plot(yrseq,Qmax, log="y",type="o",col=4,
     ylim=c(1,max(na.omit(Qmax)*2)), # Remove NAs in order to be able calculate annual peak flows
     xlim=c(1940,2017), 
     ylab=parameterInfo$parameter_desc,main=siteInfo$station_nm,xlab="Annual Summaries", 
     xaxp = c(1940,2015,15), 
     panel.first=grid(equilogs=FALSE)) 
lines(yrseq,Qmean)
lines(yrseq,Q7min,type="o",col=2, pch=2)
abline(v=c(seq(1940,2015,5)),lty=9,col="gray")
legend("bottomright", inset=c(0.02,0.04), 
       c("Maximum", "Average", "7-day minumum"), 
       col=c("blue","black", "red"), lty=1, pch=c(1,NA,2))




## HW1.7
# Plot the flow duration curve for the daily stream flow at the selected gauging station
# Report the daily flow that has a 90% probability of being exceeded

# Sorting mean daily stream flow from maximum to minimum for probability ranking
Q1 <- sort(Q, decreasing=T, na.last=NA) # omit NAs
# head(Q1)
# tail(Q1)

# Probability calculation using Weibull plotting-position formula: P=i/(n+1)
# P = probablility; i = rank, n=number of observations
P <- rep(NA,length(Q1))
for(i in 1:length(Q1)){
  P[i]=i/(length(P)+1)
}
# Probability calculation using Cunnane plotting-position formula
P2 <- rep(NA,length(Q1))
for(i in 1:length(Q1)){
  P2[i]=(i-0.4)/(length(P2)+0.2)
}

# calculation of flow rate that will be exceeded 90% of the time
exc = unique(Q1[which(abs(P-0.9)==min(abs(P-0.9)))]) 
# added unique() because the first time I ran the script, exc had a single solution
# rerunning the data retreival (which added more days), two rows satisfied the calculation for exc

# plot mean daily stream flow (descending values)
# x-axis probability values
# y-axis log scale
plot(P,Q1,log="y",
     ylab=parameterInfo$parameter_desc,
     main=paste("Exceedence Probability Curve",siteInfo$station_nm,sep="\n"),
     xlab="Exceedence Probability",xaxt = "n")
at <- seq(from = 0, to = max(P)+0.1, by = 0.1)
axis(side = 1, at = at)
abline(log10(mean(Q1)),0,col="blue") # mean of the mean daily flows for complete time serieas
points(at,y=rep(mean(Q1),length(at)),col="blue")
abline(log10(exc),0,col="red") # 90% exceedance
points(at,y=rep(exc,length(at)),col="red",pch=24)
abline(h=c(panel.first=grid(equilogs=FALSE)),v=at, col="gray", lty="dotted")
legend("topright", inset=c(0.02,0.04), col=c("blue","red"),lty=1,pch=c(1,24),
       legend=c(paste("Average Flow =",signif(mean(Q1),3),"cfs"), 
                paste("90% exceedence =",signif(exc,3),"cfs"))) 

# HW1.8
# Monthly mean streamflow
# calculation of monthly means by year
mQ=cbind(yy,mo,Q)
mQm=(aggregate(Q~mo+yy,data=mQ,mean))
# calculation of monthly means for the entire period of record
mQ2=cbind(mo,Q71)
mQm2=aggregate(Q~mo,data=mQ2,mean)
plot(mQm2,type="l",xaxt="n",
     xlab=paste("Streamflow data: 1942-2016\n",siteInfo$agency_cd,siteInfo$site_no),
     ylab=parameterInfo$parameter_desc,
     main=(paste("Mean of Monthly Streamflows\n",siteInfo$station_nm)))
#x-axis labels
mos<-seq(1,12,1)
axis(1,at=mos,labels=month.abb)
grid()


