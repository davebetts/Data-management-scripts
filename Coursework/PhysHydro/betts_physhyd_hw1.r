### Homework 1
### some of the code adapted from Rwork.r (Tarboton)

## HW1.5 - Data retrieval from USGS stream gage selected in HW1.4
library(dataRetrieval)

## retrieve site description for USGS 10011500 - BEAR RIVER NEAR UTAH-WYOMING STATE LINE 
siteNumber <- "10011500"
# bear_utwy <- readNWISsite(siteNumber)

# select daily mean streamflow data
parameterCd <- "00060"
# pCode <- readNWISpCode(parameterCd) #provides information on parameterCd ("00060")

# Retrieve all available daily data; using default statistic code for daily mean
rawdailydata <- readNWISdv(siteNumber,parameterCd,"","")
# head(rawdailydata) # inspect data

# Rename column titles for readability
rawdailydata <- renameNWISColumns(rawdailydata) # replaces codes for parameter and statistic with names
names(rawdailydata) # displays column names

# collecting attribute data for labelling plots
parameterInfo <- attr(rawdailydata, "variableInfo")
siteInfo <- attr(rawdailydata, "siteInfo")

# Plot daily stream flow data
library(ggplot2)
ts <- ggplot(data = rawdailydata,
             aes(Date, Flow)) +
  geom_line()
ts

# label plot with attribute data
ts <- ts +
  xlab("Mean Daily Discharge") +
  ylab(parameterInfo$parameter_desc) +
  ggtitle(paste(siteInfo$station_nm,": ",siteInfo$agency_cd," ",siteInfo$site_no,sep=""))
ts

## HW1.6
# For the series plotted in the question above compute the average annual flow, peak flow
# and annual seven day minimum flow.
# Plot your results similar to figure 1.16.
# Somme of this section adapted from Rwork.r (Tarboton)

Q=rawdailydata$Flow # vector of mean daily discharge
dt=rawdailydata$Date # vector of dates

# calculation of running 7-day means for entire time series
Q7=rep(NA,length(dt)) # 7-day means
for(i in dt[4:(length(dt)-3)]){
  begin = which(dt == i)-3
  end = which(dt == i)+3
  mid = which(dt == i)
  Q7[mid] = mean(Q[begin:end])
}
  
# x <- cbind(dt,Q,Q7)
# write.csv(x,"Qs.csv",row.names=F)

# Using code from Rwork.r (Tarboton) with adaptations for 7-day minimum flows
# Observations grouped by water year
yy=as.numeric(format.Date(dt,"%Y"))
mo=as.numeric(format.Date(dt,"%m"))
wy=ifelse(mo>=10,yy+1,yy)
yrseq=unique(wy)
# Vectors for peak, mean, and 7-day minimum annual flows
Qmax=rep(NA,length(yrseq))
Qmean=rep(NA,length(yrseq))
Q7min=rep(NA,length(yrseq))
for(i in 1:length(yrseq)){
  yr=yrseq[i]
  Qmean[i]=mean(Q[wy==yr])
  Q7min[i]=min(Q7[wy==yr])
  Qmax[i]=max(Q[wy==yr])
}

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

