t<-read.csv("t.csv",check.names=FALSE)
names(t)
tnames<-t[c("Names_djb","areakm2_sn")]
t1<-t[16:ncol(t)]
summary(t1)
t3=as.data.frame(t(t1))
date=rownames(t3)
date=gsub("t","",date)
date=as.Date(date,"%Y%m%d")
date2=format(as.Date(date),"%m")
t2<-t1*t$areakm2_sn/sum(t$areakm2_sn)
t4<-colSums(t2)
summary(t4)

library(reshape)
maxmin=(melt(t3))
boxplot(maxmin$value)
summary(maxmin)
mm=sort(maxmin$value,decreasing=T)
plot(mm,ylab="C",xaxt="n",main="Catchment temperatures\nHistoric climate data",xlab="Percentile",ylim=range(maxmin$value,(min(maxmin$value)-1)),yaxt="n")
axis(1,at=127920*seq(0,1,.1),labels=seq(0,100,10))
axis(2,at=seq(-15,30,5),labels=seq(-15,30,5))
abline(h=mean(t4),col=2)
legend("topright",legend="Weighted Average Temperature (5.6 C)",col=2,bty="n",inset=c(0.02,0.05),lty=1)

date3=format(as.Date(date),"%Y")
t5=t(t1)
rownames(t5)=gsub("t","",rownames(t5))
date4=as.numeric(format(as.Date(rownames(t5),"%Y%m%d"),"%Y"))
rownames(t5)=date4
t6=melt(t5)
t7=t6[,c(1,3)]
colnames(t7)=c("year","t")
boxplot(t7$t~t7$year)

year=as.matrix(t7[1])
temp=as.matrix(t7[2])
yrseq=unique(year)
Tmean=rep(NA,length(yrseq))
Tmin=rep(NA,length(yrseq))
Tmax=rep(NA,length(yrseq))
for(i in 1:length(yrseq)){
  yr=yrseq[i]
  Tmean[i]=mean(temp[yrseq==yr])
  Tmin[i]=min(temp[yrseq==yr])
  Tmax[i]=max(temp[yrseq==yr])
}
plot(yrseq,Tmean,type="l",ylim=range(Tmean,Tmax,Tmin))
lines(yrseq,Tmax,col=2)
lines(yrseq,Tmin,col=4)

Ts=cbind(Tmin,Tmean,Tmax)
boxplot(Ts)
boxplot(Tmin)
boxplot(Tmax)
boxplot(Tmean)
summary(Ts)
