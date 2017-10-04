time=sort(c(0.25,0:10))
p=1.226123678
fc=c(9.6,9.6,2.454371047,1.533,1.226,1.164,1.113,1.070,1.033,1.001,0.974,0.949)
plot(time,fc,type="o",col=2,xlab="time steps",ylab="cm / hr",xaxt="n",yaxt="n", ylim=c(0,5))
axis(1,at=0:10)
abline(h=p,col="blue")
abline(v=3,col="lightgray",lty="dotted")
legend("topright",inset=c(0.06,0.06),legend=c("Precipitation, w","Infiltration capacity, fc","Ponding begins, Fp"),lty=c(1,1,3),col=c("blue","red","lightgray"),pch=c(NA,1,NA))

#G-A
time=seq(0,2,0.25)
time[[1]]=0.0975744703456762
p=c(1.2,1.6,2.0,2.4,2.8,3.2,1.6,2.4,2.4)
fc=c(25,8.867,4.423,3.034,2.386,2.081,1.908,1.808,1.722)
plot(time,fc,type="o",col=2,xlab="time steps",ylab="cm / hr",log="y",xaxt="n")
axis(1,at=seq(0,2,0.25),labels=seq(0,2,0.25))
lines(time,p,type="S",col=4)
F.p=1.781
delta.t=
abline=

#G-A
time=seq(0,0.75,0.25)
p=c(1.2,1.6,2.0,2.4)
fc=c(25,1.8,1,0.75)
plot(time,fc,type="o",col=2,xlab="time steps",ylab="cm / hr",log="y",xaxt="n")
axis(1,at=seq(0,0.75,0.25),labels=seq(0,0.75,0.25))
lines(time,p,type="S",col=4)
abline=
  
  
# Horton example
time=seq(0,2,0.25)
f.1=1
p=c(1.2,1.6,2.0,2.4,2.8,3.2,1.6,2.4,2.4)
fc=c(6.000,5.504,4.859,4.083,3.214439141,2.363159733,1.826797878,1.512383544,1.310776228)
plot(time,fc,type="o",col=2,xlab="time steps",ylab="cm / hr", yaxt="n", ylim=c(0,6),main="Horton Method")
axis(2,at=0:6,labels=c(0,expression(paste(f[1]," = 1")),2,3,4,5,expression(paste(f[0]," = 6"))),las=1)
lines(time,p,type="S",col=4)
abline(h=f.1,lty="dashed",col="lightgray")
legend("topright",inset=0.1,legend=expression(paste(f[c]," = ",e^-kt)),lty=1,pch=1,col=2)
(T))