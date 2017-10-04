## Homework 7
## HW7.1 - Chow 7.2.2
# A system has the following unit pulse response function:
ur=c(0.27,0.36,0.18,0.09,0.05,0.03,0.01,0.01)
# Calculate the output from this system if it has input of
# a)
a=2 #units
# response to a
r.a=a*ur
# [1] 0.54 0.72 0.36 0.18 0.10 0.06 0.02 0.02
# b)
# response to b
b=3 # units
r.b=b*ur
# [1] 0.81 1.08 0.54 0.27 0.15 0.09 0.03 0.03
# c)
c.1=2 # units in the first time interval
c.2=3 #units in the second time interval
r.c=c(c.1*ur,0)+c(0,c.2*ur)
# [1] 0.54 1.53 1.44 0.72 0.37 0.21 0.11 0.05 0.03

plot(y=range(r.a,r.b,r.c),x=c(1,length(r.c)),type="n",main="Runoff from 3 scenarios for the given\nunit pulse response function",ylab="Units of runoff", xlab="Time intervals",xaxt="n")
axis(1,at=1:length(r.c),labels=1:length(r.c))
abline(v=1:length(r.c),h=seq(0,1.5,0.25),lty="dotted",col="lightgray")
lines(ur,type="l",col=1)
lines(r.a,type="o",pch=1,col=4)
lines(r.b,type="o",pch=2,col=2)
lines(r.c,type="o",pch=3,col=3)
legend("topright",inset=c(0.055,0.08),legend=c("2 units @ interval 1, 3 units @ interval 2", "3 units","2 units", "Unit pulse repsonse function"), col=c(3,2,4,1),pch=c(3,2,1,NA),lty=1,title=expression(bold("Precipitation inputs")))

tbl=as.data.frame(cbind(1:9,c(r.a,0),c(r.b,0),r.c))
colnames(tbl)=c("Interval",paste0("Scenario ",c("a","b","c")))
tbl
#   Interval Scenario a Scenario b Scenario c
# 1        1       0.54       0.81       0.54
# 2        2       0.72       1.08       1.53
# 3        3       0.36       0.54       1.44
# 4        4       0.18       0.27       0.72
# 5        5       0.10       0.15       0.37
# 6        6       0.06       0.09       0.21
# 7        7       0.02       0.03       0.11
# 8        8       0.02       0.03       0.05
# 9        9       0.00       0.00       0.03

## HW7.2 Chow7.4.5
# An intense storm with approximately constant intensity lasting 6 hours over a  watershed area of 785 km^2 produced the following discharges in Q in m^3/s
# The baseflow Qb has been estimated from the appearance of the observed hydrograph.
# Use deconvolution to determine the 2-hour unit hydrograph
# Given:
# Storm duration:
dur=6 # hours
# Watershed area:
A=785 # km^2
# Hours of observation:
H=seq(0,60,2)
# Unit length:
dt=2 # hours
# Observed streamflow:
Q=c(18,21,28,44,70,118,228,342,413,393,334,270,216,171,138,113,97,84,75,66,59,54,49,46,42,40,38,36,34,33,33) # m^3/s
# Observed baseflow:
Qb=c(18,20,25,32,40,47,54,61,68,75,79,77,73,69,66,63,60,57,55,52,49,47,44,42,40,38,37,35,34,33,33)
# Input from the storm: Streamflow - baseflow
Qd=Q-Qb # m^3/s
tbl=as.data.frame(rbind(H,Q,Qb,Qd))
rownames(tbl)=c("Hour","Q","Qb","Qd = Q-Qb")

# Watershed area in meters:
A.m=A*(1000^2)
Q.d=Qd[which(Qd>0)] # m^3/s
Qd.tot=sum(Q.d*2*3600) # m^3/s*hr*s/hr = m^3
# [1] 15696000 m^3

# number of hydrograph values; duration of direct runoff or direct runoff hydrograph (DRH):
N=length(Qd[which(Qd>0)])
# number of precipitation increments; effective rainfall hyetograph (ERH):
M=6/dt # 6 hour storm / 2 hour blocks 
# number of unit hydrograph ordinates
L=N-M+1

Pe=Qd.tot # m^3
Pe.M=((Pe/M)/A.m)*100 # (m^3/hr)/m^2 * 100 cm/m
# [1] 0.6664968 cm
Pm=c(rep(Pe.M,M),rep(0,length(Q.d)-M)) 

U=NULL
U[[1]]=round(Q.d[[1]]/Pm[[1]],0)
U[[2]]=round((Q.d[[2]]-Pm[[2]]*U[[1]])/Pm[[1]],0)
for(i in M:L){
  U[[i]]=round((Q.d[[i]]-Pm[[3]]*U[[i-2]]-Pm[[2]]*U[[i-1]])/Pm[[1]],0)
}
u=ceiling(log10(max(U)))-2
ylim=round((max(U)*1.1)/(10^(u)),0)*(10^u)
barplot(U,main="Unit hydrograph - Deconvolution method\nHW7.2 (Chow 7.4.5)",space=0,xlab="Interval",ylab=expression(paste("Runoff per unit Precipitation,     ",(m^3/s)/cm)),yaxt="n",ylim=c(0,ylim))
axis(1,at=seq(0,length(U),1),labels=seq(0,length(U),1))
axis(2,at=seq(0,ylim,ylim/10),labels=seq(0,ylim,ylim/10))
box()
U
# [1]   2   3  13  29  65 167 190 161 126  96  68
# [12]  51  34  23  18  15   8   7   6   2   3   3
# [23]   0   0   3

## HW7.3 Chow 7.5.3
# Use the two-hour unit hydrograph determined in prob 7.4.5 to calculate the streamflow hydrograph from a four hour storm in which 5cm of excess rainfall fell in the first two hours, and 6cm in the second two hours.
# Assume the same baseflow rate given in 7.4.5
Qb=c(18,20,25,32,40,47,54,61,68,75,79,77,73,69,66,63,60,57,55,52,49,47,44,42,40,38,37,35,34,33,33)
ur=c(2,3,13,29,65,167,190,161,126,96,68,51,34,23,18,15,8,7,6,2,3,3,0,0,3)

Pe.1=5 #cm
Pe.2=6 #cm
Q=c(0,Pe.1*ur,0)+c(0,0,Pe.2*ur)
Q=Q+Qb[1:length(Q)]    
Q1=c(0,Pe.1*ur,0)
Q1=Q1+Qb[1:length(Q1)]    
Q2=c(0,0,Pe.2*ur)
plot(x=range(0,length(Q)-1),y=range(0,Q),type="n",col=4,main="Chow 7.5.3\nStreamflow Hydrograph based on 2-hour unit hydrograph\nand Qb from Chow 7.4.5",xlab="Hours",ylab=expression(m^3/s),xaxt="n")
abline(h=seq(0,max(Q)+250,250),v=seq(0,length(Q),1),col="lightgray",lty="dotted")
axis(1,at=seq(0,length(Q1),2),labels=seq(0,2*length(Q1),4))
y1=c(Q,rev(Q1))
xx=c(0:(length(Q1)-1),(length(Q)-1):0)
polygon(xx, y1, col="lightblue",border="blue")
y2=c(Qb[1:length(Q)],rev(Q1))
polygon(xx, y2, col="pink",border="red")
y3=c(rep(0,length(Q)),rev(Qb[1:length(Q)]))
polygon(xx, y3, col="lightgray",border="black")
lines(y=Q,x=c(0:(length(Q)-1)),type="o",col="blue",pch=1)
lines(y=Q1,x=0:(length(Q1)-1),type="o",col="red",pch=2)
legend("topright",inset=c(0.11,0.16),legend=c("6 cm rain","5 cm rain","Baseflow, Qb"),col=c(4,2,1),lty=1,title=expression(bold("Streamflow sources")),pch=c(1,2,NA))

## HW7.4 Chow 7.6.2
# A storm on April 16, 1977, on the Shoal Creak Watershed at Northwest Park in Austin, Texas, resulted in the following rainfall-runoff values:
# length of interval:
dt=0.5 # hr.
# time series:
t=seq(dt,9,dt) # hr.
# Precipitation rate per interval:
P=c(0.28,0.12,0.13,0.14,0.18,0.14,0.07) # in
Pm=c(P,rep(0,(length(t)-length(P)))) # in/hr
Q=c(32,67,121,189,279,290,237,160,108,72,54,44,33,28,22,20,18,16) # cfs
# The watershed area:
A=7.03 # mi^2
A.ft=A*5280^2 # ft^2
# number of hydrograph values; duration of direct runoff or direct runoff hydrograph (DRH):
N=length(Q)
# number of precipitation increments; effective rainfall hyetograph (ERH):
M=length(P)
# number of unit hydrograph ordinates
L=N-M+1

# Assume that a uniform loss rate is valid.
# Determine the half-hour unit hydrograph by linear programming.  
## Calculation of the abstraction:
# Total volume of rain = total inches of rain * area of watershed
P.tot=sum(Pm)*A.ft/12 # ft^3
# Volume of direct runoff per timestep:
Q.dt=Q*3600*dt # ft^3/s*s/hr*hr = ft^3
# Total volume of direct runoff:
Q.tot=sum(Q.dt) # ft^3
# [1] 3222000
# Total volume of abstraction:
abstr.vol=P.tot-Q.tot # ft^3
# [1] 14090022
# Abstraction rate per timestep:
abstr=((abstr.vol/A.ft)/M)*12 # (ft^3/ft^2)*in
# [1] 0.1232457
# Excess rainfall per interval:
Pe=P-abstr # in
# [1]  0.156754322 -0.003245678  0.006754322  0.016754322  0.056754322  0.016754322 -0.053245678
# The number of intervals when the abstraction rate is greater than precipitation
Pe.0=length(P[which(P<abstr)])
# [1] 2
# Based on the calculated uniform abstraction rate, two of the intervals have less precipitation than the abstraction.  
### !!! couldn't find a satisfactory way to adjust for the difference in abstraction rates !!! ###

P2=c(0.28,0,0,0.14,0.18,0.14) # in
Pm2=c(P2,rep(0,(length(t)-length(P2)))) # in/hr
# number of precipitation increments; effective rainfall hyetograph (ERH):
M2=length(P2)
# number of unit hydrograph ordinates
L2=N-M2+1

# Assume that a uniform loss rate is valid.
# Determine the half-hour unit hydrograph by linear programming.  
## Calculation of the abstraction:
# Total volume of rain = total inches of rain * area of watershed
P.tot2=sum(Pm2)*A.ft/12 # ft^3
abstr.vol2=P.tot2-Q.tot # ft^3
# [1] 8863751.04
# Abstraction rate per timestep:
abstr2=((abstr.vol2/A.ft)/length(P2[which(P2>0)]))*12 # (ft^3/ft^2)*in
# [1] 0.1232457
# Excess rainfall per interval:
Pe2=ifelse(P2-abstr2<0,0,P2-abstr2) # in

Pe.m=c(Pe2,rep(0,length(t)-length(Pe2)))

# U=NULL
# U[[1]]=Q[[1]]/Pe.m[[1]]
# U[[2]]=(Q[[2]]-Pe.m[[2]]*U[[1]])/Pe.m[[1]]
# U[[3]]=(Q[[3]]-Pe.m[[3]]*U[[1]]-Pe.m[[2]]*U[[2]])/Pe.m[[1]]
# U[[4]]=(Q[[4]]-Pe.m[[4]]*U[[1]]-Pe.m[[3]]*U[[2]]-Pe.m[[2]]*U[[3]])/Pe.m[[1]]
# U[[5]]=(Q[[5]]-Pe.m[[5]]*U[[1]]-Pe.m[[4]]*U[[2]]-Pe.m[[3]]*U[[3]]-Pe.m[[2]]*U[[4]])/Pe.m[[1]]
# for(i in M2:L){
#   U[[i]]=(Q[[i]]-Pe.m[[6]]*U[[i-5]]-Pe.m[[5]]*U[[i-4]]-Pe.m[[4]]*U[[i-3]]-Pe.m[[3]]*U[[i-2]]-Pe.m[[2]]*U[[i-1]])/Pe.m[[1]]
# }
# u=ceiling(log10(max(U)))-1
# ylim=round((max(U)+(10^(u))/2)/(10^(u)),0)*(10^u)
# barplot(U,main="Unit hydrograph\nHW7.2 (Chow 7.4.5)",space=0,xlab=expression(paste(Delta,"t = 2 hours")),ylab="meters",yaxt="n",ylim=c(0,ylim))
# axis(1,at=seq(0,length(U),1),labels=seq(0,length(U)*2,2))
# axis(2,at=seq(0,ylim,ylim/10),labels=seq(0,ylim,ylim/10))
# box()


# Compare the unit hydrograph with that develped in example 7.4.1 for this watershed.

# ### Example 7.4.1 ###
# # find the half-hour unit hydrograph using the excess rainfall hyetograph and the direct runoff hydrograph in table 7.4.2
# # timesteps:
# t=1:11 # 0.5 hr.
# # Excess rainfall:
# P=c(1.06,1.93,1.81) # in.
# # Direct runoff:
# Q=c(428,1923,5297,9131,10625,7834,3921,1846,1402,830,313)
# # number of hydrograph values; duration of direct runoff or direct runoff hydrograph (DRH):
# N=length(Q)
# # number of precipitation increments; effective rainfall hyetograph (ERH):
# M=length(P)
# # number of unit hydrograph ordinates
# L=N-M+1
# U=NULL
# U[[1]]=round(Q[[1]]/P[[1]],0)
# U[[2]]=round((Q[[2]]-P[[2]]*U[[1]])/P[[1]],0)
# for(i in M:L){
#   U[[i]]=round((Q[[i]]-P[[3]]*U[[i-2]]-P[[2]]*U[[i-1]])/P[[1]],0)
# }
# U

u=c(100,200,300,400,500,400,300,200,100)
u.s=NULL
s1=u*(P[[1]]+c(0,P[[3]]))
s2=P[[1]]*u
q1=Q-s1
q2=Q-s2
u=(q1+q2)/(P[[3]]+P[[1]])
u.s[[3]]=u

barplot(Pe.m)
abline(h=abst,col=2)
barplot(P)

Pm.ft.s=(Pm/12)/3600 # ft/s
Total.Pm=Pm.ft.s*A.ft # cfs
abstr1=Total.Pm[[1]]-Q[[1]]
Q.A=Q/A.ft # ft/s
Q.in.hr=Q.A*12*3600 # in/hr
abstr2=Pm[[1]]-Q.in.hr[[1]]
P.abstr=Pm-abstr2
Q1=Q[[1]] # ft^3/s
Q1.ft3.hr=Q1*3600  
Q1.in.hr=Q1*((12)^2)*3600*1/(7.03*5280^2) # in/hr
abstr=P[[1]]-Q1.in.hr
P.total=sum(Pm*0.5*3600)*7.03*5280*5280*12*12
Q.total=sum(Q*0.5*3600) # ft^3
Q.in=(Q.total/(7.03*5280^2))*12
Q

# HW 7.5
#Using the data presented in Chow problem 7.4.5 derive the two-hour unit hydrograph using:
# a) Least squares
# The results from the least squares estimate are found in Table 7.5 and plotted as the green line in Fig. 7.5
# b) Linear programming. 
# Compare the results to the unit hydrograph derived by deconvolution in problem 7.4.5. 

# Unit hydrograph from linear programming (hw7.xlsx)
U.lp=c(1.500382297,3.000764595,13.50344068,28.50726365,64.51643879,168.0428173,189.0481695,160.5409058,127.5324953,94.52408474,67.51720338,52.51338041,33.00841054,22.50573446,19.50496987,13.00407784,11.50344068,5.501146892,4.000764595,5.501911487,1,1,1,1,1)
# Unit hydrograph from least squares (hw7.xlsx)
U.sse=c(0,8.26425546,8.345550103,26.90472824,73.62732324,160.2573023,188.0643258,168.1387056,122.0920146,93.37506372,72.04420971,49.64304383,32.48133663,24.86142168,17.56205372,13.42605458,9.223146667,6.96098992,4.56394952,3.123871456,2.747189675,1.624068092,1.537000656,0.15192708,1.494866717)
# Unit hydrograph from HW 7.2 - Chow 7.4.5
U=c(2,3,13,29,65,167,190,161,126,96,68,51,34,23,18,15,8,7,6,2,3,3,0,0,3)

# # looking at error between estimates, not needed for assignment
# Us=as.data.frame(rbind(U,U.lp,U.sse))
# str(Us)
# Us.2=sapply(Us,sd)
# Us.m=sapply(Us,mean)
# u.2=as.data.frame(t(Us))
# u.2$sd=Us.2
# u.2$mean=Us.m
# u.2$percent=u.2$sd/u.2$mean
# boxplot(u.2$percent)

### Plot to compare unit hydrograph calculation methods
# zero values changed to 1 to allow for a log10 scale on the y-axis
U[23:24]=c(1,1) 
U.sse[1]=1

par(mar=c(5,5,4,2))
u=ceiling(log10(max(U,U.lp,U.sse)))-2
ylim=round((max(U,U.lp,U.sse)*1.1)/(10^(u)),0)*(10^u)
plot(x=range(1,length(U)),y=c(1,max(U,U.sse,U.lp)),main="Comparison of Unit Hydrograph estimates\nHW 7.5 based on Chow 7.4.5",xlab="Interval",ylab=expression(paste("Runoff per unit Precipitation,     ",(m^3/s)/cm)),ylim=c(1,ylim),type="n",log="y")
axis(1,at=seq(0,length(U),1),labels=seq(0,length(U),1))
abline(h=c(1,2,5,10,20,50,100,200),v=c(1:25),col="lightgray",lty="dotted")
lines(x=1:length(U),y=U,type="o",col=2,pch=2)
lines(x=1:length(U.lp),y=U.lp,type="o",col=4)
lines(x=1:length(U.sse),y=U.sse,type="o",col=3,pch=3)
legend("topright",inset=c(0.02,0.04),legend=c("Deconvolution","Linear Programming","Least Squares"),lty=1,title="Unit Hydrograph Calculation Method",pch=c(1,2,3),col=c(4,2,3))

# HW7.6 - Chow 7.7.2
# Using the 10-minute unit hydrograph equations (7.7.9)-(7.7.13), develop the unit hydrograph for a small watershed of 0.3 mi^2 that has a main channel slope of 0.009 ft/ft.  The main channel area is 2000 feet long and the percent imperviousness is 25.  Next, develop the 10-minute unit hydrograph for the same watershed assuming the main channel length is 6000 feet long.  Plot and compare the two unit hydrographs.  Assume n = 0.05 for the main channel.

# 10-minute unit hydrographs
# Espey, Altman and Graves (1977)
# Watershed area:
A=0.3 # mi^2
# Channel slope:
S=0.009 # ft/ft
# Main channel length:
L=2000 # ft
# percent imperviousness:
I=0.25 # %
# Main channel Manning n value:
n=0.05
# Dimensionless watershed conveyance factor; a function of percent imperviousness and roughness (figure 7.7.3 - Chow et al., 1988)
phi=(0.86)
## calculation of unit hydrograph: 
# Time of rise to the peak of the unit hydrograph:
T.p=3.1*(L^0.23)*(S^(-0.25))*(I^(-0.18))*(phi^(1.57)) # minutes, Eq. (7.7.9)
# Peak flow of the unit hydrograph:
Q.p=(31.62*10^3)*(A^0.96)*(T.p^(-1.07)) # cfs/in, Eq. (7.7.10)
# Time of the base of the unit hydrograph
T.B=(125.89*10^3)*A*(Q.p^(-0.95)) # minutes, Eq. (7.7.11)
# Width of the hydrograph at 50% of Q.p
W.50=(16.22*10^3)*(A^0.93)*(Q.p^(-0.92)) # minutes, Eq. (7.7.12)
# Width of the hydrograph at 75% of Q.p
W.75=(3.25*10^3)*(A^0.79)*(Q.p^(-0.78)) # minutes, Eq. (7.7.12)

## Scenario 2
# Main channel length:
L2=6000 # ft
## calculation of unit hydrograph: 
## calculation of unit hydrograph: 
# Time of rise to the peak of the unit hydrograph:
T.p2=3.1*(L2^0.23)*(S^(-0.25))*(I^(-0.18))*(phi^(1.57)) # minutes, Eq. (7.7.9)
# Peak flow of the unit hydrograph:
Q.p2=(31.62*10^3)*(A^0.96)*(T.p2^(-1.07)) # cfs/in, Eq. (7.7.10)
# Time of the base of the unit hydrograph
T.B2=(125.89*10^3)*A*(Q.p2^(-0.95)) # minutes, Eq. (7.7.11)
# Width of the hydrograph at 50% of Q.p
W.502=(16.22*10^3)*(A^0.93)*(Q.p2^(-0.92)) # minutes, Eq. (7.7.12)
# Width of the hydrograph at 75% of Q.p
W.752=(3.25*10^3)*(A^0.79)*(Q.p2^(-0.78)) # minutes, Eq. (7.7.12)

## plot unit hydrographs
# Scenario 1
x=c(0,T.p-W.50/3,T.p-W.75/3,T.p,T.p+2*W.75/3,T.p+2*W.50/3,T.B)
y=c(0,0.5*Q.p,0.75*Q.p,Q.p,0.75*Q.p,0.5*Q.p,0)
# Scenario 2
x2=c(0,T.p2-W.502/3,T.p2-W.752/3,T.p2,T.p2+2*W.752/3,T.p2+2*W.502/3,T.B2)
y2=c(0,0.5*Q.p2,0.75*Q.p2,Q.p2,0.75*Q.p2,0.5*Q.p2,0)
# Plot
plot(x=range(x,x2),y=range(y,y2),type="n",ylab="Direct runoff, cfs / in.",xlab="Minutes",main="10-minute hydrographs for 2 scenarios\nHW 7.6 - Chow 7.7.2")
grid()
lines(y=y,x=x,type="o")
lines(y=y2,x=x2,type="o",col=2,pch=2)
legend("topright",inset=c(0.1,0.12), legend=c("Scenario 1: L = 2000 ft.","Scenario 2: L = 6000 ft."),col=c(1,2),pch=c(1,2),lty=1, title="Main Channel Lengths, L")


# R.tot=0.5*Q.p*(W.50+0.5*(T.B-W.50))+(0.75-0.5)*Q.p*(W.75+0.5*(W.50-W.75))+0.5*W.75*(Q.p-0.75*Q.p)*60
# R.tot2=0.5*Q.p2*(W.502+0.5*(T.B2-W.502))+(0.75-0.5)*Q.p2*(W.752+0.5*(W.502-W.752))+0.5*W.752*(Q.p2-0.75*Q.p2)*60

# t.b=2.67*T.p
# t.b2=2.67*T.p2
# lines(x=c(0,T.p,t.b),y=c(0,Q.p,0),type="o",pch=1,col=4)
# lines(x=c(0,T.p2,t.b2),y=c(0,Q.p2,0),type="o",pch=2,col=3)

# Table of values
Variable=c('A','S','L','I','n','phi','T.p','Q.p','T.B','W.50','W.75')
Scenario.1=c(A,S,L,I,n,phi,round(T.p,1),round(Q.p,1),round(T.B,1),round(W.50,1),round(W.75,1))
Scenario.2=c(A,S,L2,I,n,phi,round(T.p2,1),round(Q.p2,1),round(T.B2,1),round(W.502,1),round(W.752,1))
Units=c('mi^2','ft/ft','ft','%','','','minutes','cfs/in','minutes','minutes','minutes')
Description=c('Watershed area','Channel slope','Main channel length','Percent imperviousness','Manning n value','Watershed conveyance factor','Time of rise to the peak of the unit hydrograph','Peak flow of the unit hydrograph','Time of the base of the unit hydrograph','Width at 50% of peak flow','Width at 75% of peak flow')  
Equation=c('','','','','','','(7.7.9)','(7.7.10)','(7.7.11)','(7.7.12)','(7.7.12)')
tbl=as.data.frame(cbind(Variable,Scenario.1,Scenario.2,Units,Description,Equation))

# x.1=x/T.p
# x2.1=x2/T.p2
# y.1=y/Q.p
# y2.1=y2/Q.p2
# plot(x=range(x.1,x2.1),y=range(y.1,y2.1),type="n",ylab="Output / unit precipitation",xlab="Intervals",main="10-minute hydrographs for 2 scenarios\nHW 7.6 - Chow 7.7.2")
# grid()
# lines(y=y.1,x=x.1,type="o")
# lines(y=y2.1,x=x2.1,type="o",col=2,pch=2)
# legend("topright",inset=c(0.1,0.12), legend=c("L = 2000 ft.","L = 6000 ft."),col=c(1,2),pch=c(1,2),lty=1, title="Main Channel Lengths, L")

# HW7.7 - Chow 7.7.3
# Determine direct runoff hydrographs using the two 10-minute unit hydrographs derived in the previous problem for the watersheds with main channel lengths of 2000 ft and 6000 ft.  Consider a storm having 1.2 inches rainfall uniformly distributed over the first 30 minutes and 1.5 inches in the second 30 minutes.
# The infiltration losses are to be determined using the SCS method described in Chap. 5 for curve number CN = 85

# 10-minute unit hydrographs
uhs=cbind(x,y,x2,y2)
write.table(uhs,"uhs.txt",row.names=FALSE)

U1=y
U2=y2
t=x
t2=x2
dt=10
# storm duration:
dur=60

u=10
t.max=ceiling(max(t)/u)*u
t.max2=ceiling(max(t2)/u)*u

# intervals
int=seq(0,t.max,dt)
int2=seq(0,t.max2,dt)
M=dur/dt
L=t.max/dt
N=M+L-1
L2=t.max2/dt
N2=M+L2-1

new.u=NULL
for(i in 1:length(U1)){
  slope=(U1[i]-U1[i-1])/(t[i]-t[i-1])
  b=U1[i]-t[i]*slope
  nxt=int[which(int<t[i]&int>=t[i-1])]
  nxt=nxt*slope+b
  new.u=c(new.u,nxt)
}
new.u=c(new.u,0)

new.u2=NULL
for(i in 1:length(U2)){
  slope=(U2[i]-U2[i-1])/(t2[i]-t2[i-1])
  b=U2[i]-t2[i]*slope
  nxt=int2[which(int2<t2[i]&int2>=t2[i-1])]
  nxt=nxt*slope+b
  new.u2=c(new.u2,nxt)
}
new.u2=c(new.u2,0)


#HW7.8 - Chow 7.8.2
# For the data given in Chow 7.4.5, use the assumption of constant rainfall intensity for six hours to construct the S-hydrograph for this watershed.
# Linear programming solution for the 2-hour hydrograph for Chow 7.4.5
U=c(1.500382297,3.000764595,13.50344068,28.50726365,64.51643879,168.0428173,189.0481695,160.5409058,127.5324953,94.52408474,67.51720338,52.51338041,33.00841054,22.50573446,19.50496987,13.00407784,11.50344068,5.501146892,4.000764595,5.501911487,1,1,1,1,1)
dt=2
int=length(U)+h3/dt
Sh=NULL
for(i in 1:int){
  Sh[[i]]=sum(dt*U[1:i])
}
Sh[which(is.na(Sh))]=sum(dt*U)
par(mar=c(5,5,4,2))
plot(x=range(1,int),y=range(0,Sh),type="n",main="S-hydrograph (original 2-hour) and offset S-hydrographs (6- and 12-hour)\nfor the 2-hour unit hydrograph from Chow 7.4.5",xlab="Hours",ylab=expression("Direct runoff, "(ft^3/s)/in.),xaxt="n")
at=seq(0,int,1)
axis(1,at=at,labels=at*dt)
abline(v=at,lty="dotted",col="lightgray")
h1=2
h2=6
h3=12
lines(x=(h1/dt):(length(Sh)+(h1-dt)/dt),y=Sh,col="black",lwd=2)
lines(x=(h2/dt):(length(Sh)+(h2-dt)/dt),y=Sh,col="red",lwd=2)
lines(x=(h3/dt):(length(Sh)+(h3-dt)/dt),y=Sh,col="blue",lwd=2)
legend("bottomright",inset=c(0.1,0.2),legend=c("2-hours","6-hours","12-hours"),title=expression(bold(paste(Delta,"t"))),col=c(1,2,4),lwd=2)

# From the S-hydrograph for this watershed, determine the:
# 2-hour unit hydrograph
# 6-hour unit hydrograph
# 12-hour unit hydrograph

h=h2
shift=h/dt
g=c(Sh,rep(0,shift))
g.shift=c(rep(0,shift),Sh)
shift.h=(g-g.shift)/h
shift.h2=c(shift.h[which(shift.h>0)],0)
shift.h2

h=h3
shift=h/dt
g=c(Sh,rep(0,shift))
g.shift=c(rep(0,shift),Sh)
shift.h=(g-g.shift)/h
shift.h3=c(shift.h[which(shift.h>0)],0)
shift.h3

at=seq(1,length(shift.h3),1)
plot(x=range(at),y=range(1,U,shift.h2,shift.h3),type="n",main="2-, 6- and 12-hour unit hydrographs\nfor the watershed in Chow 7.4.5",xlab="Hours",ylab=expression(paste("Direct runoff, ",(ft^3/s)/in.)),xaxt="n")
axis(1,at=at,labels=at*dt)
abline(v=at,lty="dotted",col="lightgray")
lines(x=1:length(U),y=U,col="black",lwd=2)
lines(x=seq(1,length(shift.h2),1),y=shift.h2,col="red",lwd=2)
lines(x=seq(1,length(shift.h3),1),y=shift.h3,col="blue",lwd=2)
legend("topright",inset=c(0.1,0.2),legend=c("2-hour","4-hour","6-hour"),title=expression(bold("Unit hydrographs")),col=c(1,2,4),lwd=2)
abline(h=seq(0,200,25),lty="dotted",col="gray")
U
shift.h2  
shift.h3

# for(i in 1:int){
#   if(i<length(U)){
#     Sh[i]=sum(dt*U)
#     }
#   else{
#     Sh[i]=sum(dt*U[1:i])
#   }
# }
# 









# Snyder defined a standard unit hydrograph as one whose rainfall duration:
#  t.r=
# is related to the basin lag t.p by 
# t.p=5.5t.r # hr (7.7.1)
# For a standard unit hydrograph he found that:
# 1. basin lag:
# t.p=C.1*C.t*(L*L.c)^0.3 # hr (7.7.2)
# L is the length of the main stream from the outlet to the upstream divide
# L.c is the distance from the outlet to a point on the stream nearest the centroid of the watershed area
# C.1=0.75 # SI
# C.1=1 # English
# C.t is a coefficient derived from gaged watersheds in the same region
# 2. The peak discharge per unit drainage area in m^3/s*km^2 (cfs/mi^2) of the standard unit hydrograph is:
# q.p=C.2*C.p/t.p # (7.7.3)
# C.2=2.75 # SI
# C.2=640 # English
# C.p is a coefficient derived from gaged watersheds in the same region
# To compute C.t and C.p for a gaged watershed, the values of L and L.c are measured from the basin map.  From a derived uit hydrograph of the watershed are obtained values for its effective duration t.R in hours, its basin lag t.pR in hours, and its peak discharge per uit drainage area, q.pR in m^3/s*km^2*cm (cfs/mi^2*in).  If t.pR=5.5t.R, then t.R=t.r, t.pR=tp, and q.pR=q.p, and C.t and C.p are computed by Eqs. (7.7.2) and (7.7.3).  If t.pR is quite different from 5.5*t.r, the standard basin lag is:
# t.p=t.pR+(t.r-t.R)/4 # (7.7.4)
# and Eq.s (7.7.1) and (7.7.4) are solved simultaneously for t.r and t.p.  The values of C.t and C.p are then computed from (7.7.2) and (7.7.3) with q.pR=q.p and t.pR=t.p.
# When an ungaged watershed appears to be similar to a gaged watershed, the coefficients C.t and C.p for the gaged watershed can be used in the above equations to derive the required synthetic unit hydrograph for the ungaged watershed.
# 3. The relationship between q.p and the peak discharge per unit drainage area q.pR of the required unit hydrograph is:
# q.pR=(q.p*t.p)/t.pR # (7.7.5)
# 4. The base time t.b in hours of the unit hydrograph can be determined using the fact that the area under the unit hydrograph is equivalent to a direct runoff of 1 cm (1 inch).  Assuming a triangular shape for the unit hydrograph, the base time may be estimated by:
# t.b=C.3/q.pR # (7.7.6)
# where
# C.3=5.56 # SI
# C.3=1290 # English
# 5. the width in hours of a unit hydrograph at a discharge equal to a certain percent of hte peak discharge q.pr is given by:
# W=C.w*q.pR^(-1.08) # (7.7.7)
# where
# C.w=122 # SI 75-percent width
# C.w=444 # english 75-percent width
# C.w=2.14 # SI 50-percent width
# C.w=770 # english 50-percent width
# Usually one-third of this width is distributed before the unit hydrograph peak time and two thirds after the peak.
# 
# Example 7.7.1
# From the basin map of a given watershed, the following quantities are measured:
# L is the length of the main stream from the outlet to the upstream divide
L=150 #km^2
# L.c is the distance from the outlet to a point on the stream nearest the centroid of the watershed area
L.c=75 #km
# drainage area:
A=3500 # km^2
# From the unit hydrograph derived for the watershed, the following ar determined:
t.R=12 # hr
t.pR=34 # hr
# peak discharge
q.p=157.5 # m^3/s * cm
# Determine the coefficients C.t and C.p for the synthetic unit hydrograph of the watershed.
# If t.pR is quite different from 5.5*t.r, the standard basin lag is:
# t.p=t.pR+(t.r-t.R)/4 # (7.7.4)
# t.p=5.5t.r # hr (7.7.1)
# and Eq.s (7.7.1) and (7.7.4) are solved simultaneously for t.r and t.p.  
t.r=(t.R-t.pR*4)/(1-4*5.5) # hr
t.p=5.5*t.r # hr
# The values of C.t and C.p are then computed from (7.7.2) and (7.7.3) with 
C.1=0.75 # SI
C.t=t.p/(C.1*(L*L.c)^0.3) # hr (7.7.2)
# The peak discharge per unit drainage area in m^3/s*km^2 of the standard unit hydrograph is:
# q.p=C.2*C.p/t.p # (7.7.3)
C.2=2.75 # SI
C.p=(q.p/A*t.p)/C.2 # (7.7.3)

# Example 7.7.2
# Compute the 6-hour synthetic unit hydrograph of a watershed having a drainage area of 2500 km^2 with L = 100 km and L.c = 50 km.  This watershed is a sub-drainage area of the watershed in example 7.7.1

A=2500 # km^2
L=100 # km
L.c=50 #km

t.p=C.1*C.t*(L*L.c)^0.3 # hr (7.7.2)
t.r=t.p/5.5
q.p=C.2*C.p/t.p # (7.7.3)
t.R=6
t.pR=t.p-(t.r-t.R)/4 # (7.7.4)
q.pR=(q.p*t.p)/t.pR # (7.7.5)
q.pR*A
C.3=5.56 # SI
t.b=C.3/q.pR # (7.7.6)
C.w.75=1.22 # SI 75-percent width
C.w.50=2.14 # SI 50-percent width
W.75=C.w.75*q.pR^(-1.08) # (7.7.7)
W.50=C.w.50*q.pR^(-1.08) # (7.7.7)

x=c(0,(t.p+t.R/2)-W.50/3,(t.p+t.R/2)-W.75/3,(t.p+t.R/2),(t.p+t.R/2)+2*W.75/3,(t.p+t.R/2)+2*W.50/3,t.b)
y=c(0,q.pR*A*0.5,q.pR*A*0.75,q.pR*A,q.pR*A*0.75,q.pR*A*0.5,0)
plot(y~x,type="o")



C=2.08
t.r=10/60
T.c=1.25
t.p=0.6*T.c
A=3
T.p=t.r/2+t.p
q.p=(C*A)/T.p

A=0.3 # mi^2
# Channel slope:
S=0.009 # ft/ft
# Main channel length:
L=2000 # ft
# percent imperviousness:
I=25 # %
# Main channel length, scenario 2:
L.2=6000 # ft
n=0.05

# Example 7.8.1
# Use the 0.5-hour unit hydrograph in table 7.4.3 (from Example 7.4.1) to produce the S-hydrograph the the 1.5-h unit hydrograph for this watershed.
U=c(404,1079,2343,2506,1460,453,381,274,173)
dt=0.5
int=length(U)*2
Sh=NULL
# for(i in 1:int){
#   if(i<length(U)){
#     Sh[i]=sum(dt*U)
#     }
#   else{
#     Sh[i]=sum(dt*U[1:i])
#   }
# }
# 
for(i in 1:int){
Sh[[i]]=sum(dt*U[1:i])
}
Sh[which(is.na(Sh))]=sum(dt*U)

plot(x=range(dt,int),y=range(1,Sh),type="n",main="S-hydrograph (original 2-hour) and offset hydrographs (4- and 6-hour)\nfor the 2-hour unit hydrograph from Chow 7.4.5",xlab="Hours",ylab=expression((ft^3/s)/in.),xaxt="n")
at=seq(dt,int,dt)
axis(1,at=at,labels=at)
abline(v=at,lty="dotted",col="lightgray")
lines(x=1:length(Sh)*dt,y=Sh,col="black",lwd=2)
h1=0.5
h2=1.5
h3=3
lines(x=(1:int)+(h1-dt),y=Sh,col="black",lwd=2)
lines(x=(dt:int)+(h2-dt),y=Sh,col="red",lwd=2)
lines(x=(dt:int)+(h3-dt),y=Sh,col="blue",lwd=2)
legend("bottomright",inset=c(0.1,0.2),legend=c("2-hour","4-hour","6-hour"),title=expression(bold("S-hydrographs")),col=c(1,2,4),lwd=2)



h=1.5
shift=h/dt
g=c(Sh,rep(Sh[length(Sh)],shift))
g.shift=c(rep(0,shift),Sh)
shift.h=(g-g.shift)/h
shift.h=c(shift.h[which(shift.h>0)],0)
shift.h

Sh

par(mar=c(5,5,4,2))


h=h2
shift=h/dt
g=c(Sh,rep(0,shift))
g.shift=c(rep(0,shift),Sh)
shift.h=(g-g.shift)/h
shift.h2=c(shift.h[which(shift.h>0)],0)
shift.h2

h=h3
shift=h/dt
g=c(Sh,rep(0,shift))
g.shift=c(rep(0,shift),Sh)
shift.h=(g-g.shift)/h
shift.h3=c(shift.h[which(shift.h>0)],0)
shift.h3

at=seq(0,(length(shift.h3)*h3)/dt,1)
plot(x=range(at),y=range(1,U,shift.h2,shift.h3),type="n",main="2-, 4- and 6-hour unit hydrographs\nfor the watershed in Chow 7.4.5",xlab="Hours",ylab=expression((ft^3/s)/in.),xaxt="n")
axis(1,at=at,labels=at*dt)
abline(v=at,lty="dotted",col="lightgray")
lines(x=1:length(U),y=U,col="black",lwd=2)

lines(x=seq(dt,(length(shift.h2)*h2)/dt,h2/dt),y=shift.h2,col="red",lwd=2)
lines(x=seq(dt,(length(shift.h3)*h3)/dt,h3/dt),y=shift.h3,col="blue",lwd=2)

summary(U)
summary(shift.h2)
summary(shift.h3)