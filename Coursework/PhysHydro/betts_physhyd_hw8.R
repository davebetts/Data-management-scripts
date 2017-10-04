## HW 8 - Solar Radiation and Evapotranspiration
# 
# HW8
# convert to radians
rad=2*pi/360
# Consider the location:
# latitude:
lat=41 # \degree N
# latitude in radians:
Lambda=lat*rad
# Date:
d="2016-11-20"
require(lubridate)
d.n=yday(d) 
# Assume the following weather parameters:
# Air temperature:
T.a=10 # \degree C
# Relative humidity:
W.a=0.50 # % (expressed as decimal)
# surface albedo:
a=0.3
# dust attenuation parameter:
gamma.dust=0.05

2. Make sure you understand the workings of the spreadsheet SolarRad.xls from the CD in Dingman. Verify that it treats sunrise and sunset correctly in the integration of daily clear sky radiation (equations E-7 and E-25). Sunrise should be the later of the local slope and horizon sunrise. Sunset should be the earlier of the local slope and horizon sunset.

# a) Report the declination, day angle and eccentricity for this date.
# Day angle:
Gamma=2*pi*(d.n-1)/365 # radians
# Declinattion:
delta=(180/pi)*(0.006918-0.399912*cos(Gamma)+
                  0.070257*sin(Gamma)-
                  0.006758*cos(2*Gamma)+
                  0.000907*sin(2*Gamma)-
                  0.002697*cos(3*Gamma)+
                  0.00148*sin(3*Gamma)) # degrees
delta.rad=delta*rad # radians
# Eccentricity:	E.0=(r0/r)^2
E.0=1.00011+0.034221*cos(Gamma)+0.00128*sin(Gamma)+0.000719*cos(2*Gamma)+0.000077*sin(2*Gamma)

# b) Look up the optical air mass on figure E-4 and enter this in cell D-14. 
M.pot=4.6
# Report the day length
omega=360/24*2*pi/360
# Time of sunrise:
T.sr=-acos(-tan(Lambda)*tan(delta.rad))/omega # hr.
# Time of sunset:
T.ss=acos(-tan(Lambda)*tan(delta.rad))/omega # hr.
day.len=2*T.ss # hr
, extra terrestrial and clear sky radiation for a horizontal surface. 
# c) Explore the sensitivity of radiation to slope. 
# Consider an east-west slope
# Slope azimuth, west facing slope:
alpha=270 # degrees

# Plot the change in the following parameters for slope varying from 30o west facing to 30o east facing (in 5o increments):
var=seq(30,-30,-5)
# day length

# extra terrestrial radiation:
# clear sky radiation
# range of slope variation, 30 degrees west to 30 degrees east:


#HW8.b
hw8b=read.csv("8b.txt",skip=1,header=T,sep="\t")
hw8b
attach(hw8b)
plot(x=range(degrees),y=range(na.omit(c(day.WE,day.NS))),type="n")
lines(x=degrees[which(degrees<=-30)],y=na.omit(day.WE))

Then consider a north-south slope and plot day length, extra terrestrial and clear sky radiation for slope varying from 30o south facing to 30o north facing (in 5o increments). 
d) Explore the sensitivity of radiation to relative humidity. For a horizontal location with other parameters the same as above plot extra terrestrial and clear sky radiation for relative humidity varying from 0 to 1 (in 0.1 increments). 

#HW8.3
rad=2*pi/360
# Consider the location:
# latitude:
lat=41 # \degree N
# latitude in radians:
Lambda=lat*rad
# Date:
d="2016-11-20"
require(lubridate)
d.n=yday(d) 
# Assume the following weather parameters:
# Air temperature:
T.a=10 # \degree C
# Relative humidity:
W.a=0.50 # % (expressed as decimal)
# surface albedo:
a=0.3
# dust attenuation parameter:
gamma.dust=0.05
# Day angle:
Gamma=2*pi*(d.n-1)/365 # radians
# Declinattion:
delta=(180/pi)*(0.006918-0.399912*cos(Gamma)+
                  0.070257*sin(Gamma)-
                  0.006758*cos(2*Gamma)+
                  0.000907*sin(2*Gamma)-
                  0.002697*cos(3*Gamma)+
                  0.00148*sin(3*Gamma)) # degrees
delta=delta*rad # radians
# Eccentricity:	E.0=(r0/r)^2
E.0=1.00011+0.034221*cos(Gamma)+0.00128*sin(Gamma)+0.000719*cos(2*Gamma)+0.000077*sin(2*Gamma)
# the optical air mass on figure E-4 and enter this in cell D-14. 
M.pot=4.6
# Report the day length
omega=360/24*2*pi/360
# Slope:
bslope=10 # degrees
beta=bslope*rad # radians
# Azimuth:
azdir=70 # degrees clockwise from N
alpha=azdir*rad # radians
#time of day, 11:00 am:
tod=-1 # hours away from solar noon

theta=acos(sin(Lambda)*sin(delta.rad)+cos(Lambda)*cos(delta.rad)*cos(omega*tod))


(i) Equivalent plane approach (SolRad.xls) to calculate the instantaneous extraterrestrial radiation flux, k'ET:
Equivalent latitude (Equation E-23, cell D39): 
??_eq=sin^(-1)a(sina(??)???cosa(??)???cosa(??)+cosa(??)???sina(??) )
=sin^(-1)a(sina(0.175)???cosa(1.22)???cosa(0.716)+cosa(0.175)???sina(0.716) )

=0.763 rad=43.703°

Longitude difference (Equation E-22, cell D38).
????=tan^(-1)a((sina(??)???sina(??))/(cosa(??)???cosa(??)-sina(??)???sina(??)???cosa(??) ))

=0.228 rad=13.04°
Adjusted time based on the longitude difference
t_adj=t+????/(15°)=-1+13.04/15=-0.131 hr.
Calculate the instantaneous extraterrestrial radiation flux using equation E-6 with the equivalent latitude and adjusted time: 
?? = -0.345 radians
k_ET^'=I_sc???E_0???(cosa(??)???cosa???(??_eq)??????cosa(?????t_adj )+sina(??)???sina???(??_eq)??? )
=118.1???1.025???(cosa(-0.345)???cosa(0.763)???cosa(0.2618???-0.131)+sina(-0.345)???sina(0.763) )
=54.06 MJ/(m^2 day)

#(ii) Direct approach (solarrad.pdf) to calculate the instantaneous extraterrestrial radiation flux, k'ET:
# Evaluate the zenith angle, ??, using equation E-4:
delta=-0.345 # radians
theta=acos(sin(Lambda)*sin(delta)+cos(Lambda)*cos(delta)*cos(omega*tod))

# Evaluate solar azimuth A using equation (2) (solarrad.pdf)
# cosa(A)=(sina(??)-sina(??)???cosa(??))/(cosa(??)???sina(??) )
cos.A=(sin(delta)-sin(Lambda)*cos(theta))/(cos(Lambda)*sin(theta))
A=acos(cos.A)
# Evaluate solar illumination angle z using equation (1) (solarrad.pdf)
cos.z=cos(theta)*cos(beta)+sin(theta)*sin(beta)*cos(A-alpha)
z=acos(cos.z)
# Evaluate the instantaneous extraterrestrial radiation flux with eccentricity correction included for completeness, using equation (3) (solarrad.pdf).
# Gamma
# E.0=
I.sc=118.1 # MJ/m^2 day
k.ET=I.sc*E.0*cos.z

T.ss=3.79179725
theta=acos(sin(Lambda)*sin(delta)+cos(Lambda)*cos(delta)*cos(omega*T.ss)) # Eq. E-4
cos.A=(sin(delta)-sin(Lambda)*cos(theta))/(cos(Lambda)*sin(theta)) # Eq 2 (Solarrad.pdf)
A=acos(cos.A)
A=-acos(cos.A)
A=2*pi-acos(cos.A)
cos.z=cos(theta)*cos(beta)+sin(theta)*sin(beta)*cos(A-alpha)

# 3.b.i. For a range of times (from solar noon - 7 hours to solar noon + 7 hours in hourly increments on November 20) calculate the solar azimuth and solar zenith angle. Display these graphically. 
# range of times from solar noon - 7 hours to solar noon + 7 hours in 0.5 hourly increments
tod=seq(-7,7,0.5) # hr.
# solar zenith angle
theta=acos(sin(Lambda)*sin(delta)+cos(Lambda)*cos(delta)*cos(omega*tod)) 
# solar azimuth angles
cos.A=(sin(delta)-sin(Lambda)*cos(theta))/(cos(Lambda)*sin(theta)) # Eq 2 (Solarrad.pdf)
A=NULL
for(i in seq_along(tod)){
  if(tod[i]<0){
    A[[i]]=acos(cos.A[i])
  }
  else {
    A[[i]]=2*pi-acos(cos.A[i])
  }
}

Tss=4.786966209
Tsr=-4.786966209
plot(x=range(tod),y=range(theta,A),type="n",main="Solar angles for Cache Valley\nNovember 20",xlab="Hours away from noon",ylab="Solar angle (radians)",xaxt="n",panel.first=rect(c((min(tod)-1),Tss), -1e6,c(Tsr,max(tod)+1), 1e6, col='gray88', border=NA))
axis(1,at=seq(-7,7,1),labels=seq(-7,7,1))
abline(h=seq(1,4.5,0.25),lty="dotted",col="gray")
abline(v=0,lty="dashed",col="gray48")
lines(x=tod,y=theta,type="o",col=4)
lines(x=tod,y=A,type="o",pch=2,col=2)
legend("topleft",inset=c(0.05,0.1),legend=c("Azimuth angle","Zenith angle","Noon","Night"),lty=c(1,1,2,NA),pch=c(2,1,NA,NA),col=c(2,4,"gray48",NA),density=c(0,0,0,NA),fill = c("red","blue","gray48","gray"),border = c(NA,NA,NA,"gray"))

#HW3.b.ii. Considering the valley geometry
# distance from the center of the valley to the mountains on either side with valley width of 15 km:
x=15/2 # km
# height of the mountains on both sides of valley, 1200m:
H=1200/1000 # km
# calculate the angle to the horizon in the direction of the sun (solar azimuth) for each of the times in (3.b.i)
L=NULL
for(i in seq_along(A)){
  if(A[i]<=pi){
    L[[i]]=x/(cos(A[i]-pi/2))
  }
  else {
    L[[i]]=-x/(cos(A[i]-pi/2))
  }
}
L
# and solve for the time when the sun first impacts and last disappears from the location at the center of the valley (remembering that it is sloping).
h=atan(H/L)
sun.angle=(pi/2)-theta
sun=range(tod[which(sun.angle>h)])
-4 and 4
cos.z=cos(theta)*cos(beta)+sin(theta)*sin(beta)*cos(A-alpha)
tbl=rbind(tod,L,h,sun.angle,cos.z)

# iii.Calculate the daily extra-terrestrial solar radiation accounting for shadowing for this location by numerically integrating IscEocos z between the times evaluated in (ii).
K.ET=(I.sc*E.0*sum(cos.z[which(sun.angle>h)])*0.5/24)

(iv.Use the spreadsheet solarrad.xls to evaluate the extra terrestrial solar radiation by the equivalent plane approach for this location neglecting the shadowing effects of the horizons. 
(v. Modify the spreadsheet solarrad.xls to evaluate the extra terrestrial solar radiation by the equivalent plane approach between the times computed in (ii) (by evaluating equation E-25 (cell C42) between these times. 
(vi.Explain the reasons for differences between your answers in (iii), (iv) and (v). Which is most accurate?

#HW8.4
#HW8.4a
temp=22
p.s=610.78*exp(t/(t+238.3)*17.2694)
W.a=.5
p=W.a*p.s
w=log(p/610.78)
dew=w*238.3/(17.294-w)

# https://iridl.ldeo.columbia.edu/dochelp/QA/Basic/dewpoint.html
tempK=temp+273.15
E0=0.611 #kPa
L.Rv=5423 # L/R_v = 5423 K
temp0=273 # K
E.s=E0*exp(L.Rv*((1/temp0)-(1/tempK)))
E=W.a*E.s
dew=1/((L.Rv*(1/temp0))-log(E/E0))

0.5=E/E.s

# http://www.vaisala.com/Vaisala%20Documents/Application%20notes/Humidity_Conversion_Formulas_B210973EN-F.pdf
A=6.116441
temp=22
m=7.591386
Tn=240.7263
P.ws=A*10^((m*temp)/(temp+Tn))
P.w=P.ws*W.a
Td=Tn/((m/(log10(P.w/A)))-1)

#or
temp=22
tempK=temp+273.15
Tc=647.096 # K Critical temperature
v=1-tempK/Tc
Pc=220640 # Critical pressure hPa
C1=-7.85951783
C2=1.84408259
C3=-11.7866497
C4=22.6807411
C5=-15.9618719
C6=1.80122502
Pws=Pc*exp((Tc/tempK)*(C1*v+C2*v^1.5+C3*v^3+C4*v^3.5+C5*v^4+C6*v^7.5))
Pw=Pws*W.a
A=6.116441
m=7.591386
Tn=240.7263???
Tdew=Tn/((m/(log10(Pw/A)))-1)
???

###################### from book *#####
Ts=19 #\degree C
e.s=0.611*exp((17.3*Ts)/(Ts+237.3))
Ta=22 #\degree C
e.a=0.611*exp((17.3*Ta)/(Ta+237.3))
W.a=0.5
ea=e.a*W.a
Td=(log(ea)+0.4926)/(0.0708-0.00421*log(ea))
P=85 # kPa
q=(0.622*ea)/P
Ra=0.288
K=Ts
rho.a=P/(K*Ra)

#HW8.4.b.i
Ta=22 #\degree C
lambda.v=2.501*10^6-2370*Ta # J/kg
gamma=66.8 # Pa/(°C)
rho.w=1000 # kg/m-3
Ts=19 #\degree C
e.s=0.611*exp((17.3*Ts)/(Ts+237.3))
Delta=(4098*e.s)/((237.3+Ts)^2)*1000# Pa/C
alpha=1.26 # for humid regions
G=0
R.n=90*3600*24 # J/day
E.r=(1/(lambda.v*rho.w))*(R.n-G) 
E.i=alpha*Delta/(Delta+gamma)*E.r*1000


# HW8.4.b.ii. Mass Transfer/Aerodynamic 
# Relative constants:
Ts=19 #\degree C
e.s=0.611*exp((17.3*Ts)/(Ts+237.3))
Ta=22 #\degree C
e.a=0.611*exp((17.3*Ta)/(Ta+237.3))
W.a=0.5
ea=e.a*W.a
k=0.4
p=10^5 # Pa
rho.a=1.2 # kg/m^3
rho.w=1000 # kg/m^3
z2=2 # m
z0=4*10^(-4) # m
v.a=2.5 #m/s
K.E=(0.622*k^2*rho.a)/((p*(log(z2/z0))^2)*rho.w)*1000
B=K.E*v.a*3600*24*1000
E.ii=B*(e.s-ea)

#HW8.4.b.iii
Ts=19 #\degree C
e.s=0.611*exp((17.3*Ts)/(Ts+237.3))
Delta=(4098*e.s)/((237.3+Ts)^2) # kPa/C
R.n=90 # W/m^2
Rn=R.n*3600*24 # J/m^2 day
G=0
gamma=66.8
Ta=22 #\degree C
e.a=0.611*exp((17.3*Ta)/(Ta+237.3))
W.a=0.5
ea=e.a*W.a
lambda.v=2.501*10^6-2370*Ta # J/kg
P=85# kPa
v.a=2.5 # m/s
k=0.4
p=10^5 # Pa
rho.w=1000 # kg/m^3
rho.a=1.2 # kg/m^3
z2=2 # m
z0=4*10^(-4) # m
K.E=(0.622*k^2*rho.a)/(p*rho.w*(log(z2/z0))^2)*1000
B=K.E*v.a*3600*24*1000
E.a=B*(e.a-ea)
E.r=(1/(lambda.v*rho.w))*(Rn-G)*1000

E.iii=Delta/(Delta+gamma)*E.r+E.a*gamma/(gamma+Delta)

### wikipedia formula
m=Delta
Rn=R.n*3600*24/10^6
lambdav=lambda.v/10^6
gamma2=(0.0016286*P)/lambdav
delta.e=e.s-ea
m.Delta=(5336/(Ta+273.15)^2)*exp(21.07-5336/(Ta+273.15))*101.325/760

Emass=(m*Rn+gamma2*6.43*(1+0.536*v.a)*delta.e)/(lambdav*(m+gamma2))
Emass2=(m.Delta*Rn+gamma2*6.43*(1+0.536*v.a)*delta.e)/(lambdav*(m.Delta+gamma2))

## do book formulas in 7.3.5
C.a=1*10^(-3) # MJ/kg K
gamma=C.a*P/(0.622*lambda.v)


# iv # Bowen Ration Energy Balance Method
Ts=19 #\degree C
e.s=0.611*exp((17.3*Ts)/(Ts+237.3))
Ta=22 #\degree C
e.a=0.611*exp((17.3*Ta)/(Ta+237.3))
W.a=0.5
ea=e.a*W.a
p=10^5 # Pa
c.p=1005 # J/kg C
R.n=90 # W/m^2
Rn=R.n*3600*24 # MJ/m^2 day
G=0
lambda.v=2.501*10^6-2370*Ta # J/kg
gamma=(c.p*p)/(0.622*lambda.v)
rho.w=1000 # kg/m^3
beta=gamma*((Ts-Ta)/((e.s-ea)*1000))

E.iv=(1/(lambda.v*rho.w))*((Rn-G)/(1+beta))*1000 # mm/day

#HW8.4c
#HW8.4.b.i
Ta=22 #\degree C
lambda.v=2.501*10^6-2370*Ta # J/kg
gamma=66.8 # Pa/(°C)
rho.w=1000 # kg/m-3
Ts=19 #\degree C
e.s=0.611*exp((17.3*Ts)/(Ts+237.3))
Delta=(4098*e.s)/((237.3+Ts)^2)*1000# Pa/C
alpha=1.26 # for humid regions
G=0
R.n=90*3600*24 # J/day
E.r=(1/(lambda.v*rho.w))*(R.n-G) 
E.i.c=alpha*Delta/(Delta+gamma)*E.r*1000


# ii. Mass Transfer/Aerodynamic 
# Relative constants:
Ts=19 #\degree C
e.s=0.611*exp((17.3*Ts)/(Ts+237.3))
Ta=22 #\degree C
e.a=0.611*exp((17.3*Ta)/(Ta+237.3))
W.a=0.5
ea=e.a*W.a
k=0.4
p=10^5 # Pa
rho.a=1.2 # kg/m^3
rho.w=1000 # kg/m^3
z2=2 # m
z0=4*10^(-4) # m
v.a=5 #m/s
K.E=(0.622*k^2*rho.a)/((p*(log(z2/z0))^2)*rho.w)*1000
B=K.E*v.a*3600*24*1000
E.ii.c=B*(e.s-ea)

# iii
Ts=19 #\degree C
e.s=0.611*exp((17.3*Ts)/(Ts+237.3))
Delta=(4098*e.s)/((237.3+Ts)^2) # kPa/C
R.n=90 # W/m^2
Rn=R.n*3600*24 # J/m^2 day
G=0
gamma=66.8
Ta=22 #\degree C
e.a=0.611*exp((17.3*Ta)/(Ta+237.3))
W.a=0.5
ea=e.a*W.a
lambda.v=2.501*10^6-2370*Ta # J/kg
P=85# kPa
v.a=5 # m/s
k=0.4
p=10^5 # Pa
rho.a=1.2 # kg/m^3
rho.w=1000 # kg/m^3
z2=2 # m
z0=4*10^(-4) # m
K.E=(0.622*k^2*rho.a)/(p*rho.w*(log(z2/z0))^2)*1000
B=K.E*v.a*3600*24*1000
E.a=B*(e.a-ea)
E.r=(1/(lambda.v*rho.w))*(Rn-G)*1000

E.iii.c=Delta/(Delta+gamma)*E.r+E.a*gamma/(gamma+Delta)

### wikipedia formula
m=Delta
Rn=R.n*3600*24/10^6
lambdav=lambda.v/10^6
gamma2=(0.0016286*P)/lambdav
delta.e=e.s-ea
m.Delta=(5336/(Ta+273.15)^2)*exp(21.07-5336/(Ta+273.15))*101.325/760

Emass=(m*Rn+gamma2*6.43*(1+0.536*v.a)*delta.e)/(lambdav*(m+gamma2))
Emass2=(m.Delta*Rn+gamma2*6.43*(1+0.536*v.a)*delta.e)/(lambdav*(m.Delta+gamma2))

## do book formulas in 7.3.5
C.a=1*10^(-3) # MJ/kg K
gamma=C.a*P/(0.622*lambda.v)


# iv # Bowen Ration Energy Balance Method
Ts=19 #\degree C
e.s=0.611*exp((17.3*Ts)/(Ts+237.3))
Ta=22 #\degree C
e.a=0.611*exp((17.3*Ta)/(Ta+237.3))
W.a=0.5
ea=e.a*W.a
p=10^5 # Pa
c.p=1005 # J/kg C
R.n=90 # W/m^2
Rn=R.n*3600*24 # MJ/m^2 day
G=0
lambda.v=2.501*10^6-2370*Ta # J/kg
gamma=(c.p*p)/(0.622*lambda.v)
rho.w=1000 # kg/m^3
beta=gamma*((Ts-Ta)/((e.s-ea)*1000))

E.iv.c=(1/(lambda.v*rho.w))*((Rn-G)/(1+beta))*1000 # mm/day

v1=c(E.i,E.ii,E.iii,E.iv)
v2=c(E.i.c,E.ii.c,E.iii.c,E.iv.c)
plot(v2/v1)
v1
v2
x=mean(v1)
y=sqrt((v1-x)^2)
y.avg=mean(y)
y.avg
tot=max(v1)-min(v1)
tot

HW8.5 - Dingman Problem 7.1. 
# Measurements of water-balance components have been made for a one-year period on a lake.
# Lake area:
A.L=4.2 # km^2
# Drainage basin area (including the lake):
A.D=52.1 # km^2
# Precipitaiton on the lake:
W=1083 # mm
# mm*km^2*1m/1000mm*(1000m/km)^2 = m^3
W=W*A.L*1000 # m^3
# Surface water inflow:
SW.in=2.33*10^7 # m^3
# Groundwater inflow: 
GW.in=2.2*10^4 # m^3
# Groundwater outlfow
GW.out=0.6*10^5 # m^3
# Surface water outflow
SW.out=2.70*10^6 # m^3
# The lake surface elevation, h, at the end of the year was 108 mm higher than at the beginning
Deltah=108 # mm
# a.	What is the water-balance estimate of the lake evaporation for that year [Equation (7-16)]?
# Eq. (7-16) E = W + SW_in + GW_in - SW_out - GW_out - \DeltaV
# mm*km^2*1m/1000mm*(1000m/km)^2 = m^3
DeltaV=Deltah*A.L*1000 # m^3
E.m3=W+SW.in+GW.in-SW.out-GW.out-DeltaV # m^3
E=E.m3/A.L # m^3/km^2
#m^3/km^2*(1km/1000m)^2*1000mm/m = mm
E=E/1000 # mm/yr
# [1] 5870.714
E=E/365.25 # mm/day
# [1] 16.07314

# b.	Referring to table 7-2, give a qualitative evaluation of the uncertainty of this estimate.
# Table 7-2: Range of uncertainty in Precipitaiton and Streamflow Values Used in Computing Lake Water Balances
# Values are percentages of the true values.  Those without parentehses are for "best" methodology; those in parentheses are "commonly used" methodology
# # Seasonal/annual uncertainty:
# # Precipitation - general range, valudes for typical lake in No.U.S. best and common used:
# uP=c(c(5,10,7.5),8,17)/100
# # Streamflow inputs - general range, valudes for typical lake in No.U.S. best and common used:
# uSW.in=c(c(5,15,30),9,23)/100
# # Streamflow outputs - general range, valudes for typical lake in No.U.S. best and common used:
# uSW.out=c(c(5,5,15),9,12)/100
# # category names
# catn=c("General Range Low","General Range High","General Range Commonly Used","Lakes Best","Lakes Commonly Used")
# tbl=as.data.frame(rbind(uP,uSW.in,uSW.out))
# colnames(tbl)=catn
# tbl

uP=c(c(5,10,NA),8,17)/100
# Streamflow inputs - general range, valudes for typical lake in No.U.S. best and common used:
uSW.in=c(c(5,15,30),9,23)/100
# Streamflow outputs - general range, valudes for typical lake in No.U.S. best and common used:
uSW.out=c(c(5,NA,15),9,12)/100
# category names
uV=c(c(5,10,NA),8,17)/100
# Streamflow inputs - general range, valudes for typical lake in No.U.S. best and common used:
uGW.in=c(c(5,15,30),9,23)/100
# Streamflow outputs - general range, valudes for typical lake in No.U.S. best and common used:
uGW.out=c(c(5,NA,15),9,12)/100
# category names
catn=c("General Range Low","General Range High","General Range Commonly Used","Lakes Best","Lakes Commonly Used")
tbl=as.data.frame(rbind(uP,uV,uSW.in,uGW.in,uSW.out,uGW.out))
colnames(tbl)=catn
tbl

# Estimated error standard deviation for precipitation:
sP=uP*W/2 # 1.96~=2
# Estimated error standard deviation for streamflow in:
sSW.in=uSW.in*SW.in/2
# Estimated error standard deviation for streamflow out:
sSW.out=uSW.out*SW.out/2
# Estimated error standard deviation for precipitation:
sV=uV*W/2 # 1.96~=2
d# Estimated error standard deviation for streamflow in:
sGW.in=uGW.in*GW.in/2
# Estimated error standard deviation for streamflow out:
sGW.out=uGW.out*GW.out/2
sP
sSW.in
sSW.out
sV
sGW.in
sGW.out
sE=sqrt(sP^2+sSW.in^2+sSW.out^2)
sE=sqrt(sP^2+sSW.in^2+sSW.out^2+sV^2+sGW.in^2+sGW.out^2)
names(sE)=catn
sE
2*sE
uE=2*sE/E.m3*100
uE
sET=uE/100*E/2
2*sET/16

### checking some math ###
mP=10 #m3
mQ=4 #m3
uP=0.1
uQ=0.15
sP=mP*uP/2
sQ=mQ*uQ/2
sET=sqrt(sP^2+sQ^2)
uET=2*sET/(mP-mQ)*100
# 
# A.D
# A.L
# catn
# Deltah
# DeltaV
# E
# E.m3
# GW.in
# GW.out
# i
# mP
# mQ
# sE
# sET
# sP
# sQ
# sSW.in
# sSW.out
# SW.in
# SW.out
# tbl
# uE
# uET
# uP
# uQ
# uSW.in
# uSW.out
# W
# x
# y


# 6. Dingman Problem 7.2. 
# The table that follows gives the hourly air temperature, T_a (° C), relative humidity, W_a, wind speed, v_a, and waters -surface temperatuere, T_s (° C), for Lake Hefner, Ok, on 3 may 1951 (HefnrHly.xls).
# Lake area: A = 9.4 km^2
# Compare the average evaporation rate for that day via the mass transfer aporoach by
tbl=read.csv("hw8.csv")
Ts=tbl$Ts #\degree C
e.s=0.611*exp((17.3*Ts)/(Ts+237.3))
Ta=tbl$Ta #\degree C
e.a=0.611*exp((17.3*Ta)/(Ta+237.3))
W.a=tbl$Wa
ea=e.a*W.a
k=0.4
p=10^5 # Pa
rho.a=1.2 # kg/m^3
rho.w=1000 # kg/m^3
z2=2 # m
z0=4*10^(-4) # m
v.a=tbl$va #m/s
K.E=(0.622*k^2*rho.a)/((p*(log(z2/z0))^2)*rho.w)*1000
B=K.E*v.a*3600*24*1000
E.tbl=B*(e.s-ea)
E.avg=mean(E.tbl)
# a.	Calculating the evaporation rate for each hour and averageing the results
Ts=tbl$Ts #\degree C
e.s=0.611*exp((17.3*Ts)/(Ts+237.3))
Ta=tbl$Ta #\degree C
e.a=0.611*exp((17.3*Ta)/(Ta+237.3))
W.a=tbl$Wa
ea=e.a*W.a
k=0.4
p=10^5 # Pa
rho.a=1.2 # kg/m^3
rho.w=1000 # kg/m^3
z2=2 # m
z0=4*10^(-4) # m
v.a=tbl$va #m/s
K.E=(0.622*k^2*rho.a)/((p*(log(z2/z0))^2)*rho.w)*1000
B=K.E*v.a*3600*24*1000
E.tbl=B*(e.s-ea)
E.avg=mean(E.tbl)

# b.	Averaging the values in the table and using those averages in the mass-transfer equation.
tbl=read.csv("hw8.csv")
tbl=sapply(tbl,mean)
Ts=tbl["Ts"] #\degree C
e.s=0.611*exp((17.3*Ts)/(Ts+237.3))
Ta=tbl["Ta"] #\degree C
e.a=0.611*exp((17.3*Ta)/(Ta+237.3))
W.a=tbl["Wa"]
ea=e.a*W.a
k=0.4
p=10^5 # Pa
rho.a=1.2 # kg/m^3
rho.w=1000 # kg/m^3
z2=2 # m
z0=4*10^(-4) # m
v.a=tbl["va"] #m/s
K.E=(0.622*k^2*rho.a)/((p*(log(z2/z0))^2)*rho.w)*1000
B=K.E*v.a*3600*24*1000
E.day=B*(e.s-ea)
Ts
Ta
W.a
v.a
# 7. Dingman Problem 7.6. 
# Use the spreadsheet program PenMontx.xls to explore the effects on the evapotranspiration rate of any tow of the Input-Data variables, as computed via the Penman-Monteith Equation.  Write a paragraph or two, supplemented with appropriate graphs, describing the sensitivity of ET to the variables you selected.
# 
