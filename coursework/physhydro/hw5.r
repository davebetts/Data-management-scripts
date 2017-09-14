rm(list=ls())
gc()

#HW5
# RRP Chapter 5 
####################################
# Exercise 1
# 1. Consider a silty clay loam soil with the following properties:
# Porosity:
n=0.477
#Air entry tension: 
psi.a=35.6 # cm
# Pore size distribution index:
b=7.75
# Residual moisture content:
theta.r=0.15
# Hydrostatic conditions exist over a water table 1.5 m deep.
# depth to water table: 
dwt=-150 # cm
# our selected datum = water table:
zwt=0 # cm

##########################
# a) Calculate the suction (psi) and moisture content (theta) at depths of 0.5 m and 1.25 m, using:
#   * the Brooks and Corey soil moisture characteristic equations
#   * the Clapp and Hornberger simplifications
#########################
#
# If the water isn't moving (hydrostatic), the suction at the water table (psi.wt(zwt) = 0 cm) will be the same as atmospheric pressure: psi.wt = psi.surface = 0 cm.
# dh/dz = 0 cm
# q = K*dh/dz = 0 cm
# psi = -z 
# -psi is height above water table in hydrostatic conditions
# h = psi + z
# hwt = psi.wt + zwt = 0 + 0 = 0 cm
# water table depth: z0 = 0 cm
# Depths below surface:
d1=-50 # cm
d2=-125 # cm
# z values: zi = - (dwt - di):
zd1=-(dwt-d1) # cm
zd2=-(dwt-d2) # cm

### !!! ###
# Suction (psi):
psi.d1=-zd1 # cm
psi.d2=-zd2 # cm

### !!! ###
# Moisture content (theta):
# Brooks and Corey equation: |psi|=|psi.a|*S.e^(-b)
# solve for S.e: S.e=(|psi|/|psi.a|)^(-1/b)
S.e.d1=(abs(psi.d1)/abs(psi.a))^(-1/b)
S.e.d2=(abs(psi.d2)/abs(psi.a))^(-1/b)
# Effective saturation: S.e=(theta-theta.r)/(n-theta.r)
# Solve for theta: theta = (S.e*(n-theta.r))+theta.r
# The upper limit of theta is n
theta.d1=ifelse(((S.e.d1*(n-theta.r))+theta.r)>n,n,(S.e.d1*(n-theta.r))+theta.r)
theta.d2=ifelse(((S.e.d2*(n-theta.r))+theta.r)>n,n,(S.e.d2*(n-theta.r))+theta.r)
# Because the calculated ??d2 > n ??? ??d2 = n = 0.477

# Clapp and Hornberger equation: |??(??)|=|??_a|???(??/n)^(-b)
# solve for ??:  ?? = ((|??(??)|/|??_a|)^(-1/b))*n
theta.d1=ifelse(S.e.d1*n>n,n,S.e.d1*n) # The upper limit of theta is n
theta.d2=ifelse(S.e.d2*n>n,n,S.e.d2*n) # The upper limit of theta is n
# Because the calculated ??d2 > n ??? ??d2 = n = 0.477

##########################
# b) Plot a graph of the soil moisture content as a function of depth.
##########################
# range of z values: 
d=seq(0,dwt,-5)
z=-(dwt-d)
z=sort(z,T)
psi.d=-z
psi.d[length(z)]=-0.001
# Brooks and Corey moisture content estimates
S.e.d=(abs(psi.d)/abs(psi.a))^(-1/b)
# The upper limit of theta is n
# theta.d = bc
bc=ifelse(((S.e.d*(n-theta.r))+theta.r)>n,n,(S.e.d*(n-theta.r))+theta.r)
# Clapp and Hornberger moisture content estimates
# theta.d = ch
ch=ifelse(S.e.d*n>n,n,S.e.d*n) # The upper limit of theta is n
pos=seq(floor(range(c(bc,ch))[1]*100)/100-0.01,ceiling(range(c(bc,ch))[2]*100)/100,0.01)
plot(x=range(c(bc,ch)),y=range(d),type='n', xlab=expression(paste("Soil Moisture Content: ",theta)), ylab="Depth, cm", main = paste0("Soil Moisture Content as a Function of Depth\nSoil Surface (",zwt," cm), Water Table (",dwt," cm)"),xaxt="n",yaxt="n")
pos=seq(floor(range(c(bc,ch))[1]*100)/100-0.01,ceiling(range(c(bc,ch))[2]*100)/100,0.01)
axis(1,at=pos,labels=pos)
axis(2,at=seq(0,-150,-25),labels=seq(0,-150,-25))
grid()
legend("topright",inset=c(0.02,0.04),col=c("red","blue"),legend=c("Brooks and Corey", "Clapp and Hornberger"),lty=1,pch=c(1,2))

# c) Calculate the soil moisture deficit, i.e. the amount of water that could infiltrate before the occurrence of saturation excess runoff. 
# Use the Brooks and Corey soil moisture characteristic equations
z=seq(0,150,1)
D=NULL
D1=NULL
D2=NULL
for(i in 2:length(z)){
  z2=z[[i]]
  z1=z[[i-1]]
  psi.z=150-z[[2]]
  theta.z=n*((psi.z)/psi.a)^(-1/b)
  D[[i]]=(n-theta.z)*z[[i]]-z[[i-1]]
  D1[[i]]=(n-theta.r)-((n-theta.r)/(psi.a)^(-1/b))*(z2^(1-(1/b))-z1^(1-1/b))/(1-(1/b))
  D2[[i]]=(n-theta.r)*(z2-z1)-(n-theta.r)/(psi.a^(-1/b))*(z2^(1-1/b)-z1^(1-1/b))/(1-1/b)
}

D
Dsum=sum(D[-c(1,length(D))])
=(n-theta.z)(z[1])

D=(n-theta.r)-(n-theta.r)*(z2-z1)-(n-theta.r)/(psi.a^(-1/b))*(z2^(1-1/b)-z1^(1-1/b))/(1-1/b)

z1=0.01
z2=zwt-dwt
D1=(n-theta.r)-((n-theta.r)/(psi.a)^(-1/b))*(z2^(1-(1/b))-z1^(1-1/b))/(1-(1/b))

D2=(n-theta.r)*(z2-z1)-(n-theta.r)/(psi.a^(-1/b))*(z2^(1-1/b)-z1^(1-1/b))/(1-1/b)
0.412926514

z=c(psi.a,seq(ceiling(psi.a),150,0.01))
D=NULL
D1=NULL
D[[1]]=0
D1[[1]]=0
for(i in 2:length(z)){
  psi.z=z[[i]]
  theta.z=n*((psi.z)/psi.a)^(-1/b)
  D[[i]]=(n-theta.z)*(z[[i]]-z[[i-1]])
  D1[[i]]=(n-theta.r)*(z[[i]]-z[[i-1]])-(n-theta.r)/(psi.a^(-1/b))*(z[[i]]^(1-1/b)-z[[i-1]]^(1-1/b))/(1-1/b)
}
sum(D)
sum(D1)
plot(-z,D,type="l")
lines(-z,D1)
plot(-z,D1)

D=(n-theta.r)*(z2-z1)-(n-theta.r)/(psi.a^(-1/b))*(z2^(1-1/b)-z1^(1-1/b))/(1-1/b)


z1=psi.a
z2=-dwt
D.bc=(n-theta.r)*(z2-z1)-(n-theta.r)/(psi.a^(-1/b))*(z2^(1-1/b)-z1^(1-1/b))/(1-1/b)
D.ch=(n)*(z2-z1)-(n)/(psi.a^(-1/b))*(z2^(1-1/b)-z1^(1-1/b))/(1-1/b)


# 2. Consider a silty clay loam soil with the following properties 
# Porosity:
n=0.477
Ksat=0.612 # cm/hr
# Air entry tension:
psi.a=35.6 # cm
# Pore size distribution index: 
b=7.75 
# Initial moisture content:
theta.0=0.3
# a) Calculate ??f (cm) according to the Green - Ampt model.
psi.f=psi.a*(2*b+3)/(2*b+6) # [1] 30.63256
# 
# b) Given precipitation at a rate of 2 cm/h calculate the cumulative infiltration at ponding, Fp (cm), and time to ponding, tp (h)
# precipitation rate 
w=2 # cm/hr
# soil moisture content at field capacity
theta.fc=n*(340/psi.a)^(-1/b)

P=psi.f*(n-theta.fc)
Fp=(Ksat*P)/(w-Ksat)
tp=Fp/w
tp(h) = 0.81 hr
Fp = 1.63 cm
0.477-((2.39*(2-0.612))/0.612)/30.6

theta.fc=0.477-(2.39*(2-0.612)/0.612)/30.6
35.6*(0.2998609/0.477)^(-7.75)
0.477-((2.39*(2-0.612))/0.612)/30.6
# 5. Consider a soil with properties 
# Porosity
n=0.477 
Ksat=0.612 # cm/h
psi.a=35.6
b=7.75 
#	Use equation (44) to evaluate |??f| from the air entry pressure
psi.f=(2*b+3)/(2*b+6)*psi.a
# b) Use the Clapp and Hornberger (1978) simplifications of Brooks and Corey functions (equation 27) to evaluate the moisture content at field capacity defined as moisture content when ?? = -340 cm.
psi.fc=340 # cm
theta=n*(psi.fc/psi.a)^(-1/b)
# c) Assume field capacity initial conditions to evaluate the Green-Ampt parameter P=|?? | ???? f .
delta.theta=n-theta
P=psi.f*delta.theta
# d) Use the Green-Ampt model (equation 42) to plot a graph of infiltration capacity as a function of infiltrated volume for this soil.
# e) Given the following rainfall hyetograph calculate the ponding, infiltration and runoff generated in each time step.
time=c(0,1,2,3)
# Rainfall intensity (cm/hr)
w=c(1,2,4,1.4)

# 6. Consider a soil with properties 
# Porosity
n=0.477 
Ksat=0.612 # cm/h
psi.f=145.2 # cm
# Initial moisture content:
theta.0=0.3
# a) Estimate Kp = Ksat/2 and Sp from equation (61).
delta.theta=n-theta.0
kp=Ksat/2
Sp=(2*Ksat*delta.theta*psi.f)^(1/2)
# b) Use the Philip model (equation 60) to plot a graph of infiltration capacity as a function of infiltrated volume for this soil.
# c) Given the following rainfall hyetograph calculate the ponding, infiltration and runoff generated in each time step.
Time (hours) 0-1 1-2 2-3 3-4 Rainfall intensity (cm/hr) 1 2 4 1.4

#7. Consider the following storm: 
time=c(0,0.5,1,1.5)
w=c(5,3,1.5,0)
p=w*0.5

# a) Plot a graph of infiltration capacity as a function of infiltrated volume for this soil.
fc=c(6,2.659,1.566,1.221)
F=c(0,1.973,2.978,3.657)
plot(F,fc,type="l")

# b) Determine the infiltration and runoff generated in each half hour increment. 
f=c(1.973,1.005,0.679,0)
r=c(0.527,0.495,0.071,0)
# Plot your results. 
plot(range(time),range(c(p,fc,F,f,r)),type="n",xlab="hours",ylab="cm/hr")
lines(time,F,type="s",lty="dashed", col="brown")
lines(time,f,type="s",col="brown")
lines(time,fc,type="s",lty="dotted",col="red")
lines(time,r,type="s",col="blue")
lines(time,p,type="s")

plot(range(time),range(c(p,fc,F,f,r)),type="n",xlab="hours",ylab="cm/hr")
lines(time,F,type="l",lty="dashed", col="brown")
lines(time,f,type="l",col="brown")
lines(time,fc,type="l",lty="dotted",col="red")
lines(time,r,type="l",col="blue")
lines(time,p,type="l")

# State the total depths of runoff and infiltration. 
sum(r)
# total r = 1.093 cm
sum(f)
# total infiltration = 0.3657
# Indicate the times when there is ponding.
# time 1, 3
sum(p)
sum(f,r)

# 8. Consider the following runoff data on a watershed
A=0.2 # mi^2
time=1:7 # hr
# Rainfall rate
w=c(1.05,1.28,0.8,0.75,0.7,0.6,0) # in/hr
# Direct runoff per time period
r=c(0,30,60,45,30,15,0) # cfs

# a) Calculate the volume of direct runoff from this watershed in ft3. Do this by summing the cfs flows and multiplying by the number of seconds in an hour (3600).
RO=sum(r)*3600 # ft^3
# b) Calculate the per unit area depth of direct runoff by dividing your answer in (a) by the basin area. Express your answer in inches. (There are 5280 ft to a mile and 12 in to a foot).
A.in=A*(5280*12)^2
# per unit area depth of direct runoff
RO.in=RO*(12^3) # in
RO.A=RO.in/A.in # 1.39 in
# c) Calculate the total storm infiltration loss by subtracting the direct runoff (from b) from the total number of inches of precipitation.
loss=sum(w)-RO.A # 3.79 in

# d) Referring to figure 47 apportion this loss over the time steps where there is precipitation to estimate a ??-index from this storm.
# [Hint. In some time steps the rainfall rate will be less than the ??-index. You need to accommodate this in your calculations recognizing that in these cases the infiltration is the lesser of rainfall rate and ??-index.] 
phi=0.637073486
plot(time,w,type="s")
abline(h=phi,col="red")

inf=w-phi
sum(w)-sum(inf)

# e) Determine the rainfall excess generated in each time step.
# 
