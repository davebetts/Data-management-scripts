#HW4
#RainfallRunoffProcesses workbook - Chapter 4

#RRP4.7
# Oven dry mass of soil:
M.sdry=264.8 #g
M.m=M.sdry
#Length of soil sample: 
L=10 #cm
# diameter of soil sample cross-section
D=5 # cm
r=D/2 # cm
#Area of soil sample:
A=pi*r^2 # cm^2
# Total volume of soil sample:
V.s=L*A # cm^3
#Bulk density of soil sample: 
rho.b=M.m/V.s # g/cm^3
#Assumed soil particle density: 
rho.m=2.65 # g/cm^3
#Porosity: 
n=1-(rho.b/rho.m)
#Density of water:
rho.w=1000*1000*(1/100)^3 # kg/m^3 * g/kg * (m/cm)^3 = g/cm^3
# Mass of wet soil: 
M.swet=302.5 # g
#Volumetric soil moisture content: 
theta=(M.swet-M.sdry)/(rho.w*V.s)
# Degree of saturation: 
S.d=theta/n

## values for RRP4.7
for ( obj in ls() ) { cat('---',obj,'---\n'); print(get(obj)) }
# --- A ---
#   [1] 19.63495
# --- D ---
#   [1] 5
# --- L ---
#   [1] 10
# --- M.m ---
#   [1] 264.8
# --- M.sdry ---
#   [1] 264.8
# --- M.swet ---
#   [1] 302.5
# --- n ---
#   [1] 0.4910886
# --- r ---
#   [1] 2.5
# --- rho.b ---
#   [1] 1.348615
# --- rho.m ---
#   [1] 2.65
# --- rho.w ---
#   [1] 1
# --- S.d ---
#   [1] 0.3909774
# --- theta ---
#   [1] 0.1920045
# --- V.s ---
#   [1] 196.3495
rm(obj)

#RRP4.8
# Oven dry mass of soil:
M.sdry=274.5 #g
M.m=M.sdry
#Length of soil sample: 
L=10 #cm
# diameter of soil sample cross-section
D=5 # cm
r=D/2 # cm
#Area of soil sample:
A=pi*r^2 # cm^2
# Total volume of soil sample:
V.s=L*A # cm^3
#Bulk density of soil sample: 
rho.b=M.m/V.s # g/cm^3
#Assumed soil particle density: 
rho.m=2.65 # g/cm^3
#Porosity: 
n=1-(rho.b/rho.m)
#Density of water:
rho.w=1000*1000*(1/100)^3 # kg/m^3 * g/kg * (m/cm)^3 = g/cm^3
# Mass of wet soil: 
M.swet=390.5 # g
#Volumetric soil moisture content: 
theta=(M.swet-M.sdry)/(rho.w*V.s)
# Degree of saturation: 
S.d=theta/n

## values for RRP4.8
for ( obj in ls() ) { cat('---',obj,'---\n'); print(get(obj)) }
# --- A ---
#   [1] 19.63495
# --- D ---
#   [1] 5
# --- L ---
#   [1] 10
# --- M.m ---
#   [1] 274.5
# --- M.sdry ---
#   [1] 274.5
# --- M.swet ---
#   [1] 390.5
# --- n ---
#   [1] 0.4724464
# --- r ---
#   [1] 2.5
# --- rho.b ---
#   [1] 1.398017
# --- rho.m ---
#   [1] 2.65
# --- rho.w ---
#   [1] 1
# --- S.d ---
#   [1] 1.250477
# --- theta ---
#   [1] 0.5907831
# --- V.s ---
#   [1] 196.3495
rm(obj)

#RRP4.10
lseq <- function(from=x, to=y, length.out=z) {
  # logarithmic spaced sequence
  # blatantly stolen from library("emdbook"), because need only this
  exp(seq(log(from), log(to), length.out = length.out))
}

numbers=c(50,100,19,100,9.5,100,4.76,98,2,95,0.42,80,0.074,60,0.020,42,0.005,35,0.002,30)
odd=seq(1,length(numbers),2)
even=seq(2,length(numbers),2)
D=numbers[odd] # Diameter (mm)
p.p=numbers[even] # percent passing
plot(c(0.001,50),c(0,100),type="n", log="x",xaxt="n", xlim=c(0.001,50), ylim=c(0,100),main="Rainfall-Runoff Production\nChap. 4, Question 10", ylab="Percentage passing", xlab="grain size")

x=lseq(0.001,100,6)
x1=c(x,x/2)
x1[8]=5
axis(1,x1,labels=x1)
axis(2,seq(10,100,10),labels=seq(10,100,10))

y=rep(1:10,5)
x2=rep(x[-c(length(x))],each=10)
x3=x2*y
abline(v=x3,h=seq(10,100,10),lty="dotted",col="light grey")
abline(v=c(0.002,0.05,2),lty="dashed",col="blue")
legend(0.001,90,border="white",legend="", title="clay",cex=0.75,bty="n")
legend(0.007,90,border="white",legend="", title="silt",cex=0.75,bty="n")
legend(0.25,90,border="white",legend="", title="sand",cex=0.75,bty="n")
legend(10,90,border="white",legend="", title="gravel",cex=0.75,bty="n")
lines(x=c(0.0005,0.002,0.05,2,75),y=c(80,80,80,80,80),type="o",pch=4,col="red")
lines(x=D,y=p.p,type="o")

sand=95-55
silt=55-30
clay=30
all=c(sand,silt,clay)
ng=round(all/(95/100))
ng
# [1] 42 26 32
# clay loam

#RRP4.11
lseq <- function(from=x, to=y, length.out=z) {
  # logarithmic spaced sequence
  # blatantly stolen from library("emdbook"), because need only this
  exp(seq(log(from), log(to), length.out = length.out))
}

numbers=c(50,100,19,100,9.5,100,4.76,95,2,92,0.42,80,0.074,70,0.020,65,0.005,40,0.002,20)
odd=seq(1,length(numbers),2)
even=seq(2,length(numbers),2)
D=numbers[odd] # Diameter (mm)
p.p=numbers[even] # percent passing
plot(c(0.001,50),c(0,100),type="n", log="x",xaxt="n", xlim=c(0.001,50), ylim=c(0,100),main="Rainfall-Runoff Production\nChap. 4, Question 11", ylab="Percentage passing", xlab="grain size")

x=lseq(0.001,100,6)
x1=c(x,x/2)
x1[8]=5
axis(1,x1,labels=x1)
axis(2,seq(10,100,10),labels=seq(10,100,10))

y=rep(1:10,5)
x2=rep(x[-c(length(x))],each=10)
x3=x2*y
abline(v=x3,h=seq(10,100,10),lty="dotted",col="light grey")
abline(v=c(0.002,0.05,2),lty="dashed",col="blue")
legend(0.001,90,border="white",legend="", title="clay",cex=0.75,bty="n")
legend(0.007,90,border="white",legend="", title="silt",cex=0.75,bty="n")
legend(0.25,90,border="white",legend="", title="sand",cex=0.75,bty="n")
legend(10,90,border="white",legend="", title="gravel",cex=0.75,bty="n")
lines(x=c(0.0005,0.002,0.05,2,75),y=c(80,80,80,80,80),type="o",pch=4,col="red")
lines(x=D,y=p.p,type="o")

sand=92-68
silt=68-20
clay=20
all=c(sand,silt,clay)
ng=round(all/0.92,0)
ng
# [1] 26 52 22
# silt loam

## RRP4.12
# Hydraulic conductivity is determined in a Darcy experiment conducted using water at 20 °C to be 30 cm/hr. 
# The viscosity of water at 20 °C is 1.05 x 10-3 N s m-2.
# Using g=9.81 m/s2 and ??w =1000 kg/m3 calculate the intrinsic permeability of this material.
# Hydraulic conductivity:  
K = 30 # gamma/mu*k; cm/hr
# the density of water
rho.w = 1000 # kg/m^3
# gravitational acceleration
g=9.81 # m/s^2
# the specific weight of water
gamma = rho.w*g
# the dynamic viscosity of water
mu=1.05*10^(-3) # N.s/m^2
# intrisic permiability
# conversion from N to kgm/s^2
# cm/hr*(N.s/m^2)*(m^3/kg)*(s^2/m)*((kg*m)/s^2)*1/N
k = K*mu/gamma # cm/hr*s*m
kcm = k*100/3600 # cm^2
km = (k/100)/3600 # m^2

#RRP4.13
# Following is data from a Darcy experiment using the notation depicted in figure 25. 
# Fill in the blanks and 
# calculate the hydraulic conductivity. 

# flow rate in = flow rate out
# liters are converted to cm^3; 1000 cm^3/l
Q=0.5*1000 # cm^3/hr
#The internal diameter of the circular tube
D=10 # cm
# cross-sectional area of circular tube
A=pi*(D/2)^2 # cm^2
# specific discharge
q=Q/A # cm/hr
# porosity
n=0.32
# Average velocity through the sample
V=q/n # cm/hr,
# This experiment is conducted at 20 °C.
# the dynamic viscosity of water at 20 °C
mu=1.05*10^(-3) # N.s/m^2 * ((kg m)/s^2)/N = kg/(m*s) 
# hydraulic head of piezometer 1
h1=70 # cm
# hydraulic head of piezometer 2
h2=58 # cm
# difference in hydraulic head between piezometers, ??h
delta.h=h1-h2 # cm
# the length ??l, between piezometers
delta.l=40 # cm
# hydraulic gradient
dh.dl=delta.h/delta.l
# elevation head for piezometer 1
z1=50 # cm
# elevation head for piezometer 2
z2=30 # cm
# pressure head for piezometer 1, ??1	(cm)
psi.1=h1-z1 # cm
# pressure head for piezometer 2, ??2	(cm)	
psi.2=h2-z2 # cm
# the density of water
rho.w = 1000 # kg/m^3
# gravitational acceleration
g=9.81 # m/s^2
# the specific weight of water
gamma=rho.w*g # kg/m^3*m/s^2 = kg/(m^2*s^2)
# pressure at the bottom of piezometer 1
# h-z is converted from cm to m
p1=((h1-z1)/100)*gamma # Pa = kg/(m*s^2)
# pressure at the bottom of piezometer 2
# h-z is converted from cm to m
p2=((h2-z2)/100)*gamma  # Pa = kg/(m*s^2); 
# hydraulic conductivity
K=q*1/(dh.dl) # cm/hr
# specific conductivity
# convert to m^2
# cm/hr*kg/(m*s)*(m^2*s^2)/kg*1m/100cm*1hr/3600s = m^2
k=(K*mu/gamma)/(100*3600) # m^2
kcm=k*100*100 # cm^2
# effective grain diameter
d=(k/n)^(1/2) # m
# Reynold's number
# kg/m^3*cm/hr*m*(m*s)/kg = 1/m*cm/hr*s
# convert cm to m and s to hr
# cm/hr*s/m*1hr/3600s*1m/100cm=1/(3600*100)
factor=1/(3600*100)
Re=factor*rho.w*q*d/mu	# kg/(m^2*s^2)*cm/hr*m*(m*s)/kg	

## values for RRP4.13
for ( obj in ls() ) { cat('---',obj,'---\n'); print(get(obj)) }
# --- A ---
#   [1] 78.53982
# --- d ---
#   [1] 4.440311e-06
# --- D ---
#   [1] 10
# --- delta.h ---
#   [1] 12
# --- delta.l ---
#   [1] 40
# --- dh.dl ---
#   [1] 0.3
# --- factor ---
#   [1] 2.777778e-06
# --- g ---
#   [1] 9.81
# --- gamma ---
#   [1] 9810
# --- h1 ---
#   [1] 70
# --- h2 ---
#   [1] 58
# --- k ---
#   [1] 6.309234e-12
# --- K ---
#   [1] 21.22066
# --- kcm ---
#   [1] 6.309234e-08
# --- mu ---
#   [1] 0.00105
# --- n ---
#   [1] 0.32
# --- p1 ---
#   [1] 1962
# --- p2 ---
#   [1] 2746.8
# --- psi.1 ---
#   [1] 20
# --- psi.2 ---
#   [1] 28
# --- q ---
#   [1] 6.366198
# --- Q ---
#   [1] 500
# --- Re ---
#   [1] 7.478279e-05
# --- rho.w ---
#   [1] 1000
# --- V ---
#   [1] 19.89437
# --- z1 ---
#   [1] 50
# --- z2 ---
#   [1] 30
rm(obj)

#RRP4.14
# Following is data from a Darcy experiment using the notation depicted in figure 25. 
# Fill in the blanks and 
# calculate the hydraulic conductivity. 

# flow rate in = flow rate out
# liters are converted to cm^3; 1000 cm^3/l
Q=2.2*1000 # cm^3/hr
#The internal diameter of the circular tube
D=10 # cm
# cross-sectional area of circular tube
A=pi*(D/2)^2 # cm^2
# specific discharge
q=Q/A # cm/hr
# porosity
n=0.4
# Average velocity through the sample
V=q/n # cm/hr,
# This experiment is conducted at 20 °C.
# the dynamic viscosity of water at 20 °C
mu=1.05*10^(-3) # N.s/m^2 * ((kg m)/s^2)/N = kg/(m*s) 
# hydraulic head of piezometer 1
h1=56 # cm
# hydraulic head of piezometer 2
h2=35 # cm
# difference in hydraulic head between piezometers, ??h
delta.h=h1-h2 # cm
# the length ??l, between piezometers
delta.l=40 # cm
# hydraulic gradient
dh.dl=delta.h/delta.l
# elevation head for piezometer 1
z1=50 # cm
# elevation head for piezometer 2
z2=30 # cm
# pressure head for piezometer 1, ??1	(cm)
psi.1=h1-z1 # cm
# pressure head for piezometer 2, ??2	(cm)	
psi.2=h2-z2 # cm
# the density of water
rho.w = 1000 # kg/m^3
# gravitational acceleration
g=9.81 # m/s^2
# the specific weight of water
gamma=rho.w*g # kg/m^3*m/s^2 = kg/(m^2*s^2)
# pressure at the bottom of piezometer 1
# h-z is converted from cm to m
p1=((h1-z1)/100)*gamma # Pa = kg/(m*s^2)
# pressure at the bottom of piezometer 2
# h-z is converted from cm to m
p2=((h2-z2)/100)*gamma  # Pa = kg/(m*s^2); 
# hydraulic conductivity
K=q*1/(dh.dl) # cm/hr
# specific conductivity
# convert to m^2
# cm/hr*kg/(m*s)*(m^2*s^2)/kg*1m/100cm*1hr/3600s = m^2
k=(K*mu/gamma)/(100*3600) # m^2
kcm=k*100*100 # cm^2
# effective grain diameter
d=(k/n)^(1/2) # m
# Reynold's number
# kg/m^3*cm/hr*m*(m*s)/kg = 1/m*cm/hr*s
# convert cm to m and s to hr
# cm/hr*s/m*1hr/3600s*1m/100cm=1/(3600*100)
factor=1/(3600*100)
Re=factor*rho.w*q*d/mu	# kg/(m^2*s^2)*cm/hr*m*(m*s)/kg	

## values for RRP4.14
for ( obj in ls() ) { cat('---',obj,'---\n'); print(get(obj)) }
# --- A ---
#   [1] 78.53982
# --- d ---
#   [1] 6.297463e-06
# --- D ---
#   [1] 10
# --- delta.h ---
#   [1] 21
# --- delta.l ---
#   [1] 40
# --- dh.dl ---
#   [1] 0.525
# --- factor ---
#   [1] 2.777778e-06
# --- g ---
#   [1] 9.81
# --- gamma ---
#   [1] 9810
# --- h1 ---
#   [1] 56
# --- h2 ---
#   [1] 35
# --- k ---
#   [1] 1.586322e-11
# --- K ---
#   [1] 53.3548
# --- kcm ---
#   [1] 1.586322e-07
# --- mu ---
#   [1] 0.00105
# --- n ---
#   [1] 0.4
# --- p1 ---
#   [1] 588.6
# --- p2 ---
#   [1] 490.5
# --- psi.1 ---
#   [1] 6
# --- psi.2 ---
#   [1] 5
# --- q ---
#   [1] 28.01127
# --- Q ---
#   [1] 2200
# --- Re ---
#   [1] 0.0004666665
# --- rho.w ---
#   [1] 1000
# --- V ---
#   [1] 70.02817
# --- z1 ---
#   [1] 50
# --- z2 ---
#   [1] 30
rm(obj)

#RRP4.15
# 15. Consider the following soil with parameters from Table 1.
# Evaluate the field capacity moisture content, ??fc, at which pressure head is -340 cm, 
# permanent wilting point moisture content, ??pwp, at which the pressure head is -15000 cm and 
# plant available water, ??a, using the Clapp and Hornberger (1978) soil moisture characteristic parameterization.

# Texture
# sand
# Porosity
n=0.395
psi.fc=-340 # cm
psi.pwp=-15000 # cm 
# air-entry tension,|??a| (cm)
psi.a=12.1 # cm
b=4.05
# field capacity moisture content, ??fc
theta.fc=n*((abs(psi.fc)/abs(psi.a))^(-1/b))
# permanent wilting point moisture content, ??pwp
theta.pwp=n*((abs(psi.pwp)/abs(psi.a))^(-1/b))
# plant available water content, ??a
theta.a=theta.fc-theta.pwp

## values for RRP4.15
for ( obj in ls() ) { cat('---',obj,'---\n'); print(get(obj)) }
# --- b ---
#   [1] 4.05
# --- n ---
#   [1] 0.395
# --- psi.a ---
#   [1] 12.1
# --- psi.fc ---
#   [1] -340
# --- psi.pwp ---
#   [1] -15000
# --- theta.a ---
#   [1] 0.1052901
# --- theta.fc ---
#   [1] 0.1733385
# --- theta.pwp ---
#   [1] 0.06804839
# 

#RRP4.16
# 16. Consider the following soil with parameters from Table 1.
# Evaluate the field capacity moisture content, ??fc, at which pressure head is -340 cm, 
# permanent wilting point moisture content, ??pwp, at which the pressure head is -15000 cm and 
# plant available water, ??a, using the Clapp and Hornberger (1978) soil moisture characteristic parameterization.

# Texture
# loamy sand
# Porosity
n=0.41
psi.fc=-340 # cm
psi.pwp=-15000 # cm 
# air-entry tension,|??a| (cm)
psi.a=9 # cm
b=4.38
# field capacity moisture content, ??fc
theta.fc=n*((abs(psi.fc)/abs(psi.a))^(-1/b))
# permanent wilting point moisture content, ??pwp
theta.pwp=n*((abs(psi.pwp)/abs(psi.a))^(-1/b))
# plant available water content, ??a
theta.a=theta.fc-theta.pwp

## values for RRP4.16
for ( obj in ls() ) { cat('---',obj,'---\n'); print(get(obj)) }
# --- b ---
#   [1] 4.38
# --- n ---
#   [1] 0.41
# --- psi.a ---
#   [1] 9
# --- psi.fc ---
#   [1] -340
# --- psi.pwp ---
#   [1] -15000
# --- theta.a ---
#   [1] 0.1035598
# --- theta.fc ---
#   [1] 0.1789304
# --- theta.pwp ---
#   [1] 0.0753706

#RRP4.17
# 17. Consider the following experimental situation. A and B are vertical tensiometers that measure pore water pressure (tension) relative to atmospheric pressure, at depths 30 and 50 cm below the ground.
# depth below datum (ground level) of tensiometer A
z.A=-30 # cm
# depth below datum (ground level) of tensiometer B
z.B=-50 # cm
# Following are pressure measurements recorded at A and B. Negative denotes suction. 
# Pressure at A (Pa) -4000
p.A=-4000 # Pa = kg/(m*s^2)
# Pressure at B (Pa) -3000 
p.B=-3000 # Pa = kg/(m*s^2)
# Using the surface as a datum, evaluate:
# .	the pressure head at A and B, and 
# the density of water
rho=1000 # kg/m^3
# gravitational acceleration
g=9.81 # m/s^2
# convert to centimeters
factor=100/1 # 100cm/1m
# pressure head, ??, at A (cm) 
# factor*(kg/(m*s^2)*m^3/kg*s^2/m) = factor*m = cm  
psi.A=factor*p.A/(rho*g) # cm
# pressure head, ?? at B (cm) 
psi.B=factor*p.B/(rho*g) # cm
# .	total head at A and B
# Total head at A (cm) 
h.A=psi.A+z.A #cm
# Total head at B (cm) 
h.B=psi.B+z.B #cm
# .	Indicate the direction of flow (i.e. downwards into the ground from A to B, or upwards from B to A).
# Direction of flow.
# h.B < h.A = -81 < -71
# flow downwards from A (z.A = -30 cm) to B (z.B = -50 cm)

## values for RRP4.17
for ( obj in ls() ) { cat('---',obj,'---\n'); print(get(obj)) }
# --- factor ---
#   [1] 100
# --- g ---
#   [1] 9.81
# --- h.A ---
#   [1] -70.77472
# --- h.B ---
#   [1] -80.58104
# --- p.A ---
#   [1] -4000
# --- p.B ---
#   [1] -3000
# --- psi.A ---
#   [1] -40.77472
# --- psi.B ---
#   [1] -30.58104
# --- rho ---
#   [1] 1000
# --- z.A ---
#   [1] -30
# --- z.B ---
#   [1] -50

#RRP4.18
# 18. Consider the following experimental situation. A and B are vertical tensiometers that measure pore water pressure (tension) relative to atmospheric pressure, at depths 30 and 50 cm below the ground.
# depth below datum (ground level) of tensiometer A
z.A=-30 # cm
# depth below datum (ground level) of tensiometer B
z.B=-50 # cm
# Following are pressure measurements recorded at A and B. Negative denotes suction. 
# Pressure at A (Pa) -5500
p.A=-5500 # Pa = kg/(m*s^2)
# Pressure at B (Pa) -3000 
p.B=-3000 # Pa = kg/(m*s^2)
# Using the surface as a datum, evaluate:
# .	the pressure head at A and B, and 
# the density of water
rho=1000 # kg/m^3
# gravitational acceleration
g=9.81 # m/s^2
# convert to centimeters
factor=100/1 # 100cm/1m
# pressure head, ??, at A (cm) 
# factor*(kg/(m*s^2)*m^3/kg*s^2/m) = factor*m = cm  
psi.A=factor*p.A/(rho*g) # cm
# pressure head, ?? at B (cm) 
psi.B=factor*p.B/(rho*g) # cm
# .	total head at A and B
# Total head at A (cm) 
h.A=psi.A+z.A #cm
# Total head at B (cm) 
h.B=psi.B+z.B #cm
# .	Indicate the direction of flow (i.e. downwards into the ground from A to B, or upwards from B to A).
# Direction of flow.
# h.A < h.B = -86 < -81
# flow upwards from B (z.B = -50 cm) to A (z.A = -30 cm)

## values for RRP4.18
for ( obj in ls() ) { cat('---',obj,'---\n'); print(get(obj)) }
# --- factor ---
#   [1] 100
# --- g ---
#   [1] 9.81
# --- h.A ---
#   [1] -86.06524
# --- h.B ---
#   [1] -80.58104
# --- p.A ---
#   [1] -5500
# --- p.B ---
#   [1] -3000
# --- psi.A ---
#   [1] -56.06524
# --- psi.B ---
#   [1] -30.58104
# --- rho ---
#   [1] 1000
# --- z.A ---
#   [1] -30
# --- z.B ---
#   [1] -50

####################################
# for submission
####################################
#RRP4.7 - online
# Oven dry mass of soil:
M.sdry=388.6 #g
M.m=M.sdry
#Length of soil sample: 
L=10 #cm
# diameter of soil sample cross-section
D=5 # cm
r=D/2 # cm
#Area of soil sample:
A=pi*r^2 # cm^2
# Total volume of soil sample:
V.s=L*A # cm^3
#Bulk density of soil sample: 
rho.b=M.m/V.s # g/cm^3
#Assumed soil particle density: 
rho.m=2.65 # g/cm^3
#Porosity: 
n=1-(rho.b/rho.m)
#Density of water:
rho.w=1000*1000*(1/100)^3 # kg/m^3 * g/kg * (m/cm)^3 = g/cm^3
# Mass of wet soil, field mass: 
M.swet=422.6 # g
#Volumetric soil moisture content: 
theta=(M.swet-M.sdry)/(rho.w*V.s)
# Degree of saturation: 
S.d=theta/n

## values for RRP4.7
for ( obj in ls() ) { cat('---',obj,'---\n'); print(get(obj)) }
# --- A ---
#   [1] 19.63495
# --- D ---
#   [1] 5
# --- L ---
#   [1] 10
# --- M.m ---
#   [1] 388.6
# --- M.sdry ---
#   [1] 388.6
# --- M.swet ---
#   [1] 422.6
# --- n ---
#   [1] 0.2531609
# --- r ---
#   [1] 2.5
# --- rho.b ---
#   [1] 1.979124
# --- rho.m ---
#   [1] 2.65
# --- rho.w ---
#   [1] 1
# --- S.d ---
#   [1] 0.6839941
# --- theta ---
#   [1] 0.1731606
# --- V.s ---
#   [1] 196.3495
rm(list=ls())

#RRP4.8 - online
# Oven dry mass of soil:
M.sdry=374.5 #g
M.m=M.sdry
#Length of soil sample: 
L=10 #cm
# diameter of soil sample cross-section
D=5 # cm
r=D/2 # cm
#Area of soil sample:
A=pi*r^2 # cm^2
# Total volume of soil sample:
V.s=L*A # cm^3
#Bulk density of soil sample: 
rho.b=M.m/V.s # g/cm^3
#Assumed soil particle density: 
rho.m=2.65 # g/cm^3
#Porosity: 
n=1-(rho.b/rho.m)
#Density of water:
rho.w=1000*1000*(1/100)^3 # kg/m^3 * g/kg * (m/cm)^3 = g/cm^3
# Mass of wet soil, field mass: 
M.swet=390.5 # g
#Volumetric soil moisture content: 
theta=(M.swet-M.sdry)/(rho.w*V.s)
# Degree of saturation: 
S.d=theta/n

## values for RRP4.8
for ( obj in ls() ) { cat('---',obj,'---\n'); print(get(obj)) }
rm(list=ls())


#RRP4.10
lseq <- function(from=x, to=y, length.out=z) {
  # logarithmic spaced sequence
  # blatantly stolen from library("emdbook"), because need only this
  exp(seq(log(from), log(to), length.out = length.out))
}

numbers=c(50,100,19,100,9.5,100,4.76,98,2,95,0.42,80,0.074,60,0.020,42,0.005,35,0.002,30)
odd=seq(1,length(numbers),2)
even=seq(2,length(numbers),2)
D=numbers[odd] # Diameter (mm)
p.p=numbers[even] # percent passing
plot(c(0.001,50),c(0,100),type="n", log="x",xaxt="n", xlim=c(0.001,50), ylim=c(0,100),main="Grain Size Distribution", ylab="Percentage passing", xlab="Grain size, mm")

x=lseq(0.001,100,6)
x1=c(x,x/2)
x1[8]=5
axis(1,x1,labels=x1)
axis(2,seq(10,100,10),labels=seq(10,100,10))

y=rep(1:10,5)
x2=rep(x[-c(length(x))],each=10)
x3=x2*y
abline(v=x3,h=seq(10,100,10),lty="dotted",col="light grey")
abline(v=c(0.002,0.05,2),lty="dashed",col="blue")
legend(0.001,90,border="white",legend="", title="clay",cex=0.75,bty="n")
legend(0.007,90,border="white",legend="", title="silt",cex=0.75,bty="n")
legend(0.25,90,border="white",legend="", title="sand",cex=0.75,bty="n")
legend(10,90,border="white",legend="", title="gravel",cex=0.75,bty="n")
lines(x=c(0.0005,0.002,0.05,2,75),y=c(80,80,80,80,80),type="o",pch=4,col="red")
lines(x=D,y=p.p,type="o")

sand=95-55
silt=55-30
clay=30
all=c(sand,silt,clay)
ng=round(all/(95/100))
ng
# [1] 42 26 32
# clay loam

## RRP4.12 (prob 11 online)
# Hydraulic conductivity is determined in a Darcy experiment conducted using water at 20 °C to be 30 cm/hr. 
# The viscosity of water at 20 °C is 1.05 x 10-3 N s m-2.
# Using g=9.81 m/s2 and ??w =1000 kg/m3 calculate the intrinsic permeability of this material.
# Hydraulic conductivity:  
K = 30 # gamma/mu*k; cm/hr
# the density of water
rho.w = 1000 # kg/m^3
# gravitational acceleration
g=9.81 # m/s^2
# the specific weight of water
gamma = rho.w*g
# the dynamic viscosity of water
mu=1.05*10^(-3) # N.s/m^2
# intrisic permiability
# conversion from N to kgm/s^2
# cm/hr*(N.s/m^2)*(m^3/kg)*(s^2/m)*((kg*m)/s^2)*1/N
k = K*mu/gamma # cm/hr*s*m
kcm = k*100/3600 # cm^2
km = (k/100)/3600 # m^2
rm(list=ls())

#RRP4.13 (prob 12 online)
# Following is data from a Darcy experiment using the notation depicted in figure 25. 
# Fill in the blanks and 
# calculate the hydraulic conductivity. 

# flow rate in = flow rate out
# liters are converted to cm^3; 1000 cm^3/l
Q=0.5*1000 # cm^3/hr
#The internal diameter of the circular tube
D=10 # cm
# cross-sectional area of circular tube
A=pi*(D/2)^2 # cm^2
# specific discharge
q=Q/A # cm/hr
# porosity
n=0.32
# Average velocity through the sample
V=q/n # cm/hr,
# This experiment is conducted at 20 °C.
# the dynamic viscosity of water at 20 °C
mu=1.05*10^(-3) # N.s/m^2 * ((kg m)/s^2)/N = kg/(m*s) 
# hydraulic head of piezometer 1
h1=70 # cm
# hydraulic head of piezometer 2
h2=58 # cm
# difference in hydraulic head between piezometers, ??h
delta.h=h1-h2 # cm
# the length ??l, between piezometers
delta.l=40 # cm
# hydraulic gradient
dh.dl=delta.h/delta.l
# elevation head for piezometer 1
z1=50 # cm
# elevation head for piezometer 2
z2=30 # cm
# pressure head for piezometer 1, ??1	(cm)
psi.1=h1-z1 # cm
# pressure head for piezometer 2, ??2	(cm)	
psi.2=h2-z2 # cm
# the density of water
rho.w = 1000 # kg/m^3
# gravitational acceleration
g=9.81 # m/s^2
# the specific weight of water
gamma=rho.w*g # kg/m^3*m/s^2 = kg/(m^2*s^2)
# pressure at the bottom of piezometer 1
# h-z is converted from cm to m
p1=((h1-z1)/100)*gamma # Pa = kg/(m*s^2)
# pressure at the bottom of piezometer 2
# h-z is converted from cm to m
p2=((h2-z2)/100)*gamma  # Pa = kg/(m*s^2); 
# hydraulic conductivity
K=q*1/(dh.dl) # cm/hr
# specific conductivity
# convert to m^2
# cm/hr*kg/(m*s)*(m^2*s^2)/kg*1m/100cm*1hr/3600s = m^2
k=(K*mu/gamma)/(100*3600) # m^2
kcm=k*100*100 # cm^2
# effective grain diameter
d=(k/n)^(1/2) # m
# Reynold's number
# kg/m^3*cm/hr*m*(m*s)/kg = 1/m*cm/hr*s
# convert cm to m and s to hr
# cm/hr*s/m*1hr/3600s*1m/100cm=1/(3600*100)
factor=1/(3600*100)
Re=factor*rho.w*q*d/mu	# kg/(m^2*s^2)*cm/hr*m*(m*s)/kg	

## values for RRP4.13
for ( obj in ls() ) { cat('---',obj,'---\n'); print(get(obj)) }
# --- A ---
#   [1] 78.53982
# --- d ---
#   [1] 4.440311e-06
# --- D ---
#   [1] 10
# --- delta.h ---
#   [1] 12
# --- delta.l ---
#   [1] 40
# --- dh.dl ---
#   [1] 0.3
# --- factor ---
#   [1] 2.777778e-06
# --- g ---
#   [1] 9.81
# --- gamma ---
#   [1] 9810
# --- h1 ---
#   [1] 70
# --- h2 ---
#   [1] 58
# --- k ---
#   [1] 6.309234e-12
# --- K ---
#   [1] 21.22066
# --- kcm ---
#   [1] 6.309234e-08
# --- mu ---
#   [1] 0.00105
# --- n ---
#   [1] 0.32
# --- p1 ---
#   [1] 1962
# --- p2 ---
#   [1] 2746.8
# --- psi.1 ---
#   [1] 20
# --- psi.2 ---
#   [1] 28
# --- q ---
#   [1] 6.366198
# --- Q ---
#   [1] 500
# --- Re ---
#   [1] 7.478279e-05
# --- rho.w ---
#   [1] 1000
# --- V ---
#   [1] 19.89437
# --- z1 ---
#   [1] 50
# --- z2 ---
#   [1] 30
rm(list=ls())

#RRP4.15 - (13online)
# 15. Consider the following soil with parameters from Table 1.
# Evaluate the field capacity moisture content, ??fc, at which pressure head is -340 cm, 
# permanent wilting point moisture content, ??pwp, at which the pressure head is -15000 cm and 
# plant available water, ??a, using the Clapp and Hornberger (1978) soil moisture characteristic parameterization.

# Texture
# sand
# Porosity
n=0.435
psi.fc=-340 # cm
psi.pwp=-15000 # cm 
# air-entry tension,|??a| (cm)
psi.a=21.8 # cm
b=4.9
# field capacity moisture content, ??fc
theta.fc=n*((abs(psi.fc)/abs(psi.a))^(-1/b))
# permanent wilting point moisture content, ??pwp
theta.pwp=n*((abs(psi.pwp)/abs(psi.a))^(-1/b))
# plant available water content, ??a
theta.a=theta.fc-theta.pwp
rm(list=ls())

#RRP4.15 - (14online)
# 15. Consider the following soil with parameters from Table 1.
# Evaluate the field capacity moisture content, ??fc, at which pressure head is -340 cm, 
# permanent wilting point moisture content, ??pwp, at which the pressure head is -15000 cm and 
# plant available water, ??a, using the Clapp and Hornberger (1978) soil moisture characteristic parameterization.

# Texture
# sand
# Porosity
n=0.42
psi.fc=-340 # cm
psi.pwp=-15000 # cm 
# air-entry tension,|??a| (cm)
psi.a=29.9 # cm
b=7.12
# field capacity moisture content, ??fc
theta.fc=n*((abs(psi.fc)/abs(psi.a))^(-1/b))
# permanent wilting point moisture content, ??pwp
theta.pwp=n*((abs(psi.pwp)/abs(psi.a))^(-1/b))
# plant available water content, ??a
theta.a=theta.fc-theta.pwp
rm(list=ls())

#RRP4.17 (15 online)
# 17. Consider the following experimental situation. A and B are vertical tensiometers that measure pore water pressure (tension) relative to atmospheric pressure, at depths 30 and 50 cm below the ground.
# depth below datum (ground level) of tensiometer A
z.A=-30 # cm
# depth below datum (ground level) of tensiometer B
z.B=-50 # cm
# Following are pressure measurements recorded at A and B. Negative denotes suction. 
# Pressure at A (Pa) -4000
p.A=-4000 # Pa = kg/(m*s^2)
# Pressure at B (Pa) -3000 
p.B=-3000 # Pa = kg/(m*s^2)
# Using the surface as a datum, evaluate:
# .	the pressure head at A and B, and 
# the density of water
rho=1000 # kg/m^3
# gravitational acceleration
g=9.81 # m/s^2
# convert to centimeters
factor=100/1 # 100cm/1m
# pressure head, ??, at A (cm) 
# factor*(kg/(m*s^2)*m^3/kg*s^2/m) = factor*m = cm  
psi.A=factor*p.A/(rho*g) # cm
# pressure head, ?? at B (cm) 
psi.B=factor*p.B/(rho*g) # cm
# .	total head at A and B
# Total head at A (cm) 
h.A=psi.A+z.A #cm
# Total head at B (cm) 
h.B=psi.B+z.B #cm
# .	Indicate the direction of flow (i.e. downwards into the ground from A to B, or upwards from B to A).
# Direction of flow.
# h.B < h.A = -81 < -71
# flow downwards from A (z.A = -30 cm) to B (z.B = -50 cm)

rm(list=ls())

#RRP4.17 (16 online)
# 17. Consider the following experimental situation. A and B are vertical tensiometers that measure pore water pressure (tension) relative to atmospheric pressure, at depths 30 and 50 cm below the ground.
# depth below datum (ground level) of tensiometer A
z.A=-30 # cm
# depth below datum (ground level) of tensiometer B
z.B=-50 # cm
# Following are pressure measurements recorded at A and B. Negative denotes suction. 
# Pressure at A (Pa) -4000
p.A=-5500 # Pa = kg/(m*s^2)
# Pressure at B (Pa) -3000 
p.B=-3000 # Pa = kg/(m*s^2)
# Using the surface as a datum, evaluate:
# .	the pressure head at A and B, and 
# the density of water
rho=1000 # kg/m^3
# gravitational acceleration
g=9.81 # m/s^2
# convert to centimeters
factor=100/1 # 100cm/1m
# pressure head, ??, at A (cm) 
# factor*(kg/(m*s^2)*m^3/kg*s^2/m) = factor*m = cm  
psi.A=factor*p.A/(rho*g) # cm
# pressure head, ?? at B (cm) 
psi.B=factor*p.B/(rho*g) # cm
# .	total head at A and B
# Total head at A (cm) 
h.A=psi.A+z.A #cm
# Total head at B (cm) 
h.B=psi.B+z.B #cm
# .	Indicate the direction of flow (i.e. downwards into the ground from A to B, or upwards from B to A).
# Direction of flow.
# h.B < h.A = -81 < -71
# flow downwards from A (z.A = -30 cm) to B (z.B = -50 cm)
