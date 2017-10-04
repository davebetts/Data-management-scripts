# air entry pressure
psi.a=21.8 # cm
# porosity
n=0.435
# height above water table  
z=c(21.8,seq(0,80,5)) # cm
z=sort(z,decreasing=T)
b=4.9

# soil moisture content
theta=n*(z/psi.a)^(-1/b)
# upper limit of theta = n
theta[which(theta>n)]=n
plot(theta,z, xlim=range(theta),type="o", ylab="height above water table, cm", xlab="Soil moisture content, theta", main="Moisture Content %\nfrom soil surface (80 cm)\nto water table (0 cm)")

cbind(z,theta)

# pressure head at depths z, based on theta at depth z
psi.theta=psi.a*(theta/n)^(-b)
plot(psi.theta,z, xlim=range(psi.theta),type="o", ylab="height above water table, cm", xlab="Pressure head, psi", main="Pressure head, cm\nfrom soil surface (z = 80 cm)\nto water table (z = 0 cm)")


# problem 2
year=seq(0,2,0.25)
Q=c(rep(c(200,350,200,50),2),200)
plot(year,Q,type="l")
abline(h=200,col="blue")
yield=Q[1]-100
for(i in 2:length(Q)){
  yield[[i]]=Q[i]-100+Q[i-1]
}
yield.vol=yield*(3600*24*365.25)/4

# problem 3
# 3. Watershed Water Balance and Storage-Yield. Consider a stream in which the seasonal cycle of monthly streamflow is as illustrated
month=c(0,seq(3,24,3))
streamflow=c(80,rep(c(80,200,100,80),2)) # ft^3/s
plot(month,streamflow,type="o",xaxt="n")
abline(h=100, col=2)
abline(v=seq(0.5,24,0.5),lty="dotted",col="light gray")
axis(1,at=1:24,labels=c(rep(1:12,2)))


# The watershed area is 100 mi2. 
A = 100 #mi^2
# Average quarterly precipitation (snow and rain) totals
month.p=c(1,seq(3,24,3))
p=c(13.33333, rep(c(20,15,5,10),2))
plot(month.p,p,type="l",xaxt="n", ylim=c(0,max(p)))
axis(1,at=1:24,labels=c(rep(1:12,2)))

# a. Determine the mean annual flow in ft3/s. [6]
meanQ=mean(streamflow[-1]) # 115 ft^3/s

#b. Determine the storage (ft3) required to support a firm yield of 100 ft3/s. [8]
# total area of deficit: from sept to half way through march
# deficit = area*ft^3/s = 1/2 3months (sept - dec) * 20cfs + 20cfs*3 months (dec - march) + 1/2 0.5 months (half march)*20 cfs
deficit=((1/2)*(3.5/12)*20)+(3/12)*20 # year*ft^3/s
d.ft3=deficit*60*60*24*365.25 # ft^3
# [1] 249831000 ft^3

# c. Estimate the mean annual evapotranspiration (in inches). [8]
# mean annual precipitaiton = sum of mean monthly precip
meanP=sum(p)# 50 inches
# inches of runoff per area for the year
# requires factor to convert seconds to years
s.yr=60*60*24*365.25
# factor to convert miles to inches
mi.in=5280*12
# factor to convert ft to inches
ft.in=12
# mean annual flow (Q) produced per unit area
meanQ.A=(meanQ*ft.in^3)*s.yr/(A*mi.in^2) # 15.62 inches
# mean annual evapotranspiration
meanET=meanP-meanQ.A # 34.4 inches  

# d. Determine the runoff ratio for this watershed. [8]

monthQ.A=((streamflow*ft.in^3)*s.yr/(A*mi.in^2))/4 #  
lines(month.p,monthQ.A,col="blue")
5.05*1000/10
505/361


#problem 5.c
#In this figure 100 units of incoming solar radiation may be equated with the planetary average solar radiation forcing of 342 W/m2.
# c) The surface radiative temperature (a planetary average)
.	Solar Flux (S) = 1.74 ??? 1017 W/m2
.	Albedo (ap) = 0.3
.	Stefan-Boltzman constant (??) = 5.78 ??? 10-8
.	Area of the earth (A) = 5.1 ??? 1014 m2

?planetary temperature=T_p=[(S???(1-a_p ))/(?????A)]^(1/4)


#problem 6
# 6. Water Balance. Consider the following watershed with four stream gages and subwatersheds draining directly to each gage as indicated.
#The mean annual streamflow at each gage is
gage=1:4
m3.s=c(7.7,6.4,2.4,2.3) # m^3/s
#This mean annual streamflow includes baseflow.
#Subwatershed area and mean annual precipitation for each subwatershed is
region=c("A","B","C","D")
area=c(62,75,50,58) # km2
precip=c(1400,1600,1300,1900) # mm/yr

# a) Estimate the mean annual evapotranspiration and runoff ratio for each subwatershed, assuming that deep infiltration losses to groundwater are negligible. [10]
# calculate runoff, subtracting the outflow from upstream gages
RO=c(m3.s[2]-m3.s[3]-m3.s[4],m3.s[4],m3.s[1]-m3.s[2],m3.s[3]) # m^3/s
RO.yr=RO*60*60*24*365.25 # m^3/yr
# m^3/km2*(1km/1000m)^2 = m/(1000^2)*1000mm/m = 1/1000 mm
RO_A=RO.yr/(area*1000) # mm/yr
ET=precip-RO_A # mm/yr
ET
# [1] 534.7110 632.2336 479.5024 594.1683
w=RO_A/precip
w
# [1] 0.6180636 0.6048540 0.6311520 0.6872799

# b) Consider a land use change in watershed A that converts 20% of the area from natural vegetation to urban. 
# Indicate the stream gauges where you expect the mean annual streamflow to change and whether it is likely to increase or decrease. 
## increases at stream gages 1 & 2 b/c both are downstream from the 20% increase in urban (imperveous) land cover.  
# Explain why? 
## >% of impervious --> more direct runoff rather than infiltration and transpiration.
# Estimate upper and lower limits to these changes and explain the basis for your estimates. 


