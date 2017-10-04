### Homework 6
### some of the code adapted from Rwork.r (Tarboton)

## HW6.1.a
# 1. The probability distribution of wetness index represented by a histogram is sufficient to describe the hydrologic response of a watershed using TOPMODEL. Consider a watershed that has TOPMODEL wetness index ln(a/S) distributed according to the histogram shown:
prop=c(0.4,0.3,0.2,0.1)
count=prop*10
lower=seq(3,12,3)
upper=seq(6,15,3)
ln.a.s=(lower+upper)/2
values=rep(ln.a.s,count)
hist(values,breaks=seq(3,15,3),yaxt="n",ylab="Proportion of area",xlab=expression(paste(lambda==ln(a/S)," (a in meters)")),right=F,xlim=range(0,15),xaxt="n")
axis(2,at=count,labels=prop)
axis(1,at=seq(0,15,3),labels=seq(0,15,3))
box()
  
## The watershed has TOPMODEL parameters:
# Hydraulic conductivity at the surface; Spatially homogeneous:
K.o=4 # m/hr
# Sensitivity parameter
# how rapidly hydraulic conductivity decreases with depth:
f=2 # m^-1
# Antecedent baseflow:
Q.b=12 # m^3/s
# Drainage area:
A=400 # km^2
# Effective porosity of unsaturated zone:
theta.e=0.2
                         
## a) Estimate the TOPMODEL ?? parameter for this watershed by averaging the histogram above.
lambda.bar=sum(prop*ln.a.s) # m

## b) Estimate the recharge parameter r by dividing baseflow by the drainage area and expressing the result in m/hr.
r=(Q.b/A)*(3600/1)*(1/1000)^2 # m/hr 

## c) In TOPMODEL locations with wetness index ln(a/S) greater than a threshold are saturated to the surface. This threshold can be obtained from setting D=0 in equation (82) or (83) and solving for ln(a/S).
# 82: D=-m*ln((r*a)/(T.0*S))
# 83: D=-m*ln(r)+m*ln(T.0)-m*lambda
T.0=K.o/f
# Report the threshold ln(a/S) above which saturation occurs for the antecedent conditions given.
lambda=log(T.0)-log(r)

##  d) The fraction of watershed that is initially saturated is obtained by integrating the histogram over values larger than the threshold determined in (c). 
# Report the fraction of watershed that is initially saturated.
# Fraction saturated
sat=0.2*(1-(lambda-9)/(12-9))+0.1
# Or perhaps

# e) Following infiltration of rainfall the saturated area expands to encompass all locations where D was less than the rainfall.
# Use equation (82) or (83) to determine the threshold ln (a/S) corresponding to D=0.025 m (25mm).
m=theta.e/f
D.p=0.025 # m
lambda.p=(-m*log(r)+m*log(T.0)-D.p)/m


# f) Integrate the histogram over values larger than the threshold determined in (e) to determine the fraction of watershed that is saturated at the end of a 25 mm rainstorm.
sat.p=0.2*(1-(lambda.p-9)/(12-9))+0.1

D.bar=-m*log(r)+m*log(T.0)-m*lambda.bar
D.1=-m*log(r)+m*log(T.0)-m*12
D.2=-m*log(r)+m*log(T.0)-m*9

D.1=D.bar-m*(12-lambda.bar)
D.2=D.bar-m*(9-lambda.bar)
0.025-D.2

D.prop=D.bar-m*(ln.a.s-lambda.bar)

# g) Estimate the volume of runoff from 25 mm of rain, by summing the runoff from the initially saturated area and the area that becomes saturated during the storm. Report your answer on a per unit total area basis.
g.       Rainfall on area fraction 0.245 all becomes runoff.  Rainfall on area fraction 0.245 to 0.261 becomes runoff after saturation has been reached.  Because the histogram is flat (linear) across this range the net runoff from this area is 12.5 mm.  Area average runoff is therefore
25 x 0.245 + 12.5 x (0.261-0.245) = 6.3 mm

z.w=1/f*(log(T.0)-log(r)-10.5
De=theta.e*z.w         
z_w=1/f (lna???????_???? ??????lna???????lna(????/????)  )


# h) Equation (88) gives the baseflow in terms of average soil moisture deficit D . This decreases by 25 mm due to infiltration during the storm and this equation may be used to calculate the corresponding increase in r which corresponds to an increase in baseflow. Use equation (88) to determine the baseflow that you expect after the direct runoff hydrograph from the 25 mm of rainfall has receded. Report your result in m3/s.

r=T.0*exp(-(D.bar-0.025))*exp(-lambda.bar)

r.1=T.0*exp(D.bar-0.025)

r1
ln= 0.4*(3+6)/2 + 0.3*(6+9)/2 + 0.2*(9+12)/12 + 0.1*(12+15)/2

## HW6.3 - Logan River Hydrologic Annual Water Balance and Runoff Ratio exercise.
library(dataRetrieval)
# Important quantities in the water balance of a watershed are the streamflow, expressed on a per unit area basis, area average precipitation and the runoff ratio r=q/P. 
# Let's determine these for the Logan River. Streamflow we will get from the USGS. Precipitation we will get from Oregon State University (PRISM).

# Use the USGS NWIS website for Logan River above state dam (10109000)
siteNumber <- "10109000"
# loganR <- readNWISsite(siteNumber)

# select daily mean streamflow data
parameterCd <- "00060"
# pCode <- readNWISpCode(parameterCd) #provides information on parameterCd ("00060")

# Retrieve all available daily data; using default statistic code for daily mean.  
# Determine mean annual streamflow for the years 1981-2010
# !!! i'm using water years !!!
rawdailydata <- readNWISdv(siteNumber,parameterCd,"1980-10-01","2010-09-30")
# head(rawdailydata) # inspect data
waterYearStat <- readNWISdata(site=siteNumber,service="stat",statReportType="annual",statYearType="water", startDate="1980",endDate="2010")

# Rename column titles for readability
rawdailydata <- renameNWISColumns(rawdailydata) # replaces codes for parameter and statistic with names
names(rawdailydata) # displays column names
# Rename column titles for readability
waterYearStat <- renameNWISColumns(waterYearStat) # replaces codes for parameter and statistic with names
colnames(waterYearStat)[6:7]=c("WaterYear", "MeanAnnualFlow")


# Compute the average annual flow
Q=rawdailydata$Flow # vector of mean daily discharge
dt=rawdailydata$Date # vector of dates

# Using code from Rwork.r (Tarboton) with adaptations for 7-day minimum flows
# Observations grouped by water year
yy=as.numeric(format.Date(dt,"%Y"))
mo=as.numeric(format.Date(dt,"%m"))
wy=ifelse(mo>=10,yy+1,yy)
yrseq=unique(wy)
# Vectors for peak, mean, and 7-day minimum annual flows
Qmean=rep(NA,length(yrseq))
for(i in 1:length(yrseq)){
  yr=yrseq[i]
  Qmean[i]=mean(Q[wy==yr])
}

Qmean
waterYearStat
# Calculating mean annual flow, and converting to ft^3/yr 
annualQ=mean(Qmean)*3600*24*365.25 # ft^3/yr
# Discharge area; given in square miles and converted to square feet
A=214*5280^2 # ft^2

q=annualQ/A # ft/yr
qmm=q*304.8 # mm

# Mean annual precipitation from Prism data following HW6 instructions
P=941.510749 # mm (according to HW6)
## Calculate the runoff ratio for the Logan River
r=qmm/P

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

## HW6.4.8
# To turn in:.
# A histogram of wetness index distribution ln(a/S) for Spawn Creek.
low=1.631
high=18.2053
lower=c(low,seq(4,18,2),high)
upper=sort(c(seq(4,20,2),high))
count=c(3339,63228,66928,8646,2222,1195,548,208,3,18)
prop=count/sum(count)
ln.a.s=(lower+upper)/2
values=rep(ln.a.s,count)
hist(values,breaks=seq(0,max(upper),2),yaxt="n",ylab="Proportion of area",xlab=expression(paste(lambda==ln(a/S),":    (a in meters)")),right=F,xlim=range(0,max(upper)),xaxt="n",ylim=c(0,max(count)),main="Histogram of the Wetness Distribution Index\nSpawn Creek, near Logan, Utah",col="lightgray",cex.lab=1.5,cex.main=1.5)
at=seq(0,max(upper),2)
u=0.1
y=max(prop)
lim=ceiling(y/u)*u
x=sum(count)*lim
at2=seq(0,x,(x*u))
axis(1,at=at,labels=at)
axis(2,at=at2,labels=seq(0,lim,0.05))
box()
legend("topright",inset=c(0.04,0.4),legend=c(expression(paste(lambda>=18," < 0.02% of total area"),"Including grid cells with no slope")),seg.len=0,bty="n",cex=1.5)

## HW6.4.9
K.o=10 # m/hr
# Sensitivity parameter
# how rapidly hydraulic conductivity decreases with depth:
f=5 # m^-1
# Antecedent baseflow:
Q.b=0.8 # m^3/s
# Drainage area:
A=14633500 # m^2
# Effective porosity of unsaturated zone:
theta.e=0.2

lambda.bar=920965.438975/(146317+18) # m

r=(Q.b/A)*(3600/1) # m/hr 

T.0=K.o/f

m=theta.e/f

D.bar=-m*log(r)+m*log(T.0)-m*lambda.bar


## c) In TOPMODEL locations with wetness index ln(a/S) greater than a threshold are saturated to the surface. This threshold can be obtained from setting D=0 in equation (82) or (83) and solving for ln(a/S).
# 82: D=-m*ln((r*a)/(T.0*S))
# 83: D=-m*ln(r)+m*ln(T.0)-m*lambda
# Report the threshold ln(a/S) above which saturation occurs for the antecedent conditions given.
lambda=log(T.0)-log(r)

##  d) The fraction of watershed that is initially saturated is obtained by integrating the histogram over values larger than the threshold determined in (c). 
# Report the fraction of watershed that is initially saturated.
# Fraction saturated
sat=0.2*(1-(lambda-9)/(12-9))+0.1
# Or perhaps

# e) Following infiltration of rainfall the saturated area expands to encompass all locations where D was less than the rainfall.
# Use equation (82) or (83) to determine the threshold ln (a/S) corresponding to D=0.025 m (25mm).
D.p=0.025 # m
lambda.p=(-m*log(r)+m*log(T.0)-D.p)/m


# f) Integrate the histogram over values larger than the threshold determined in (e) to determine the fraction of watershed that is saturated at the end of a 25 mm rainstorm.
sat.p=0.2*(1-(lambda.p-9)/(12-9))+0.1

D.1=-m*log(r)+m*log(T.0)-m*12
D.2=-m*log(r)+m*log(T.0)-m*9

D.1=D.bar-m*(12-lambda.bar)
D.2=D.bar-m*(9-lambda.bar)
0.025-D.2

D.prop=D.bar-m*(ln.a.s-lambda.bar)

# g) Estimate the volume of runoff from 25 mm of rain, by summing the runoff from the initially saturated area and the area that becomes saturated during the storm. Report your answer on a per unit total area basis.
g.       Rainfall on area fraction 0.245 all becomes runoff.  Rainfall on area fraction 0.245 to 0.261 becomes runoff after saturation has been reached.  Because the histogram is flat (linear) across this range the net runoff from this area is 12.5 mm.  Area average runoff is therefore
25 x 0.245 + 12.5 x (0.261-0.245) = 6.3 mm

z.w=1/f*(log(T.0)-log(r)-10.5
         De=theta.e*z.w         
         z_w=1/f (lna???????_???? ??????lna???????lna(????/????)  )
         
         
         # h) Equation (88) gives the baseflow in terms of average soil moisture deficit D . This decreases by 25 mm due to infiltration during the storm and this equation may be used to calculate the corresponding increase in r which corresponds to an increase in baseflow. Use equation (88) to determine the baseflow that you expect after the direct runoff hydrograph from the 25 mm of rainfall has receded. Report your result in m3/s.
         
         r=T.0*exp(-(D.bar-0.025))*exp(-lambda.bar)
         
         r.1=T.0*exp(D.bar-0.025)
         
         r1
         ln= 0.4*(3+6)/2 + 0.3*(6+9)/2 + 0.2*(9+12)/12 + 0.1*(12+15)/2
         
         ## c) In TOPMODEL locations with wetness index ln(a/S) greater than a threshold are saturated to the surface. This threshold can be obtained from setting D=0 in equation (82) or (83) and solving for ln(a/S).
         # 82: D=-m*ln((r*a)/(T.0*S))
         # 83: D=-m*ln(r)+m*ln(T.0)-m*lambda
         # Report the threshold ln(a/S) above which saturation occurs for the antecedent conditions given.
         lambda=log(T.0)-log(r)
         
         ##  d) The fraction of watershed that is initially saturated is obtained by integrating the histogram over values larger than the threshold determined in (c). 
         # Report the fraction of watershed that is initially saturated.
         # Fraction saturated
         sat=0.2*(1-(lambda-9)d/(12-9))+0.1
         # Or perhaps
         
         # e) Following infiltration of rainfall the saturated area expands to encompass all locations where D was less than the rainfall.
         # Use equation (82) or (83) to determine the threshold ln (a/S) corresponding to D=0.025 m (25mm).
         D.p=0.025 # m
         lambda.p=(-m*log(r)+m*log(T.0)-D.p)/m
         
         # f) Integrate the histogram over values larger than the threshold determined in (e) to determine the fraction of watershed that is saturated at the end of a 25 mm rainstorm.
         sat.p=0.2*(1-(lambda.p-9)/(12-9))+0.1
         
         D.1=-m*log(r)+m*log(T.0)-m*12
         D.2=-m*log(r)+m*log(T.0)-m*9
         
         D.1=D.bar-m*(12-lambda.bar)
         D.2=D.bar-m*(9-lambda.bar)
         0.025-D.2
         
         D.prop=D.bar-m*(ln.a.s-lambda.bar)
         
         # g) Estimate the volume of runoff from 25 mm of rain, by summing the runoff from the initially saturated area and the area that becomes saturated during the storm. Report your answer on a per unit total area basis.
         g.       Rainfall on area fraction 0.245 all becomes runoff.  Rainfall on area fraction 0.245 to 0.261 becomes runoff after saturation has been reached.  Because the histogram is flat (linear) across this range the net runoff from this area is 12.5 mm.  Area average runoff is therefore
         25 x 0.245 + 12.5 x (0.261-0.245) = 6.3 mm
         
         z.w=1/f*(log(T.0)-log(r)-10.5
                  De=theta.e*z.w         
                  z_w=1/f (lna???????_???? ??????lna???????lna(????/????)  )
                  
                  # h) Equation (88) gives the baseflow in terms of average soil moisture deficit D . This decreases by 25 mm due to infiltration during the storm and this equation may be used to calculate the corresponding increase in r which corresponds to an increase in baseflow. Use equation (88) to determine the baseflow that you expect after the direct runoff hydrograph from the 25 mm of rainfall has receded. Report your result in m3/s.
                  
                  r=T.0*exp(-(D.bar-0.025))*exp(-lambda.bar)
                  
                  r.1=T.0*exp(D.bar-0.025)
                  
                  r1
                  ln= 0.4*(3+6)/2 + 0.3*(6+9)/2 + 0.2*(9+12)/12 + 0.1*(12+15)/2
                  