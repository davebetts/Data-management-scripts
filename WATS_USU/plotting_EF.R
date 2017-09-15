rm(list=ls())
gc()
dir()

file1=file.choose()
file2=file.choose()

#####
band=read.csv(file1,skip=5)
colnames(band)=c("year","month","usgs","weap")
# band[,1]=as.factor(band[,1])
# band[,1]=as.factor(band[,1])
# band[,2]=as.factor(band[,2])
head(band)
str(band)

#####
# yr=aggregate(. ~ year, data = band, sum)
# 
# plot(range(yr[,1]),range(yr[,3:4]),type="n",ylab="Millions of CFS",xlab="Year")
# lines(yr$year, yr$usgs,col=4,lwd=2)
# lines(yr$year, yr$weap, col=6,lwd=2)
# 
# summary(yr)
# 
# total=lapply(band,sum)
# yes=total[[3]]-total[[4]]

#####

noband=read.csv(file2,skip=5)
colnames(noband)=c("year","month","usgs","weap")
# noband[,1]=as.factor(noband[,1])
# noband[,1]=as.factor(noband[,1])
# noband[,2]=as.factor(noband[,2])
head(noband)
str(noband)

#####
yr=aggregate(. ~ year, data = noband, sum)

plot(range(yr[,1]),range(yr[,3:4]),type="n",ylab="Millions of CFS",xlab="Year")
lines(yr$year, yr$usgs,col=4,lwd=2)
lines(yr$year, yr$weap, col=2,lwd=2)

summary(yr)

nototal=lapply(noband,sum)
no=nototal[[3]]-nototal[[4]]

yes
no
#####

all=band
all$noband=noband[,4]
colnames(all)[4]="band"
date=as.Date(paste(all[,1],all[,2],"01",sep="-"))
all[,1]=date
all=all[,-2]
plot(range(all$year),range(all[,2:4]),type="n",ylab="Average CFS / Month",xlab="Monthly time steps", 
     # log="y",
     main="East Fork Models vs. USGS 10011500\nFinal Calibration")
lines(all$year, all$usgs,col=4,lwd=2)
lines(all$year, all$band, col=3,lwd=2)
lines(all$year, all$noband, col=2,lwd=2)
legend("topright", legend=c("USGS","Bands","No Bands"), lwd=2, col=c(4,2,3),bty="n")

all2=all
all2$usgs=all$usgs*60*60*24*30/1000000
all2$band=all$band*60*60*24*30/1000000
all2$noband=all$noband*60*60*24*30/1000000
head(all2)
# Water year flows
yy=band[,1]
mo=band[,2]
wy=ifelse(mo>=10,yy+1,yy)
all2[,1]=wy
all2=aggregate(.~year,data=all2,sum)
all2=all2[1:26,]

plot(range(all2$year),range(all2[,2:4]),type="n",ylab="Total CF [Millions]",xlab="Water Years", log="y", main="Total CF per Water Year\nEast Fork Models vs. USGS 10011500")
lines(all2$year, all2$usgs,col=4,lwd=2)
lines(all2$year, all2$band, col=3,lwd=2)
lines(all2$year, all2$noband, col=2,lwd=2)
legend("topright", legend=c("USGS","Bands","No Bands"), lwd=2, col=c(4,2,3),bty="n")

#####
## snotel with three lines
snotel = read.csv("snotel_EFB.csv")
snotel[,1] = as.Date(snotel[,1])
ef = snotel[,c(1:5)]
colnames(ef) = c("date", "obs", "noband", "band", "noband2")
hd = snotel[,c(1,6:9)]            
colnames(hd) = c("date", "obs", "noband", "band", "noband2")

plot(range(ef$date),range(ef[,2:4]),type="n",ylab="SWE [mm]", xlab="WEAP simulations calibrated to SNOTEL only", main="East Fork vs. Lily Lake SNOTEL\nSnow Water Equivalent"
     # , log="y"
     )
lines(ef$date, ef$obs, col=4,lwd=2)
lines(ef$date, ef$noband, col=2,lwd=2)
lines(ef$date, ef$band, col=3,lwd=2)
legend("topright", legend=c("SNOTEL","No Bands","Bands"), lwd=2, col=c(4,2,3),bty="n")


plot(range(hd$date),range(hd[,2:4]),type="n",ylab="SWE [mm]", xlab="WEAP simulations calibrated to SNOTEL only", main="Hayden Fork vs. Hayden SNOTEL\nSnow Water Equivalent"
     # , log="y"
)
lines(hd$date, hd$obs, col=4,lwd=2)
lines(hd$date, hd$noband, col=2,lwd=2)
lines(hd$date, hd$band, col=3,lwd=2)
legend("topright", legend=c("SNOTEL","No Bands","Bands"), lwd=2, col=c(4,2,3),bty="n")

#####
## snotel with 4 lines
plot(range(ef$date),range(ef[,2:5]),type="n",ylab="SWE [mm]", xlab="WEAP simulations calibrated to SNOTEL only", main="East Fork vs. Lily Lake SNOTEL\nSnow Water Equivalent"
     # , log="y"
)
lines(ef$date, ef$obs, col=4,lwd=2)
lines(ef$date, ef$noband2, col=5, lwd=2)
lines(ef$date, ef$noband, col=2,lwd=2)
lines(ef$date, ef$band, col=3,lwd=2)
legend("topright", legend=c("SNOTEL", "2 SNOTEL", "1 SNOTEL", "No Bands","Bands"), lwd=2, col=c(4,5,2,3),bty="n")


plot(range(hd$date),range(hd[,2:4]),type="n",ylab="SWE [mm]", xlab="WEAP simulations calibrated to SNOTEL only", main="Hayden Fork vs. Hayden SNOTEL\nSnow Water Equivalent"
     # , log="y"
)
lines(hd$date, hd$obs, col=4,lwd=2)
lines(hd$date, hd$noband, col=2,lwd=2)
lines(hd$date, hd$band, col=3,lwd=2)
legend("topright", legend=c("SNOTEL","No Bands","Bands"), lwd=2, col=c(4,2,3),bty="n")

#####
## snotel differences
## snotel with three lines
efd = ef
efd[,2:5]=(ef[,2:5]-ef[,2])
efd[,2:5]=efd[,2:5]/(ef[,2]+.0001)
# efd[,2:5]=ifelse(efd[,2:5]<0,1/efd[,2:5])

hdd = hd
hdd[,2:5]=(hd[,2:5]-hd[,2])
hdd[,2:5]=hdd[,2:5]/(hd[,2]+.0001)+1


plot(range(efd$date),range(efd[,3:5]),type="n",ylab="% Difference", xlab="WEAP simulations calibrated to SNOTEL only", main="East Fork vs. Lily Lake SNOTEL\nPercent Difference from Observed"
     #, log="y", yaxt="n"
     )
lines(efd$date, efd$obs, col=4,lwd=2)
lines(efd$date, efd$noband, col=2,lwd=2)
lines(efd$date, efd$band, col=3,lwd=2)
legend("topright", legend=c("Observed","No Bands","Bands"), lwd=2, col=c(4,2,3),bty="n")


plot(range(hdd$date),range(hdd[,2:4]),type="n",ylab="% Difference", xlab="WEAP simulations calibrated to SNOTEL only", main="Hayden Fork vs. Hayden SNOTEL\nPercent Difference from Observed"
     # , log="y"
)
lines(hdd$date, hdd$obs, col=4,lwd=2)
lines(hdd$date, hdd$noband, col=2,lwd=2)
lines(hdd$date, hdd$band, col=3,lwd=2)
legend("topright", legend=c("Observed","No Bands","Bands"), lwd=2, col=c(4,2,3),bty="n")

#####
## snotel differences
## snotel with 4 lines
plot(range(efd$date),range(efd[,2:5]),type="n",ylab="% Difference", xlab="WEAP simulations calibrated to SNOTEL only", main="East Fork vs. Lily Lake SNOTEL\nSPercent Difference from Observed"
     # , log="y"
)
lines(efd$date, efd$obs, col=4,lwd=2)
lines(efd$date, efd$noband, col=2,lwd=2)
lines(efd$date, efd$noband2, col=5, lwd=2)
lines(efd$date, efd$band, col=3,lwd=2)
legend("topright", legend=c("Observed", "2 SNOTEL", "1 SNOTEL","Bands"), lwd=2, col=c(4,5,2,3),bty="n")


plot(range(hdd$date),range(hdd[,2:4]),type="n",ylab="SWE [mm]", xlab="WEAP simulations calibrated to SNOTEL only", main="Hayden Fork vs. Hayden SNOTEL\nPercent Difference from Observed"
     # , log="y"
)
lines(hdd$date, hdd$obs, col=4,lwd=2)
lines(hdd$date, hdd$noband, col=2,lwd=2)
lines(hdd$date, hdd$band, col=3,lwd=2)
lines(hdd$date, hdd$noband2, col=5,lwd=2)
legend("topright", legend=c("Observed","2 SNOTEL","1 SNOTEL","Bands"), lwd=2, col=c(4,5,2,3),bty="n")



#####





#####



#####
library(lubridate)
ef$month = month(ef$date)
hd$month = month(hd$date)

efm=aggregate(.~month,data=ef,mean)
hdm=aggregate(.~month,data=hd,mean)

plot(range(efm$month),range(ef[,3:5]),type="n",ylab="Monthly SWE [mm]", xlab="WEAP simulations calibrated to SNOTEL only", main="East Fork vs. Lily Lake SNOTEL\nSnow Water Equivalent", xaxt="n")
axis(1,1:12,month.abb)
lines(efm$month, efm$obs, col=4,lwd=2)
lines(efm$month, efm$noband, col=2,lwd=2)
lines(efm$month, efm$band, col=3,lwd=2)
legend("topright", inset=c(0.03,0.06), legend=c("SNOTEL","No Bands","Bands"), lwd=2, col=c(4,2,3),bty="n")
legend("top",legend = "This gauge was not used for calibration\nwith the unbanded model because of\nthe distance from catchment centroid",bty="n")


plot(range(hdm$month),range(hd[,2:4]),type="n",ylab="Average SWE [mm]", xlab="WEAP simulations calibrated to SNOTEL only", main="Hayden Fork vs. Hayden SNOTEL\nSnow Water Equivalent", xaxt="n")
axis(1,1:12,month.abb)
lines(hdm$month, hdm$obs, col=4,lwd=2)
lines(hdm$month, hdm$noband, col=2,lwd=2)
lines(hdm$month, hdm$band, col=3,lwd=2)
legend("topright", inset=c(0.03,0.06), legend=c("SNOTEL","No Bands","Bands"), lwd=2, col=c(4,2,3),bty="n")
