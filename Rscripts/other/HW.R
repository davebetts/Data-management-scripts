setwd("C:/Users/davebetts/Dropbox/PhD/Coursework/WaterQuality&Pollution/Homework Assignments/Assignments/LittleBearWatershed")
dir()
riv0<-read.csv("RiverSites.csv",header=T,skip=1)
dim(riv0)
head(riv0)
str(riv0)
riv1<-riv0[c(1:4,10)]
riv1$mmddyy<-as.Date(riv1$mmddyy,format="%d/%m/%y")
str(riv1)

riv1<-read.csv("RiverSites1.csv",header=T,skip=1)
riv1$mmddyy<-as.Date(riv1$mmddyy,format="%m/%d/%y")
tail(riv1)

library(lattice)
xyplot(QTP~mmddyy | SiteName,data=riv1)
xyplot

pnt1<-read.csv("PointSource1.csv",header=T)
str(pnt1)
pnt1$mmddyyyy<-as.Date(pnt1$mmddyyyy,format="%m/%d/%Y")
str(pnt1)
tail(pnt1)
xyplot(QTP~mmddyyyy | SiteName,data=pnt1)
unique(pnt1$SiteName)

pnt1<-read.csv("PointSource1.csv",header=T)
str(pnt1)
pnt1$mmddyyyy<-as.Date(pnt1$mmddyyyy,format="%m/%d/%Y")
str(pnt1)
tail(pnt1)
xyplot(QTP~mmddyyyy | SiteName,data=pnt1)
unique(pnt1$SiteName)

tot1<-read.csv("Total1.csv",header=T,skip=1)
str(tot1)
tot1$mmddyy<-as.Date(tot1$mmddyy,format="%m/%d/%Y")
str(tot1)
tail(tot1)
xyplot(QTP~mmddyy | SiteName,data=tot1)
unique(tot1$SiteName)
names(tot1)

tot2<-read.csv("Total2.csv",header=T,skip=1)
tot2$mmddyy<-as.Date(tot2$mmddyy,format="%m/%d/%Y")
str(tot2)
tail(tot2)

site0<-split(tot2,tot2$SiteName)
str(site0)

#If you want individual object with the group SiteNames names you could assign the elements of X from split to objects of those names, though this seems like extra work when you can just index the data frames from the list split creates.

#you could use lapply to drop the SiteNames column, but in this case, all columns were kept.
site1<-lapply(seq_along(site0), function(x) as.data.frame(site0[[x]])[, 1:6])

#Assign the dataframes in the list site1 to individual objects
A1<- site1[[1]]
A2<- site1[[2]]
A3<- site1[[3]]
A4<- site1[[4]]
A5<- site1[[5]]
A6<- site1[[6]]
A7<- site1[[7]]
A8<- site1[[8]]
A9<- site1[[9]]
A10<- site1[[10]]
A11<- site1[[11]]
A12<- site1[[12]]
A13<- site1[[13]]
A14<- site1[[14]]
A15<- site1[[15]]
A16<- site1[[16]]
A17<- site1[[17]]


#Or use lapply with assign to assign each piece to an object all at once
#i couldn't get this function to work
lapply(seq_along(site1), function(x) {
  assign(c("B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "B10", "B11", "B12", "B13", "B14", "B15", "B16", "B17???)[x], site1[[x]], envir=.GlobalEnv)
}
)

