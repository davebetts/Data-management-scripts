rm(list=ls())
gc()
dir()

## downloaded flow data "USU-LBR-Mendon-USU44.csv" 
## deleted all columns in CSV except "DataValue" and "LocalDateTime"
## changed column names to "Qcfs" and "date"
## modified the "date" column within the CSV to be readable by POSIXct functions (%Y-%m-%d %H:%M)

Qmendon0<-read.csv("Qmendon0.csv",header=T) #read original flow data with the modifications as listed above

## checking data for basic structure and components
dim(Qmendon0)
str(Qmendon0)
summary(Qmendon0)
# checking for errors in CSV file
head(Qmendon0)
tail(Qmendon0)
# checking the number of times the highest and lowest values appear
head(table(Qmendon0$Qcfs),12) #displaying the bottom 12 flow
tail(table(Qmendon0$Qcfs),12) #displaying the top 12 flow
max(table(Qmendon0$Qcfs)) #checking the maximum number of repeated values for flow

## downloaded gage height data "USU-LBR-Mendon-USU13.csv" 
## deleted all columns in CSV except "DataValue" and "LocalDateTime"
## changed column names to "ghft" and "date"
## modified the "date" column within the CSV to be readable by POSIXct functions (%Y-%m-%d %H:%M)

GHmendon0<-read.csv("GHmendon0.csv",header=T) #read original gage height data with the modifications as listed above

## checking data for basic structure and components
dim(GHmendon0)
str(GHmendon0)
summary(GHmendon0)
# checking for errors in CSV file
head(GHmendon0)
tail(GHmendon0)
# checking the number of times the highest and lowest values appear
head(table(GHmendon0$ghft),12) #displaying the bottom 12 gage height
tail(table(GHmendon0$ghft),12) #displaying the top 12 gage height
max(table(GHmendon0$ghft)) #checking the maximum number of repeated values for gage height

#merging files by "date" in order to calculate a relationship equation in excel
#find out how to do this in R someday
QGH<-merge(Qmendon0,GHmendon0,by=c("date","date"))
ls(QGH)
str(QGH)
dim(QGH)

plot(QGH$Qcfs~QGH$ghft,abline(m1))
m1<-lm(QGH$Qcfs~QGH$ghft)
summary(m1)

write.csv(QGH,"C:/Users/Sarah/Documents/Dropbox/PhD/Coursework/WaterQuality&Pollution/SmallGroupProjects/QGH.csv",row.names=F)

## downloaded gage height data "USU-LBR-Mendon-USU13.csv" 
## deleted all columns in CSV except "DataValue" and "LocalDateTime"
## changed column names to "ghft" and "date"
## modified the "date" column within the CSV to be readable by POSIXct functions (%Y-%m-%d %H:%M)

GHmendon.add<-read.csv("GHmendon_add.csv",header=T,na.strings="-9999") #read original gage height data with the modifications as listed above

## checking data for basic structure and components
dim(GHmendon.add)
str(GHmendon.add)
summary(GHmendon.add)
# checking for errors in CSV file
head(GHmendon.add)
tail(GHmendon.add)
# checking the number of times the highest and lowest values appear
head(table(GHmendon.add$ghft),12) #displaying the bottom 12 gage height
tail(table(GHmendon.add$ghft),12) #displaying the top 12 gage height
max(table(GHmendon.add$ghft)) #checking the maximum number of repeated values for gage height

#merging files by "date" in order to calculate a relationship equation in excel
#find out how to do this in R someday
GH.add<-merge(GHmendon.add,GHmendon0,by=c("date","date"))
ls(GH.add)
str(GH.add)
dim(GH.add)
summary(GH.add)

plot(GH.add$ghft~GH.add$ghft.add)
m2<-lm(GH.add$ghft~GH.add$ghft.add)
summary(m2)

write.csv(GH.add,"C:/Users/Sarah/Documents/Dropbox/PhD/Coursework/WaterQuality&Pollution/SmallGroupProjects/GHadd.csv",row.names=F)

## downloaded gage height data "USU-LBR-Mendon-USU13.csv" 
## deleted all columns in CSV except "DataValue" and "LocalDateTime"
## changed column names to "ghft" and "date"
## modified the "date" column within the CSV to be readable by POSIXct functions (%Y-%m-%d %H:%M)

QParadise0<-read.csv("USGS_10105900_LBR_Paradise.csv",header=T) #read original gage height data with the modifications as listed above

## checking data for basic structure and components
dim(QParadise0)
str(QParadise0)
summary(QParadise0)
# checking for errors in CSV file
head(QParadise0)
tail(QParadise0)
# checking the number of times the highest and lowest values appear
head(table(QParadise0$Qcfs),12) #displaying the bottom 12 flow
tail(table(QParadise0$Qcfs),12) #displaying the top 12 flow
max(table(QParadise0$Qcfs)) #checking the maximum number of repeated values for flow

## Graph the flow vs. date of the entire run of data to check for blips
QParadise0$date2=as.POSIXct(QParadise0$date) # paste "date" as a POSIXct column within the data.frame "QParadise0"
summary(QParadise0) #check to see if "date" is considered as a date.
plot(QParadise0$Qcfs~QParadise0$date2)
#found a few sections of outliers that might be worth omitting, but overall data seems okay at this scale


plot