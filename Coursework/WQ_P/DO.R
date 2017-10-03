rm(list=ls())
gc()

#############################################################################################
###           Analysis of High Frequency DO Data on the Little Bear River                 ###
###                 3 sites: South Fork, Paradise, Mendon Road                            ###
###          All data from: http://littlebearriver.usu.edu/sites/Default.aspx             ###
###                         David J. Betts - December 2014                                ###
###                     Water Quality & Pollution (WATS 4530/6530)                        ###   
#############################################################################################

#############################################################################################
###                                   Mendon DO data                                      ###
###                                Water years: 2010-2012                                 ###
#############################################################################################
## downloaded continuous DO data "USU-LBR-Mendon-USU32.csv" 
## deleted all columns in CSV except "DataValue" and "LocalDateTime"
## changed column names to "Mendon" and "date"
## deleting all dates except from 10/1/2009 to 9/30/2012 within csv file, and saving as "DOm.csv"
## modified the "date" column within the CSV to be readable by POSIXct functions 
## Excel format: yyyy-m-d h:mm

DOm<-read.csv("DOm.csv",header=T) #read original DO data with the modifications as listed above

## checking data for basic structure and components
dim(DOm) #data.frame dimensions
str(DOm) #data.frame structure
summary(DOm) #column summaries
# checking for errors in CSV file
head(DOm) # first 6 rows of the data.frame
tail(DOm) # last 6 rows of the data.frame

## Graph the DO vs. date to check for outliers and patterns indicating error
DOm$date2=as.POSIXct(DOm$date) # paste "date" as a new POSIXct column within the data.frame "DOm"
summary(DOm) #check new data.frame for added column and if the column has the time class.
plot(DOm$Mendon~DOm$date2) # plot data and check for outliers

# I repeatedly loaded and plotted the data with the previous steps to clean up the data
# A search for all 0s was done within the CSV to see if the 0s matched visual outliers in the plot
# or for removal if the zeros and some of the immediately surrounding values did not match 
# tends in the surrounding data. In some cases large strings of repeated exact values were deleted.
# Using the CV file to search for outliers, I created a third column with the following formula in C4:
# =ABS(A4-A3)+ABS(A4-A2)+ABS(A4-A5)+ABS(A4-A6) <- this compares the sum of the absolute values 
# for the differences between A4 and the 2 cells prior and the 2 cells after A4
# copying and pasting this formula through the end of the column (minus the last 2 cells) 
# helps identify cells that are the most different from the surrounding cells. 
# then I copied the results as text into a 4th column, in order to be able to search for the maximum
# values within that column.
# I deleted cells that appeared to be sensor or data errors.

#############################################################################################
###                              Analysis by days - Mendon                                ###
###              Objective: find the mean, maximum and minimum values per day             ###
#############################################################################################
library(xts) # this package allows the data in the date column to be considered as dates
mDO.xts<-xts(x=DOm$Mendon,as.POSIXct(DOm$date)) #build a relationship between the DO values and the dates

mTimeFrame <- endpoints(mDO.xts,'days')  # sets the time frame by which to analyze the data
minm = as.matrix(period.apply(mDO.xts,INDEX = mTimeFrame, FUN =min)) # minimum value per day
meanm = as.matrix(period.apply(mDO.xts,INDEX = mTimeFrame, FUN =mean)) # mean value per day
maxm = as.matrix(period.apply(mDO.xts,INDEX = mTimeFrame, FUN =max)) # maximum value per day

mDO<-cbind(minm,meanm,maxm) # bind the min, mean, and max vectors into a single object
summary(mDO) # display summary of results
mDO<-data.frame(        				# convert bound objects (mDO) into a data.frame (mDO)
  as.POSIXct(rownames(mDO),tz=""),mDO)  # converting the row.names to a column of POSIXct data
summary(mDO) # display summary of results
colnames(mDO)<-c("Date","Min.DO","Mean.DO","Max.DO") #specify column names
summary(mDO) # display summary of results

### write CSV file containing columns for the days, minimum, mean, and maximum DO levels 
write.csv(mDO,"C:/Users/davebetts/Dropbox/PhD/Coursework/WaterQuality&Pollution/SmallGroupProjects/Mendon_DO_data.csv",row.names=F,na="")

#############################################################################################
###                                   Paradise DO data                                    ###
###                                Water years: 2010-2012                                 ###
#############################################################################################
## downloaded continuous DO data "USU-LBR-Paradise-USU32.csv" 
## deleted all columns in CSV except "DataValue" and "LocalDateTime"
## changed column names to "Paradise" and "date"
## deleting all dates except from 10/1/2009 to 9/30/2012 within csv file, and saving as "DOp.csv"
## modified the "date" column within the CSV to be readable by POSIXct functions 
## Excel format: yyyy-m-d h:mm

DOp<-read.csv("DOp.csv",header=T) #read original DO data with the modifications as listed above

## checking data for basic structure and components
dim(DOp) #data.frame dimensions
str(DOp) #data.frame structure
summary(DOp) #column summaries
# checking for errors in CSV file
head(DOp) # first 6 rows of the data.frame
tail(DOp) # last 6 rows of the data.frame

## Graph the DO vs. date to check for outliers and patterns indicating error
DOp$date2=as.POSIXct(DOp$date) # paste "date" as a new POSIXct column within the data.frame "DOp"
summary(DOp) #check new data.frame for added column and if the column has the time class.
plot(DOp$Paradise~DOp$date2) # plot data and check for outliers

# I repeatedly loaded and plotted the data with the previous steps to clean up the data
# A search for all 0s was done within the CSV to see if the 0s matched visual outliers in the plot
# or for removal if the zeros and some of the immediately surrounding values did not match 
# tends in the surrounding data. In some cases large strings of repeated exact values were deleted.
# Using the CV file to search for outliers, I created a third column with the following formula in C4:
# =ABS(A4-A3)+ABS(A4-A2)+ABS(A4-A5)+ABS(A4-A6) <- this compares the sum of the absolute values 
# for the differences between A4 and the 2 cells prior and the 2 cells after A4
# copying and pasting this formula through the end of the column (minus the last 2 cells) 
# helps identify cells that are the most different from the surrounding cells. 
# then I copied the results as text into a 4th column, in order to be able to search for the maximum
# values within that column.
# I deleted cells that appeared to be sensor or data errors.

#############################################################################################
###                               Analysis by days - Paradise                             ###
###              Objective: find the mean, maximum and minimum values per day             ###
#############################################################################################
library(xts) # this package allows the data in the date column to be considered as dates
pDO.xts<-xts(x=DOp$Paradise,as.POSIXct(DOp$date)) #build a relationship between the DO values and the dates

TimeFrame <- endpoints(pDO.xts,'days')  # sets the time frame by which to analyze the data
minp = as.matrix(period.apply(pDO.xts,INDEX = pTimeFrame, FUN =min)) # minimum value per day
meanp = as.matrix(period.apply(pDO.xts,INDEX = pTimeFrame, FUN =mean)) # mean value per day
maxp = as.matrix(period.apply(pDO.xts,INDEX = pTimeFrame, FUN =max)) # maximum value per day

pDO<-cbind(minp,meanp,maxp) # bind the min, mean, and max vectors into a single object
summary(pDO) # display summary of results
pDO<-data.frame(    						# convert bound objects (pDO) into a data.frame (pDO)
  as.POSIXct(rownames(pDO),tz=""),pDO)  # converting the row.names to a column of POSIXct data
summary(pDO) # display summary of results
colnames(pDO)<-c("Date","Min.DO","Mean.DO","Max.DO") #specify column names
summary(pDO) # display summary of results

### write CSV file containing columns for the days, minimum, mean, and maximum DO levels 
write.csv(pDO,"C:/Users/davebetts/Dropbox/PhD/Coursework/WaterQuality&Pollution/SmallGroupProjects/Paradise_DO_data.csv",row.names=F,na="")

#############################################################################################
###                                  SouthFork DO data                                    ###
###                                Water years: 2010-2012                                 ###
#############################################################################################
## downloaded continuous DO data "USU-LBR-SFLower-USU32.csv" 
## deleted all columns in CSV except "DataValue" and "LocalDateTime"
## changed column names to "SouthFork" and "date"
## deleting all dates except from 10/1/2009 to 9/30/2012 within csv file, and saving as "DOs.csv"
## modified the "date" column within the CSV to be readable by POSIXct functions 
## Excel format: yyyy-m-d h:mm

DOs<-read.csv("DOs.csv",header=T) #read original DO data with the modifications as listed above

## checking data for basic structure and components
dim(DOs) #data.frame dimensions
str(DOs) #data.frame structure
summary(DOs) #column summaries
  # checking for errors in CSV file
head(DOs) # first 6 rows of the data.frame
tail(DOs) # last 6 rows of the data.frame

## Graph the DO vs. date to check for outliers and patterns indicating error
DOs$date2=as.POSIXct(DOs$date) # paste "date" as a new POSIXct column within the data.frame "DOs"
summary(DOs) #check new data.frame for added column and if the column has the time class.
plot(DOs$SouthFork~DOs$date2) # plot data and check for outliers

# I repeatedly loaded and plotted the data with the previous steps to clean up the data
# A search for all 0s was done within the CSV to see if the 0s matched visual outliers in the plot
# or for removal if the zeros and some of the immediately surrounding values did not match 
# tends in the surrounding data. In some cases large strings of repeated exact values were deleted.
# Using the CV file to search for outliers, I created a third column with the following formula in C4:
# =ABS(A4-A3)+ABS(A4-A2)+ABS(A4-A5)+ABS(A4-A6) <- this compares the sum of the absolute values 
# for the differences between A4 and the 2 cells prior and the 2 cells after A4
# copying and pasting this formula through the end of the column (minus the last 2 cells) 
# helps identify cells that are the most different from the surrounding cells. 
# then I copied the results as text into a 4th column, in order to be able to search for the maximum
# values within that column.
# I deleted cells that appeared to be sensor or data errors.

#############################################################################################
###                              Analysis by days - SouthFork                             ###
###              Objective: find the mean, maximum and minimum values per day             ###
#############################################################################################
library(xts) # this package allows the data in the date column to be considered as dates
sDO.xts<-xts(x=DOs$SouthFork,as.POSIXct(DOs$date)) #build a relationship between the DO values and the dates

sTimeFrame <- endpoints(sDO.xts,'days')  # sets the time frame by which to analyze the data
mins = as.matrix(period.apply(sDO.xts,INDEX = sTimeFrame, FUN =min)) # minimum value per day
means = as.matrix(period.apply(sDO.xts,INDEX = sTimeFrame, FUN =mean)) # mean value per day
maxs = as.matrix(period.apply(sDO.xts,INDEX = sTimeFrame, FUN =max)) # maximum value per day

sDO<-cbind(mins,means,maxs) # bind the min, mean, and max vectors into a single object
summary(sDO) # display summary of results
sDO<-data.frame(    						# convert bound objects (sDO) into a data.frame (sDO)
  as.POSIXct(rownames(sDO),tz=""),sDO)  # converting the row.names to a column of POSIXct data
summary(sDO) # display summary of results
colnames(sDO)<-c("Date","Min.DO","Mean.DO","Max.DO") #specify column names
summary(sDO) # display summary of results

### write CSV file containing columns for the days, minimum, mean, and maximum DO levels 
write.csv(sDO,"C:/Users/davebetts/Dropbox/PhD/Coursework/WaterQuality&Pollution/SmallGroupProjects/SouthFork_DO_data.csv",row.names=F,na="")

#############################################################################################
###                     Direct Comparison of DO Data at 3 sites:                          ###
###                        South Fork, Paradise, Mendon Road                              ###
###            data objects mDO, pDO and sDO from previous sections are required          ###
#############################################################################################

########################################################################################
###             Combine mean DO data for each site into a single table               ###
###                                     ---                                          ###
###             This table can be used to simultaneously plot DO levels              ###
###                    at all three sites for the entire time frame                  ###
########################################################################################

summary(mDO) # making sure mDO is still an active object
summary(pDO) # making sure mDO is still an active object
summary(sDO) # making sure mDO is still an active object

DO<-merge(sDO,pDO, by=c("Date","Date"),all.x=T) # merge all rows of sDO and pDO by "Date"
summary(DO) #display summary of results
DO<-merge(DO,mDO,by=c("Date","Date"),all.x=T) # merge all rows of DO with mDO by "Date"
summary(DO)  #display summary of results
DO<-DO[,c(1,3,6,9)] # keep only columns of the "Date" and mean daily DO levels for each site
summary(DO)  #display summary of results
colnames(DO)<-c("date","SouthFork","Paradise","Mendon") #relabel columns 
summary(DO)  #display summary of results

# create CSV file from the combined average daily DO levels
write.csv(DO,"C:/Users/davebetts/Dropbox/PhD/Coursework/WaterQuality&Pollution/SmallGroupProjects/DO3yr.csv",row.names=F,na="")

########################################################################################
###                  Comparison of mean DO levels by day of year                     ###
###                                     ---                                          ###
###               All dates are converted to a decimal format:                       ###
###                       Jan 1st = 0; December 31st = 1                             ###
###    All dates are converted within the 0-1 scale, regardless of original year     ###
###        The resulting table can be used to simultaneously plot DO levels          ###
###      representing the average DO level of a given day within a single year       ###
########################################################################################

# "DO" from the previous section is required
DO$decimal.date<-( 					# create a new column expressing the date as a decimal
  ((as.numeric(format(DO$date,format="%m")))-1)/12)+ 		#based on the month
  (((as.numeric(format(DO$date,format="%d")))-1)/365) 	#and day, but excluding the year
summary(DO)

# average DO values for Mendon based on day of the year
mavg<-aggregate(DO$Mendon,list(DO$decimal.date),mean,na.rm=T); colnames(mavg)=c("date","Mendon") 
summary(mavg) #display summary of results

# average DO values for Paradise based on day of the year
pavg<-aggregate(DO$Paradise,list(DO$decimal.date),mean,na.rm=T); colnames(pavg)=c("date","Paradise")
summary(pavg) # display summary of results

# average DO values for SouthFork based on day of the year
savg<-aggregate(DO$SouthFork,list(DO$decimal.date),mean,na.rm=T); colnames(savg)=c("date","SouthFork")
summary(savg) # display summary of results

DOavg<-cbind(savg,pavg,mavg) # combine data from all three sites into a single table
summary(DOavg) # display summary of results
DO.avg<-DOavg[,c(1:2,4,6)] # remove unnecessary columns
summary(DO.avg) # display summary of results

# write table "DO.avg" as a CSV file
write.csv(DO.avg,"C:/Users/davebetts/Dropbox/PhD/Coursework/WaterQuality&Pollution/SmallGroupProjects/DO_avg.csv",row.names=F,na="")

