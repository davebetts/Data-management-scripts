rm(list=ls())
gc()

dir()

#############################################################################################
###           Analysis of High Frequency flow data on the Little Bear River               ###
###                 3 sites: South Fork, Paradise, Mendon Road                            ###
###          All data from: http://littlebearriver.usu.edu/sites/Default.aspx             ###
#############################################################################################

#############################################################################################
###                                   Mendon flow data                                    ###
###                                Water years: 2010-2012                                 ###
#############################################################################################
## downloaded continuous flow data "USU-LBR-Mendon-USU44.csv" 
## deleted all columns in CSV except "DataValue" and "LocalDateTime"
## changed column names to "Mendon" and "date"
## deleting all dates except from 10/1/2009 to 9/30/2012 within csv file, and saving as "Qm.csv"
## modified the "date" column within the CSV to be readable by POSIXct functions 
## Excel format: yyyy-m-d h:mm

Qm<-read.csv("Qm.csv",header=T) #read original flow data with the modifications as listed above

## checking data for basic structure and components
dim(Qm) #data.frame dimensions
str(Qm) #data.frame structure
summary(Qm) #column summaries
# checking for errors in CSV file
head(Qm) # first 6 rows of the data.frame
tail(Qm) # last 6 rows of the data.frame

## Graph the flow rates vs. date to check for outliers and patterns indicating error
Qm$date2=as.POSIXct(Qm$date) # paste "date" as a new POSIXct column within the data.frame "Qm"
summary(Qm) #check new data.frame for added column and if the column has the time class.
plot(Qm$Mendon~Qm$date2) # plot data and check for outliers

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
###                      Objective: find the mean flow rate per day                       ###
#############################################################################################
library(xts) # this package allows the data in the date column to be considered as dates
mQ.xts<-xts(x=Qm$Mendon,as.POSIXct(Qm$date)) #build a relationship between the flow rates and the dates

mTimeFrame <- endpoints(mQ.xts,'days')  # sets the time frame by which to analyze the data
meanm = as.matrix(period.apply(mQ.xts,INDEX = mTimeFrame, FUN =mean)) # mean value per day
meanm<-data.frame(          			# convert meanm into a data.frame
  as.POSIXct(rownames(meanm),tz=""),meanm)  # converting the row.names to a column of POSIXct data
summary(meanm) # display summary of results
colnames(meanm)<-c("Date","Mean.Q") #specify column names
summary(meanm) # display summary of results

### write CSV file containing columns for the days, minimum, mean, and maximum flow rates 
write.csv(meanm,"C:/Users/davebetts/Dropbox/PhD/Coursework/WaterQuality&Pollution/SmallGroupProjects/Mendon_flow_data.csv",row.names=F,na="")

#############################################################################################
###                                  Paradise flow data                                   ###
###                                Water years: 2010-2012                                 ###
#############################################################################################
## downloaded continuous flow data "USU-LBR-Paradise-USU44.csv" 
## deleted all columns in CSV except "DataValue" and "LocalDateTime"
## changed column names to "Paradise" and "date"
## deleting all dates except from 10/1/2009 to 9/30/2012 within csv file, and saving as "Qp.csv"
## modified the "date" column within the CSV to be readable by POSIXct functions 
## Excel format: yyyy-m-d h:mm

Qp<-read.csv("Qp.csv",header=T) #read original flow data with the modifications as listed above

## checking data for basic structure and components
dim(Qp) #data.frame dimensions
str(Qp) #data.frame structure
summary(Qp) #column summaries
# checking for errors in CSV file
head(Qp) # first 6 rows of the data.frame
tail(Qp) # last 6 rows of the data.frame

## Graph the flow rates vs. date to check for outliers and patterns indicating error
Qp$date2=as.POSIXct(Qp$date) # paste "date" as a new POSIXct column within the data.frame "Qp"
summary(Qp) #check new data.frame for added column and if the column has the time class.
plot(Qp$Paradise~Qp$date2) # plot data and check for outliers

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
###                        Objective: find the mean flow rate per day                      ###
#############################################################################################
library(xts) # this package allows the data in the date column to be considered as dates
Qp.xts<-xts(x=Qp$Paradise,as.POSIXct(Qp$date)) #build a relationship between the flow rates and the dates

TimeFrame <- endpoints(pQ.xts,'days')  # sets the time frame by which to analyze the data
meanp = as.matrix(period.apply(pQ.xts,INDEX = pTimeFrame, FUN =mean)) # mean value per day

meanp<-data.frame(    						# convert meanp into a data.frame 
  as.POSIXct(rownames(meanp),tz=""),meanp)  # converting the row.names to a column of POSIXct data
summary(meanp) # display summary of results
colnames(meanp)<-c("Date","Mean.Q") #specify column names
summary(meanp) # display summary of results

### write CSV file containing columns for the days, minimum, mean, and maximum flow rates 
write.csv(meanp,"C:/Users/davebetts/Dropbox/PhD/Coursework/WaterQuality&Pollution/SmallGroupProjects/Paradise_flow_data.csv",row.names=F,na="")

#############################################################################################
###                                 SouthFork flow data                                   ###
###                                Water years: 2010-2012                                 ###
#############################################################################################
## downloaded continuous flow data "USU-LBR-SFLower-USU44.csv" 
## deleted all columns in CSV except "DataValue" and "LocalDateTime"
## changed column names to "SouthFork" and "date"
## deleting all dates except from 10/1/2009 to 9/30/2012 within csv file, and saving as "Qs.csv"
## modified the "date" column within the CSV to be readable by POSIXct functions 
## Excel format: yyyy-m-d h:mm

Qs<-read.csv("Qs.csv",header=T) #read original flow data with the modifications as listed above

## checking data for basic structure and components
dim(Qs) #data.frame dimensions
str(Qs) #data.frame structure
summary(Qs) #column summaries
# checking for errors in CSV file
head(Qs) # first 6 rows of the data.frame
tail(Qs) # last 6 rows of the data.frame

## Graph the flow rates vs. date to check for outliers and patterns indicating error
Qs$date2=as.POSIXct(Qs$date) # paste "date" as a new POSIXct column within the data.frame "Qs"
summary(Qs) #check new data.frame for added column and if the column has the time class.
plot(Qs$SouthFork~Qs$date2) # plot data and check for outliers

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
###                      Objective: find the mean flow rate per day                       ###
#############################################################################################
library(xts) # this package allows the data in the date column to be considered as dates
sQ.xts<-xts(x=Qs$SouthFork,as.POSIXct(Qs$date)) #build a relationship between the flow rates and the dates

sTimeFrame <- endpoints(sQ.xts,'days')  # sets the time frame by which to analyze the data
means = as.matrix(period.apply(sQ.xts,INDEX = sTimeFrame, FUN =mean)) # mean value per day

means<-data.frame(    						# convert means into a data.frame
  as.POSIXct(rownames(means),tz=""),means)  # converting the row.names to a column of POSIXct data
summary(means) # display summary of results
colnames(means)<-c("Date","Mean.Q") #specify column names
summary(means) # display summary of results

### write CSV file containing columns for the days, minimum, mean, and maximum flow rates 
write.csv(means,"C:/Users/davebetts/Dropbox/PhD/Coursework/WaterQuality&Pollution/SmallGroupProjects/SouthFork_flow_data.csv",row.names=F,na="")

#############################################################################################
###                   Direct Comparison of flow data at 3 sites:                          ###
###                        South Fork, Paradise, Mendon Road                              ###
###          data objects meanm, meanp and means from previous sections are required      ###
#############################################################################################

########################################################################################
###           Combine mean flow data for each site into a single table               ###
###                                     ---                                          ###
###           This table can be used to simultaneously plot flow rates               ###
###                at all three sites for the entire time frame                      ###
########################################################################################

summary(meanm) # making sure meanm is still an active object
summary(meanp) # making sure meanQ is still an active object
summary(means) # making sure mQ is still an active object

Q<-merge(means,meanp, by=c("Date","Date"),all.x=T) # merge all rows of means and meanp by "Date"
summary(Q) #display summary of results
Q<-merge(Q,meanm,by=c("Date","Date"),all.x=T) # merge all rows of Q with meanm by "Date"
summary(Q)  #display summary of results
Q<-Q[,c(1,3,6,9)] # keep only columns of the "Date" and mean daily flow rates for each site
summary(Q)  #display summary of results
colnames(Q)<-c("date","SouthFork","Paradise","Mendon") #relabel columns 
summary(Q)  #display summary of results

# create CSV file from the combined average daily flow rates
write.csv(Q,"C:/Users/davebetts/Dropbox/PhD/Coursework/WaterQuality&Pollution/SmallGroupProjects/Q3yr.csv",row.names=F,na="")

########################################################################################
###                 Comparison of mean flow rates by day of year                     ###
###                                     ---                                          ###
###               All dates are converted to a decimal format:                       ###
###                       Jan 1st = 0; December 31st = 1                             ###
###    All dates are converted within the 0-1 scale, regardless of original year     ###
###        The resulting table can be used to simultaneously plot flow rates         ###
###       representing the average Q level of a given day within a single year       ###
########################################################################################

# "Q" from the previous section is required
Q$decimal.date<-( 					# create a new column expressing the date as a decimal
  ((as.numeric(format(Q$date,format="%m")))-1)/12)+ 		#based on the month
  (((as.numeric(format(Q$date,format="%d")))-1)/365) 	#and day, but excluding the year
summary(Q)

# average flow rates for Mendon based on day of the year
mavg<-aggregate(Q$Mendon,list(Q$decimal.date),mean,na.rm=T); colnames(mavg)=c("date","Mendon") 
summary(mavg) #display summary of resultsh
# average flow rates for Paradise based on day of the year
pavg<-aggregate(Q$Paradise,list(Q$decimal.date),mean,na.rm=T); colnames(pavg)=c("date","Paradise")
summary(pavg) # display summary of results

# average flow rates for SouthFork based on day of the year
savg<-aggregate(Q$SouthFork,list(Q$decimal.date),mean,na.rm=T); colnames(savg)=c("date","SouthFork")
summary(savg) # display summary of results

Qavg<-cbind(savg,pavg,mavg) # combine data from all three sites into a single table
summary(Qavg) # display summary of results
Q.avg<-Qavg[,c(1:2,4,6)] # remove unnecessary columns
summary(Q.avg) # display summary of results

# write table "Q.avg" as a CSV file
write.csv(Q.avg,"C:/Users/davebetts/Dropbox/PhD/Coursework/WaterQuality&Pollution/SmallGroupProjects/Q_avg.csv",row.names=F,na="")
