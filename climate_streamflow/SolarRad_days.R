rm(list=ls())
gc()
#############################################################################################
###            Solar Radiation Analysis: The search for the perfect day(s)                ###
###                 Data site:                             ###
###          All data from:             ###
###                         David J. Betts - April 2015                                ###
###                              ACWA Lab - Data Processing                               ###
#############################################################################################

#############################################################################################
###                                   Data Preparation                                    ###
###                                Formatting Date & Time                                 ###
###                    Additional columns for data frame calculations                     ###
#############################################################################################
##
## The initial data preparation of the CSV file was done in Excel to delete unwanted
## columns and convert dates to a format that the R packages could interpret as a date.
##
## * Data downloaded from "   " 
## * Delete all columns of data except two: date and time, and solar radiation.
## * (Be sure to delete the columns and not just the data within the column or R will look for
##   additional data in the empty spaces.)
## * Change column names to "date.time" and "sol.rad"
## * Modify "date.time" column within the table to be readable by POSIXct functions by 
##   selecting entire column, and changing the number formatting to a "Custom" format.
## * In the number format window, in the space below "Type:": yyyy-m-d h:mm
## * Save the two columns as "sr.csv"
## 

########## Reading the data and checking for errors ##########
sr<-read.csv("sr.csv",header=T) # import the CSV into R to create a data.frame object "sr"

## checking data for basic structure and components
dim(sr) #data.frame dimensions
str(sr) #data.frame structure (labels and data types)
summary(sr) # print summary of all columns (maximums, minimums, means, quartiles, and the number of NA(null) values, and unique labels)
# checking for errors in CSV file (e.g too many or too few rows, ragged edges of the data)
head(sr) # first 6 rows of the data.frame
tail(sr) # last 6 rows of the data.frame

##########     Formatting "date.time" to be read as a date     ##########
### !!! Hourly data (at the top of the hour - ":00" reported for  !!! ###
### !!! the minutes) was processing poorly for me (5/8/15).       !!! ###
### !!! Formatting the original "date.time" column to be read as  !!! ###
### !!! POSIXct date & time data produced NULL values ("NA") from !!! ###
### !!! 12/21 to 1/20 for every year of data.  Creating a new     !!! ###
### !!! column of POSIXct date & time data rounded to the day -   !!! ###
### !!! - all hours were dropped.  
### ** I need to come back and add a statement for the timzone.  
### ** I need to look at how GMT is used, but but converting to POSIXct assumes local time  !!! ###
sr$date.timeP=as.POSIXct(sr$date.time,tz="") # creates a new column of POSIXct date & time data, from the original "date.time" column.  The new column "date.timeP" will allow R to read all of the date/time elements from the data (year, month, day, hour, minutes, seconds).
#check to see if the new column "date.timeP" has been added and is being read as POSIXct date & time data
str(sr)  
summary(sr)
unique(sr$date.time) # print all unique values - looking for NAs or gaps
unique(sr$date.timeP) # print all unique values - looking for NAs or gaps

#this plot does not require POSIXct format of date&time data
plot(sr$sol.rad~sr$date.timeP) # plot solar radiation vs. dates & times to see the general shape of the graph

##### If you want to see this data as a continuous line rather than a scatterplot of dots do the following #####
# install.packages("ggplot2") # if not already loaded, load graphics package (remove "#" at the head of this line to run the script)
library(ggplot2) # load package into current session
# options below for either column of date&time data
ggplot(sr, aes(x=date.time, y=sol.rad)) + geom_line() # plot solar radiation vs. dates & times to see the general shape of the graph
ggplot(sr, aes(x=date.timeP, y=sol.rad)) + geom_line() # plot solar radiation vs. dates & times to see the general shape of the graph

########## !!! There are differences if you are using hourly data vs. data every 15 min !!! ##########

########## Adding columns to enable identification of smooth curves (no cloud cover days) ##########
### !!! 15 minute data !!! ###
### Create a column of the decimal hours of the day ###
### Column does not have to be POSIXct data? ###
time <- strptime(sr$date.time, "%Y-%m-%d %H:%M") # splits "date.time" into individual units (years, months, etc.) and names the new object "time"
sr$hour<-                                # create new column "hour" representing each 15min timestep
     as.numeric(format(time,"%H")) +     # extract hours as a number
       as.numeric(format(time, "%M"))/60 # add minutes as a decimal to the hour
str(sr) #check to see if "hour" column has been added to the table
summary(sr)

########## Adding columns to enable identification of smooth curves (no cloud cover days) ##########
### !!! Hourly data !!! ###
### Create a column of the decimal hours of the day ###
time <- strptime(sr$date.time, "%Y-%m-%d %H:%M") # splits "date.time" into individual units (years, months, etc.) and names the new object "time"
sr$hour<-                         # create new column "hour" representing each 15min timestep
	as.numeric(format(time,"%H")) # extract hours as a number
#check to see if "hour" column has been added to the table
str(sr)
summary(sr)

### Create a column of the days of the year as a factor (a name rather than a number) ###
sr$day <- as.factor(format(time,"%Y%m%d")) # create new column "day" representing each unique date
#check to see if "day" column has been added to the table
str(sr)
summary(sr)

### Identify the peak time of solar radation ### 
peak<-aggregate(sr$sol.rad~sr$hour,FUN=mean); colnames(peak)<-c("hr","rad") # average all data by hour of the day
peakhr<-peak[peak$rad==max(peak$rad),1]; peakhr # identify time of day for peak solar radiation

### Create column to center data analysis around peak radiation timestep ###
sr$center<-sr$hour-peakhr # creates a column with a numeric value of "0" at the peak hour, with positive or negative values extending on either side of the "0"
sr$center<-ifelse(sr$center<(-12),sr$center+24,sr$center) # balances the number of positives and negatives on either side of "0".  (For use with time data not recorded as local time, e.g. GMT)
str(sr)  # check to see if new column has been added
summary(sr)

#### Remove low values for "sol.rad" - non-daylight hours ####
## Because there is variation in the length of daylight due to season of the year, unnecessary 
## variation would be added to the quadratic regression analysis.  Limiting the data to the 
## times to when "sol.rad" > 1 elminates unneccessary low values outsided of the expected curve
## and the quadratic regression analysis can be applied evenly to each day of the year.
sr<-subset(sr, sr$sol.rad > 1) #keep only the rows where "sol.rad" > 1

### Remove outlier and cloudy days ###
## This section seeks to eliminate days when the solar radiation is too high (an assumed error
## during data collection) or is too low due to cloud cover.  Selection is based on the 
## relationship between the maximum value of any given day and the maximum values of the days
## preceeding and following that day.

days<-do.call(rbind, lapply(unique(sr$day), function(x) {
  frame <- model.frame(sol.rad ~ poly(center, 2, raw = TRUE), data = sr[sr$day==x,])
  qr <- lm(sol.rad ~ ., data = frame)
  m <- aggregate(sr$sol.rad ~ sr$day==x, FUN=max, raw = TRUE)
  data.frame(date = x, maximum = m, r_sqrd = summary(qr)$adj.r.squared)
}))

days <- subset(days, days [,2]==TRUE)

days <- days[,c(1,3,4)]
colnames(days)<-c("date","max","r_sqrd")

write.csv(days,"days.csv",row.names=F)

sr$out <- with(sr, ave(sol.rad, day, FUN=max) )
summary(sr)

reg<-do.call(rbind, lapply(sr$day, function(x) {
  frame <- model.frame(sol.rad ~ poly(center, 2, raw = TRUE), data = sr[sr$day==x,])
  qr <- lm(sol.rad ~ ., data = frame)
  data.frame(r_sqrd = summary(qr)$adj.r.squared)
}))

sr<-cbind(sr,reg)

write.csv(sr,"done.csv",row.names=F)

sr1<-subset(sr,sr$r_sqrd>.96)
ggplot(sr1, aes(x=date.time2, y=sol.rad)) + geom_line() # plot solar radiation vs. dates & times to see the general shape of the graph

# lapply recursive fit lm() and put date and adjusted R sqared in a data frame
# do.call bind them
cv<-do.call(rbind, lapply(unique(dayscv$date), function(x) {
  frame <- model.frame(Sol.Rad ~ poly(center, 2, raw = TRUE), data = dayscv[dayscv$date==x,])
  qr <- lm(Sol.Rad ~ ., data = frame)
  data.frame(date = x, r_sqrd = summary(qr)$adj.r.squared)
}))
cv

dayssm<-read.csv("C:/Users/davebetts/Desktop/smith.csv", header=T)
str(dayssm)

# lapply recursive fit lm() and put date and adjusted R sqared in a data frame
# do.call bind them
sm<-do.call(rbind, lapply(unique(dayssm$date), function(x) {
  frame <- model.frame(Sol.Rad ~ poly(center, 2, raw = TRUE), data = dayssm[dayssm$date==x,])
  qr <- lm(Sol.Rad ~ ., data = frame)
  data.frame(date = x, r_sqrd = summary(qr)$adj.r.squared)
}))
sm

dayssm2<-read.csv("C:/Users/davebetts/Desktop/smith2.csv", header=T)
str(dayssm2)

# lapply recursive fit lm() and put date and adjusted R sqared in a data frame
# do.call bind them
sm2<-do.call(rbind, lapply(unique(dayssm2$date), function(x) {
  frame <- model.frame(Sol.Rad ~ poly(center, 2, raw = TRUE), data = dayssm2[dayssm2$date==x,])
  qr <- lm(Sol.Rad ~ ., data = frame)
  data.frame(date = x, r_sqrd = summary(qr)$adj.r.squared)
}))
sm2

dayscv2<-read.csv("C:/Users/davebetts/Desktop/cvtrial2.csv", header=T)
str(dayscv2)

# lapply recursive fit lm() and put date and adjusted R sqared in a data frame
# do.call bind them
cv2<-do.call(rbind, lapply(unique(dayscv2$date), function(x) {
  frame <- model.frame(Sol.Rad ~ poly(center, 2, raw = TRUE), data = dayscv2[dayscv2$date==x,])
  qr <- lm(Sol.Rad ~ ., data = frame)
  data.frame(date = x, r_sqrd = summary(qr)$adj.r.squared)
}))
cv2
DateTime1<-c("13/10/23 07:00", "13/10/23 08:00", "13/10/23 09:00", "13/10/23 10:00", "13/10/23 11:00", "13/10/23 12:00", "13/10/23 13:00", "13/10/23 14:00", "13/10/23 15:00", "13/10/23 16:00", "13/10/23 17:00", "13/10/23 18:00", "13/10/23 19:00")
Sol.Rad1<-c(0, 68.78761823, 214.961307, 369.733448, 498.7102322, 576.0963027, 601.8916595, 541.7024936, 447.1195185, 352.5365434, 189.1659501, 8.598452279, 0)
DateTime2<-c("13/10/24 07:00", "13/10/24 08:00", "13/10/24 09:00", "13/10/24 10:00", "13/10/24 11:00", "13/10/24 12:00", "13/10/24 13:00", "13/10/24 14:00", "13/10/24 15:00", "13/10/24 16:00", "13/10/24 17:00", "13/10/24 18:00", "13/10/24 19:00")
Sol.Rad2<-c(0, 68.78761823, 214.961307, 369.733448, 498.7102322, 309.544282, 576.0963027, 386.9303525, 464.316423, 326.7411866, 167.6698194, 8.598452279, 0)
Centered<-c( -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6)
day1<-data.frame(DateTime1,Centered,Sol.Rad1)
day2<-data.frame(DateTime2,Centered,Sol.Rad2)

plot(day1$Sol.Rad1 ~ day1$Centered)
plot(day2$Sol.Rad2 ~ day2$Centered)

qr1<- lm(day1$Sol.Rad ~ poly(day1$Centered, 2, raw=TRUE))
summary(qr1)
qr2<- lm(day2$Sol.Rad ~ poly(day2$Centered, 2, raw=TRUE))
summary(qr2)

dayscv<-read.csv("C:/Users/davebetts/Desktop/cvtrial.csv", header=T)
str(dayscv)

# convert into date
dayscv$date <- as.Date(dayscv$date, "%y/%m/%d")
unique(dayscv$date)
#[1] "2013-10-23" "2013-10-24"

# lapply recursive fit lm() and put date and adjusted R sqared in a data frame
# do.call bind them
cv<-do.call(rbind, lapply(unique(dayscv$date), function(x) {
  frame <- model.frame(Sol.Rad ~ poly(center, 2, raw = TRUE), data = dayscv[dayscv$date==x,])
  qr <- lm(Sol.Rad ~ ., data = frame)
  data.frame(date = x, r_sqrd = summary(qr)$adj.r.squared)
}))
cv

dayssm<-read.csv("C:/Users/davebetts/Desktop/smith.csv", header=T)
str(dayssm)

# lapply recursive fit lm() and put date and adjusted R sqared in a data frame
# do.call bind them
sm<-do.call(rbind, lapply(unique(dayssm$date), function(x) {
  frame <- model.frame(Sol.Rad ~ poly(center, 2, raw = TRUE), data = dayssm[dayssm$date==x,])
  qr <- lm(Sol.Rad ~ ., data = frame)
  data.frame(date = x, r_sqrd = summary(qr)$adj.r.squared)
}))
sm

dayssm2<-read.csv("C:/Users/davebetts/Desktop/smith2.csv", header=T)
str(dayssm2)

# lapply recursive fit lm() and put date and adjusted R sqared in a data frame
# do.call bind them
sm2<-do.call(rbind, lapply(unique(dayssm2$date), function(x) {
  frame <- model.frame(Sol.Rad ~ poly(center, 2, raw = TRUE), data = dayssm2[dayssm2$date==x,])
  qr <- lm(Sol.Rad ~ ., data = frame)
  data.frame(date = x, r_sqrd = summary(qr)$adj.r.squared)
}))
sm2

dayscv2<-read.csv("C:/Users/davebetts/Desktop/cvtrial2.csv", header=T)
str(dayscv2)

# lapply recursive fit lm() and put date and adjusted R sqared in a data frame
# do.call bind them
cv2<-do.call(rbind, lapply(unique(dayscv2$date), function(x) {
  frame <- model.frame(Sol.Rad ~ poly(center, 2, raw = TRUE), data = dayscv2[dayscv2$date==x,])
  qr <- lm(Sol.Rad ~ ., data = frame)
  data.frame(date = x, r_sqrd = summary(qr)$adj.r.squared)
}))
cv2



#############################################################################################
###                              Analysis by days - Mendon                                ###
###              Objective: find the mean, maximum and minimum values per day             ###
#############################################################################################
install.packages(xts) # installs a package of commands and code that is not part of the base R installation
# once a package is installed on a computer, the package will remain until R is uninstalled from the computer
# this package allows R to perform calculations on data that R can read as POSIXct dates & times
library(xts) # loads previously installed package for the entire R session
mT.xts<-xts(x=sr$Mendon,as.POSIXct(sr$date)) #builds a relationship between the data values & dates

# Averaging time series data in R
# Time period options: “us” (microseconds), “microseconds”, “ms” (milliseconds), “milliseconds”, 
# “secs” (seconds), “seconds”, “mins” (minutes), “minutes”, “hours”, “days”, “weeks”, “months”, 
# “quarters”, and “years”
mTimeFrame <- endpoints(mT.xts,'days')  # sets the time frame to "days" for analyzing temperature data

# create new vectors (columns of data) based on analyses within the selected time frames
minm = as.matrix(period.apply(mT.xts,INDEX = mTimeFrame, FUN =min)) # minimum value per day
meanm = as.matrix(period.apply(mT.xts,INDEX = mTimeFrame, FUN =mean)) # mean value per day
maxm = as.matrix(period.apply(mT.xts,INDEX = mTimeFrame, FUN =max)) # maximum value per day

mT<-cbind(minm,meanm,maxm) # bind the calculated vectors into a single object "mT"
summary(mT) # display summary of combined vectors
mT<-data.frame(                         # converts object "mT" to a data.frame still known as "mT"
  as.POSIXct(rownames(mT),tz=""),mT)    # converts the row.names to a column of POSIXct data
summary(mT) # display summary of results
colnames(mT)<-c("Date","Min.temp","Mean.temp","Max.temp") #specify column names for data.frame "mT"
summary(mT) # display summary of results

### write a new CSV file containing columns for the dates (days), and minimum, mean and maximum temperatures 
write.csv(mT,"C:/Users/davebetts/Dropbox/PhD/Coursework/WaterQuality&Pollution/SmallGroupProjects/Mendon_temp_data.csv",row.names=F,na="")

#############################################################################################
###                              Paradise temperature data                                ###
###                                Water years: 2010-2012                                 ###
#############################################################################################
##
## The initial data preparation of the CSV file was done in Excel to delete unwanted
## columns and convert dates to a format that the R packages could interpret as a date.
##
## * downloaded continuous temperature data "USU-LBR-Paradise-USU10.csv" 
## * deleted all columns in CSV except "DataValue" and "LocalDateTime"
## * changed column names to "Paradise" and "date"
## * deleted all dates except from 10/1/2009 to 9/30/2012 within csv file, and saving as "Tp.csv"
## * modified the "date" column within the CSV to be readable by POSIXct functions by selecting the
##   custom number formatting and entering the following format in the space below "Type:": yyyy-m-d h:mm

