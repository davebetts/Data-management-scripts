# Direct R to use the folder where the CSV files of my data has been saved
# make sure the "/" slants in the right direction in file names
setwd("C:/Users/davebetts/Dropbox/PhD/Coursework/WaterQuality&Pollution/SmallGroupProjects") 

#############################################################################################
###       Analysis of High Frequency Temperature Data on the Little Bear River            ###
###                 3 sites: South Fork, Paradise, Mendon Road                            ###
###          All data from: http://littlebearriver.usu.edu/sites/Default.aspx             ###
###                         David J. Betts - December 2014                                ###
###                     Water Quality & Pollution (WATS 4530/6530)                        ###   
#############################################################################################

#############################################################################################
###                               Mendon temperature data                                 ###
###                                Water years: 2010-2012                                 ###
#############################################################################################
##
## The initial data preparation of the CSV file was done in Excel to delete unwanted
## columns and convert dates to a format that the R packages could interpret as a date.
##
## * downloaded continuous temperature data "USU-LBR-Mendon-USU10.csv" 
## * deleted all columns in CSV except "DataValue" and "LocalDateTime"
## * changed column names to "Mendon" and "date"
## * deleted all dates except from 10/1/2009 to 9/30/2012 within csv file, and saving as "Tm.csv"
## * modified the "date" column within the CSV to be readable by POSIXct functions by selecting the
##   custom number formatting and entering the following format in the space below "Type:": yyyy-m-d h:mm

Tm<-read.csv("Tm.csv",header=T) #read the previously modified CSV to create a data.frame object "Tm"

## checking data for basic structure and components
dim(Tm) #data.frame dimensions
str(Tm) #data.frame structure (labels and data types)
summary(Tm) #column summaries to look at maximum, minimum, and the number of NA(null) values
# checking for errors in CSV file (e.g too many or too few rows, ragged edges of the data)
head(Tm) # first 6 rows of the data.frame
tail(Tm) # last 6 rows of the data.frame

## Graphing the temp vs. date to check for outliers and other patterns that may indicate erroneous data
Tm$date2=as.POSIXct(Tm$date) # creates and pastes a third column by converting text to POSIXct dates
summary(Tm) #check to see if "Tm" has added the third column, and that R reads the new column dates & times
plot(Tm$Mendon~Tm$date2) # plot temperatures vs. dates & times to check for potential outliers

# I repeated the process of plotting the data, deleting the indicated outliers within the CSV using Excel
# and reloading the data using the previous steps. (I need to find a way to automatically identify the 
# outliers or apply a rule to select and modify / delete the outliers.)
# Cell values of 0 were used to identify potential outliers. In some cases only the 0 was eliminated, while
# in other cases, the 0 and some of the data entries immediately surrounding values did not match the trends
# or expected values for that date & time. In some cases large strings of repeated values were deleted.
# Using the CV file to search for outliers, I created a third column with the following formula in C4:
# =ABS(A4-A3)+ABS(A4-A2)+ABS(A4-A5)+ABS(A4-A6) <- this compares the sum of the absolute values for the 
# differences between A4 and the 2 cells prior and the 2 cells after A4. copying and pasting this formula 
# through the end of the column (minus the last 2 cells) helps identify cells that are the most different 
# from the surrounding cells.  Copying these values and pasting them as plain text into a 4th column allowed
# me to search the new column for maximum and minimum values that would indicate high deviation from surrounding
# trends.  (Figure out a way to do this in R as well.)
# Assumed errors identified in this manner were deleted rather than interpolating between time steps.

#############################################################################################
###                              Analysis by days - Mendon                                ###
###              Objective: find the mean, maximum and minimum values per day             ###
#############################################################################################
install.packages(xts) # installs a package of commands and code that is not part of the base R installation
# once a package is installed on a computer, the package will remain until R is uninstalled from the computer
# this package allows R to perform calculations on data that R can read as POSIXct dates & times
library(xts) # loads previously installed package for the entire R session
mT.xts<-xts(x=Tm$Mendon,as.POSIXct(Tm$date)) #builds a relationship between the data values & dates

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

Tp<-read.csv("Tp.csv",header=T) #read the previously modified CSV to create a data.frame object "Tp"

## checking data for basic structure and components
dim(Tp) #data.frame dimensions
str(Tp) #data.frame structure (labels and data types)
summary(Tp) #column summaries to look at maximum, minimum, and the number of NA(null) values
# checking for errors in CSV file (e.g too many or too few rows, h)
head(Tp) # first 6 rows of the data.frame
tail(Tp) # last 6 rows of the data.frame

## Graphing the temp vs. date to check for outliers and other patterns that may indicate erroneous data
Tp$date2=as.POSIXct(Tp$date) # creates and pastes a third column by converting text to POSIXct dates
summary(Tp) #check to see if "Tp" has added the third column, and that R reads the new column dates & times
plot(Tp$Paradise~Tp$date2) # plot temperatures vs. dates & times to check for potential outliers

# I repeated the process of plotting the data, deleting the indicated outliers within the CSV using Excel
# and reloading the data using the previous steps. (I need to find a way to automatically identify the 
# outliers or apply a rule to select and modify / delete the outliers.)
# Cell values of 0 were used to identify potential outliers. In some cases only the 0 was eliminated, while
# in other cases, the 0 and some of the data entries immediately surrounding values did not match the trends
# or expected values for that date & time. In some cases large strings of repeated values were deleted.
# Using the CV file to search for outliers, I created a third column with the following formula in C4:
# =ABS(A4-A3)+ABS(A4-A2)+ABS(A4-A5)+ABS(A4-A6) <- this compares the sum of the absolute values for the 
# differences between A4 and the 2 cells prior and the 2 cells after A4. copying and pasting this formula 
# through the end of the column (minus the last 2 cells) helps identify cells that are the most different 
# from the surrounding cells.  Copying these values and pasting them as plain text into a 4th column allowed
# me to search the new column for maximum and minimum values that would indicate high deviation from surrounding
# trends.  (Figure out a way to do this in R as well.)
# Assumed errors identified in this manner were deleted rather than interpolating between time steps.

#############################################################################################
###                               Analysis by days - Paradise                             ###
###              Objective: find the mean, maximum and minimum values per day             ###
#############################################################################################
install.packages(xts) # installs a package of commands and code that is not part of the base R installation
# once a package is installed on a computer, the package will remain until R is uninstalled from the computer
# this package allows R to perform calculations on data that R can read as POSIXct dates & times
library(xts) # loads previously installed package for the entire R session
pT.xts<-xts(x=Tp$Paradise,as.POSIXct(Tp$date)) #builds a relationship between the data values & dates

# Averaging time series data in R
# Time period options: “us” (microseconds), “microseconds”, “ms” (milliseconds), “milliseconds”, 
# “secs” (seconds), “seconds”, “mins” (minutes), “minutes”, “hours”, “days”, “weeks”, “months”, 
# “quarters”, and “years”
pTimeFrame <- endpoints(pT.xts,'days')  # sets the time frame to "days" for analyzing temperature data

# create new vectors (columns of data) based on analyses within the selected time frames
minm = as.matrix(period.apply(pT.xts,INDEX = pTimeFrame, FUN =min)) # minimum value per day
meanm = as.matrix(period.apply(pT.xts,INDEX = pTimeFrame, FUN =mean)) # mean value per day
maxm = as.matrix(period.apply(pT.xts,INDEX = pTimeFrame, FUN =max)) # maximum value per day

pT<-cbind(minm,meanm,maxm) # bind the calculated vectors into a single object "pT"
summary(pT) # display summary of combined vectors
pT<-data.frame(                         # converts object "pT" to a data.frame still known as "pT"
  as.POSIXct(rownames(pT),tz=""),pT)    # converts the row.names to a column of POSIXct (date & time) data
summary(pT) # display summary of results
colnames(pT)<-c("Date","Min.temp","Mean.temp","Max.temp") #specify column names for data.frame "pT"
summary(pT) # display summary of results

### write a new CSV file containing columns for the dates (days), and minimum, mean and maximum temperatures 
write.csv(pT,"C:/Users/davebetts/Dropbox/PhD/Coursework/WaterQuality&Pollution/SmallGroupProjects/Paradise_temp_data.csv",row.names=F,na="")

#############################################################################################
###                              SouthFork temperature data                               ###
###                                Water years: 2010-2012                                 ###
#############################################################################################
##
## The initial data preparation of the CSV file was done in Excel to delete unwanted
## columns and convert dates to a format that the R packages could interpret as a date.
##
## * downloaded continuous temperature data "USU-LBR-SouthFork-USU10.csv" 
## * deleted all columns in CSV except "DataValue" and "LocalDateTime"
## * changed column names to "SouthFork" and "date"
## * deleted all dates except from 10/1/2009 to 9/30/2012 within csv file, and saving as "Ts.csv"
## * modified the "date" column within the CSV to be readable by POSIXct functions by selecting the
##   custom number formatting and entering the following format in the space below "Type:": yyyy-m-d h:mm

Ts<-read.csv("Ts.csv",header=T) #read the previously modified CSV to create a data.frame object "Ts"

## checking data for basic structure and components
dim(Ts) #data.frame dimensions
str(Ts) #data.frame structure (labels and data types)
summary(Ts) #column summaries to look at maximum, minimum, and the number of NA(null) values
# checking for errors in CSV file (e.g too many or too few rows, h)
head(Ts) # first 6 rows of the data.frame
tail(Ts) # last 6 rows of the data.frame

## Graphing the temp vs. date to check for outliers and other patterns that may indicate erroneous data
Ts$date2=as.POSIXct(Ts$date) # creates and pastes a third column by converting text to POSIXct dates
summary(Ts) #check to see if "Ts" has added the third column, and that R reads the new column dates & times
plot(Ts$SouthFork~Ts$date2) # plot temperatures vs. dates & times to check for potential outliers

# I repeated the process of plotting the data, deleting the indicated outliers within the CSV using Excel
# and reloading the data using the previous steps. (I need to find a way to automatically identify the 
# outliers or apply a rule to select and modify / delete the outliers.)
# Cell values of 0 were used to identify potential outliers. In some cases only the 0 was eliminated, while
# in other cases, the 0 and some of the data entries immediately surrounding values did not match the trends
# or expected values for that date & time. In some cases large strings of repeated values were deleted.
# Using the CV file to search for outliers, I created a third column with the following formula in C4:
# =ABS(A4-A3)+ABS(A4-A2)+ABS(A4-A5)+ABS(A4-A6) <- this compares the sum of the absolute values for the 
# differences between A4 and the 2 cells prior and the 2 cells after A4. copying and pasting this formula 
# through the end of the column (minus the last 2 cells) helps identify cells that are the most different 
# from the surrounding cells.  Copying these values and pasting them as plain text into a 4th column allowed
# me to search the new column for maximum and minimum values that would indicate high deviation from surrounding
# trends.  (Figure out a way to do this in R as well.)
# Assumed errors identified in this manner were deleted rather than interpolating between time steps.

#############################################################################################
###                               Analysis by days - SouthFork                            ###
###              Objective: find the mean, maximum and minimum values per day             ###
#############################################################################################
install.packages(xts) # installs a package of commands and code that is not part of the base R installation
# once a package is installed on a computer, the package will remain until R is uninstalled from the computer
# this package allows R to perform calculations on data that R can read as POSIXct dates & times
library(xts) # loads previously installed package for the entire R session
sT.xts<-xts(x=Ts$SouthFork,as.POSIXct(Ts$date)) #builds a relationship between the data values & dates

# Averaging time series data in R
# Time period options: “us” (microseconds), “microseconds”, “ms” (milliseconds), “milliseconds”, 
# “secs” (seconds), “seconds”, “mins” (minutes), “minutes”, “hours”, “days”, “weeks”, “months”, 
# “quarters”, and “years”
sTimeFrame <- endpoints(sT.xts,'days')  # sets the time frame to "days" for analyzing temperature data

# create new vectors (columns of data) based on analyses within the selected time frames
minm = as.matrix(period.apply(sT.xts,INDEX = sTimeFrame, FUN =min)) # minimum value per day
meanm = as.matrix(period.apply(sT.xts,INDEX = sTimeFrame, FUN =mean)) # mean value per day
maxm = as.matrix(period.apply(sT.xts,INDEX = sTimeFrame, FUN =max)) # maximum value per day

sT<-cbind(minm,meanm,maxm) # bind the calculated vectors into a single object "sT"
summary(sT) # display summary of combined vectors
sT<-data.frame(           # converts object "sT" to a data.frame still known as "sT"
  as.POSIXct(rownames(sT),tz=""),sT)    # converts the row.names to a column of POSIXct (date & time) data
summary(sT) # display summary of results
colnames(sT)<-c("Date","Min.temp","Mean.temp","Max.temp") #specify column names for data.frame "sT"
summary(sT) # display summary of results

### write a new CSV file containing columns for the dates (days), and minimum, mean and maximum temperatures 
write.csv(sT,"C:/Users/davebetts/Dropbox/PhD/Coursework/WaterQuality&Pollution/SmallGroupProjects/SouthFork_temp_data.csv",row.names=F,na="")

#############################################################################################
###                Direct Comparison of Temperature Data at 3 sites:                      ###
###                        South Fork, Paradise, Mendon Road                              ###
###          data objects mT, pT and sT from previous sections are required               ###
#############################################################################################

########################################################################################
###         Combine mean temperature data for each site into a single table          ###
###                                     ---                                          ###
###         This table can be used to simultaneously plot temperatures               ###
###                at all three sites for the entire time frame                      ###
########################################################################################

summary(mT) # making sure object mT is still an active object
summary(pT) # making sure object pT is still an active object
summary(sT) # making sure object sT is still an active object

temp<-merge(sT,pT, by=c("Date","Date"),all=T) # merge all rows of sT and pT by "Date"
summary(temp) #display summary of results
temp<-merge(temp,mT,by=c("Date","Date"),all=T) # merge all rows of temp with mT by "Date"
summary(temp)  #display summary of results to determine which columns to keep (see next line)
temp<-temp[,c(1,3,6,9)] # keep only columns of the "Date" (1) and mean daily temperatures for each site (3,6,9)
summary(temp)  #display summary of results
colnames(temp)<-c("date","SouthFork","Paradise","Mendon") #relabel columns 
summary(temp)  #display summary of results

# create CSV file from the new table "temp" of combined average daily temperatures
write.csv(temp,"C:/Users/davebetts/Dropbox/PhD/Coursework/WaterQuality&Pollution/SmallGroupProjects/temp3yr.csv",row.names=F,na="")

########################################################################################
###               Comparison of mean temperatures by day of year                     ###
###                                     ---                                          ###
###               All dates are converted to a decimal format:                       ###
###                       Jan 1st = 0; December 31st = 1                             ###
###                                                                                  ###   
###    All dates are converted within the 0-1 scale, regardless of original year     ###
###      The resulting table can be used to simultaneously plot temperatures         ###
###    representing the average temperature of a given day within a single year      ###
###         (Before plotting in Excel, convert decimals back to dates                ###
###               to be able to use month names in axis labels)                      ###
########################################################################################

# "temp" from the previous section is required
temp$decimal.date<-(      # create a new column expressing the date as a decimal
  ((as.numeric(format(temp$date,format="%m")))-1)/12)+   #based on the month
  (((as.numeric(format(temp$date,format="%d")))-1)/365)  #and day, but excluding the year
summary(temp) # check if new column has been added to the table "temp"

# average temperature values for Mendon based on day of the year (decimal.date)
mavg<-aggregate(temp$Mendon,list(temp$decimal.date),mean,na.rm=T); colnames(mavg)=c("date","Mendon") 
summary(mavg) #display summary of results

# average temperature values for Paradise based on day of the year (decimal.date)
pavg<-aggregate(temp$Paradise,list(temp$decimal.date),mean,na.rm=T); colnames(pavg)=c("date","Paradise")
summary(pavg) # display summary of results

# average temperature values for SouthFork based on day of the year (decimal.date)
savg<-aggregate(temp$SouthFork,list(temp$decimal.date),mean,na.rm=T); colnames(savg)=c("date","SouthFork")
summary(savg) # display summary of results

Tavg<-cbind(savg,pavg,mavg) # combine averaged data from all three sites into a single table "Tavg"
summary(Tavg) # display summary of results
temp.avg<-Tavg[,c(1:2,4,6)] # remove unnecessary columns
summary(temp.avg) # display summary of results

# write "Tavg" as a new CSV file "temp.avg"
write.csv(temp.avg,"C:/Users/davebetts/Dropbox/PhD/Coursework/WaterQuality&Pollution/SmallGroupProjects/temp_avg.csv",row.names=F,na="")
