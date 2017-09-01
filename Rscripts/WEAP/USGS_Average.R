rm(list=ls())
gc()

# set working directory to the location of the required files
setwd("C:/Users/davebetts/Dropbox/PhD/WEAP/stream gauges/ForCalibration")
setwd("C:/Users/davebetts/Dropbox/PhD/WEAP/stream gauges")
setwd("C:/Users/Sarah/Documents/Dropbox/PhD/WEAP/stream gauges")
setwd("C:/Users/Sarah/Documents/Dropbox/PhD/WEAP/Data/Data_Processed/stream gauges")
setwd("C:/Users/davebetts/Dropbox/PhD/WEAP/Data/Data_Processed/stream gauges")
dir()


############################################################################################
###                                                                                      ###
###                  Climate change impacts along the Wasatch front                      ###
###       (The Bear River, Weber River and "Provo - Jordan" River watersheds)            ###
###                       A stream flow model using WEAP                                 ###
###                                                                                      ###
###                          David J. Betts - July 2014                                  ###
###                        Utah State University & iUTAH                                 ###
###                                                                                      ###
###           Processing USGS stream gauge data and WEAP output for comparison           ###
###      USGS stream data retrieved from: http://waterdata.usgs.gov/nwis/inventory       ###
###                Required files for this script: "all_gaugedata.xlsx",                 ###
###                "gauges_WEAPNodes.csv", "WEAP_output_m3_7_10_15.csv"                  ###
###                                                                                      ###
############################################################################################
## !!! All erroneous stream flow observations should have been removed from the reported!!!
## !!! data prior to processign with this script.  All quality control of stream gauge  !!!
## !!! data should be done prior to this point!!!
## 
## The quality control done for USGS's daily average flows seems sufficient.  USGS gives 
## estimates in some cases, such as when stream gauges were impaired by icy conditions. 
##
## For the USGS stream gauge data, the necessary data columns are: USGS station ID, Date, 
## Daily average flow (cfs).  The data processed with this script used Excel's default 
## format for dates: "mm/dd/yyyy".
## 
## Column headers ("ID", "Date", "cfs")  should be in the first row in each worksheet. 
## Remove the additional rows reported by USGS before using this script.
## 
## Additional stream gauge data can be added through the addition of new sheets within the
## "all_gaugedata.xlsx" workbook, and by adding a new row to "gauges_WEAPNodes.csv" which
## identifies the WEAP stream node that corresponds to the added stream gauge data.  
##
## Alternative scenarios and additional outputs from updates to the WEAP model can be 
## processed with this script.  Replace "WEAP_output_m3_7_10_15.csv" with the file name of 
## the updated or additional data.  For saving the processed data, new file names should be
## for the scenarios in WEAP.  Updates to the WEAP model (e.g. calibration) should use the 
## file names as they are below.
##
## Additional information regarding the files for the Bear-Weber-Jordan Climate model 
## can be found in "FileDescriptions.txt"
##
##

############################################################################################
###                                                                                     ###
###          Total Monthy Flow estimated from observed daily averages (USGS)            ###
###            for comparison with the expected total monthly flow (WEAP)               ###
###                                                                                     ###
###########################################################################################
##
## Required files and objects: "mysheets", "gauge_IDs", 
##
## Daily average flow (cfs) for each day within a given YearMonth are averaged and then
## multiplied by the number of days in that month to estimate the total monlthly flow in 
## cubic feet.  Estimates are created for every month that reports at least one 
## satisfactory daily average flow for each stream gauge.  The script then converts cubic 
## feet per month to cubic meters per month to correspond with the output from the WEAP 
## model.
##
##

# Total flow per month function
library(lubridate)
Tot_flow_month <- vector(mode = "list", length = length(mysheets))
for (i in seq_along(mysheets)) {
# require(zoo) # no longer needed
  require(lubridate) # required for "days_in_month()"
  mysheets[[i]][,c("YearMonth")] = as.factor(format(mysheets[[i]][,"Date"],"%Y%m"))
  mysheets[[i]][,c("Days")] = days_in_month(mysheets[[i]][,"Date"])
  TotFlow = aggregate(mysheets[[i]][,4] ~ mysheets[[i]][,c("YearMonth")] + 
                        mysheets[[i]][,c("Days")], FUN = mean, na.omit = TRUE)
  colnames(TotFlow) = c("YearMonth", "Days", "AvgCFS")
  # Convert average cfs per day to total cubic feet for that month
  spd = 24*60*60 # Seconds per day (hours * minutes * seconds)
  TotFlow$TotCF = TotFlow$AvgCFS*TotFlow$Days*spd # days per month * seconds per day
  #Convert cubic feet per second to cubic meters per second
  cm_cf = 0.0283168464 # cubic meters per cubic foot
  TotFlow$AvgCMS = TotFlow$AvgCFS*cm_cf # Avg cfs * cubic meters per cubic feet
  # Convert average CMS per day to total cubic meters for that month
  TotFlow$TotCM = TotFlow$AvgCMS*TotFlow$Days*spd # days per month * seconds per day
  TotFlow$LogAvgCMS = log10(TotFlow$AvgCMS)
  TotFlow$LogTotCM = log10(TotFlow$TotCM)
  x = mysheets[[i]][1,2] # create a label from the gauge ID
  ID = as.vector(rep(x, each = nrow(TotFlow)), mode = "character") # vector for ID label
  Tot_flow_month[[i]] = cbind(ID, TotFlow[,c(-2)]) # Bind ID vector to data row
  names(Tot_flow_month)[i] <- ID  # label table i with stream gauge ID [i]
}

# Output from selected tables
# Are the tables in the list "Tot_flow_month" are named with appropriate gauge ID? 
# Values in the column "ID" should match the individual table number (files[i])
#files <- c(names(Tot_flow_month))
#print(c("Gauge #:",files[1])); head(Tot_flow_month[[1]])
#print(c("Gauge #:",files[45])); head(Tot_flow_month[[45]])
#print(c("Gauge #:",files[33])); head(Tot_flow_month[[33]])
#str(Tot_flow_month[[22]])

# Save each sheet of the imported list as an individual CSV file
##files <- c(names(Tot_flow_month))
##lapply(seq_along(Tot_flow_month), function(i){
##  write.csv(Tot_flow_month[[i]], paste("tot",files[i],sep = ""), row.names = FALSE)
##})

# Combine each table in the list "Tot_flow_month" into a single table for comparison with
# modeled output from WEAP
singlesheet <- data.frame(YearMonth = character())
# for each table (i) in "Tot_flow_month"
for (i in seq_along(Tot_flow_month)) {
  # include only the columns for the date ("YearMonth") and total cubic meters ("TotCM")
  # ss = as.data.frame(Tot_flow_month[[i]][,c("YearMonth","TotCM")]) 
  ss = as.data.frame(Tot_flow_month[[i]][,c("YearMonth","LogTotCM")]) 
  # merge each table (i) with the pervious tables, keeping all rows
  singlesheet = merge(singlesheet,ss, by.x = "YearMonth", by.y = "YearMonth", all = TRUE)
}

colnames(singlesheet)<-c("YearMonth",names(Tot_flow_month)) # rename columns
# singlesheet <- singlesheet[,c(colnames(singlesheet) %in% gauge_IDs)] # this line only needed if I can't subset "mysheets" from "fullsheets"
singlesheet <- singlesheet[,order(names(singlesheet))] # sort columns alphabetically 
# move column "YearMonth" to be column 1
singlesheet <- cbind(singlesheet$YearMonth,
                     singlesheet[ , -which(names(singlesheet) %in% c("YearMonth"))])
colnames(singlesheet)[1] <- "YearMonth" # rename column 1

# write all gauge data in total cubic meters per month as CSV
write.csv(singlesheet, "TotalFlow_Month_all.csv", row.names = FALSE, na = "")
# do log version of this eventually
# write.csv(singlesheet, "TotalFlow_Month_all.csv", row.names = FALSE, na = "")

### !!! WEAP Output does not match water years !!!

## Subset the stream gauge data to the WEAP model timeframe: 1985 - 2005 water years
# add a numeric index based on "singlesheet$YearMonth" for subsetting
singlesheet$Index <- as.numeric(as.character(singlesheet$YearMonth))
MY <- singlesheet[singlesheet$Index < 200601,] # remove rows after Sept.2006 <-- !!! fix???
MY <- MY[MY$Index > 198412,] # before Jan.2005 <-- !!! fix???
MY$Index <- NULL
modelyears <- MY

# write output of gauge data within timeframe of ??? scenario in WEAP model
write.csv(modelyears, "myTotalFlow_Month_all.csv", row.names = FALSE)

