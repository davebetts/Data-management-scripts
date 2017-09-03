rm(list=ls())
gc()

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
###                   Pairing WEAP output to USGS stream gauges                          ###
###                                      and                                             ###
###                        Omitting additional WEAP output                               ###
############################################################################################
##
## Required files: "gauges_WEAPNodes.csv", "WEAP_output_m3_7_10_15.csv"
##
## The number of nodes that the WEAP model reports streamflow output far outnumber the 
## available observed data from USGS.  Using the gauge locations as represented in the 
## ArcMap layer "BAR_WEB_JOR_gages_snapped_projsn", corresponding stream flow nodes were 
## identified within the WEAP model.  The nearest upstream stream flow node from a gauging 
## station was selected as the node where the modeled streamflows are most likely to be the 
## most similar to the observed data reported by USGS.
##
## The dates in the first row of "WEAP_output_m3_7_10_15.csv" were modified within Excel to 
## a custom format "yyyymm" (e.g. 200803 = March 2008) before saving the file again as a 
## CSV. This date format was easier to process within the script in order to combine the
## output from WEAP with other data.  R will add 'X' as the first character of a column 
## name that is a number, which is removed later by this script.
##
##

# read file containing USGS identification information (station ID numbers, names, ocations)
# and corresponding stream flow node names (WEAP)
gaugeNodes <- read.csv("gauges_WEAPNodes.csv")
# read file containing the modeled output of monthly total stream flow for ALL nodes within 
# the WEAP model.  Output was exported from WEAP as a CSV file.
weapoutput <- read.csv("WEAP_output_m3_7_10_15.csv") 
# Merging "gaugeNodes" and "WEAPoutput" to omit the columns of data from "WEAPoutput" that 
# do not correspond to a USGS gauging station as identified within "gauges_WEAPNodes.csv"
wo_gn <- merge(gaugeNodes, weapoutput, by.x ="WEAP_Node", by.y = "Nodes") 
gauge_IDs <- wo_gn[,"USGS_ID"]  # a vector of the stream gauge IDs for labelling elsewhere

# create a table of the USGS gauge station IDs and WEAP stream nodes pairings from "wo_gn"
Nodes <- wo_gn[,c("Basin", "USGS_ID", "Site_name", "WEAP_Node", "Start_Date", "End_Date",
                    "Latitude", "Longitude", "Lat_DD", "Long_DD", "Reach")]
# save the new table as a CSV file
write.csv(Nodes,"gauges_nodes.csv", row.names = FALSE) 

# Modify "wo_gn" to create a table with gauge ID numbers, YearMonth timesteps, and the 
# estimated outputs from WEAP
justoutput <- subset(wo_gn, 
  select = -c(Basin, Site_name, WEAP_Node, Recheck, Start_Date, # extract unwanted colums
  End_Date, Latitude, Longitude, Lat_DD, Long_DD, Reach, FID, Sum)) # extract unwanted colums
cnms <- colnames(justoutput) # creates a vector from the column names for labelling
YearMonth <- cnms[-1]    # remove the first element ("USGS_ID") from the vector "cnms", 
# leaving only the values that represent Years and Months.

# transpose the reduced WEAP output ("justoutput") so that the gauge station IDs are the 
# column names and the YearMonth labels are the row names
t_jo <- as.data.frame(t(justoutput))  # transposing the table into a new data frame
colnames(t_jo) <- gauge_IDs # write column names
# How many columns have incorrect titles? 
# length(which((names(t_jo) == t_jo[1,]) == FALSE)) # number of mismatches in column names
# which((names(t_jo) == t_jo[1,]) == FALSE) # which column names are incorrect?
OutputNodes <- subset(t_jo[-1,]) # Remove first row from "t_jo" containing gauge ID #s

OutputNodes <- OutputNodes[,order(names(OutputNodes))] # sort the columns by gauge IDs
OutputNodes1 <- sapply(OutputNodes, log10)
# OutputNodesDates <- cbind(YearMonth, OutputNodes) # attach column of YearMonth timesteps
OutputNodesDates <- cbind(YearMonth, OutputNodes1) # attach column of YearMonth timesteps

# Are the rows still in the expected order?
# Row names should match the values in the first column
#length(which((rownames(OutputNodesDates) == 
#                paste("X",OutputNodesDates[,1],sep = "")) == FALSE))
# Which which YearMonth values are incorrect
#which((rownames(OutputNodesDates) == 
#         paste("X",OutputNodesDates[,1],sep = "")) == FALSE) 

write.csv(OutputNodesDates, "WEAP85to05.csv", row.names = FALSE)

###########################################################################################
###                           Read USGS stream gauge data                               ###
###       Format: Daily average flow    -=-   Units: cubic feet per second (cfs)        ###
###       Date of retrieval: July 2013  -=-   Range: Oct 1903 to June 2013              ###
###              Data range for climate model: Oct 1985 to Sept 2006                    ###
###########################################################################################
##
## !!! All erroneous observations should have been removed prior to processing stream !!!
## !!! flow data using this script.  Observed stream flow QC should be complete before!!!
## !!! proceeding
##
## Required files and objects: "all_gaugedata.xlsx", "gauge_IDs" (created previously)
##
## The data columns necessary for analysis with this script are: USGS station ID, Date, 
## and Daily average flow (cfs).  "mm/dd/yyyy" was the date format within the Excel
## worksheets.  
## 
## Removing any additional rows before the column headers and data tables is necessary in
## order to use this script.  
## 
## Additional stream gauge data can be added through the addition of new sheets within the
## "all_gaugedata.xlsx" workbook, and by adding a new row to "gauges_WEAPNodes.csv".  The 
## new row identifies the WEAP stream node corresponds to the added stream gauge data.  
## Run the entire script to process the new data.
##
## The USGS stream gauge data was original placed in an Excell Workbook.  The data has 
## been maintiained with the xlsx format because of the simplicity of adding additional 
## data from stream gauges as well as maintain the formatting of number sand dates that 
## is sometimes lost when opening and resaving CSVs through Excel.
##
##

## Function to read stream gauge data from individual sheets in xlsx workbook 
library(readxl) # required package for the following function
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, col_types = c("text","text","date","numeric","text")))
  names(x) <- sheets
  x
}
# More gauge data was downloaded than have been correlated with Nodes within the WEAP model
# Splitting a subset of the data in "fullsheets" to the data table "mysheets" limits the 
# final CSV ("TotalFlow_Month_all.csv") to the data that corresponds to  those WEAP stream 
# flow nodes that have been paired with USGS stream gauges.  Pairing additional gauges and
# nodes within "gauges_WEAPNodes.csv" and running the entire R script again will add 
# any additional data to the final CSV ("TotalFlow_Month_all.csv"). 
#
# Warnings are expected in the next round of processing due to the string "Ice" within the 
# numeric column for cfs.  "Ice" identifies when ice has interfeared with the stream gauge.  
# ??? extracting "Ice" from the data tables.
# Other warnings of unexpected formats appear later, but have not been investigated. The 
# subsequent calculations appear to be correct.
#
# Originally the script for "fullsheets" produced the the original list of data tables.
# The modification of this first list into into the new list "mysheets" is a small 
# reduction on the ammount of data processed in R, and the code used to format any output.
#

# Import each worsheet in the Excel workbook "all_gaugedata.xlsx" as a table within the 
# list "fullsheets"  
fullsheets <- read_excel_allsheets("all_gaugedata.xlsx") 

# subset "fullsheets" to a list of the data tables that correspond to the USGS stream 
# gauges identified in "gauge_Nodes" and related objects
mysheets <- fullsheets[which(names(fullsheets) %in% gauge_IDs)] 

#ls(mysheets) # names of each table in the list "mysheets"
#class(mysheets) 

# double check output in some of the tables in the list "mysheets"
#head(mysheets[[1]])
#head(mysheets[[65]])
#head(mysheets[[33]])
#str(mysheets[[33]]) # str(mysheets) is more oput than Rstudio can display
# ??? table(fullsheets[[1]][,"cfs"])
# ??? which(fullsheets, "10011500")

# Save each sheet of the imported list as an individual CSV file
#files <- c(names(mysheets))
#lapply(seq_along(mysheets), function(i){
#write.csv(mysheets[[i]], files[i], row.names = FALSE, col.names = c("Agency","ID","Date","cfs","Quality"))
#})

# Do the tables correspond with their item names ("ID")?
#head(mysheets[[1]]); files[1]
#head(mysheets[[45]]); files[45]
#head(mysheets[[23]]); files[23]
#write.csv(mysheets[[15]],files[15],row.names = FALSE)

## not using these lines any more
## Column names in each data table have to be the same (including capitilization) in order
## to use "rbind()"
##x<-do.call(rbind, mysheets) # bind the entire list of tables into a single table
##rownames(x) <- NULL # replaces complicated row names with integers

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


############################################################################################
###                                                                                     ###
###               Goodness of fit: Observed (USGS) vs Estimated (WEAP)                  ###
###                  GOF values reported per USGS stream gauge ID                       ###
###                                                                                     ###
###########################################################################################
##
## Required files and objects: "OutputNodesDates", "myTotalFlow_Month_all.csv", 
##
##

library(hydroGOF)
# Remove YearMonth column
Est <- OutputNodesDates[,-1]
Obs <- modelyears[,-1]

OvsE <- vector(mode = "list", length = ncol(Est))
emptyset <- as.data.frame(rep(NA,20))
goflab <- c("ME", "MAE", "MSE", "RMSE", "NRMSE %", "PBIAS %", "RSR", "rSD", "NSE", "mNSE", "rNSE", "d", "md", "rd", "cp", "r", "R2", "bR2", "KGE", "VE")
rownames(emptyset) <- goflab
oenames <- colnames(Est)

library(hydroGOF)
for(i in seq_along(OvsE)) {
    if ((sum(Obs[,i],na.rm = TRUE) == 0)) {
        nxt_col <- emptyset
        OvsE[[i]] <- nxt_col
    }
    else {
        sim = Est[,i]
        obs = Obs[,i]
        nxt_col <- gof(sim = sim, obs = obs)
        OvsE[[i]] <- nxt_col
    }
}
OvsE <- do.call(cbind, OvsE)
colnames(OvsE) <- oenames

write.csv(OvsE,"GoodnessofFit.csv")

rownames(OvsE)
OvE1 <- as.data.frame(t(OvsE))
names(OvE1)
OvE1 <- OvE1[ , c("MAE", "PBIAS %", "NSE", "d")]

summary(OvE1)
y <- colnames(OvE1)
OvE1[,"PBIAS %"] <- abs(OvE1[,"PBIAS %"])
for (i in names(OvE1)) {
    x <- rank(OvE1[i])
    OvE1 <- cbind(OvE1,x)
}
colnames(OvE1) <- c(y, paste("rank",y,sep = ""))
OvE1$rank <- rowSums(OvE1[,5:8])
OvE1 <- OvE1[order(OvE1$rank, decreasing = TRUE),]

write.csv(OvE1, "ranks.csv")

#######
## n-size
#######
##
n <- sapply(modelyears, na.omit)
n_size <- as.data.frame(sapply(n, length))

write.csv(n_size, "n_size.csv")

#######
## elevations
#######
##

gaugeNodes$bins <- cut(gaugeNodes$Elevation, 
                       breaks = c(-Inf, 1400, 1600, 1800, 2000, 2200, 2400, Inf), 
                       labels = c("Below 1400", "1400 to 1600", "1600 to 1800", "1800 to 2000", "2000 to 2200", "2200 to 2400", "Above 2400"), 
                       right = FALSE)
elev_groups <- gaugeNodes[,c("USGS_ID", "Basin", "Elevation", "bins")]

names(gaugeNodes)


library(plyr)
ddply(elev_groups, "bins")

write.csv(elev_groups,"elevation_bins.csv")

##### 
# figure out what's below

Obs2 <- apply(!is.na(Obs),2,all)
Obs2 <- Obs[, colSums(is.na(Obs)) != nrow(Obs)]
notNA <- colnames(Obs2)
Est2 <- Est[,which(colnames(Est) %in% notNA)] 
fullGOF <- gof(Est,Obs3)

library(reshape)
Est3 <- melt(Est2)
Obs3 <- melt(Obs2)
fullGOF3 <- gof(Est3[,2],Obs3[,2])
write.csv(fullGOF3, "GOF_single.csv")

    OvsE2 <- gof(Est,Obs)
dim(Est2)



############################################################################################
###                                                                                     ###
###                          Plot gauging stations vs WEAP                              ###
###                                                                                     ###
###                                                                                     ###
###########################################################################################
##
## Required files and objects: "??????
##
## ???????
##

weap <- read.csv("WEAP85to05.csv")
gauges <- read.csv("myTotalFlow_Month_all.csv")

weap <- OutputNodesDates
gauges <- modelyears

plot_oe <- vector(mode = "list", length = ncol(gauges))
plot_oenames <- colnames(gauges)
# x <- modelyears[,1]
for(i in seq_along(plot_oe)) {
  plot_oe[[i]] <- cbind(gauges[,i], weap[,i])
  # plot_oe[[i]] <- cbind(x, modelyears[,i], OutputNodesDates[,i])
  colnames(plot_oe[[i]]) <- c(plot_oenames[i], "WEAP")
  # colnames(plot_oe[[i]]) <- c("YearMonth", plot_oenames[i], "WEAP")
  # plot(plot_oe[i][,2:3]~plot_oe[[i]][,1]
}
plotting <- do.call(cbind, plot_oe)

plotz <- 1:((ncol(plotting)/2))
plotz <- plotz+plotz
for (i in plotz) {
  plot.ts(plotting[,1], y = c(plotting[,i], plotting[,i+1]))
}

write.csv(plotting, "plots.csv", row.names = FALSE, na = "")
plotting <- read.csv("plots.csv", as.is = FALSE, colClasses = "numeric")
y <- names(plotting)
yz <- gsub("X","",y)
names(plotting) <- yz
plotting[is.na(plotting)] <- ""
plotting
summary(plotting)
plotting <- sapply(plotting, as.numeric)
str(plotting)

plot(plotting, screen = rep(colnames(plotting)[c(TRUE, FALSE)], each = 2), col = 1:2)


for (i in seq_along(plot))
head(plot_oe,1)
x <- cbind(modelyears[,1],OutputNodesDates[,1])
x
plo <- cbind(modelyears[,1], OutputNodesDates[,1])
plo
