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

## Read stream gauge data from xlsx workbook 
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
#str(mysheets[[33]])

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
