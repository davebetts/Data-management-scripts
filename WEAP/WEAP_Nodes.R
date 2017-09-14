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

