rm(list=ls())
gc()

# set working directory to the location of the required files
setwd("C:/Users/a01987147/Dropbox/PhD/WEAP/Data/Data_Processed/stream gauges")
setwd("C:/Users/davebetts/Dropbox/PhD/WEAP/Data/Data_Processed/stream gauges")
dir()

############################################################################################
###                                                                                      ###
###                  Unregulated Surface Flows for the Wasatch Range                     ###
###       (The Bear River, Weber River and "Provo - Jordan" River watersheds)            ###
###      A Rainfall-runoff model (WEAP) of surface flows to the Great Salt Lake          ###
###                                                                                      ###
###                          David J. Betts - March 2016                                 ###
###                        Utah State University & iUTAH                                 ###
###                                                                                      ###
############################################################################################
##
############################################################################################
###                   Pairing WEAP output to USGS stream gages                          ###
###                                      and                                             ###
###                        Omitting additional WEAP output                               ###
############################################################################################
##

# WEAP output: Streams > All reaches and nodes ; All years ; All months > Export as CSV with all boxes checked: "Include table title", "Include column titles", and "ReadFromFile format"
# read file containing the modeled output of monthly total stream flow for ALL nodes within 
# the WEAP model.  Output was exported from WEAP as a CSV file.

# read the WEAP output into R by selecting the appropriate file
fn <- file.choose()
wout <- read.csv(fn, skip = 5, check.names = FALSE)
library(zoo) # allows me to combine month and year as actual date, rather than numeric factors
row.names(wout) <- as.Date(as.yearmon(paste(wout[,1], wout[,2], sep = "-"))) # change row names to date (format = "%Y-%m-%d") by combining columns containing year and month data
head(wout)

weapoutput <- as.data.frame(t(wout[,-c(1:2)])) # remove raw year and month data columns, and transpose; column names are now dates 
z <- row.names(weapoutput) # extract "reach \ node" labels from row names
x <- data.frame(do.call(rbind, strsplit(z, '\\s*[\\]\\s*'))); names(x) <- c('reach', 'site') # split "reach \ node" at "\" to form two columns of data
x$site <- gsub("\\[Cubic Meter\\]", "", x$site)
x$site <- gsub("\\[Cubic Meter\\]", "", x$site)
x$gage <- gsub("[^0-9]","",x$site)  #remove all non-numeric characters 
x$gage <- ifelse(as.numeric(x$gage) < 1000, "", x$gage)
y <- table(x$reach)[table(x$reach) > 1] # y = reaches with multiple "nodes"; identifying overlap of WEAP estimates and USGS gauges
ng_match <- row.names(y) # list of reaches that include gauges

weapoutput <- cbind(x,weapoutput)

head(weapoutput)



#####


# read in gauge data 
# what files are required?
# do i have all of the potential gauges in the xls file?
# read in WEAP data
# how do i restrict the number of nodes to be read from weap?
# dates?

# combine gauge data and WEAP output?

# read file containing USGS identification information (station ID numbers, names, 
# locations) and corresponding stream flow node names (WEAP)
gaugeNodes <- read.csv("gauges_WEAPnodes.csv")

# Merging "gaugeNodes" and "WEAPoutput" to omit the columns of data from "WEAPoutput" that 
# do not correspond to a USGS gauging station as identified within "gauges_WEAPNodes.csv"
wo_gn <- merge(gaugeNodes, weapoutput, by.x ="WEAP_Node", by.y = "Nodes") 
gauge_IDs <- wo_gn[,"USGS_ID"]  # a vector of the stream gauge IDs for labelling elsewhere

# create a table of the USGS gauge station IDs and WEAP stream nodes pairings from "wo_gn"
Nodes <- wo_gn[,c("Basin", "USGS_ID", "Site_name", "WEAP_Node", "Reach")] # save the new table as a CSV file

# create folder for output from R if folder doesn't already exist
ifelse(!dir.exists(file.path(getwd(), "output_R")), dir.create(file.path(getwd(), "output_R")), FALSE)

write.csv(Nodes,"./output_R/gauges_nodes.csv", row.names = FALSE) 

# Modify "wo_gn" to create a table with gauge ID numbers, YearMonth timesteps, and the 
# estimated outputs from WEAP
justoutput <- subset(wo_gn, 
                     select = -c(Basin, Site_name, WEAP_Node, Reach)) # extract unwanted columns
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

write.csv(OutputNodesDates, "./output_R/WEAP85to05.csv", row.names = FALSE)
