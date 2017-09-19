rm(list=ls())
gc()

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
###           Pairing WEAP gauge estimates to USGS stream gauge output                   ###
###                                      and                                             ###
###                        Omitting additional WEAP output                               ###
############################################################################################
##

# WEAP output: Streams > All reaches and nodes ; All years ; All months > Export as CSV with all boxes checked: "Include table title", "Include column titles", and "ReadFromFile format"
# read file containing the modeled output of monthly total stream flow for ALL nodes within 
# the WEAP model.  Output was exported from WEAP as a CSV file.

# read the WEAP output into R by selecting the appropriate file
fn <- file.choose()

###
# on hold for now
# wout <-read.table(fn, header = FALSE, sep = ",", skip = 5) # output from WEAP using CSV format

weapoutput <- read.csv(fn, check.names = FALSE)

#####
## The next section may not be necessary.  grabbed all of the nodes by downloading a CSV of a report of "Streamflow Relative to Gauge (Absolute)" to get all of the comparison nodes.using 
# wnr <- read.table(fn, header = FALSE, sep = ",", skip = 5, nrow = 1)
# read first line to get a list of the nodes and reaches 
# wnr2 <- wnr[, -c(1,2)] # WEAP nodes and reaches = wnr; deletes first two columns which contain year and month
# wnr3 <- (unlist(c(wnr2), use.names = FALSE)) # converts row into a list; removing names
#colnames(wout)[1] <- "YearMonth"
#wout$YearMonth <- as.character(c("YearMonth", 198501, 198502, 198503, 198504, 198505, 198506, 198507, 198508, 198509, 198510, 198511, 198512, 198601, 198602, 198603, 198604, 198605, 198606, 198607, 198608, 198609, 198610, 198611, 198612, 198701, 198702, 198703, 198704, 198705, 198706, 198707, 198708, 198709, 198710, 198711, 198712, 198801, 198802, 198803, 198804, 198805, 198806, 198807, 198808, 198809, 198810, 198811, 198812, 198901, 198902, 198903, 198904, 198905, 198906, 198907, 198908, 198909, 198910, 198911, 198912, 199001, 199002, 199003, 199004, 199005, 199006, 199007, 199008, 199009, 199010, 199011, 199012, 199101, 199102, 199103, 199104, 199105, 199106, 199107, 199108, 199109, 199110, 199111, 199112, 199201, 199202, 199203, 199204, 199205, 199206, 199207, 199208, 199209, 199210, 199211, 199212, 199301, 199302, 199303, 199304, 199305, 199306, 199307, 199308, 199309, 199310, 199311, 199312, 199401, 199402, 199403, 199404, 199405, 199406, 199407, 199408, 199409, 199410, 199411, 199412, 199501, 199502, 199503, 199504, 199505, 199506, 199507, 199508, 199509, 199510, 199511, 199512, 199601, 199602, 199603, 199604, 199605, 199606, 199607, 199608, 199609, 199610, 199611, 199612, 199701, 199702, 199703, 199704, 199705, 199706, 199707, 199708, 199709, 199710, 199711, 199712, 199801, 199802, 199803, 199804, 199805, 199806, 199807, 199808, 199809, 199810, 199811, 199812, 199901, 199902, 199903, 199904, 199905, 199906, 199907, 199908, 199909, 199910, 199911, 199912, 200001, 200002, 200003, 200004, 200005, 200006, 200007, 200008, 200009, 200010, 200011, 200012, 200101, 200102, 200103, 200104, 200105, 200106, 200107, 200108, 200109, 200110, 200111, 200112, 200201, 200202, 200203, 200204, 200205, 200206, 200207, 200208, 200209, 200210, 200211, 200212, 200301, 200302, 200303, 200304, 200305, 200306, 200307, 200308, 200309, 200310, 200311, 200312, 200401, 200402, 200403, 200404, 200405, 200406, 200407, 200408, 200409, 200410, 200411, 200412, 200501, 200502, 200503, 200504, 200505, 200506, 200507, 200508, 200509, 200510, 200511, 200512))
#str(wout$YearMonth)
# #wout <- wout[,-c(wout$Timestep)]
#wout <- wout[,-2]
#
#weapoutput <- as.data.frame(t(wout))  # transposing the table into a new data frame

#####


# combine year and month column as single column (factor)
## still need to write


# read in gauge data 
# what files are required?
# do i have all of the potential gauges in the xls file?
# read in WEAP data
# how do i restrict the number of nodes to be read from weap?
# dates?

# combine gauge data and WEAP output?


# read file containing USGS identification information (station ID numbers, names, 
# locations) and corresponding stream flow node names (WEAP)
WEAPgauges <- read.csv("gauges_WEAPgauges.csv")

# Merging "gaugeNodes" and "WEAPoutput" to omit the columns of data from "WEAPoutput" that 
# do not correspond to a USGS gauging station as identified within "gauges_WEAPNodes.csv"
wg_gn <- merge(WEAPgauges, weapoutput, by.x ="WEAP_Node", by.y = "Nodes") 
gauge_IDs_g <- wg_gn[,"USGS_ID"]  # a vector of the stream gauge IDs for labelling elsewhere

# create a table of the USGS gauge station IDs and WEAP stream nodes pairings from "wg_gn"
Nodes_g <- wg_gn[,c("Basin", "USGS_ID", "Site_name", "WEAP_Node", "Reach")] # save the new table as a CSV file

ifelse(!dir.exists(file.path(getwd(), "output_R")), dir.create(file.path(getwd(), "output_R")), FALSE)

write.csv(Nodes_g,"./output_R/gauges_nodes_g.csv", row.names = FALSE) 

# Modify "wg_gn" to create a table with gauge ID numbers, YearMonth timesteps, and the 
# estimated outputs from WEAP
justoutput_g <- subset(wg_gn, 
                     select = -c(Basin, Site_name, WEAP_Node, Reach)) # extract unwanted columns
cnms_g <- colnames(justoutput_g) # creates a vector from the column names for labelling
YearMonth_g <- cnms_g[-1]    # remove the first element ("USGS_ID") from the vector "cnms", 
# leaving only the values that represent Years and Months.

# transpose the reduced WEAP output ("justoutput") so that the gauge station IDs are the 
# column names and the YearMonth labels are the row names
t_jo_g <- as.data.frame(t(justoutput_g))  # transposing the table into a new data frame
colnames(t_jo_g) <- gauge_IDs_g # write column names
# How many columns have incorrect titles? 
# length(which((names(t_jo) == t_jo[1,]) == FALSE)) # number of mismatches in column names
# which((names(t_jo) == t_jo[1,]) == FALSE) # which column names are incorrect?
OutputNodes_g <- subset(t_jo_g[-1,]) # Remove first row from "t_jo" containing gauge ID #s

OutputNodes_g <- OutputNodes_g[,order(names(OutputNodes_g))] # sort the columns by gauge IDs
OutputNodes_g1 <- sapply(OutputNodes_g, log10)
# OutputNodesDates_g <- cbind(YearMonth_g, OutputNodes_g) # attach column of YearMonth timesteps
OutputNodesDates_g <- cbind(YearMonth_g, OutputNodes_g1) # attach column of YearMonth timesteps

# Are the rows still in the expected order?
# Row names should match the values in the first column
#length(which((rownames(OutputNodesDates) == 
#                paste("X",OutputNodesDates[,1],sep = "")) == FALSE))
# Which which YearMonth values are incorrect
#which((rownames(OutputNodesDates) == 
#         paste("X",OutputNodesDates[,1],sep = "")) == FALSE) 

write.csv(OutputNodesDates_g, "./output_R/WEAP85to05_g.csv", row.names = FALSE)
