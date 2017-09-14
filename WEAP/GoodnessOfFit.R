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

write.csv(OvsE, paste("GoodnessofFit", format(Sys.time(), "%Y_%m_%d_%H%M%S"), ".csv", sep = "_"))

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

write.csv(OvE1, paste("ranks.csv", format(Sys.time(), "%Y_%m_%d_%H%M%S"), ".csv", sep = "_"))
)


#######
## n-size
#######
##
n <- sapply(modelyears, na.omit)
n_size <- as.data.frame(sapply(n, length))

write.csv(n_size, paste("n_size.csv", format(Sys.time(), "%Y_%m_%d_%H%M%S"), ".csv", sep = "_"))

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

write.csv(elev_groups, paste("elevation_bins.csv", format(Sys.time(), "%Y_%m_%d_%H%M%S"), ".csv", sep = "_"))


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
write.csv(fullGOF3, paste("GOF_single.csv", format(Sys.time(), "%Y_%m_%d_%H%M%S"), ".csv", sep = "_"))
)

    OvsE2 <- gof(Est,Obs)
dim(Est2)
