rm(list=ls())
gc()

# set working directory to the location of the required files
setwd("C:/Users/a01987147/Dropbox/PhD/WEAP/Data/Data_Processed/stream gauges")
setwd("C:/Users/davebetts/Dropbox/PhD/WEAP/Data/Data_Processed/stream gauges")
dir()


############################################################################################
###                                                                                      ###
###                  Climate change impacts along the Wasatch front                      ###
###       (The Bear River, Weber River and "Provo - Jordan" River watersheds)            ###
###                       A stream flow model using WEAP                                 ###
###                                                                                      ###
###                          David J. Betts - J                                  ###
###                        Utah State University & iUTAH                                 ###
###                                                                                      ###
###           Processing USGS stream gauge d for comparison           ###
###      USGS stream data retrieved from: http://sgs.gov/nwis/inventory       ###
###                Required files for this scriedat                ###
###                "gauges_W                  ###
###                                                                                      ###
############################################################################################
## 
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

# read weap output
# read weap output of gauges
# read gauge data

## a version for log versions as well

# read files
OutputNodesDates <- read.csv("./output_R/myTotalFlow_Month_all.csv", check.names = FALSE)
modelyears <- read.csv("./output_R/WEAP85to05.csv", check.names = FALSE)


# Remove YearMonth column
Est <- OutputNodesDates[,-1]
Obs <- as.numeric(modelyears[,-1])

OvsE <- vector(mode = "list", length = ncol(Est))
emptyset <- as.data.frame(rep(NA,20))
goflab <- c("ME", "MAE", "MSE", "RMSE", "NRMSE %", "PBIAS %", "RSR", "rSD", "NSE", "mNSE", "rNSE", "d", "md", "rd", "cp", "r", "R2", "bR2", "KGE", "VE")
rownames(emptyset) <- goflab
oenames <- colnames(Est)

library(hydroGOF)
for(i in seq_along(OvsE)) {
    if ((sum(as.numeric(Obs[,i]),na.rm = TRUE) == 0)) {
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

write.csv(OvsE, paste("./output_R/GoodnessofFit.csv", format(Sys.time(), "%Y_%m_%d_%H%M"), ".csv", sep = "_")))

rownames(OvsE)
OvE1 <- as.data.frame(t(OvsE))
names(OvE1)
OvE1 <- OvE1[ , c("RMSE", "PBIAS %", "NSE", "d", "MAE")]

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

write.csv(OvE1, paste("./output_R/ranks.csv", format(Sys.time(), "%Y_%m_%d_%H%M"), ".csv", sep = "_"))

#######
## n-size
#######
##
n <- sapply(modelyears, na.omit)
n_size <- as.data.frame(sapply(n, length))

write.csv(n_size, paste("./output_R/n_size.csv", format(Sys.time(), "%Y_%m_%d_%H%M"), ".csv", sep = "_"))

#######
## elevations
#######
##

gaugeNodes$bins <- cut(gaugeNodes$Elevation, 
                       breaks = c(-Inf, 1400, 1600, 1800, 2000, 2200, 2400, Inf), 
                       labels = c("Below 1400", "1400 to 1600", "1600 to 1800", "1800 to 2000", "2000 to 2200", "2200 to 2400", "Above 2400"), 
                       right = FALSE)
elevroups <- gaugeNodes[,c("USGS_ID", "Basin", "Elevation", "bins")]

names(gaugeNodes)


library(plyr)
ddply(elevroups, "bins")

write.csv(elevroups,"elevation_bins.csv")

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
