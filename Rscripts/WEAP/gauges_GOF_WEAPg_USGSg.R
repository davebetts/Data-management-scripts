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

library(hydroGOF)
# Remove YearMonth column
Est <- OutputNodesDates[,-1]
Est_g <- OutputNodesDates_g[,-1]
Obs <- modelyears[,-1]

E_gvsE <- vector(mode = "list", length = ncol(Est))
emptyset <- as.data.frame(rep(NA,20))
goflab <- c("ME", "MAE", "MSE", "RMSE", "NRMSE %", "PBIAS %", "RSR", "rSD", "NSE", "mNSE", "rNSE", "d", "md", "rd", "cp", "r", "R2", "bR2", "KGE", "VE")
rownames(emptyset) <- goflab
egenames <- colnames(Est)

library(hydroGOF)
for(i in seq_along(E_gvsE)) {
    if ((sum(Est_g[,i],na.rm = TRUE) == 0)) {
        nxt_col <- emptyset
        E_gvsE[[i]] <- nxt_col
    }
    else {
        sim = Est[,i]
        obs = Est_g[,i]
        nxt_col <- gof(sim = sim, obs = obs)
        E_gvsE[[i]] <- nxt_col
    }
}
E_gvsE <- do.call(cbind, E_gvsE)
colnames(E_gvsE) <- egenames

write.csv(E_gvsE, "./output_R/GoodnessofFit_gvsg.csv")

rownames(E_gvsE)
E_gvE1 <- as.data.frame(t(E_gvsE))
names(E_gvE1)
E_gvE1 <- E_gvE1[ , c("MAE", "PBIAS %", "NSE", "d")]

summary(E_gvE1)
y <- colnames(E_gvE1)
E_gvE1[,"PBIAS %"] <- abs(E_gvE1[,"PBIAS %"])
for (i in names(E_gvE1)) {
    x <- rank(E_gvE1[i])
    E_gvE1 <- cbind(E_gvE1,x)
}
colnames(E_gvE1) <- c(y, paste("rank",y,sep = ""))
E_gvE1$rank <- rowSums(E_gvE1[,5:8])
E_gvE1 <- E_gvE1[order(E_gvE1$rank, decreasing = TRUE),]

write.csv(E_gvE1, "./output_R/ranks_gvsg.csv")

#######
## n-size
#######
##
n <- sapply(modelyears, na.omit)
n_size <- as.data.frame(sapply(n, length))

write.csv(n_size, "./output_R/n_size.csv")

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
