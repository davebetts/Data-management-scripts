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
## 
##
##

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

weap <- read.table("./output_R/WEAP85to05.csv", sep = ",", header = TRUE, check.names = FALSE)
gauges <- read.table("./output_R/myTotalFlow_Month_all.csv", sep = ",", header = TRUE, check.names = FALSE)

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

write
.csv(plotting, "plots.csv", row.names = FALSE, na = "")
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