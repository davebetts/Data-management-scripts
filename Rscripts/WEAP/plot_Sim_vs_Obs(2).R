rm(list=ls())
gc()

# set working directory to the location of the required files
setwd("C:/Users/davebetts/Dropbox/PhD/WEAP/Data/Data_Processed/stream gauges")
dir()

############################################################################################
###                                                                                      ###
###  !!!             Climate change impacts along the Wasatch front                      ###
###  !!!  (The Bear River, Weber River and "Provo - Jordan" River watersheds)            ###
###  !!!                  A stream flow model using WEAP                                 ###
###  !!!                                                                                 ###
###  !!!                     David J. Betts - July 2014                                  ###
###  !!!                   Utah State University & iUTAH                                 ###
###  !!!                                                                                 ###
###  !!!      Processing USGS stream gauge data and WEAP output for comparison           ###
###  !!! USGS stream data retrieved from: http://waterdata.usgs.gov/nwis/inventory       ###
###  !!!           Required files for this script: "all_gaugedata.xlsx",                 ###
###  !!!           "gauges_WEAPNodes.csv", "WEAP_output_m3_7_10_15.csv"                  ###
###  !!!                                                                                 ###
############################################################################################
## !!! 


############################################################################################
###  !!!              Pairing WEAP output to USGS stream gauges                          ###
###  !!!                                 and                                             ###
###  !!!                   Omitting additional WEAP output                               ###
############################################################################################
##
## 

weap <- read.csv("./output_R/WEAP85to05.csv", check.names = FALSE)
gauges <- read.csv("./output_R/myTotalFlow_Month_all.csv", check.names = FALSE)
head(weap)
head(gauges)

gaugeIDs <- names(gauges[-1])
YrMon <- as.character(gauges[,1])

# Function: plot WEAP output and stream gauge data
# ?not needed? WEAPvsUSGS <- vector(mode = "list", length = length(gauges))
WEAPvsUSGS <- # empty data frame? 
for (i in seq_along(gaugeIDs)) {
    WEAP <- weap[ ,which(names(weap) == gaugeIDs[i])]/1000000
    USGS <- gauges[ ,which(names(gauges) == gaugeIDs[i])]/1000000
    SimObs <- as.data.frame(cbind(WEAP, USGS))
    WEAPvsUSGS <- cbind(YrMon, SimObs)
    WEAPvsUSGS$YrMon <- as.Date(paste(as.character(WEAPvsUSGS$YrMon), "01", sep = ""), format = "%Y%m%d")
    mainDir <- "."
    subDir <- paste("sim_vs_obs_plots", format(Sys.time(), "%Y_%m_%d_%H%M"),sep = "_")
    if (!file_test("-d", file.path(mainDir, subDir))) {
        if(file_test("-f", file.path(mainDir, subDir))) {
            stop ("Plots will be stored in ", subDir)
        } else {
            dir.create(file.path(mainDir, subDir))
        }
    }
    title <- as.character(gaugeIDs[i])
    png(filename = paste(mainDir, "/", subDir, "/", title, ".png", sep =""), width = 960, height = 600, units = "px", pointsize = 12)
    # plot(range(WEAPvsUSGS$YrMon), range(c(WEAPvsUSGS$WEAP, WEAPvsUSGS$USGS), na.rm = T), type='n', xlab = "", ylab = "Total Flow (Millions Cubic Meters)", main = paste("WEAP output vs USGS", gaugeIDs[i]))
    plot(range(WEAPvsUSGS$YrMon), range(c(WEAPvsUSGS$WEAP, WEAPvsUSGS$USGS), na.rm = T), type='n', xlab = "", ylab = "Total Flow (Log(Mm^3)", main = paste("WEAP output vs USGS", gaugeIDs[i]))
    lines(WEAPvsUSGS$YrMon, WEAPvsUSGS$WEAP, col="red")
    lines(WEAPvsUSGS$YrMon, WEAPvsUSGS$USGS, col="blue")
    dev.off()
}
    
    
    
    
    
The ggplot function is used by specifying a data frame and the aes maps the Date to the x-axis and the number of Views to the y-axis.

ggplot(yt.views, aes(Date, Views)) + geom_line() +
  scale_x_date(format = "%b-%Y") + xlab("") + ylab("Daily Views")
  
ggplot(uk.df, aes(Year, Population)) + geom_point() +
  xlab("Year") + ylab("Total Population (Thousands)") +
  opts(title = "UK Population (1992-2009)")
The geom_point specifies the type of graph to create  
  
  
  [,c("YearMonth")] = as.factor(format(mysheets[[i]][,"Date"],"%Y%m"))
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
  x = mysheets[[i]][1,2] # create a label from the gauge ID
  ID = as.vector(rep(x, each = nrow(TotFlow)), mode = "character") # vector for ID label
  Tot_flow_month[[i]] = cbind(ID, TotFlow[,c(-2)]) # Bind ID vector to data row
  names(Tot_flow_month)[i] <- ID  # label table i with stream gauge ID [i]
}








WEAPvsUSGS <- as.data.frame# empty data frame? 
for (i in seq_along(gaugeIDs)) {
    WEAP <- weap[ ,which(names(weap) == gaugeIDs[3])]
    USGS <- gauges[ ,which(names(gauges) == gaugeIDs[3])]    
    WEAPvsUSGS <- as.data.frame(cbind(YrMon, WEAP, USGS), colClasses = "numeric")
  str(WEAPvsUSGS)
  class(WEAPvsUSGS)
  transform(WEAPvsUSGS)
  !!! check if file exists and if not create the new directory
  create folder sim_vs_obs_plots
  title <- as.character(gaugeIDs[i])
  png(filename = paste("./sim_vs_obs_plots/",title,".png", sep =""), width = 480, height = 480, units = "px", pointsize = 12)
  plot(WEAPvsUSGUS[,1]~WEAPvsUSGS[,2:3], col = c("red", "blue"), type = "l", xlab = "Date", ylab = "Total Flow", main = paste("WEAP output vs USGS", gaugeIDs[i])
       dev.off()



YrMon <- as.numeric(gauges[,1])
WEAP <- weap[ ,which(names(weap) == gaugeIDs[3])]
USGS <- gauges[ ,which(names(gauges) == gaugeIDs[3])]
WEAPvsUSGS <- as.data.frame(cbind(YrMon, WEAP, USGS))
View(WEAPvsUSGS)
WEAPvsUSGS <- as.data.frame(cbind(YrMon, WEAP, USGS), colClasses = c("numeric"))
View(WEAPvsUSGS)
str(WEAPvsUSGS)
       
       










# read file containing 

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
