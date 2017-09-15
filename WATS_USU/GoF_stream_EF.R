rm(list=ls())
gc()

## i saved CF files as CFS rather than CF.  not purposefully different than original script, but now '/1000000' is no longer part of the plotting

########################################################################################
###                                                                                  ###
###                          Goodness of Fit Comparisons for                         ###
###                WEAP simulations vs. USGS stream gage observations                ###
###                   (Reported in millions of cubic feet / month)                   ###
###                          Dave Betts - March 2016                                 ###
###                                                                                  ###
########################################################################################
## !!!                                                                               !!!
## !!! Functions within this script: weapgof(), GoF_tables()                         !!!
## !!!                                                                               !!!
## !!! Required files: "selected_gauges.csv"                                         !!!
## !!! Required packages: "hydroGOF"                                                 !!!
## !!! Data retrieval from WEAP: WEAP>Results under Favorites>!Streamflow_CF         !!!
## !!! Results need to be saved as CSV, and file names need to start with "CF_..."   !!!
## !!! All files in the subfolder "./CF" will be processed if the entire script is   !!!
## !!! used.                                                                         !!!
## !!! This script overwrites previous versions of the same output.                  !!!
## !!!                                                                               !!!
## !!! All output (GoF tables, plots) are reported in millions of cubic feet / month !!!
## !!!                                                                               !!!
#
# WEAP contains daily average streamflow observations from USGS stream gages, using
# the option for data input through "ReadFromFile".
#
# Summaries of observed data were calculated within WEAP and outside of WEAP and 
# compared.  These summaries convert the USGS daily average streamflows to monthly 
# totals. No differences were found between the summaries from WEAP and the summaries
# done outside of WEAP --> The data tables produced by Favorites>!Streamflow_CF in 
# WEAP are the only files necessary to make comparisons between simulated and observed
# values. 
#
########################################################################################
###           Function: weapgof() - Reads given output of stream flow data           ###
###                 from WEAP, as provided within the subfolder "CF".                ###
###                Output: GoF tables and plots of sim. vs. obs. data                ###
########################################################################################
#
# The function weapgof() will produce an individual goodness of fit report and a 
# complete set of plots for each CSV data table located in the subfolder "CF" within
# the working directory. All of the output is saved within the folder "output_R" which
# is created by this script.
#
# The function weapgof() produces plots of simulated and observed data for each of the
# selected stream gages listed in the file "selected_gauges.csv".  For each gage, two
# plots are produced: arithmetic and log10 scales (y-axes)
#
# The complete set of GoF measures from hydroGOF::gof can be produced, but only a subset
# is included in the final GoF tables.  The subset of GoF measures can be changed by
# modifying line ~102 below.
# 
########################################################################################

weapgof <- function(filename, title) {
  # input WEAP output 
  wout <- read.csv(filename, skip = 5, check.names = FALSE)
  row.names(wout) <- as.Date(paste(wout[,1], wout[,2], "01", sep = "-"))  
  weapoutput <- as.data.frame(t(wout[,-c(1:2)])) 
  z <- row.names(weapoutput)
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  for (i in 1:length(z)) {
    z[i] = gsub('"',"", z[i])
    z[i] = trim(z[i])
  }
  z
  x <- data.frame(do.call(rbind, strsplit(z, '\\s*[\\]\\s*'))) 
  names(x) <- c('reach', 'site') 
  x$site <- gsub("\\[Cubic Meter\\]", "", x$site)
  x$site <- gsub("\\[Cubic Feet\\]", "", x$site)
  x$site <- gsub('\\[Cubic Feet per Second\\]', "", x$site)
  x$site <- gsub("\\(gauge\\)", "", x$site)
  x$gage <- gsub("[^0-9]","",x$site) 
  x$gage <- ifelse(as.numeric(x$gage) < 1000, "", x$gage)
  x$gage <- ifelse(is.na(x$gage), "", x$gage)
  x2<-x
  x2$gage<-as.numeric(x2$gage)
  elbasbin <- read.csv("selected_gauges.csv", check.names = FALSE)
  ebins <- elbasbin[which(elbasbin$Tier < 4),c("gage", "site_name_WEAP","Basin", "elev_m")]
  ebins$bins <- cut(ebins$elev_m, 
                    breaks = c(-Inf, 1400, 1600, 1800, 2000, 2200, 2400, Inf), 
                    labels = c("Below 1400m", "1400m to 1600m", "1600m to 1800m", 
                               "1800m to 2000m", "2000m to 2200m", "2200m to 2400m", 
                               "Above 2400m"),
                    right = FALSE)
  ng_match <- as.character(x2$reach[which(x2$gage %in% ebins$gage)])
  weapoutput <- cbind(x,weapoutput) 
  weapoutput <- weapoutput[which(weapoutput$reach %in% ng_match),] 
  rownames(weapoutput) <- NULL
  weap_gauges <- weapoutput[which(weapoutput$gage > 1), ]
  gauge_IDs <- as.character(weap_gauges[,2])
  weap_est <- weapoutput[which(weapoutput$reach %in% weap_gauges$reach & weapoutput$gage < 1), -3]
  twest <- as.data.frame(t(weap_est[,-c(1:2)]))
  cnms <- as.character(weap_est[,1])
  colnames(twest) <- cnms
  tgage <- as.data.frame(t(weap_gauges[,-c(1:3)]))
  cnms <- as.character(weap_gauges[,1])
  colnames(tgage) <- cnms
  # goodness of fit tables 
  OvsE <- vector(mode = "list", length = ncol(tgage))
  require(hydroGOF)
  for(i in seq_along(OvsE)) {
    sim = twest[,cnms[[i]]]
    obs = tgage[,i]
    nxt_col <- gof(sim = sim, obs = obs)
    OvsE[[i]] <- nxt_col
  }
  OvsE <- do.call(cbind, OvsE)
  colnames(OvsE) <- gauge_IDs
  OvE1<- as.data.frame(t(OvsE))
  OvE1 <- OvE1[ , c("PBIAS %", "RSR", "RMSE", "NSE", "d", "MAE")]
  n <- sapply(tgage, na.omit) 
  n_size <- length(n)
  OvE1$n <- as.numeric(as.character(n_size))
  # create folder for output from R if folder doesn't already exist
  ifelse(!dir.exists(file.path(getwd(), "output_R/GoF")), 
         dir.create(file.path(getwd(), "output_R/GoF"), recursive = TRUE), 
         FALSE)
  ove <- as.matrix(OvE1) 
  ove <- cbind(row.names(OvE1), OvE1)
  colnames(ove)[1] <- "site"
  write.csv(ove, paste("./output_R/GoF/GoodnessofFit_", title, "_", ".csv", sep = ""), row.names = FALSE)
  ## plotting the data
  y.range=range(c(as.list(twest), as.list(tgage)), na.rm=T)
  y.range[2]=y.range[2]
  gauges2plot <- names(tgage)
  YrMon <- as.character(row.names(tgage))
  for (i in seq_along(gauges2plot)) {
    WEAP <- twest[ ,which(names(twest) == gauges2plot[i])] # convert CFS to MCFS
    USGS <- tgage[ ,i] # convert CFS to MCFS
    SimObs <- as.data.frame(cbind(WEAP, USGS))
    WEAPvsUSGS <- cbind(YrMon, SimObs)
    WEAPvsUSGS$YrMon <- as.Date(WEAPvsUSGS$YrMon, format = "%Y-%m-%d")
    mainDir <- "."
    subDir <- paste("output_R/stream_gages", title, sep = "_")
    if (!file_test("-d", file.path(mainDir, subDir))) {
      if(file_test("-f", file.path(mainDir, subDir))) {
        stop ("Plots will be stored in ", subDir)
      } else {
        dir.create(file.path(mainDir, subDir))
      }
    }
    ID <- gauge_IDs[i]
    png(filename = paste(mainDir, "/", subDir, "/", ID, ".png", sep =""), width = 960, height = 600, units = "px", pointsize = 12)
    plot(range(WEAPvsUSGS$YrMon), y.range, type='n', xlab = "", ylab = "Avarge CFS / Month", main = paste("WEAP estimate vs ", ID, '\n', title, sep = ""))
    lines(WEAPvsUSGS$YrMon, WEAPvsUSGS$WEAP, col="red")
    lines(WEAPvsUSGS$YrMon, WEAPvsUSGS$USGS, col="blue")
    legend("topright", 
           c("Observed","Simulated"), 
           lty=c(1,1),
           lwd=c(2.5,2.5),
           col=c("blue","red"),
           bty="n",
           inset=c(.0125,.025)) 
    dev.off()
  }
  for (i in seq_along(gauges2plot)) {
    WEAP <- twest[ ,which(names(twest) == gauges2plot[i])] # not convert CFS to MCFS
    USGS <- tgage[ ,i] # convert CFS to MCFS
    SimObs <- as.data.frame(cbind(WEAP, USGS))
    WEAPvsUSGS <- cbind(YrMon, SimObs)
    WEAPvsUSGS$YrMon <- as.Date(WEAPvsUSGS$YrMon, format = "%Y-%m-%d")
    mainDir <- "."
    subDir <- paste("output_R/stream_gages", title, "Log", sep = "_")
    if (!file_test("-d", file.path(mainDir, subDir))) {
      if(file_test("-f", file.path(mainDir, subDir))) {
        stop ("Plots will be stored in ", subDir)
      } else {
        dir.create(file.path(mainDir, subDir))
      }
    }
    ID <- gauge_IDs[i]
    png(filename = paste(mainDir, "/", subDir, "/", "Log_", ID, ".png", sep =""), width = 960, height = 600, units = "px", pointsize = 12)
    plot(range(WEAPvsUSGS$YrMon), y=c(1,y.range[2]), type='n', xlab = "", log = "y", ylab = "Average CFS / Month", main = paste("WEAP estimates vs ", ID, "\non a log10 scale", '\n', title, sep = ""))
    lines(WEAPvsUSGS$YrMon, WEAPvsUSGS$WEAP, col="red")
    lines(WEAPvsUSGS$YrMon, WEAPvsUSGS$USGS, col="blue")
    legend("bottomright",
           inset = c(.0125, 0.025),
           c("Observed","Simulated"), 
           lty=c(1,1),
           lwd=c(2.5,2.5),
           col=c("blue", "red"),
           bty="n") 
    dev.off()
  }
}

########################################################################################
###                    List files within the subfolder "./CF" and                    ###
###                  process each file using the function weapgof()                  ###
########################################################################################
##

filenames <- list.files("CF", pattern="*.csv", full.names=TRUE)

# Process each file in diretory "./CF" with function "weapgof()"
for (i in filenames) {
  filename <- i
  title <- gsub(".csv", "", filename)
  title <- gsub("CF/CF_", "", title)
  ## Saves GoF into separate folder
  weapgof(filename, title)
}
# next  line normally below next section
GoF_tables(dirloc)


########################################################################################
###          Function: GoF_tables - Collates all the GoF tables produced by          ###
###                     weapgof() into a set of combined tables                      ###
###   Source: Each GoF table produced by weapgof() as found in "output/GoF" folder   ###
########################################################################################
## !!!                                                                               !!!
## !!! requires "dirloc" to be defined (below)                                       !!!
## !!! Overwrites previous iterations of output                                      !!!
## !!!                                                                               !!!
## 
## Produces a single combined table of all of the goodness of fit measures selected
## within weapgof(), in addition to separate tables for each of the individual measures.
##
## All tables are saved within the "output_R" folder within the working directory
##
########################################################################################

GoF_tables <- function(dirloc) {
  GoF_files <- list.files(dirloc, pattern="*.csv", full.names=TRUE)
  gofnames <- gsub(".*Fit_", "", GoF_files)
  gofnames <- gsub("_.csv","", gofnames)
  x <- c("PBIAS %", "RSR", "RMSE", "NSE", "d", "MAE")
  y <- read.csv(GoF_files[1], check.names = FALSE)
  z <- as.data.frame(t(y[-1]))
  z[,1]=as.character(rownames(z))
  colnames(z) <- "calibration"
  for (i in seq_along(GoF_files)) {
    w <- read.csv(GoF_files[i], check.names = FALSE)
    ww <- t(w[-1])
    z = cbind(z,ww)
    colnames(z)[i+1] = gofnames[i]
  }
  write.csv(z, "output_R/all.csv", row.names = FALSE)
}

# define "dirloc"
dirloc <- "output_R/GoF"
# run function "GoF_Tables()" for all files within directory as defined by "dirloc"
GoF_tables(dirloc)

