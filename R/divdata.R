rm(list=ls())
gc()

## this file splits the output from ArcGIS.  ArcGIS averaged the climate data through spatial joins by HUC12 (plus a few divisions of HUC12)
## Each column (an individual HUC12) is paired with the rownames (dates) and saved as a two column CSV
divdata <- function(filename) { 
  data <- read.csv(filename)
  not <- c("FID", "Join_Count", "TARGET_FID", "States_1", "HU_12_Na_1", 
           "areakm2_sn", "Lat_sn", "HUC_12", "HUC_10", "HUC_8", "HUC_6", 
           "HUC_4", "Names_djb", "Lon", "Lat")
  hucs <- t(data[,-which(names(data) %in% not)])
  colnames(hucs) <- data$Names_djb
  lttr <- substr(filename, 1, 1)
  if (lttr == "p") {
    units = "[mm]"
  } else {
    if (lttr == "r") {
      units = "[%]"
    } else {
      units = "[C]"
    }
  }
#  dates <- as.Date(gsub("[[:alpha:]]", "", row.names(hucs)), format = "%Y%m%d")
  #  dates <- format(as.Date(gsub("[[:alpha:]]", "", row.names(hucs)), format = "%Y%m%d"), "%m/%d/%Y")
### !!! THIS ONE!!! ###  dates <- as.Date(gsub("[[:alpha:]]", "", row.names(hucs)), format = "%Y%m%d")
  sets <- matrix(nrow = 313, ncol = ncol(hucs)+2)
  yrs <- 1985:2010
  mnths <- 1:12
  m <- length(yrs)
  x <- rep(yrs, each = length(mnths))
  z <- rep(mnths, m)
  for (i in 1:312){
    sets[i,1] <- x[i]
    sets[i,2] <- z[i]
  }
  for (i in 3:(ncol(hucs)+2)) {
    for (j in 1:nrow(hucs)) {
      sets[j,i] <- hucs[j, (i-2)]
    }
  }
  sets1 <- as.data.frame(sets)
  names(sets1) <- c("Monthly: $Columns =  Year", "Month", colnames(hucs))
  mainDir <- "."
  subDir <- lttr
#  hdr <- c("$DateFormat = m/d/y")
  if (!file_test("-d", file.path(mainDir, subDir))) {
    if(file_test("-f", file.path(mainDir, subDir))) {
      stop ("Plots will be stored in ", subDir)
    } else {
      dir.create(file.path(mainDir, subDir))
    }
  }
  for (i in 3:ncol(sets1)) {
    sets2 <- data.frame(sets1[,1], sets1[,2], sets1[,i])
    colnames(sets2) <- c("Monthly: $Columns =  Year", "Month", paste(colnames(sets1)[i], units, sep = ""))
    flnm <- paste("./",lttr,"/",lttr,names(sets1)[i],".csv", sep ="")
    write.csv(sets2, file = flnm, row.names = FALSE, na = "")
  }
}

filename = "p.csv"
divdata(filename)
filename = "r.csv"
divdata(filename)
filename = "t.csv"
divdata(filename)


divdata(filename)

filename = file.choose()

filename = "p.csv"
divdata(filename)
filename = "r.csv"
divdata(filename)
filename = "t.csv"
divdata(filename)

########################################################################
########################################################################
########################################################################

rm(list=ls())
gc()
dir()

## this file splits the output from ArcGIS.  ArcGIS averaged the climate data through spatial joins by HUC12 (plus a few divisions of HUC12)
## Each column (an individual HUC12) is paired with the rownames (dates) and saved as a two column CSV
divdata <- function(filename) { 
  data <- read.csv(filename)
  not <- c("FID", "Join_Count", "TARGET_FID", "States_1", "HU_12_Na_1", 
           "areakm2_sn", "Lat_sn", "HUC_12", "HUC_10", "HUC_8", "HUC_6",
           "HUC_4", "Names_djb", "Lon", "Lat")
  hucs <- t(data[,-which(names(data) %in% not)])
  colnames(hucs) <- data$Names_djb
  lttr <- substr(filename, 1, 1)
  if (lttr == "p") {
    units = "[mm]"
  } else {
    if (lttr == "r") {
      units = "[%]"
    } else {
      units = "[C]"
    }
  }
  #  dates <- as.Date(gsub("[[:alpha:]]", "", row.names(hucs)), format = "%Y%m%d")
  #  dates <- format(as.Date(gsub("[[:alpha:]]", "", row.names(hucs)), format = "%Y%m%d"), "%m/%d/%Y")
  ### !!! THIS ONE!!! ###  dates <- as.Date(gsub("[[:alpha:]]", "", row.names(hucs)), format = "%Y%m%d")
  sets <- matrix(nrow = 121, ncol = ncol(hucs)+2)
  yrs <- 2085:2094
  mnths <- 1:12
  m <- length(yrs)
  x <- rep(yrs, each = length(mnths))
  z <- rep(mnths, m)
  for (i in 1:120){
    sets[i,1] <- x[i]
    sets[i,2] <- z[i]
  }
  for (i in 3:(ncol(hucs)+2)) {
    for (j in 1:nrow(hucs)) {
      sets[j,i] <- hucs[j, (i-2)]
    }
  }
  sets1 <- as.data.frame(sets)
  names(sets1) <- c("Monthly: $Columns =  Year", "Month", colnames(hucs))
  mainDir <- "."
  subDir <- lttr
  #  hdr <- c("$DateFormat = m/d/y")
  if (!file_test("-d", file.path(mainDir, subDir))) {
    if(file_test("-f", file.path(mainDir, subDir))) {
      stop ("Plots will be stored in ", subDir)
    } else {
      dir.create(file.path(mainDir, subDir))
    }
  }
  for (i in 3:ncol(sets1)) {
    sets2 <- data.frame(sets1[,1], sets1[,2], sets1[,i])
    colnames(sets2) <- c("Monthly: $Columns =  Year", "Month", paste(colnames(sets1)[i], units, sep = ""))
    flnm <- paste("./",lttr,"2/",lttr,names(sets1)[i],".csv", sep ="")
    write.csv(sets2, file = flnm, row.names = FALSE, na = "")
  }
}

filename = "p2.csv"
divdata(filename)
filename = "t2.csv"
divdata(filename)
filename = "r2.csv"
divdata(filename)

divdata(filename)

filename = file.choose()

