rm(list = ls())
gc()

snotelgof<-function(filename,title){
  snowgauge <- read.csv("snotel_gauges.csv")
  snotel_names <- snowgauge[,c("site_WEAP", "Names_djb", "Catchment_SNOTEL")]
  snotel_names <- as.data.frame(lapply(snotel_names, factor))
  snotel_list <- as.character(snotel_names[,"Names_djb"])
  snotel_y <- c("date",as.character(snotel_names[,"site_WEAP"]))
  snotel_catchments <- snotel_names[,c("Names_djb", "Catchment_SNOTEL")]
  row.names(snotel_catchments) <- as.character(snotel_names[,"site_WEAP"])
  snotel_IDs <- as.data.frame(snotel_catchments[,2])
  colnames(snotel_IDs) <- "ID"
  row.names(snotel_IDs) <- snotel_names[,"site_WEAP"]

  snotel_data <- read.csv("snotel_daily.csv", check.names = FALSE)
  x <- snotel_data[-nrow(snotel_data),] # removing data from Jan. 1, 2011
  colnames(x) <- gsub("_.*", "", colnames(x))
  x <- x[,colnames(x) %in% snotel_y] # removing unmatched gages
  
  ## Dates of 'x'
  dates <- as.Date(x[,1], format = "%m/%d/%Y")
  years <- as.character(dates, format = "%Y")
  months <- as.character(dates, format = "%m")
  library(lubridate)
  doy <- yday(dates)
  x[,1]=doy
  
  x<-cbind(years,months,x)
  colnames(x)[1:3] <- c("Year", "Month", "day_of_yr")
  
  snw_allmonths <- aggregate(.~ Month + Year, data = x, FUN = mean, na.rm = TRUE, na.action = NULL)
  
  date <- as.Date(paste(snw_allmonths[,2], snw_allmonths[,1], 01, sep = "-"))
  snw_allmonths[,1] <- date
  snw_allmonths <- snw_allmonths[,-c(2:3)]
  snw_allmonths <- snw_allmonths[,c(1,22,19)]
  snotel_IDs = colnames(snw_allmonths)[2:3] # new addition
  colnames(snw_allmonths)[1] <- "Date"
  # colnames(snw_allmonths) <- as.character(colnames(snw_allmonths))
  
  SvsW <- vector(mode = "list", length = ncol(snw_allmonths)-1)
  names(SvsW) <- as.character(colnames(snw_allmonths)[-1])
  svw_IDs <- snotel_IDs
  
  swe_weap_all <- read.csv(filename, skip = 5, check.names = FALSE)
  #### !!!! ASSUMED COLUMN ORDER TO MATCH BETWEEN SIM and OBS !!! ###
  catchments <- as.character(snotel_catchments[which(rownames(snotel_catchments) %in% snotel_IDs),2])
  # catchments <- gsub("*.)
  # catchments <- gsub("\\[.*", "", catchments)
  colnames(swe_weap_all) <- c("Year", "Month", catchments)
  colnames(snw_allmonths)[2:3] <- catchments

  swe_weap <- swe_weap_all
  date <- as.Date(paste(swe_weap[,1],swe_weap[,2],01, sep = "-"))
  swe_weap[,1] <- date
  swe_weap <- swe_weap[,-2]
  colnames(swe_weap)[1] <- "Date"
  
  for (i in as.character(colnames(snw_allmonths)[-1])) {
    date <- as.Date(snw_allmonths[,1])
    obs <- snw_allmonths[,i]
    weap <- swe_weap[,i]
    
    # GoF
    require(hydroGOF)
    nxt_col <- gof(sim = weap, obs = obs)
    SvsW[[i]] <- nxt_col
    g_range <- range(obs, weap, na.rm = TRUE)
    ID <- i
    # create folder for output from R if folder doesn't already exist
    ifelse(!dir.exists(file.path(getwd(), "output_R/sweGoF")), 
           dir.create(file.path(getwd(), "output_R/sweGoF"), recursive = TRUE), 
           FALSE)
    mainDir <- "."
    subDir <- paste("output_R/sweGoF", title, sep = "_")
    if (!file_test("-d", file.path(mainDir, subDir))) {
      if(file_test("-f", file.path(mainDir, subDir))) {
        stop ("Plots will be stored in ", subDir)
      } else {
        dir.create(file.path(mainDir, subDir))
      }
    }
    png(filename = paste(mainDir, "/", subDir, "/", ID, ".png", sep =""), width = 960, height = 600, units = "px", pointsize = 12)
    plot(date, obs, type="l", lwd=2, 
         col="blue", 
         ylim = g_range, 
         xaxs="i", 
         xlab = "Date", ylab = "Snow Water Equivalent [mm]", 
         main = ID)
    lines(date, weap, lwd=2, col="red")
    legend("topright", legend=c("SNOTEL","WEAP"), lwd=c(2,2), 
           col=c("blue","red"), inset = .05, bty="n")
    dev.off()
  }
  svw1 <- do.call(cbind, SvsW)
  colnames(svw1) <- svw_IDs
  svw2<- as.data.frame(t(svw1))
  svw3 <- svw2[ , c("PBIAS %", "RSR", "RMSE", "NSE", "d", "MAE")]
  svw4 <- cbind(as.character(row.names(svw3)), svw3)
  colnames(svw4)[1] <- "SNOTEL"
  svw5 <- merge(snotel_names, svw4, by.x = "site_WEAP", by.y = "SNOTEL")
  svw6 <- svw5[,-3]
  colnames(svw6)[1:2] <- c("SNOTEL", "Catchment")
  write.csv(svw6, paste("./output_R/sweGoF/SNOTEL_", "GoF_", title, ".csv", sep = ""), row.names = FALSE)
}
  
########################################################################################
###                  List files within the subfolder "./sweGoF" and                  ###
###                 process each file using the function snotelgof()                 ###
########################################################################################
##

filenames <- list.files("SWE", pattern="*.csv", full.names=TRUE)

# Process each file in diretory "./SWE" with function "snotelgof()"
for (i in filenames) {
  filename <- i
  title <- gsub(".csv", "", filename)
  title <- gsub("SWE/SWE_", "", title)
  ## Saves GoF into separate folder
  snotelgof(filename, title)
}
  
########################################################################################
###          Function: GoF_tables - Collates all the GoF tables produced by          ###
###                   snotelgof() into a set of combined tables                      ###
###  Source: Each GoF table produced by snotelgof() as found in "output/SWE" folder  ###
########################################################################################
## !!!                                                                               !!!
## !!! requires "dirloc" to be defined (below)                                       !!!
## !!! Overwrites previous iterations of output                                      !!!
## !!!                                                                               !!!
## 
## Produces a single combined table of all of the goodness of fit measures selected
## within snotelgof(), in addition to separate tables for each of the individual measures.
##
## All tables are saved within the "output_swe" folder within the working directory
##
  
GoF_tables <- function(dirloc) {
  GoF_files <- list.files(dirloc, pattern="*.csv", full.names=TRUE)
  gofnames <- gsub(".*GoF_", "", GoF_files)
  gofnames <- gsub("_201.*","", gofnames)
  x <- c("PBIAS %", "RSR", "RMSE", "NSE", "d", "MAE")
  y <- read.csv(GoF_files[1], check.names = FALSE)
  z <- vector(mode = "list", length = length(x))
  names(z) <- x
  sites <- y[,!(colnames(y) %in% x)]
  write.csv(sites, "swe_sites.csv", row.names = FALSE)
  for (i in seq_along(z)) {
    z[[i]] <- as.data.frame(sites)
    colnames(z[[i]]) <- c("site","catchment")
  }
  for (i in seq_along(GoF_files)) {
    w <- read.csv(GoF_files[i], check.names = FALSE)
    for (j in x) {
      z[[j]] <- cbind(z[[j]], as.data.frame(w[,j]))
      colnames(z[[j]])[ncol(z[[j]])] <- paste(j, gofnames[i], sep = "_")
    }
    sapply(names(z), function (zz) write.csv(z[[zz]], file=paste("output_R/swe_GoF",zz, "csv", sep="."), row.names = FALSE))
  }
  final <- do.call(cbind, z)
  write.csv(final, "output_R/all_swe.csv", row.names = FALSE)
}
  
# define "dirloc"
dirloc <- "output_R/sweGoF"

GoF_tables(dirloc)
