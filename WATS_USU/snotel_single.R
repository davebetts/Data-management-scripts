rm(list = ls())
gc()

filenames <- list.files("snotel", pattern="*.csv", full.names=TRUE)

# Process each file in diretory "./swe" with function "snotelgof()"
OvsE <- vector(mode = "list", length = length(filenames))
for (i in 1:length(filenames)) {
  filename <- filenames[i]
  title <- gsub(".csv", "", filename)
  title <- gsub("snotel/", "", title)
  snotel_all <- read.csv(filename, skip = 5, check.names = FALSE)
  colnames(snotel_all) <- c("Year", "Month", "depth","gauge")
  obs <- snotel_all[,4]
  weap <- snotel_all[,3]
  date <- as.Date(paste(snotel_all[,1],snotel_all[,2],01, sep = "-"))
  snotel_all[,1] <- date
  snotel_all <- snotel_all[,-2]
  colnames(snotel_all)[1] <- "Date"
  # GoF
  require(hydroGOF)
  nxt_col <- gof(sim = weap, obs = obs)
  colnames(nxt_col)=title
  OvsE[[i]]=nxt_col
  # names(OvsE)[[i]]=title
  g_range <- range(obs, weap, na.rm = TRUE)
  ID <- as.character(title)
  # create folder for output from R if folder doesn't already exist
  ifelse(!dir.exists(file.path(getwd(), "output_R/snotel")), 
           dir.create(file.path(getwd(), "output_R/snotel"), recursive = TRUE), 
           FALSE)
  mainDir <- "."
  subDir <- paste("output_R/snotel", title, sep = "_")
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
         col=c("blue","red"), inset = .05)
  dev.off()
}
svw1=do.call(cbind,OvsE)
svw2 <- as.data.frame(svw1)
svw3 <- cbind(as.character(row.names(nxt_col)), svw2)
svw4 <- svw3[c("PBIAS %", "RSR", "RMSE", "NSE", "d", "MAE"),]
colnames(svw4)[1]<-"GoF"
  # svw5 <- svw4
  # svw6 <- svw5[,-3]
write.csv(svw4, paste("./output_R/snotel/SNOTEL_", "GoF_", title, ".csv", sep = ""), row.names = FALSE)

  
########################################################################################
###                  List files within the subfolder "./sweGoF" and                  ###
###                 process each file using the function snotelgof()                 ###
########################################################################################
##

  
########################################################################################
###          Function: GoF_tables - Collates all the GoF tables produced by          ###
###                   snotelgof() into a set of combined tables                      ###
###  Source: Each GoF table produced by snotelgof() as found in "output/swe" folder  ###
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
