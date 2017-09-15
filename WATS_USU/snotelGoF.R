rm(list = ls())
gc()

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

wsnow <- file.choose()
swe_weap_all <- read.csv(wsnow, skip = 5, check.names = FALSE)
catchments <- names(swe_weap_all)[-c(1:2)]
catchments <- gsub("\\[.*", "", catchments)
colnames(swe_weap_all) <- c("Year", "Month", catchments)

swe_weap <- swe_weap_all[,colnames(swe_weap_all) %in% snotel_list]
swe_weap <- cbind(swe_weap_all[,1:2],swe_weap)
date <- as.Date(paste(swe_weap[,1],swe_weap[,2],01, sep = "-"))
swe_weap[,1] <- date
swe_weap <- swe_weap[,-2]
colnames(swe_weap)[1] <- "Date"

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
colnames(snw_allmonths)[1] <- "Date"
colnames(snw_allmonths) <- as.character(colnames(snw_allmonths))

SvsW <- vector(mode = "list", length = ncol(snw_allmonths)-1)
names(SvsW) <- as.character(colnames(snw_allmonths)[-1])
svw_IDs <- names(SvsW)

for (i in as.character(colnames(snw_allmonths)[-1])) {
  date <- as.Date(snw_allmonths[,1])
  obs <- snw_allmonths[,i]
  weap <- swe_weap[,as.character(snotel_catchments[which(row.names(snotel_catchments) == i),1])]
  
  # GoF
  require(hydroGOF)
  nxt_col <- gof(sim = weap, obs = obs)
  SvsW[[i]] <- nxt_col
  
  g_range <- range(obs, weap, na.rm = TRUE)
  ID <- as.character(snotel_IDs[i,])
  png(filename = paste(ID, ".png", sep =""), width = 960, height = 600, units = "px", pointsize = 12)
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
svw1 <- do.call(cbind, SvsW)
colnames(svw1) <- svw_IDs
svw2<- as.data.frame(t(svw1))
svw3 <- svw2[ , c("PBIAS %", "RSR", "RMSE", "NSE", "d", "MAE")]
#ifelse(!dir.exists(file.path(getwd(), "output_R/GoF")), dir.create(file.path(getwd(), "output_R/GoF")), FALSE)
dtstamp <- format(Sys.time(), "%Y%m%d_%H%M")
svw4 <- cbind(as.character(row.names(svw3)), svw3)
colnames(svw4)[1] <- "SNOTEL"
svw5 <- merge(snotel_names, svw4, by.x = "site_WEAP", by.y = "SNOTEL")
svw6 <- svw5[,-3]
colnames(svw6)[1:2] <- c("SNOTEL", "Catchment")
write.csv(svw6, paste("GoodnessofFit_", "SNOTEL", "_", dtstamp, ".csv", sep = ""), row.names = FALSE)




elbasbin <- read.csv("selected_gauges.csv", check.names = FALSE)
ebins <- elbasbin[,c("site_name_WEAP","Basin", "elev_m", "Tier")]
ebins$bins <- cut(ebins$elev_m, 
                  breaks = c(-Inf, 1400, 1600, 1800, 2000, 2200, 2400, Inf), 
                  labels = c("Below 1400m", "1400m to 1600m", "1600m to 1800m", 
                             "1800m to 2000m", "2000m to 2200m", "2200m to 2400m", 
                             "Above 2400m"),
                  right = FALSE)
ove1 <- merge(as.data.frame(ove), ebins, by.x = "site", "site_name_WEAP")



## n-size
n <- sapply(tgage, na.omit) 
n_size <- lapply(n, length)
OvE1$n <- as.numeric(as.character(n_size))

# create folder for output from R if folder doesn't already exist

## plotting the data
gauges2plot <- names(tgage)
YrMon <- as.character(row.names(tgage))
for (i in seq_along(gauges2plot)) {
  WEAP <- twest[ ,which(names(twest) == gauges2plot[i])]/1000000
  USGS <- tgage[ ,i]/1000000
  SimObs <- as.data.frame(cbind(WEAP, USGS))
  WEAPvsUSGS <- cbind(YrMon, SimObs)
  WEAPvsUSGS$YrMon <- as.Date(WEAPvsUSGS$YrMon, format = "%Y-%m-%d")
  mainDir <- "."
  subDir <- paste("output_R/sim_vs_obs_plots", title, dtstamp, sep = "_")
  if (!file_test("-d", file.path(mainDir, subDir))) {
    if(file_test("-f", file.path(mainDir, subDir))) {
      stop ("Plots will be stored in ", subDir)
    } else {
      dir.create(file.path(mainDir, subDir))
    }
  }
  ID <- gauge_IDs[i]
  png(filename = paste(mainDir, "/", subDir, "/", ID, ".png", sep =""), width = 960, height = 600, units = "px", pointsize = 12)
  # plot(range(WEAPvsUSGS$YrMon), range(c(WEAPvsUSGS$WEAP, WEAPvsUSGS$USGS), na.rm = T), type='n', xlab = "", ylab = "Total Flow (Millions Cubic Meters)", main = paste("WEAP output vs USGS", gauges2plot[i]))
  plot(range(WEAPvsUSGS$YrMon), range(c(WEAPvsUSGS$WEAP, WEAPvsUSGS$USGS), na.rm = T), type='n', xlab = "", ylab = "Total Flow [Millions CF/month]", main = paste("WEAP estimate vs ", ID, sep = ""))
  lines(WEAPvsUSGS$YrMon, WEAPvsUSGS$WEAP, col="red")
  lines(WEAPvsUSGS$YrMon, WEAPvsUSGS$USGS, col="blue")
  legend("topright", 
         c("Simulated","Observed"), 
         lty=c(1,1),
         lwd=c(2.5,2.5),,
         col=c("red","blue")) 
  dev.off()
}

## Monthly precipitation of all the stations in 'x'
## Not run: 

## Sum of the monthly values in each station of 'x'
z <- zoo( x[, 2:ncol(x)], dates)

# Amount of years in 'x' (needed for computing the average)
nyears <- yip(from=start(z), to=end(z), out.type="nmbr" )

m <- monthlyfunction(z, FUN=sum)


## Another way of computing the sum of the monthly values in each station of 'x'
## This way is usefult for posteriori boxplots
m2 <- monthlyfunction(x, FUN=sum, dates=1, out.type="db")

## Average monthly precipitation in each station of 'x'
m2$Value <- m2$Value / nyears 

## Creating monthly factors
m2$Month <- factor(m2$Month, levels=month.abb)

## boxplot of the monthly values in all stations
boxplot(Value ~ Month, m2, col="lightyellow", main="Monthly Precipitation, [mm/month]")

## End(Not run)



ggplot(nxtplot, aes(Date)) + 
  geom_line(aes(y = nxtplot[,2], colour = "y")) + 
  geom_line(aes(y = nxtplot[,3], colour = "z"))

require(lattice)
xyplot(nxtplot[,2] + nxtplot[,3] ~ Date, data=nxtplot, type = c('l','l'), col = c("blue", "red"), auto.key=T)

g_range = range(nxtplot[,2:3])
plot(nxtplot$Date, nxtplot[,2], type="l", lwd=2, col="blue", ylim=g_range, xaxs="i")
lines(nxtplot$Date, nxtplot[,3], lwd=2, col="red")
legend("topright", legend=c("y","z"), lwd=c(2,2), col=c("blue","red"), inset = .05)

WEAPvsUSGS$YrMon <- as.Date(WEAPvsUSGS$YrMon, format = "%Y-%m-%d")
mainDir <- "."
subDir <- paste("output_R/sim_vs_obs_plots", title, dtstamp, sep = "_")
if (!file_test("-d", file.path(mainDir, subDir))) {
  if(file_test("-f", file.path(mainDir, subDir))) {
    stop ("Plots will be stored in ", subDir)
  } else {
    dir.create(file.path(mainDir, subDir))
  }
}
ID <- gauge_IDs[i]
png(filename = paste(mainDir, "/", subDir, "/", ID, ".png", sep =""), width = 960, height = 600, units = "px", pointsize = 12)
# plot(range(WEAPvsUSGS$YrMon), range(c(WEAPvsUSGS$WEAP, WEAPvsUSGS$USGS), na.rm = T), type='n', xlab = "", ylab = "Total Flow (Millions Cubic Meters)", main = paste("WEAP output vs USGS", gauges2plot[i]))
plot(range(WEAPvsUSGS$YrMon), range(c(WEAPvsUSGS$WEAP, WEAPvsUSGS$USGS), na.rm = T), type='n', xlab = "", ylab = "Total Flow [Millions CF/month]", main = paste("WEAP estimate vs ", ID, sep = ""))
lines(WEAPvsUSGS$YrMon, WEAPvsUSGS$WEAP, col="red")
lines(WEAPvsUSGS$YrMon, WEAPvsUSGS$USGS, col="blue")
legend("topright", 
       c("Simulated","Observed"), 
       lty=c(1,1),
       lwd=c(2.5,2.5),,
       col=c("red","blue")) 
dev.off()
}

## Monthly precipitation of all the stations in 'x'
## Not run: 

## Sum of the monthly values in each station of 'x'
z <- zoo( x[, 2:ncol(x)], dates)

# Amount of years in 'x' (needed for computing the average)
nyears <- yip(from=start(z), to=end(z), out.type="nmbr" )

m <- monthlyfunction(z, FUN=sum)


## Another way of computing the sum of the monthly values in each station of 'x'
## This way is usefult for posteriori boxplots
m2 <- monthlyfunction(x, FUN=sum, dates=1, out.type="db")

## Average monthly precipitation in each station of 'x'
m2$Value <- m2$Value / nyears 

## Creating monthly factors
m2$Month <- factor(m2$Month, levels=month.abb)

## boxplot of the monthly values in all stations
boxplot(Value ~ Month, m2, col="lightyellow", main="Monthly Precipitation, [mm/month]")

## End(Not run)



ggplot(nxtplot, aes(Date)) + 
  geom_line(aes(y = nxtplot[,2], colour = "y")) + 
  geom_line(aes(y = nxtplot[,3], colour = "z"))

require(lattice)
xyplot(nxtplot[,2] + nxtplot[,3] ~ Date, data=nxtplot, type = c('l','l'), col = c("blue", "red"), auto.key=T)

g_range = range(nxtplot[,2:3])
plot(nxtplot$Date, nxtplot[,2], type="l", lwd=2, col="blue", ylim=g_range, xaxs="i")
lines(nxtplot$Date, nxtplot[,3], lwd=2, col="red")
legend("topright", legend=c("y","z"), lwd=c(2,2), col=c("blue","red"), inset = .05)




str(snotel_names2)
