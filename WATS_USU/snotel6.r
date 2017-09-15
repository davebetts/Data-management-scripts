### former parts of snotel4.r

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

### Now back to snotel6.r



## Loading the monthly time series of precipitation within the Ebro River basin.
data(EbroPPtsMonthly)
x <- EbroPPtsMonthly

## Dates of 'x'
dates <- as.Date(x[,1])

## Monthly precipitation of all the stations in 'x'
## Not run: 

## Sum of the monthly values in each station of 'x'
z <- zoo( x[, 2:ncol(x)], dates)

# Amount of years in 'x' (needed for computing the average)
nyears <- yip(from=start(z), to=end(z), out.type="nmbr" )

m <- monthlyfunction(z, FUN=sum)


## Another way of computing the sum of the monthly values in each station of 'x'
## This way is usefult for posteriori boxplots
m2 <- monthlyfunction(x, FUN=mean, dates=1, date.fmt = "%m/%d/%Y", out.type="data.frame")

## Average monthly precipitation in each station of 'x'
m2$Value <- m2$Value / nyears 

## Creating monthly factors
m2$Month <- factor(m2$Month, levels=month.abb)

## boxplot of the monthly values in all stations
boxplot(Value ~ Month + StationID, m2, col="lightyellow", main="Monthly Precipitation, [mm/month]")

## End(Not run)

unq <- unique(m2[,1])
unq
for (i in unq) {
  boxplot(Value ~ Month, m2[which(m2$StationID == i),], col="lightyellow", main="Monthly Precipitation, [mm/month]")
}


snw <- aggregate(.~ Month, data = snotel, FUN = mean, na.rm = TRUE, na.action = NULL)
