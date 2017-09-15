## CMIP5 air temperature and precipitation figures
## Sarah Null
## 2-25-2016

setwd("C:/Sarah_USU/Hetchy-water_rights_3-25-16/CMIP5_Tuolumne/bcsd5")
getwd ()

#rm(list=ls())


#####################################################################
## ~~~ Load Data
#####################################################################

Ta_data = read.csv("Tavg_stats.csv", header = TRUE)
head(Ta_data) #show headers
dim(Ta_data)  #show dimensions of dataset
na.omit(Ta_data)

pr_data = read.csv("precip_stats.csv", header = TRUE)
head(pr_data) #show headers
dim(pr_data)  #show dimensions of dataset

#####################################################################
## ~~~ Plot air temperature for CMIP5 models and RCP scenarios
#####################################################################
#graphics.off()

# las = 1 orients y labels horizontally

windows(width = 8, height = 4)
par(mfrow = c(1,2), mar = c(4, 4, 4, 2), oma = c(0, 0, 0, 0))

plot(Ta_data$Year, Ta_data$Observed.1_8, type="l", ylab = "Annual average air temperature, C", xlab = "Year", ylim = c(6,18), las = 1, col = "black")

polygon(c(Ta_data$Year, rev(Ta_data$Year[])), c(Ta_data$X26Maximum, rev(Ta_data$X26Minimum)), col = rgb(0.69, 0.769, 0.871, 0.5), border = NA) #lightsteelblue1
polygon(c(Ta_data$Year, rev(Ta_data$Year)), c(Ta_data$X45Maximum, rev(Ta_data$X45Minimum)), col = rgb(0.961, 1, 0.98, 0.5), border = NA) #mintcreme
polygon(c(Ta_data$Year, rev(Ta_data$Year)), c(Ta_data$X60Maximum, rev(Ta_data$X60Minimum)), col = rgb(1, 0.98, 0.804, 0.5), border = NA) #lemonchiffon
polygon(c(Ta_data$Year, rev(Ta_data$Year)), c(Ta_data$X85Maximum, rev(Ta_data$X85Minimum)), col = rgb(1, 0.894, 0.882, 0.5), border = NA) # mistyrose
polygon(c(Ta_data$Year, rev(Ta_data$Year)), c(Ta_data$max_hist, rev(Ta_data$min_hist)), col = "light gray", border = NA, xlim = c(1951-2000)) 

lines(Ta_data$Year, Ta_data$X26Average, col = "dark blue", lwd = 1.5)
lines(Ta_data$Year, Ta_data$X45Average, col = "skyblue3", lwd = 1.5)
lines(Ta_data$Year, Ta_data$X60Average, col = "orange", lwd = 1.5)
lines(Ta_data$Year, Ta_data$X85Average, col = "red", lwd = 1.5)
lines(Ta_data$Year, Ta_data$Observed.1_8, col = "black", lwd = 2)

legend("topleft", col=c("black", "red", "orange", "skyblue3", "dark blue"), legend=c("Modeled Historical", "RCP 8.5", "RCP 6.0", "RCP 4.5", "RCP 2.6"), 
       lwd = c(2, 1, 1, 1, 1), ncol=1, bty="n", lty=1, pt.cex = 1, cex = 0.9)


#####################################################################
## ~~~ Plot precipitation for CMIP5 models and RCP scenarios
#####################################################################

plot(pr_data$Year, pr_data$pr.Observed.1_8, type="l", ylab = "Annual precipitation, cm", xlab = "Year", ylim = c(30,330), las = 1, col = "black")

polygon(c(pr_data$Year, rev(pr_data$Year)), c(pr_data$pr.26Maximum, rev(pr_data$pr.26Minimum)), col = rgb(0.69, 0.769, 0.871, 0.5), border = NA) #lightsteelblue1
polygon(c(pr_data$Year, rev(pr_data$Year)), c(pr_data$pr.45Maximum, rev(pr_data$pr.45Minimum)), col = rgb(0.961, 1, 0.98, 0.5), border = NA) #mintcreme
polygon(c(pr_data$Year, rev(pr_data$Year)), c(pr_data$pr.60Maximum, rev(pr_data$pr.60Minimum)), col = rgb(1, 0.98, 0.804, 0.5), border = NA) #lemonchiffon
polygon(c(pr_data$Year, rev(pr_data$Year)), c(pr_data$pr.85Maximum, rev(pr_data$pr.85Minimum)), col = rgb(1, 0.894, 0.882, 0.5), border = NA) # mistyrose
polygon(c(pr_data$Year, rev(pr_data$Year)), c(pr_data$max_hist, rev(pr_data$min_hist)), col = "light gray", border = NA, xlim = c(1951-2000)) 

lines(pr_data$Year, pr_data$pr.26Average, col = "dark blue", lwd = 1.5)
lines(pr_data$Year, pr_data$pr.45Average, col = "skyblue3", lwd = 1.5)
lines(pr_data$Year, pr_data$pr.60Average, col = "orange", lwd = 1.5)
lines(pr_data$Year, pr_data$pr.85Average, col = "red", lwd = 1.5)
lines(pr_data$Year, pr_data$pr.Observed.1_8, col = "black", lwd = 2)

legend("topleft", col=c("black", "red", "orange", "skyblue3", "dark blue"), legend=c("Modeled Historical", "RCP 8.5", "RCP 6.0", "RCP 4.5", "RCP 2.6"), 
       lwd = c(2, 1, 1, 1, 1), ncol=1, bty="n", lty=1, pt.cex = 1, cex = 0.9)

