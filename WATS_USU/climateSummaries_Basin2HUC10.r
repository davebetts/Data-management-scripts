rm(list=ls())
hist <- read.csv("basintemp_hist.csv")
drops <- c("FID","Join_Count","TARGET_FID","OBJECTID","LoadDate","Shape_Leng","Shape_Le_1","Shape_Area","area","Lon","Lat","areakm2_db","Lat_djb")
hist1 <- hist[,!(names(hist) %in% drops)]
hist2 <- t(hist1[,-1])
hist2 <- as.data.frame(hist2)
colnames(hist2) <- as.character(hist1$Basin)

basintemps <- sapply(hist2,fivenum)
basintemps  <- basintemps [-c(2,4),]
row.names(basintemps ) <- c('Minimum', 'Average', 'Maximum')
dates <- rownames(hist2)
dates <- gsub("t","",dates)
dates <- as.Date(dates, format = "%Y%m%d")
yr <-  as.numeric(format.Date(dates,"%Y"))
mon <- as.numeric(format.Date(dates,"%m"))
hist3 <- hist2
hist3$year <- yr
hist3$month <- mon

basintemps_yr <- aggregate(.~ year, data = hist3, mean)
basintemps_yr <- basintemps_yr[,-c(ncol(basintemps_yr))]
basintemps_mon <- aggregate(.~ month, data = hist3, mean)
basintemps_mon <- basintemps_mon[,-c(ncol(basintemps_mon))]

write.csv(basintemps, "Summaryt_basin_hist.csv")
write.csv(basintemps_yr, "AnnualTemps_basin_hist.csv", row.names = F)
write.csv(basintemps_mon, "MonthlyTemps_basin_hist.csv", row.names = F)

rm(list=ls())
proj <- read.csv("basintemp_proj.csv")
drops <- c("FID","Join_Count","TARGET_FID","OBJECTID","LoadDate","Shape_Leng","Shape_Le_1","Shape_Area","area","Lon","Lat","areakm2_db","Lat_djb")
proj1 <- proj[,!(names(proj) %in% drops)]
proj2 <- t(proj1[,-1])
proj2 <- as.data.frame(proj2)
colnames(proj2) <- as.character(proj1$Basin)

basintemps <- sapply(proj2,fivenum)
basintemps  <- basintemps [-c(2,4),]
row.names(basintemps ) <- c('Minimum', 'Average', 'Maximum')
dates <- rownames(proj2)
dates <- gsub("t","",dates)
dates <- as.Date(dates, format = "%Y%m%d")
yr <-  as.numeric(format.Date(dates,"%Y"))
mon <- as.numeric(format.Date(dates,"%m"))
proj3 <- proj2
proj3$year <- yr
proj3$month <- mon

basintemps_yr <- aggregate(.~ year, data = proj3, mean)
basintemps_yr <- basintemps_yr[,-c(ncol(basintemps_yr))]
basintemps_mon <- aggregate(.~ month, data = proj3, mean)
basintemps_mon <- basintemps_mon[,-c(ncol(basintemps_mon))]

write.csv(basintemps, "Summaryt_basin_proj.csv")
write.csv(basintemps_yr, "AnnualTemps_basin_proj.csv", row.names = F)
write.csv(basintemps_mon, "MonthlyTemps_basin_proj.csv", row.names = F)

rm(list=ls())
hist <- read.csv("basinp_hist.csv")
drops <- c("FID","Join_Count","TARGET_FID","OBJECTID","LoadDate","Shape_Leng","Shape_Le_1","Shape_Area","area","Lon","Lat","areakm2_db","Lat_djb")
hist1 <- hist[,!(names(hist) %in% drops)]
hist2 <- t(hist1[,-1])
hist2 <- as.data.frame(hist2)
colnames(hist2) <- as.character(hist1$Basin)

dates <- rownames(hist2)
dates <- gsub("p","",dates)
dates <- as.Date(dates, format = "%Y%m%d")
yr <-  as.numeric(format.Date(dates,"%Y"))
mon <- as.numeric(format.Date(dates,"%m"))
hist3 <- hist2
hist3$year <- yr
hist3$month <- mon

basinp_yr <- aggregate(.~ year, data = hist3, sum)
basinp_yr <- basinp_yr[,-c(ncol(basinp_yr))]
basinp <- sapply(basinp_yr,fivenum)
basinp <- basinp[-c(2,4),-1]
row.names(basinp)<-c("Minimum","Average","Maximum")
basinp_mon <- aggregate(.~ month, data = hist3, mean)
basinp_mon <- basinp_mon[,-c(ncol(basinp_mon))]

write.csv(basinp, "Summaryp_basin_hist.csv")
write.csv(basinp_yr, "Annualp_basin_hist.csv", row.names = F)
write.csv(basinp_mon, "Monthlyp_basin_hist.csv", row.names = F)

rm(list=ls())
proj <- read.csv("basinp_proj.csv")
drops <- c("FID","Join_Count","TARGET_FID","OBJECTID","LoadDate","Shape_Leng","Shape_Le_1","Shape_Area","area","Lon","Lat","areakm2_db","Lat_djb")
proj1 <- proj[,!(names(proj) %in% drops)]
proj2 <- t(proj1[,-1])
proj2 <- as.data.frame(proj2)
colnames(proj2) <- as.character(proj1$Basin)

dates <- rownames(proj2)
dates <- gsub("p","",dates)
dates <- as.Date(dates, format = "%Y%m%d")
yr <-  as.numeric(format.Date(dates,"%Y"))
mon <- as.numeric(format.Date(dates,"%m"))
proj3 <- proj2
proj3$year <- yr
proj3$month <- mon

basinp_yr <- aggregate(.~ year, data = proj3, sum)
basinp_yr <- basinp_yr[,-c(ncol(basinp_yr))]
basinp <- sapply(basinp_yr,fivenum)
basinp <- basinp[-c(2,4),-1]
row.names(basinp)<-c("Minimum","Average","Maximum")
basinp_mon <- aggregate(.~ month, data = proj3, mean)
basinp_mon <- basinp_mon[,-c(ncol(basinp_mon))]

write.csv(basinp, "Summaryp_basin_proj.csv")
write.csv(basinp_yr, "Annualp_basin_proj.csv", row.names = F)
write.csv(basinp_mon, "Monthlyp_basin_proj.csv", row.names = F)

rm(list=ls())
hist <- read.csv("basinr_hist.csv")
drops <- c("FID","Join_Count","TARGET_FID","OBJECTID","LoadDate","Shape_Leng","Shape_Le_1","Shape_Area","area","Lon","Lat","areakm2_db","Lat_djb")
hist1 <- hist[,!(names(hist) %in% drops)]
hist2 <- t(hist1[,-1])
hist2 <- as.data.frame(hist2)
colnames(hist2) <- as.character(hist1$Basin)

basinr <- sapply(hist2,fivenum)
basinr  <- basinr [-c(2,4),]
row.names(basinr ) <- c('Minimum', 'Average', 'Maximum')
dates <- rownames(hist2)
dates <- gsub("r","",dates)
dates <- as.Date(dates, format = "%Y%m%d")
yr <-  as.numeric(format.Date(dates,"%Y"))
mon <- as.numeric(format.Date(dates,"%m"))
hist3 <- hist2
hist3$year <- yr
hist3$month <- mon

basinr_yr <- aggregate(.~ year, data = hist3, mean)
basinr_yr <- basinr_yr[,-c(ncol(basinr_yr))]
basinr_mon <- aggregate(.~ month, data = hist3, mean)
basinr_mon <- basinr_mon[,-c(ncol(basinr_mon))]

write.csv(basinr, "Summaryr_basin_hist.csv")
write.csv(basinr_yr, "Annualr_basin_hist.csv", row.names = F)
write.csv(basinr_mon, "Monthlyr_basin_hist.csv", row.names = F)

rm(list=ls())
proj <- read.csv("basinr_proj.csv")
drops <- c("FID","Join_Count","TARGET_FID","OBJECTID","LoadDate","Shape_Leng","Shape_Le_1","Shape_Area","area","Lon","Lat","areakm2_db","Lat_djb")
proj1 <- proj[,!(names(proj) %in% drops)]
proj2 <- t(proj1[,-1])
proj2 <- as.data.frame(proj2)
colnames(proj2) <- as.character(proj1$Basin)

basinr <- sapply(proj2,fivenum)
basinr  <- basinr [-c(2,4),]
row.names(basinr ) <- c('Minimum', 'Average', 'Maximum')
basinr <- sapply(proj2,fivenum)
basinr  <- basinr [-c(2,4),]
row.names(basinr ) <- c('Minimum', 'Average', 'Maximum')
dates <- rownames(proj2)
dates <- gsub("r","",dates)
dates <- as.Date(dates, format = "%Y%m%d")
yr <-  as.numeric(format.Date(dates,"%Y"))
mon <- as.numeric(format.Date(dates,"%m"))
proj3 <- proj2
proj3$year <- yr
proj3$month <- mon

basinr_yr <- aggregate(.~ year, data = proj3, mean)
basinr_yr <- basinr_yr[,-c(ncol(basinr_yr))]
basinr_mon <- aggregate(.~ month, data = proj3, mean)
basinr_mon <- basinr_mon[,-c(ncol(basinr_mon))]

write.csv(basinr, "Summaryr_basin_proj.csv")
write.csv(basinr_yr, "Annualr_basin_proj.csv", row.names = F)
write.csv(basinr_mon, "Monthlyr_basin_proj.csv", row.names = F)

rm(list=ls())
hist <- read.csv("HUC06t_hist.csv")
drops <- c("FID","Join_Count","TARGET_FID","OBJECTID","LoadDate","Shape_Leng","Shape_Le_1","Shape_Area","area","Lon","Lat","areakm2_db","Lat_djb")
hist1 <- hist[,!(names(hist) %in% drops)]
hist2 <- t(hist1[,-1])
hist2 <- as.data.frame(hist2)
colnames(hist2) <- as.character(hist1$HUC_6)

HUC06temps <- sapply(hist2,fivenum)
HUC06temps  <- HUC06temps [-c(2,4),]
row.names(HUC06temps ) <- c('Minimum', 'Average', 'Maximum')
dates <- rownames(hist2)
dates <- gsub("t","",dates)
dates <- as.Date(dates, format = "%Y%m%d")
yr <-  as.numeric(format.Date(dates,"%Y"))
mon <- as.numeric(format.Date(dates,"%m"))
hist3 <- hist2
hist3$year <- yr
hist3$month <- mon

HUC06temps_yr <- aggregate(.~ year, data = hist3, mean)
HUC06temps_yr <- HUC06temps_yr[,-c(ncol(HUC06temps_yr))]
HUC06temps_mon <- aggregate(.~ month, data = hist3, mean)
HUC06temps_mon <- HUC06temps_mon[,-c(ncol(HUC06temps_mon))]

write.csv(HUC06temps, "Summaryt_HUC06_hist.csv")
write.csv(HUC06temps_yr, "AnnualTemps_HUC06_hist.csv", row.names = F)
write.csv(HUC06temps_mon, "MonthlyTemps_HUC06_hist.csv", row.names = F)

rm(list=ls())
proj <- read.csv("HUC06t_proj.csv")
drops <- c("FID","Join_Count","TARGET_FID","OBJECTID","LoadDate","Shape_Leng","Shape_Le_1","Shape_Area","area","Lon","Lat","areakm2_db","Lat_djb")
proj1 <- proj[,!(names(proj) %in% drops)]
proj2 <- t(proj1[,-1])
proj2 <- as.data.frame(proj2)
colnames(proj2) <- as.character(proj1$HUC_6)

HUC06temps <- sapply(proj2,fivenum)
HUC06temps  <- HUC06temps [-c(2,4),]
row.names(HUC06temps ) <- c('Minimum', 'Average', 'Maximum')
dates <- rownames(proj2)
dates <- gsub("t","",dates)
dates <- as.Date(dates, format = "%Y%m%d")
yr <-  as.numeric(format.Date(dates,"%Y"))
mon <- as.numeric(format.Date(dates,"%m"))
proj3 <- proj2
proj3$year <- yr
proj3$month <- mon

HUC06temps_yr <- aggregate(.~ year, data = proj3, mean)
HUC06temps_yr <- HUC06temps_yr[,-c(ncol(HUC06temps_yr))]
HUC06temps_mon <- aggregate(.~ month, data = proj3, mean)
HUC06temps_mon <- HUC06temps_mon[,-c(ncol(HUC06temps_mon))]

write.csv(HUC06temps, "Summaryt_HUC06_proj.csv")
write.csv(HUC06temps_yr, "AnnualTemps_HUC06_proj.csv", row.names = F)
write.csv(HUC06temps_mon, "MonthlyTemps_HUC06_proj.csv", row.names = F)

rm(list=ls())
hist <- read.csv("HUC06p_hist.csv")
drops <- c("FID","Join_Count","TARGET_FID","OBJECTID","LoadDate","Shape_Leng","Shape_Le_1","Shape_Area","area","Lon","Lat","areakm2_db","Lat_djb")
hist1 <- hist[,!(names(hist) %in% drops)]
hist2 <- t(hist1[,-1])
hist2 <- as.data.frame(hist2)
colnames(hist2) <- as.character(hist1$HUC_6)

dates <- rownames(hist2)
dates <- gsub("p","",dates)
dates <- as.Date(dates, format = "%Y%m%d")
yr <-  as.numeric(format.Date(dates,"%Y"))
mon <- as.numeric(format.Date(dates,"%m"))
hist3 <- hist2
hist3$year <- yr
hist3$month <- mon

HUC06p_yr <- aggregate(.~ year, data = hist3, sum)
HUC06p_yr <- HUC06p_yr[,-c(ncol(HUC06p_yr))]
HUC06p <- sapply(HUC06p_yr,fivenum)
HUC06p <- HUC06p[-c(2,4),-1]
row.names(HUC06p)<-c("Minimum","Average","Maximum")
HUC06p_mon <- aggregate(.~ month, data = hist3, mean)
HUC06p_mon <- HUC06p_mon[,-c(ncol(HUC06p_mon))]

write.csv(HUC06p, "Summaryp_HUC06_hist.csv")
write.csv(HUC06p_yr, "Annualp_HUC06_hist.csv", row.names = F)
write.csv(HUC06p_mon, "Monthlyp_HUC06_hist.csv", row.names = F)

rm(list=ls())
proj <- read.csv("HUC06p_proj.csv")
drops <- c("FID","Join_Count","TARGET_FID","OBJECTID","LoadDate","Shape_Leng","Shape_Le_1","Shape_Area","area","Lon","Lat","areakm2_db","Lat_djb")
proj1 <- proj[,!(names(proj) %in% drops)]
proj2 <- t(proj1[,-1])
proj2 <- as.data.frame(proj2)
colnames(proj2) <- as.character(proj1$HUC_6)

dates <- rownames(proj2)
dates <- gsub("p","",dates)
dates <- as.Date(dates, format = "%Y%m%d")
yr <-  as.numeric(format.Date(dates,"%Y"))
mon <- as.numeric(format.Date(dates,"%m"))
proj3 <- proj2
proj3$year <- yr
proj3$month <- mon

HUC06p_yr <- aggregate(.~ year, data = proj3, sum)
HUC06p_yr <- HUC06p_yr[,-c(ncol(HUC06p_yr))]
HUC06p <- sapply(HUC06p_yr,fivenum)
HUC06p <- HUC06p[-c(2,4),-1]
row.names(HUC06p)<-c("Minimum","Average","Maximum")
HUC06p_mon <- aggregate(.~ month, data = proj3, mean)
HUC06p_mon <- HUC06p_mon[,-c(ncol(HUC06p_mon))]

write.csv(HUC06p, "Summaryp_HUC06_proj.csv")
write.csv(HUC06p_yr, "Annualp_HUC06_proj.csv", row.names = F)
write.csv(HUC06p_mon, "Monthlyp_HUC06_proj.csv", row.names = F)

rm(list=ls())
hist <- read.csv("HUC06r_hist.csv")
drops <- c("FID","Join_Count","TARGET_FID","OBJECTID","LoadDate","Shape_Leng","Shape_Le_1","Shape_Area","area","Lon","Lat","areakm2_db","Lat_djb")
hist1 <- hist[,!(names(hist) %in% drops)]
hist2 <- t(hist1[,-1])
hist2 <- as.data.frame(hist2)
colnames(hist2) <- as.character(hist1$HUC_6)

HUC06r <- sapply(hist2,fivenum)
HUC06r  <- HUC06r [-c(2,4),]
row.names(HUC06r ) <- c('Minimum', 'Average', 'Maximum')
dates <- rownames(hist2)
dates <- gsub("r","",dates)
dates <- as.Date(dates, format = "%Y%m%d")
yr <-  as.numeric(format.Date(dates,"%Y"))
mon <- as.numeric(format.Date(dates,"%m"))
hist3 <- hist2
hist3$year <- yr
hist3$month <- mon

HUC06r_yr <- aggregate(.~ year, data = hist3, mean)
HUC06r_yr <- HUC06r_yr[,-c(ncol(HUC06r_yr))]
HUC06r_mon <- aggregate(.~ month, data = hist3, mean)
HUC06r_mon <- HUC06r_mon[,-c(ncol(HUC06r_mon))]

write.csv(HUC06r, "Summaryr_HUC06_hist.csv")
write.csv(HUC06r_yr, "Annualr_HUC06_hist.csv", row.names = F)
write.csv(HUC06r_mon, "Monthlyr_HUC06_hist.csv", row.names = F)

rm(list=ls())
proj <- read.csv("HUC06r_proj.csv")
drops <- c("FID","Join_Count","TARGET_FID","OBJECTID","LoadDate","Shape_Leng","Shape_Le_1","Shape_Area","area","Lon","Lat","areakm2_db","Lat_djb")
proj1 <- proj[,!(names(proj) %in% drops)]
proj2 <- t(proj1[,-1])
proj2 <- as.data.frame(proj2)
colnames(proj2) <- as.character(proj1$HUC_6)

HUC06r <- sapply(proj2,fivenum)
HUC06r  <- HUC06r [-c(2,4),]
row.names(HUC06r ) <- c('Minimum', 'Average', 'Maximum')
dates <- rownames(proj2)
dates <- gsub("r","",dates)
dates <- as.Date(dates, format = "%Y%m%d")
yr <-  as.numeric(format.Date(dates,"%Y"))
mon <- as.numeric(format.Date(dates,"%m"))
proj3 <- proj2
proj3$year <- yr
proj3$month <- mon

HUC06r_yr <- aggregate(.~ year, data = proj3, mean)
HUC06r_yr <- HUC06r_yr[,-c(ncol(HUC06r_yr))]
HUC06r_mon <- aggregate(.~ month, data = proj3, mean)
HUC06r_mon <- HUC06r_mon[,-c(ncol(HUC06r_mon))]

write.csv(HUC06r, "Summaryr_HUC06_proj.csv")
write.csv(HUC06r_yr, "Annualr_HUC06_proj.csv", row.names = F)
write.csv(HUC06r_mon, "Monthlyr_HUC06_proj.csv", row.names = F)

rm(list=ls())
hist <- read.csv("HUC08t_hist.csv")
drops <- c("FID","Join_Count","TARGET_FID","OBJECTID","LoadDate","Shape_Leng","Shape_Le_1","Shape_Area","area","Lon","Lat","areakm2_db","Lat_djb")
hist1 <- hist[,!(names(hist) %in% drops)]
hist2 <- t(hist1[,-1])
hist2 <- as.data.frame(hist2)
colnames(hist2) <- as.character(hist1$HUC_8)

HUC08temps <- sapply(hist2,fivenum)
HUC08temps  <- HUC08temps [-c(2,4),]
row.names(HUC08temps ) <- c('Minimum', 'Average', 'Maximum')
dates <- rownames(hist2)
dates <- gsub("t","",dates)
dates <- as.Date(dates, format = "%Y%m%d")
yr <-  as.numeric(format.Date(dates,"%Y"))
mon <- as.numeric(format.Date(dates,"%m"))
hist3 <- hist2
hist3$year <- yr
hist3$month <- mon

HUC08temps_yr <- aggregate(.~ year, data = hist3, mean)
HUC08temps_yr <- HUC08temps_yr[,-c(ncol(HUC08temps_yr))]
HUC08temps_mon <- aggregate(.~ month, data = hist3, mean)
HUC08temps_mon <- HUC08temps_mon[,-c(ncol(HUC08temps_mon))]

write.csv(HUC08temps, "Summaryt_HUC08_hist.csv")
write.csv(HUC08temps_yr, "AnnualTemps_HUC08_hist.csv", row.names = F)
write.csv(HUC08temps_mon, "MonthlyTemps_HUC08_hist.csv", row.names = F)

rm(list=ls())
proj <- read.csv("HUC08t_proj.csv")
drops <- c("FID","Join_Count","TARGET_FID","OBJECTID","LoadDate","Shape_Leng","Shape_Le_1","Shape_Area","area","Lon","Lat","areakm2_db","Lat_djb")
proj1 <- proj[,!(names(proj) %in% drops)]
proj2 <- t(proj1[,-1])
proj2 <- as.data.frame(proj2)
colnames(proj2) <- as.character(proj1$HUC_8)

HUC08temps <- sapply(proj2,fivenum)
HUC08temps  <- HUC08temps [-c(2,4),]
row.names(HUC08temps ) <- c('Minimum', 'Average', 'Maximum')
dates <- rownames(proj2)
dates <- gsub("t","",dates)
dates <- as.Date(dates, format = "%Y%m%d")
yr <-  as.numeric(format.Date(dates,"%Y"))
mon <- as.numeric(format.Date(dates,"%m"))
proj3 <- proj2
proj3$year <- yr
proj3$month <- mon

HUC08temps_yr <- aggregate(.~ year, data = proj3, mean)
HUC08temps_yr <- HUC08temps_yr[,-c(ncol(HUC08temps_yr))]
HUC08temps_mon <- aggregate(.~ month, data = proj3, mean)
HUC08temps_mon <- HUC08temps_mon[,-c(ncol(HUC08temps_mon))]

write.csv(HUC08temps, "Summaryt_HUC08_proj.csv")
write.csv(HUC08temps_yr, "AnnualTemps_HUC08_proj.csv", row.names = F)
write.csv(HUC08temps_mon, "MonthlyTemps_HUC08_proj.csv", row.names = F)

rm(list=ls())
hist <- read.csv("HUC08p_hist.csv")
drops <- c("FID","Join_Count","TARGET_FID","OBJECTID","LoadDate","Shape_Leng","Shape_Le_1","Shape_Area","area","Lon","Lat","areakm2_db","Lat_djb")
hist1 <- hist[,!(names(hist) %in% drops)]
hist2 <- t(hist1[,-1])
hist2 <- as.data.frame(hist2)
colnames(hist2) <- as.character(hist1$HUC_8)

dates <- rownames(hist2)
dates <- gsub("p","",dates)
dates <- as.Date(dates, format = "%Y%m%d")
yr <-  as.numeric(format.Date(dates,"%Y"))
mon <- as.numeric(format.Date(dates,"%m"))
hist3 <- hist2
hist3$year <- yr
hist3$month <- mon

HUC08p_yr <- aggregate(.~ year, data = hist3, sum)
HUC08p_yr <- HUC08p_yr[,-c(ncol(HUC08p_yr))]
HUC08p <- sapply(HUC08p_yr,fivenum)
HUC08p <- HUC08p[-c(2,4),-1]
row.names(HUC08p)<-c("Minimum","Average","Maximum")
HUC08p_mon <- aggregate(.~ month, data = hist3, mean)
HUC08p_mon <- HUC08p_mon[,-c(ncol(HUC08p_mon))]

write.csv(HUC08p, "Summaryp_HUC08_hist.csv")
write.csv(HUC08p_yr, "Annualp_HUC08_hist.csv", row.names = F)
write.csv(HUC08p_mon, "Monthlyp_HUC08_hist.csv", row.names = F)

rm(list=ls())
proj <- read.csv("HUC08p_proj.csv")
drops <- c("FID","Join_Count","TARGET_FID","OBJECTID","LoadDate","Shape_Leng","Shape_Le_1","Shape_Area","area","Lon","Lat","areakm2_db","Lat_djb")
proj1 <- proj[,!(names(proj) %in% drops)]
proj2 <- t(proj1[,-1])
proj2 <- as.data.frame(proj2)
colnames(proj2) <- as.character(proj1$HUC_8)

dates <- rownames(proj2)
dates <- gsub("p","",dates)
dates <- as.Date(dates, format = "%Y%m%d")
yr <-  as.numeric(format.Date(dates,"%Y"))
mon <- as.numeric(format.Date(dates,"%m"))
proj3 <- proj2
proj3$year <- yr
proj3$month <- mon

HUC08p_yr <- aggregate(.~ year, data = proj3, sum)
HUC08p_yr <- HUC08p_yr[,-c(ncol(HUC08p_yr))]
HUC08p <- sapply(HUC08p_yr,fivenum)
HUC08p <- HUC08p[-c(2,4),-1]
row.names(HUC08p)<-c("Minimum","Average","Maximum")
HUC08p_mon <- aggregate(.~ month, data = proj3, mean)
HUC08p_mon <- HUC08p_mon[,-c(ncol(HUC08p_mon))]

write.csv(HUC08p, "Summaryp_HUC08_proj.csv")
write.csv(HUC08p_mon, "Monthlyp_HUC08_proj.csv", row.names = F)
write.csv(HUC08p_yr, "Annualp_HUC08_proj.csv", row.names = F)

rm(list=ls())
hist <- read.csv("HUC08r_hist.csv")
drops <- c("FID","Join_Count","TARGET_FID","OBJECTID","LoadDate","Shape_Leng","Shape_Le_1","Shape_Area","area","Lon","Lat","areakm2_db","Lat_djb")
hist1 <- hist[,!(names(hist) %in% drops)]
hist2 <- t(hist1[,-1])
hist2 <- as.data.frame(hist2)
colnames(hist2) <- as.character(hist1$HUC_8)

HUC08r <- sapply(hist2,fivenum)
HUC08r  <- HUC08r [-c(2,4),]
row.names(HUC08r ) <- c('Minimum', 'Average', 'Maximum')
dates <- rownames(hist2)
dates <- gsub("r","",dates)
dates <- as.Date(dates, format = "%Y%m%d")
yr <-  as.numeric(format.Date(dates,"%Y"))
mon <- as.numeric(format.Date(dates,"%m"))
hist3 <- hist2
hist3$year <- yr
hist3$month <- mon

HUC08r_yr <- aggregate(.~ year, data = hist3, mean)
HUC08r_yr <- HUC08r_yr[,-c(ncol(HUC08r_yr))]
HUC08r_mon <- aggregate(.~ month, data = hist3, mean)
HUC08r_mon <- HUC08r_mon[,-c(ncol(HUC08r_mon))]

write.csv(HUC08r, "Summaryr_HUC08_hist.csv")
write.csv(HUC08r_yr, "Annualr_HUC08_hist.csv", row.names = F)
write.csv(HUC08r_mon, "Monthlyr_HUC08_hist.csv", row.names = F)

rm(list=ls())
proj <- read.csv("HUC08r_proj.csv")
drops <- c("FID","Join_Count","TARGET_FID","OBJECTID","LoadDate","Shape_Leng","Shape_Le_1","Shape_Area","area","Lon","Lat","areakm2_db","Lat_djb")
proj1 <- proj[,!(names(proj) %in% drops)]
proj2 <- t(proj1[,-1])
proj2 <- as.data.frame(proj2)
colnames(proj2) <- as.character(proj1$HUC_8)

HUC08r <- sapply(proj2,fivenum)
HUC08r  <- HUC08r [-c(2,4),]
row.names(HUC08r ) <- c('Minimum', 'Average', 'Maximum')
dates <- rownames(proj2)
dates <- gsub("r","",dates)
dates <- as.Date(dates, format = "%Y%m%d")
yr <-  as.numeric(format.Date(dates,"%Y"))
mon <- as.numeric(format.Date(dates,"%m"))
proj3 <- proj2
proj3$year <- yr
proj3$month <- mon

HUC08r_yr <- aggregate(.~ year, data = proj3, mean)
HUC08r_yr <- HUC08r_yr[,-c(ncol(HUC08r_yr))]
HUC08r_mon <- aggregate(.~ month, data = proj3, mean)
HUC08r_mon <- HUC08r_mon[,-c(ncol(HUC08r_mon))]

write.csv(HUC08r, "Summaryr_HUC08_proj.csv")
write.csv(HUC08r_yr, "Annualr_HUC08_proj.csv", row.names = F)
write.csv(HUC08r_mon, "Monthlyr_HUC08_proj.csv", row.names = F)

rm(list=ls())
hist <- read.csv("HUC10t_hist.csv")
drops <- c("FID","Join_Count","TARGET_FID","OBJECTID","LoadDate","Shape_Leng","Shape_Le_1","Shape_Area","area","Lon","Lat","areakm2_db","Lat_djb")
hist1 <- hist[,!(names(hist) %in% drops)]
hist2 <- t(hist1[,-1])
hist2 <- as.data.frame(hist2)
colnames(hist2) <- as.character(hist1$HUC10)

HUC10temps <- sapply(hist2,fivenum)
HUC10temps  <- HUC10temps [-c(2,4),]
row.names(HUC10temps ) <- c('Minimum', 'Average', 'Maximum')
dates <- rownames(hist2)
dates <- gsub("t","",dates)
dates <- as.Date(dates, format = "%Y%m%d")
yr <-  as.numeric(format.Date(dates,"%Y"))
mon <- as.numeric(format.Date(dates,"%m"))
hist3 <- hist2
hist3$year <- yr
hist3$month <- mon

HUC10temps_yr <- aggregate(.~ year, data = hist3, mean)
HUC10temps_yr <- HUC10temps_yr[,-c(ncol(HUC10temps_yr))]
HUC10temps_mon <- aggregate(.~ month, data = hist3, mean)
HUC10temps_mon <- HUC10temps_mon[,-c(ncol(HUC10temps_mon))]

write.csv(HUC10temps, "Summaryt_HUC10_hist.csv")
write.csv(HUC10temps_yr, "AnnualTemps_HUC10_hist.csv", row.names = F)
write.csv(HUC10temps_mon, "MonthlyTemps_HUC10_hist.csv", row.names = F)

rm(list=ls())
proj <- read.csv("HUC10t_proj.csv")
drops <- c("FID","Join_Count","TARGET_FID","OBJECTID","LoadDate","Shape_Leng","Shape_Le_1","Shape_Area","area","Lon","Lat","areakm2_db","Lat_djb")
proj1 <- proj[,!(names(proj) %in% drops)]
proj2 <- t(proj1[,-1])
proj2 <- as.data.frame(proj2)
colnames(proj2) <- as.character(proj1$HUC10)

HUC10temps <- sapply(proj2,fivenum)
HUC10temps  <- HUC10temps [-c(2,4),]
row.names(HUC10temps ) <- c('Minimum', 'Average', 'Maximum')
dates <- rownames(proj2)
dates <- gsub("t","",dates)
dates <- as.Date(dates, format = "%Y%m%d")
yr <-  as.numeric(format.Date(dates,"%Y"))
mon <- as.numeric(format.Date(dates,"%m"))
proj3 <- proj2
proj3$year <- yr
proj3$month <- mon

HUC10temps_yr <- aggregate(.~ year, data = proj3, mean)
HUC10temps_yr <- HUC10temps_yr[,-c(ncol(HUC10temps_yr))]
HUC10temps_mon <- aggregate(.~ month, data = proj3, mean)
HUC10temps_mon <- HUC10temps_mon[,-c(ncol(HUC10temps_mon))]

write.csv(HUC10temps, "Summaryt_HUC10_proj.csv")
write.csv(HUC10temps_yr, "AnnualTemps_HUC10_proj.csv", row.names = F)
write.csv(HUC10temps_mon, "MonthlyTemps_HUC10_proj.csv", row.names = F)

rm(list=ls())
hist <- read.csv("HUC10p_hist.csv")
drops <- c("FID","Join_Count","TARGET_FID","OBJECTID","LoadDate","Shape_Leng","Shape_Le_1","Shape_Area","area","Lon","Lat","areakm2_db","Lat_djb")
hist1 <- hist[,!(names(hist) %in% drops)]
hist2 <- t(hist1[,-1])
hist2 <- as.data.frame(hist2)
colnames(hist2) <- as.character(hist1$HUC10)

dates <- rownames(hist2)
dates <- gsub("p","",dates)
dates <- as.Date(dates, format = "%Y%m%d")
yr <-  as.numeric(format.Date(dates,"%Y"))
mon <- as.numeric(format.Date(dates,"%m"))
hist3 <- hist2
hist3$year <- yr
hist3$month <- mon

HUC10p_yr <- aggregate(.~ year, data = hist3, sum)
HUC10p_yr <- HUC10p_yr[,-c(ncol(HUC10p_yr))]
HUC10p <- sapply(HUC10p_yr,fivenum)
HUC10p <- HUC10p[-c(2,4),-1]
row.names(HUC10p)<-c("Minimum","Average","Maximum")
HUC10p_mon <- aggregate(.~ month, data = hist3, mean)
HUC10p_mon <- HUC10p_mon[,-c(ncol(HUC10p_mon))]

write.csv(HUC10p, "Summaryp_HUC10_hist.csv")
write.csv(HUC10p_yr, "Annualp_HUC10_hist.csv", row.names = F)
write.csv(HUC10p_mon, "Monthlyp_HUC10_hist.csv", row.names = F)

rm(list=ls())
proj <- read.csv("HUC10p_proj.csv")
drops <- c("FID","Join_Count","TARGET_FID","OBJECTID","LoadDate","Shape_Leng","Shape_Le_1","Shape_Area","area","Lon","Lat","areakm2_db","Lat_djb")
proj1 <- proj[,!(names(proj) %in% drops)]
proj2 <- t(proj1[,-1])
proj2 <- as.data.frame(proj2)
colnames(proj2) <- as.character(proj1$HUC10)

dates <- rownames(proj2)
dates <- gsub("p","",dates)
dates <- as.Date(dates, format = "%Y%m%d")
yr <-  as.numeric(format.Date(dates,"%Y"))
mon <- as.numeric(format.Date(dates,"%m"))
proj3 <- proj2
proj3$year <- yr
proj3$month <- mon

HUC10p_yr <- aggregate(.~ year, data = proj3, sum)
HUC10p_yr <- HUC10p_yr[,-c(ncol(HUC10p_yr))]
HUC10p <- sapply(HUC10p_yr,fivenum)
HUC10p <- HUC10p[-c(2,4),-1]
row.names(HUC10p)<-c("Minimum","Average","Maximum")
HUC10p_mon <- aggregate(.~ month, data = proj3, mean)
HUC10p_mon <- HUC10p_mon[,-c(ncol(HUC10p_mon))]

write.csv(HUC10p, "Summaryp_HUC10_proj.csv")
write.csv(HUC10p_yr, "Annualp_HUC10_proj.csv", row.names = F)
write.csv(HUC10p_mon, "Monthlyp_HUC10_proj.csv", row.names = F)
rm(list=ls())

hist <- read.csv("HUC10r_hist.csv")
drops <- c("FID","Join_Count","TARGET_FID","OBJECTID","LoadDate","Shape_Leng","Shape_Le_1","Shape_Area","area","Lon","Lat","areakm2_db","Lat_djb")
hist1 <- hist[,!(names(hist) %in% drops)]
hist2 <- t(hist1[,-1])
hist2 <- as.data.frame(hist2)
colnames(hist2) <- as.character(hist1$HUC10)

HUC10r <- sapply(hist2,fivenum)
HUC10r  <- HUC10r [-c(2,4),]
row.names(HUC10r ) <- c('Minimum', 'Average', 'Maximum')
dates <- rownames(hist2)
dates <- gsub("r","",dates)
dates <- as.Date(dates, format = "%Y%m%d")
yr <-  as.numeric(format.Date(dates,"%Y"))
mon <- as.numeric(format.Date(dates,"%m"))
hist3 <- hist2
hist3$year <- yr
hist3$month <- mon

HUC10r_yr <- aggregate(.~ year, data = hist3, mean)
HUC10r_yr <- HUC10r_yr[,-c(ncol(HUC10r_yr))]
HUC10r_mon <- aggregate(.~ month, data = hist3, mean)
HUC10r_mon <- HUC10r_mon[,-c(ncol(HUC10r_mon))]

write.csv(HUC10r, "Summaryr_HUC10_hist.csv")
write.csv(HUC10r_yr, "Annualr_HUC10_hist.csv", row.names = F)
write.csv(HUC10r_mon, "Monthlyr_HUC10_hist.csv", row.names = F)

rm(list=ls())
proj <- read.csv("HUC10r_proj.csv")
drops <- c("FID","Join_Count","TARGET_FID","OBJECTID","LoadDate","Shape_Leng","Shape_Le_1","Shape_Area","area","Lon","Lat","areakm2_db","Lat_djb")
proj1 <- proj[,!(names(proj) %in% drops)]
proj2 <- t(proj1[,-1])
proj2 <- as.data.frame(proj2)
colnames(proj2) <- as.character(proj1$HUC10)

HUC10r <- sapply(proj2,fivenum)
HUC10r  <- HUC10r [-c(2,4),]
row.names(HUC10r ) <- c('Minimum', 'Average', 'Maximum')
dates <- rownames(proj2)
dates <- gsub("r","",dates)
dates <- as.Date(dates, format = "%Y%m%d")
yr <-  as.numeric(format.Date(dates,"%Y"))
mon <- as.numeric(format.Date(dates,"%m"))
proj3 <- proj2
proj3$year <- yr
proj3$month <- mon

HUC10r_yr <- aggregate(.~ year, data = proj3, mean)
HUC10r_yr <- HUC10r_yr[,-c(ncol(HUC10r_yr))]
HUC10r_mon <- aggregate(.~ month, data = proj3, mean)
HUC10r_mon <- HUC10r_mon[,-c(ncol(HUC10r_mon))]

write.csv(HUC10r, "Summaryr_HUC10_proj.csv")
write.csv(HUC10r_yr, "Annualr_HUC10_proj.csv", row.names = F)
write.csv(HUC10r_mon, "Monthlyr_HUC10_proj.csv", row.names = F)


# to do side by side boxplots
proj3=melt(proj2)
hist3=melt(hist2)

hist3$set="historic"
proj3$set="projected"
full=as.data.frame(rbind(hist3,proj3))
full

boxplot(value~set+variable,data=full,col=c(2,4))
grid(nx=NA, ny=NULL) #grid over boxplot
par(new=TRUE)
boxplot(value~set+variable,data=full,col=c(2,4))
