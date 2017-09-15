# setwd("C:/Users/a01987147/Dropbox/PhD/WEAP/Data/Data_Processed/climate")
dir()

##########################################################################################
###                  Modifying original data from long form to wide form:              ###
###                              1 column per SNOTEL gage                              ### ##########################################################################################
library(reshape2)

snotel <- read.csv("snotel_raw_161116.txt", skip = 5, check.names = FALSE) # this file as eliminated unwanted snotel gauges (< 5 years of data)
s1 <- snotel
### creating headings
names(s1) <- c("date", "name", "id", "SWE[mm]")
s1[,2] <- gsub("[^[:alnum:] ]", "", s1[,2])
s1[,2] <- gsub(" ", "", s1[,2])
### reformatting
st <- melt(s1, id=1:3, na.rm=FALSE, drop = FALSE)
st2 <- dcast(st, date ~ name + id) # wide form data
st2$date<-format(as.Date(st2$date),format="%m/%d/%Y")
### adding two new columns columns.  One column for each of the two catchments which will
### use the average reading of the two gauges within each catchment
st2$CutlerCreekAvg=(st2$BenLomondPeak_332+st2$BenLomondTrail_333)/2
st2$ChalkCreekAvg=(st2$ChalkCreek1_392+st2$ChalkCreek2_393)/2
### renaming column headings
clnms <- vector(mode = "list", length = length(names(st2)))
clnms[[1]] <- "Daily: $Columns =  Date"
for (i in 2:length(names(st2))) {
  clnms[[i]] <- paste(names(st2[i]), "[mm]", sep = "")
}
##########################################################################################
###                     Write data table for Goodness of Fit Testing                   ###
###                           to be used with "snotelGoF.R"                            ### ##########################################################################################
write.csv(st2,"snotel_daily.csv",row.names=F)



##########################################################################################
###                               Data Quality Analysis                                ###
###                    Assuming SNOTEL QA/QC has already done this                     ### ##########################################################################################
# ### plot all of the gages at once
# x=as.Date(st2$date,format="%m/%d/%Y")
# tbl<-st2[,-1]
# matplot(x,tbl,type="l")
# ### plot individual gauges for the entire time series
# x=as.Date(st2$date,format="%m/%d/%Y")
# for(i in 2:ncol(st2)){
#   plot(x,st2[,i],main=colnames(st2)[i],type="l")
# }
# ### log(10) plot individual gauges for the entire time series
# x=as.Date(st2$date,format="%m/%d/%Y")
# for(i in 2:ncol(st2)){
#   plot(x,(st2[,i]+1),main=colnames(st2)[i],type="l",log="y")
# }
# ### plot individual gauges per month
# x=format(as.Date(st2$date,format="%m/%d/%Y"),format="%m")
# for(i in 2:ncol(st2)){
#   plot(x,st2[,i],main=colnames(st2)[i])
# }

### Creating header for "ReadFomFile" formating for WEAP
# create a one-row matrix the same length as st2
temprow <- matrix(c(rep.int(NA,length(st2))),nrow=1,ncol=length(st2))
# make "temprow" a data.frame and give cols the same names as data ("st2")
newrow <- data.frame(temprow)
# naming columns in newrow to allow for [rbind] of "newrow", "metrow", and "st2"
colnames(newrow) <- colnames(st2)
# Formatting header for "ReadFromFile" in WEAP
newrow[1,1] = "$DateFormat = m/d/y"
# adding column names that will be read by WEAP as a dataframe
metrow <- data.frame(clnms)
# naming columns in metrow to allow for [rbind] of "newrow", "metrow", and "st2"
colnames(metrow) <- colnames(st2)
# building final table that can be read by "ReadFromFile" in WEAP
st3 <- rbind(newrow, metrow, st2)

### !!!       The final table needs to be edited after it is written          !!! ###
### !!! Do the edits in Notepad (NOT Excel).  Excel will alter the formatting !!! ###

### Write final table to be read by WEAP
write.table(st3, "snotel_NWCC.csv", 
    row.names = FALSE, 
    na = "", 
    col.names = FALSE, 
    sep = ",")

### !!! The first line of the final table needs to be edited, but
### !!! NOT using MS Excel (Excel ruins the formatting)
### !!!
### !!! Delete additional ','s after '..m/d/y' so that WEAP can read this line
### !!! as a header
### !!! 
### !!! Delete all " from the entire table