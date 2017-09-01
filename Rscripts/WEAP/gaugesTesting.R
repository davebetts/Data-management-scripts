rm(list=ls())
gc()
setwd("C:/Users/Sarah/Documents/Dropbox/PhD/WEAP/stream gauges")
setwd("C:/Users/davebetts/Dropbox/PhD/WEAP/stream gauges")
dir()

gaugeNodes <- read.csv("gauges_WEAPNodes_all.csv")
weapoutput <- read.csv("WEAP_output_m3_7_10_15.csv")

outputNodes <- merge(gaugeNodes, weapoutput, by.x ="WEAP_Node", by.y = "Nodes")

write.csv(outputNodes,"Nodes_Data.csv")

library(readxl)    
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <-    lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, col_types = c("text","text","date","numeric","text")))
  names(x) <- sheets
  x
}

mysheets <- read_excel_allsheets("all_data.xlsx")

### The average flow per month is bad, because it does not factor in missing data
# summarize daily average flows into a single monthly average flow to correspond with the time step in WEAP
# this process will be done to each table in the list of tables "mysheets"
avg_flow_month <- vector(mode = "list", length = length(mysheets)) # creates a new empty list, with the same number of spaces for variables as "mysheets"
for (i in seq_along(mysheets)) {                                   # process each table in the list "mysheets"
  YearMonth = as.factor(strftime(mysheets[[i]][,3], "%Y_%m"))      # create an element to summarize observations by year and month for sheet i
  AvgFlow = aggregate(mysheets[[i]][,4] ~ YearMonth, FUN = mean, na.omit = TRUE) # aggregate daily averages by year and month from sheet i
  colnames(AvgFlow)[2] = "AvgCFS"                                  # labels the new aggregate data frame
  x = mysheets[[i]][1,2]                                           # create an object using the stream gauge ID number
  ID = as.vector(rep(x, each = nrow(AvgFlow)), mode = "character") # create a column of the stream gauge ID number the same length as the aggregate data "AvgCFS"
  avg_flow_month[[i]] = AvgFlow                         # bind "ID" and "AvgFlow" into a single data frame 
  names(avg_flow_month)[i] <- ID                                   # label table i with stream gauge ID [i]
}



lapply(mysheets,mysheets$YearMonth)


Monthly_AvgFlow = do.call(rbind, avg_flow_month)
str(Monthly_AvgFlow)
summary(Monthly_AvgFlow)
sapply(Monthly_AvgFlow,class)
head(Monthly_AvgFlow)

write.csv(Monthly_AvgFlow, "MAF.csv", row.names=FALSE)

Tot_flow_month <- vector(mode = "list", length = length(mysheets))
for (i in seq_along(mysheets)) {
  require(zoo)
  require(lubridate)
  mysheets[[i]][,c("YearMonth")] = as.yearmon(mysheets[[i]][,"Date"])
  mysheets[[i]][,c("Days")] = days_in_month(mysheets[[i]][,"Date"])
  Tot_flow_month[[i]] = aggregate(mysheets[[i]][,4] ~ mysheets[[i]][,c("YearMonth")] + mysheets[[i]][,c("Days")], FUN = mean, na.omit = TRUE)
  colnames(Tot_flow_month[[i]]) = c("YearMonth", "Days", "AvgCFS")
  Tot_flow_month[[i]][,c("TotCFS")] = Tot_flow_month[[i]][,c("AvgCFS")]*Tot_flow_month[[i]][,c("Days")]
  Tot_flow_month[[i]][,c("TotCMS")] = Tot_flow_month[[i]][,c("TotCFS")]*0.0283168464
  x = mysheets[[i]][1,2]
  ID = as.vector(rep(x, each = nrow(TotFlow)), mode = "character")
  Tot_flow_month[[i]] = cbind(ID, TotFlow[,c(-2)])
  names(Tot_flow_month[[i]]) <- x
}
Tot_flow_month[[3]]

names(Tot_flow_month)
ls(Tot_flow_month)
crntdir <- getwd()
dir.create("temp")
setwd("temp")
n <- names(Tot_flow_month)
n
files <- c(names(Tot_Flow_month))
lapply(seq_along(Tot_Flow_month), function(i){
  write.csv(Tot_Flow_month[[i]], files[i], row.names = FALSE, col.names = c("Agency","ID","Date","CFS","Quality"))
})
setwd(crntdir)

class(Tot_flow_month)

### try this again later to see if there is a way to write the merge directly

func <- function(x,y){merge(x, y, by.x="YearMonth", by.y="YearMonth", all = TRUE)}  
Monthly_TotFlow = do.call(func, Tot_flow_month)
str(Monthly_TotFlow)
summary(Monthly_TotFlow)
sapply(Monthly_TotFlow,class)
head(avg_flow_month)

write.csv(Monthly_TotFlow, "TMF.csv", row.names=FALSE)

library(reshape2)
MonthlyCMS <- dcast(Monthly_TotFlow, TotCMS ~ YearMonth + ID, drop = FALSE)

str(mysheets[[1]])
head(mysheets[[1]])
head(TotFlow)
str(TotFlow)

library(xts) # loads previously installed package for the entire R session
cfs.xts<-xts(x=x$CFS, as.POSIXct(x$Date))
  
# Averaging time series data in R
# Time period options: "us" (microseconds), "microseconds", "ms" (milliseconds), "milliseconds", 
# "secs" (seconds), "seconds", "mins" (minutes), "minutes", "hours", "days", "weeks", "months", 
# "quarters", and "years"
cfsTimeFrame <- endpoints(cfs.xts,'years')  # sets the time frame to "months" for analyzing temperature data

# create new vectors (columns of data) based on analyses within the selected time frames
sm = as.matrix(period.apply(cfs.xts,INDEX = cfsTimeFrame, FUN = mean)) # mean value per day
plot(sm)

setwd("C:/Users/davebetts/Dropbox/PhD/WEAP/stream gauges")
setwd("C:/Users/Sarah/Documents/Dropbox/PhD/WEAP/stream gauges")
dir()

gaugeNodes <- read.csv("gauges_WEAPNodes_all.csv")
weapoutput <- read.csv("WEAP_output_m3_7_10_15.csv") 
wo_gn <- merge(gaugeNodes, weapoutput, by.x ="WEAP_Node", by.y = "Nodes") 
gauge_IDs <- wo_gn[,"USGS_ID"]  # a vector of the stream gauge IDs for labelling elsewhere

Nodes <- wo_gn[,c("Basin", "USGS_ID", "Site_name", "WEAP_Node", "Start_Date", "End_Date",
                  "Latitude", "Longitude", "Lat_DD", "Long_DD", "Reach")]
write.csv(Nodes,"gauges_nodes.csv", row.names = FALSE) 

justoutput <- subset(wo_gn, 
                     select = -c(Basin, Site_name, WEAP_Node, Recheck, Start_Date, # extract unwanted colums
                                 End_Date, Latitude, Longitude, Lat_DD, Long_DD, Reach, FID, Sum)) # extract unwanted colums
cnms <- colnames(justoutput) # creates a vector from the column names for labelling
YM <- cnms[-1]    # remove the first element ("USGS_ID") from the vector "cnms", 
YearMonth <- gsub("X","",YM)  # remove the "X" in each cell (X198501 --> 198501) to make 

t_jo <- as.data.frame(t(justoutput))  # transposing the table into a new data frame
colnames(t_jo) <- gauge_IDs # write column names
OutputNodes <- subset(t_jo[-1,]) # Remove first row from "t_jo" containing gauge ID #s

OutputNodes <- OutputNodes[,order(names(OutputNodes))] # sort the columns by gauge IDs
OutputNodesDates <- cbind(YearMonth,OutputNodes) # attach column of YearMonth timesteps

write.csv(OutputNodesDates, "WEAP85to05.csv", row.names = FALSE)

library(readxl) # required package for the following function
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, col_types = c("text","text","date","numeric","text"),na = ""))
  names(x) <- sheets
  x
}
fullsheets <- read_excel_allsheets("all_gaugedata.xlsx") 

mysheets <- fullsheets[which(names(fullsheets) %in% gauge_IDs)] 

Tot_flow_month <- vector(mode = "list", length = length(mysheets))
for (i in seq_along(mysheets)) {
  require(lubridate) # required for "days_in_month()"
  mysheets[[i]][,c("YearMonth")] = as.factor(format(mysheets[[i]][,"Date"],"%Y%m"))
  mysheets[[i]][,c("Days")] = days_in_month(mysheets[[i]][,"Date"])
  TotFlow = aggregate(mysheets[[i]][,4] ~ mysheets[[i]][,c("YearMonth")] + 
                        mysheets[[i]][,c("Days")], FUN = mean, na.omit = TRUE)
  colnames(TotFlow) = c("YearMonth", "Days", "AvgCFS")
  spd = 24*60*60 # Seconds per day (hours * minutes * seconds)
  TotFlow$TotCF = TotFlow$AvgCFS*TotFlow$Days*spd # days per month * seconds per day
  cm_cf = 0.0283168464 # cubic meters per cubic foot
  TotFlow$AvgCMS = TotFlow$AvgCFS*cm_cf # Avg cfs * cubic meters per cubic feet
  TotFlow$TotCM = TotFlow$AvgCMS*TotFlow$Days*spd # days per month * seconds per day
  x = mysheets[[i]][1,2] # create a label from the gauge ID
  ID = as.vector(rep(x, each = nrow(TotFlow)), mode = "character") # vector for ID label
  Tot_flow_month[[i]] = cbind(ID, TotFlow[,c(-2)]) # Bind ID vector to data row
  names(Tot_flow_month)[i] <- ID  # label table i with stream gauge ID [i]
}

singlesheet <- data.frame(YearMonth = character())
for (i in seq_along(Tot_flow_month)) {
  ss = as.data.frame(Tot_flow_month[[i]][,c("YearMonth","TotCM")]) 
  singlesheet = merge(singlesheet,ss, by.x = "YearMonth", by.y = "YearMonth", all = TRUE)
}
colnames(singlesheet)<-c("YearMonth",names(Tot_flow_month)) # rename columns
singlesheet <- singlesheet[,order(names(singlesheet))] # sort columns alphabetically 
singlesheet <- cbind(singlesheet$YearMonth,
                     singlesheet[ , -which(names(singlesheet) %in% c("YearMonth"))])
colnames(singlesheet)[1] <- "YearMonth" # rename column 1

write.csv(singlesheet, "TotalFlow_Month_all.csv", row.names = FALSE, na = "")

singlesheet$Index <- as.numeric(as.character(singlesheet$YearMonth))
MY <- singlesheet[singlesheet$Index < 200601,] # remove rows after Sept.2006 <-- !!! fix???
MY <- MY[MY$Index > 198412,] # before Jan.2005 <-- !!! fix???
MY$Index <- NULL
modelyears <- MY

write.csv(modelyears, "myTotalFlow_Month_all.csv", row.names = FALSE)

names(modelyears)
names(OutputNodesDates)
length(names(modelyears))
length(names(OutputNodesDates))
sum(names(modelyears) == names(OutputNodesDates))


library(hydroGOF)
GOFnames <- c(names(modelyears[-1]))
Est <- OutputNodesDates[,-1]
# head(Est)
Obs<- modelyears[,-1]
# summary(Est)
class(c(Est,Obs))
OvsE <- gof(Est[[1]],Obs[[1]],na.rm = TRUE)
write.csv(OvsE,"GoodnessofFit.csv")

OvsE <- vector(mode = "list", length = ncol(Est))
emptyset <- as.data.frame(rep(NA,20))
goflab <- c("ME", "MAE", "MSE", "RMSE", "NRMSE %", "PBIAS %", "RSR", "rSD", "NSE", "mNSE", "rNSE", "d", "md", "rd", "cp", "r", "R2", "bR2", "KGE", "VE")
rownames(emptyset) <- goflab
oenames <- colnames(Est)
library(hydroGOF)
for(i in seq_along(OvsE)) {
    if ((sum(Obs[,i],na.rm = TRUE) == 0)) {
        nxt_col <- emptyset
        OvsE[[i]] <- nxt_col
        }
    else {
        sim = Est[,i]
        obs = Obs[,i]
        nxt_col <- gof(sim = sim, obs = obs)
        OvsE[[i]] <- nxt_col
    }
}
OvsE <- do.call(cbind, OvsE)
colnames(OvsE) <- oenames

sapply(Obs, sum)

head(OvsE)

gof(Est,Obs)
str(Est)
str(Obs)
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts
sim <- obs
gof(sim=sim, obs=obs)
type(sim)
class(sim)
Est = as.zoo(OutputNodesDates[,c(1,2)])
Est
class(Est)

if (is.na(sum(Obs[,2]) == TRUE)) {
    nxt_col <- emptyset
    OvsE[[2]] <- nxt_col
}
head(OvsE)
