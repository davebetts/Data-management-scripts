rm(list=ls())
gc()
setwd("C:/Users/a01987147/Dropbox/PhD/WEAP/Data/Data_Processed/stream gauges")
setwd("C:/Users/davebetts/Dropbox/PhD/WEAP/Data/Data_Processed/stream gauges")
dir()

library(readxl) # required package for the following function
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, col_types = c("text","text","date","numeric","text")))
  names(x) <- sheets
  x
}
# Import each worsheet in the Excel workbook "all_gaugedata.xlsx" as a table within the 
# list "fullsheets"  
fullsheets <- read_excel_allsheets("all_gaugedata.xlsx") 

sgs <- read.csv("selected_gauges.csv", check.names = FALSE)
curgaug <- unlist(sgs[,1])
rm(sgs)

mysheets <- fullsheets[which(names(fullsheets) %in% curgaug)] 

Dates <- read.csv("dates85to05.csv")
Dates$Date <- as.POSIXct(Dates$Date, format = "%m/%d/%Y", tz = "")
# Dates$Date <- as.factor(format(Dates$Date, "%Y-%m-%d"))
str(Dates)

just_gauges <- vector(mode = "list", length = length(mysheets))
for (i in seq_along(mysheets)) {
  ID = as.character(mysheets[[i]][2,2])
  jg = subset(mysheets[[i]], Date >= as.POSIXct("1984-12-31") & Date <= as.POSIXct("2010-12-31"))
  just_gauges[[i]] = jg[,c("Date","cfs")]
  colnames(just_gauges[[i]])<- c("Date", ID)  # label table i with stream gauge ID [i]
}

singlesheet <- data.frame(Date = character())
for (i in seq_along(just_gauges)) {
  ss = as.data.frame(just_gauges[[i]])
  # merge each table (i) with the pervious tables, keeping all rows
  singlesheet = merge(singlesheet,ss, by.x = "Date", by.y = "Date", all = TRUE)
}

Date <- singlesheet$Date

singlesheet <- singlesheet[,-1]
numun <- colnames(singlesheet)
for (i in seq_along(numun)) {
  numun[i] <- paste("USGS_", numun[i],"[CFS]", sep = "")
}
colnames(singlesheet) <- numun
singlesheet <- singlesheet[,order(names(singlesheet))]
singlesheet <- cbind(Date, singlesheet)
pf2 <- paste(colnames(singlesheet), collapse = ", ")

# cat("$DateFormat = y-m-d\nMonthly: $Columns = ", paste(numun, collapse = ", "), "\n", sep = "", append = TRUE)
# write.csv(singlesheet, "g2use.csv", row.names = FALSE, na = "")
# 
# prefix <- matrix(nrow = 2)
# prefix[1,] = "$DateFormat = y-m-d"
# prefix[2,] = paste("Daily: $Columns = ", pf2, collapse = "")

prefix1 = "$DateFormat = y-m-d"
prefix2 = paste("Daily: $Columns = ", pf2, collapse = "")

# sswpf <- rbind(prefix, singlesheet)

cat(prefix1, "\n", prefix2, "\n",file="g2use.csv")

# write.csv(singlesheet, file = "g2use.csv", na = "")
write.table(singlesheet, file = "g2use.csv", append = TRUE, sep = ",", na = "", row.names = FALSE, col.names = FALSE)
