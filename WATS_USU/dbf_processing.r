#############################################################################################
#                       Convert multiple .dbf files to a single table                       #
#                             Rownames = dates; Columns = zones                             #
#                              Written by Dave Betts - May 2017                             #
#############################################################################################
# (djb) this script is run once for each climate variable.  It processes each .dbf file found within the designated folder
# (djb) the .dbf files are zonal tables created with "zonaltables.py"

# (djb) cleaning up the workspace
rm(list=ls())
gc()

# list all .dbf files in the folder
filenames <- list.files("kriging", pattern="*.dbf", full.names=TRUE)
# optional 
# head(filenames,10)
# tail(filenames,10)

library(foreign)
dbfs=read.dbf(filenames[1])
# the first term (e.g. "FID_") identifies the column in "dbfs" that identifies the zones that will become the column titles in the data table that is the final output
# the second term (e.g. "MEAN") identifies the column in "dbfs" that identifies contains the values that will be recorded within a single column
dbf1=dbfs[,c("FID_","MEAN")] 
# dbf1=dbfs[,c("Names_dj_1","MEAN")] # this line and the previous line are essentially the same

# creating file names for data table
title <- gsub("k.dbf", "", filenames[1])
title <- gsub("kriging/", "", title)
lttr <- substr(title, 1, 1)
title <- gsub(lttr, "", title)
colnames(dbf1) = c("catchment",title)

# combine all .dbf files within the indicated folder into a single data table
for (i in 2:length(filenames)) {
  filename <- filenames[i]
  title <- gsub("k.dbf", "", filename)
  title <- gsub("kriging/", "", title)
  lttr <- substr(title, 1, 1)
  title <- gsub(lttr, "", title)
  nxt <- read.dbf(filename)
  # nxt1 <- nxt[,c("Names_dj_1","MEAN")]
  nxt1 <- nxt[,c("FID_","MEAN")]
  colnames(nxt1) = c("catchment",title)
  # dbfs[[i]]<-nxt1
  dbf1=merge(dbf1,nxt1,by.x="catchment",by.y="catchment")
}
nmcls=as.character(dbf1[,1])
dbf2=as.data.frame(t(dbf1[,-1]))
colnames(dbf2)=nmcls
nmrws=as.Date(colnames(dbf1[-1]),format="%Y%m%d")
dbf3<-cbind(nmrws,dbf2)
colnames(dbf3)[1] <- "Date"
write.csv(dbf3, paste0(lttr,".csv"), row.names=F)


#####
# h12 <- read.csv("catchment_FIDs.csv")
# h12_ID <- h12[,c("FID","Names_djb")]

# lttrs <- c("p","t","r")
# for (i in lttrs){
  # filename = paste0(i,".csv")
  # tosplit <- read.csv(filename, check.names=F)
  # colnames(tosplit)[2:411] <- as.character(h12_ID[,2])
  # write.csv(tosplit,paste0(i,"3.csv"),row.names=F)
# }
