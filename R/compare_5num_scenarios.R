############
# compare 5 number summaries of historic and projected data
# 5 number summaries were automatically generated in R and have been saved in the folder "Summaries/Summary"
# collate all of the comparisons into a single table
############
#
# required input files: 5 number summaries automatically generated in R; historic and projected
# 

rm(list=ls())
# setwd(
dirloc <- "Summaries/Summary"
climate_files <- list.files(dirloc, pattern="*.csv", full.names=TRUE)
z <- vector(mode = "list", length = length(climate_files))# /2)
climnames <- gsub(".*\\/", "", climate_files)
climnames <- gsub("Summary","", climnames)
climnames <- gsub(".csv","", climnames)
climnames <- gsub("Temp","t", climnames)
nxt=rep(NA,length(climate_files))
for(i in seq_along(1:length(climnames))){
  nxt[i]=gsub("_([^_]*)$","",climnames[i])
}
names(z) <- nxt
nxt1<-unique(nxt)

for(i in seq(1,length(climate_files),2)) {
  w <- read.csv(climate_files[i], check.names = FALSE, row.names=1)
  x <- read.csv(climate_files[i+1], check.names = FALSE, row.names=1)
  y = (x-w)/w
  z[[i]]<-y
}
z1 <- z[seq(1,length(z),2)]
final <- do.call(cbind,z1)

write.csv(final,"final.csv")