rm(list=ls())
dirloc <- "summaries/Summary"
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

  for(i in seq(1,length(climate_files),2)) {
    w <- read.csv(climate_files[i], check.names = FALSE, row.names=1)
    x <- read.csv(climate_files[i+1], check.names = FALSE, row.names=1)
    y = (w-x)/w
    
    write.csv(y,paste0(nxt,"_y.csv"))
  }