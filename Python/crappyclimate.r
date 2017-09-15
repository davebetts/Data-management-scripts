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
nxt<-unique(nxt)
names(z) <- nxt

for(i in seq(1,length(climate_files),2)) {
  w <- read.csv(climate_files[i], check.names = FALSE, row.names=1)
  x <- read.csv(climate_files[i+1], check.names = FALSE, row.names=1)
  y = (x-w)/w
  z[[i]]<-y
}
z1 <- z[seq(1,length(z),2)]
final <- do.call(cbind,z1)

write.csv(final,"final.csv")

# climate_sums <- function(dirloc) {
  for(i in seq(1,length(climate_files),2)) {
    w <- read.csv(climate_files[i], check.names = FALSE, row.names=1)
    x <- read.csv(climate_files[i+1], check.names = FALSE, row.names=1)
    y = (w-x)/w
    
    write.csv(y,paste0(nxt,"_y.csv"))
  }
  



















climate_sums <- function(dirloc) {
  climate_files <- list.files(dirloc, pattern="*.csv", full.names=TRUE)
  # climnames <- gsub(".*\\/", "", climate_files)
  # climnames <- gsub("Summary","", climnames)
  # climnames <- gsub(".csv","", climnames)
  # climnames <- gsub("Temp","t", climnames)
  for(i in seq(1,length(climate_files),2)) {
    w <- read.csv(climate_files[i], check.names = FALSE, row.names=1)
    x <- read.csv(climate_files[i+1], check.names = FALSE, row.names=1)
    y = (w-x)/w
    nxt = gsub("_([^_]*)$","",climate_files[i])
    write.csv(y,paste0(nxt,"_y.csv"))
  }
  climate_files2 <- list.files(dirloc, pattern="*.csv", full.names=TRUE)
  climnames <- gsub(".*\\/", "", climate_files2)
  climnames <- gsub("Summary","", climnames)
  climnames <- gsub(".csv","", climnames)
  climnames <- gsub("Temp","t", climnames)
  z <- vector(mode = "list", length = length(climate_files2))
  names(z) <- climnames
  for (i in 1:length(climnames)) {
    w <- read.csv(climate_files2[i], check.names = FALSE)
    w1 <- t(w[,-1])
    colnames(w1)<-w[,1]
    z[[i]] <- w1
  }
  final <- do.call(rbind, z)
  write.csv(final, "climate_sums.csv")
}


dirloc <- "summaries/Summary"
climate_sums(dirloc)
