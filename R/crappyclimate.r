
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
