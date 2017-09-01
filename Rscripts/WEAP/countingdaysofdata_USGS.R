# starting point is inputting the data from all_gauges.xlsx
# requires the addition of the YearMonth column

gauge_IDs <- c(10011500, 10020100, 10023000, 10032000, 10075000, 10113500, 10128500, 10131000, 10153800, 10154200, 10164500, 10172200, 10015700, 10016900, 10038000, 10039500, 10068500, 10092700, 10105900, 10126000, 10130500, 10133600, 10136500, 10137500, 10146400, 10148200, 10149500, 10150500, 10155000, 10163000, 10166430, 10026500, 10041000, 10093000, 10099000, 10102200, 10102250, 10104700, 10111700, 10129900, 10130000, 10133540, 10133650, 10133800, 10133895, 10137900, 10143500, 10148510, 10149000, 10149400, 10152000, 10157000, 10167450, 10167499, 10169999, 10171600, 10172499, 404517111422801)

rm(list = c("cm_cf", "fullsheets", "i", "ID", "spd", "Tot_flow_month",  "TotFlow")) 

x <- mysheets
y <- as.data.frame(as.numeric(names(x)))
colnames(y) <- "ID"
y$count = 1
for(i in seq_along(x)){
    x[[i]]$YearMonth <- as.numeric(as.character(x[[i]]$YearMonth))
    x[[i]] <- x[[i]][x[[i]]$YearMonth > 198412,]
    x[[i]] <- x[[i]][x[[i]]$YearMonth < 201101,]
    x[[i]] <- x[[i]][is.na(x[[i]]$cfs) == FALSE,]
    y[i,2] <- as.numeric(as.character(na.omit(length(x[[i]]$YearMonth))))
}
y
z <- x[[1]]
z <- z[,-5]
z <- z[,-1]
for(i in 2:length(x)){
    z <- merge(z, x[[i]]$YearMonth, by.x = "YearMonth", by.y = "YearMonth", all = TRUE)
}
y

head(x[[7]])
