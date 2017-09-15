z <- merge(names_nodes, all_gauges, by.x = "gage", by.y = "site_no")
colnames(z)[2] <- "site_name_WEAP"
z$elev_m <- z$alt_va_ft*0.3048
z1 <- z[,c(1,4,5,2,7,8,10,24,16:20)]
write.csv(z1, "selected_gauges.csv", row.names = FALSE)
