setwd("C:/Users/davebetts/Dropbox/PhD/TA/Biol1620-spr16/5_TAmeeting_22Feb16")
dir()

as = read.csv("all_stat.csv")
str(as)
summary(as)

R2 <- as[,c("Paradox_r2", "Prana_r2")]
boxplot(R2, as, col = c("blue", "red"), names = c("Paradoxides", "Phacops rana"), main = "All R2 values > 0.9", ylab = "R2 value", xlab = "Species")

slopes <- as[,c("Paradox_low95", "Prana_low95", "Paradox_slope", "Prana_slope", "Paradox_high95", "Prana_high95")]
boxplot(slopes, as, col = c("blue", "red"), outpch = 21, outcol = c("blue", "red"), outbg = c("light blue", "pink"), names = c("Low 95%", "Low 95%", "Slope", "Slope", "High 95%", "High 95%"), main = "Which species is Isometric?")
legend(x = 0.4, y = 1.425, legend = c("Paradoxides", "Phacops rana"), fill = c("blue", "red"))
abline(a = 1, b = 0, col = "orange", lwd = 5)


allslope <- as[,c("Group", "Range")]
boxplot(Range ~ Group, data = as, col = c(rep("blue", 4), rep(c("red","blue"), 6), rep("blue", 3)), main = "Which speices has isometric growth?", xaxt = "n", xlab = "Individual lab group reports", ylab = "Slope with 95% confidence intervals")
abline(a = 1, b = 0, col = "orange", lwd = 5)
legend(x = 15, y = 1.15, legend = c("Paradoxides", "Phacops rana", "Isometric growth"), fill = c("blue", "red", "orange"))

px <- c(1.27, 1.292, 1.313)
pr <- c(0.926, 0.959, 0.992)
pxpr <- cbind(px,pr)
boxplot(pxpr, col = c("red", "blue"), main = "Combined Data", names = c("Paradoxides", "Phacops rana"), xlab = "Species", ylab = "Slopes with 95% confidence intervals", boxwex = 0.5, at = c(0.8, 1.6))
abline(a = 1, b = 0, col = "orange", lwd = 5)
legend(x = 1, y = 1.25, legend = c("Paradoxides", "Phacops rana", "Isometric growth"), fill = c("blue", "red", "orange"))


