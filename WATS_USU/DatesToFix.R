df <- data.frame(x=rnorm(36, 1, 10), month=rep(1:12, each = 3),
                 year=c(2000,2001,2002))
head(df)

library(zoo)

df$tt <- as.yearmon(paste(df$year, df$month, sep = "-"))
head(df$tt) # look at it
plot(x ~ tt, df) # plot it

z <- zoo(df$x, df$tt)
head(z) # look at it
plot(z) # plot i

df$tt <- as.Date(df$tt, format = "%Y%m")
