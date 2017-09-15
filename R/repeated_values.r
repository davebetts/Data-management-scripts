dt <- read.csv("table2.csv")

summary(dt)
X <- rle(dt$x)
Y <- rle(dt$y)

X
Y

X.cumsum <- cumsum(c(1, X$lengths[-length(X$lengths)]))
Y.cumsum <- cumsum(c(1, Y$lengths[-length(Y$lengths)]))

X.1 <- X[1]
X.2 <- X[2]
Z <- cbind(unlist(X.1),unlist(X.2))
colnames(Z) <- c("length", "value")
Z

Y.1 <- Y[1]
Y.2 <- Y[2]
Z.Y <- cbind(unlist(Y.1),unlist(Y.2))
colnames(Z.Y) <- c("length", "value")
Z.Y

myruns.X = which(X$values > 0 & X$lengths >= 2)
any(myruns.X)

myruns.Y = which(Y$values > 0 & Y$lengths >= 2)
any(myruns.Y)

# Next, we can do a cumulative sum of the run lengths and extract the end positions of the runs with length of at least 5 using the above found indices.
X.lengths.cumsum = cumsum(X$lengths)
ends.X = X.lengths.cumsum[myruns.X]

Y.lengths.cumsum = cumsum(Y$lengths)
ends.Y = Y.lengths.cumsum[myruns.Y]

# Next, we find the start positions of these runs.
newindex.X = ifelse(myruns.X>1, myruns.X-1, 0)
starts.X = X.lengths.cumsum[newindex.X] + 1
if (0 %in% newindex.X) starts.X = c(1,starts.X)

newindex.Y = ifelse(myruns.Y>1, myruns.Y-1, 0)
starts.Y = Y.lengths.cumsum[newindex.Y] + 1
if (0 %in% newindex.Y) starts.Y = c(1,starts.Y)

k.X = vector(mode = "list", length = length(starts.X))
for (i in seq_along(starts.X)) {
  k.X[[i]] <- which(sequence(ends.X[i])>starts.X[i]-1)
}
kk.X <- do.call(c,k.X)
kk.X

k.Y = vector(mode = "list", length = length(starts.Y))
for (i in seq_along(starts.Y)) {
  k.Y[[i]] <- which(sequence(ends.Y[i])>starts.Y[i]-1)
}
kk.Y <- do.call(c,k.Y)
kk.Y

kk.X.Y <- intersect(kk.X,kk.Y)
kk.X.Y

# jj.X = vector(mode = "list", length = length(dt$x))

x_y <- paste(dt$x,dt$y,sep="_")
x_y

XY <- rle(x_y)
XY

XY.cumsum <- cumsum(c(1, XY$lengths[-length(XY$lengths)]))
XY.cumsum

XY.1 <- XY[1]
XY.2 <- XY[2]
Z.XY <- cbind(unlist(XY.1),unlist(XY.2))
colnames(Z.XY) <- c("length", "value")
Z.XY

myruns.XY = which(XY$values > 0 & XY$lengths > 3)
any(myruns.XY)

#if statement

# Next, we can do a cumulative sum of the run lengths and extract the end positions of the runs with length of at least 5 using the above found indices.
XY.lengths.cumsum = cumsum(XY$lengths)
ends.XY = XY.lengths.cumsum[myruns.XY]

# Next, we find the start positions of these runs.
newindex.XY = ifelse(myruns.XY>1, myruns.XY-1, 0)
starts.XY = XY.lengths.cumsum[newindex.XY] + 1
if (0 %in% newindex.XY) starts.XY = c(1,starts.XY)

k.XY = vector(mode = "list", length = length(starts.XY))
for (i in seq_along(starts.XY)) {
  k.XY[[i]] <- which(sequence(ends.XY[i])>starts.XY[i]-1)
}
kk.XY <- do.call(c,k.XY)
kk.XY

dt2 <- dt[kk.XY,]

j <- c(1,2,3)
k <- cumsum(j)
k
newindex = ifelse(myruns>1, myruns-1, 0)
runs.lengths.cumsum = cumsum(runs$lengths)
ends = runs.lengths.cumsum[myruns]
starts = runs.lengths.cumsum[newindex] + 1
starts = runs.lengths.cumsum[newindex] + 1
if (0 %in% newindex) starts = c(1,starts)
0 %in% newindex
newindex <- c(0,newindex)
if (0 %in% newindex) starts = c(1,starts)
newindex = ifelse(myruns>1, myruns-1, 0)
starts = runs.lengths.cumsum[newindex] + 1
if (0 %in% newindex) starts = c(1,starts)
print(starts)
print(ends)
print(rnums[starts[1]:ends[1]])
print(rnums[starts[2]:ends[2]])
## [1] 0.5311486 0.1588756 1.1229208 0.7904306 2.0994378 0.8786987
print(rnums[starts[3]:ends[3]])
## [1] 0.5789541 0.6795760 1.1309282 1.0107847 1.9778476
for (i in seq_along(starts)) {
k[i] = paste(starts[i],ends[i], sep = ":")
}
k
class(k)
k[i] = as.numeric(paste(starts[i],ends[i], sep = ":"))
for (i in seq_along(starts)) {
k[i] = as.numeric(paste(starts[i],ends[i], sep = ":"))
}
k
k[i] = (paste(starts[i],ends[i], sep = ":"))
j <- as.numeric(k)
k
i
i
for (i in seq_along(starts)) {
k[i] = (paste(starts[i],ends[i], sep = ":"))
}
k
for (i in seq_along(starts)) {
k[i] = print(starts[i],":",ends[i])
}
k
print(starts[1])
}
for (i in seq_along(starts)) {
k[i] = starts[i]:ends[i]
}
for (i in seq_along(starts)) {
k[i] = starts[i]:ends[i]
}
k
starts[1]:ends[1]
for (i in seq_along(starts)) {
k[i] = starts[i]:ends[i]
}
k
k = vector(mode = "list", length = length(starts))
for (i in seq_along(starts)) {
k[i] = starts[i]:ends[i]
}
k
k[i] = starts[i]:ends[i]str()
str(starts)
str(ends)
rm(k)
14-10
14-9
73-67
(14-9)+(73-67)+79-74
for (i in seq_along(starts)) {
k <- rnums[-c(starts[i]:ends[i]),]
}
for (i in seq_along(starts)) {
k <- rnums[-c(starts[i]:ends[i])]
}
paste(as.numeric("10","35",sep = ":"))
paste("10","35",sep = ":")
as.numeric(paste("10","35",sep = ":"))
as.numeric(paste(10,35,sep = ":"))
paste(10,35,sep = ":")
st <- paste(10,35,sep = ":")
st
stt <- c(st)
stt
j <- sequence(10)
k <- sequence(35)
intersect(j,k)
!intersect(j,k)
which(sequence(35)>10-1)
k = vector(mode = "list", length = length(starts))
k[i] <- c(sequence(ends[i])>starts[i]-1)
k = vector(mode = "list", length = length(starts))
for (i in seq_along(starts)) {
k[i] <- c(sequence(ends[i])>starts[i]-1)
}
k
k = vector(mode = "list", length = length(starts))
for (i in seq_along(starts)) {
k[i] <- sequence(ends[i])>(starts[i]-1)
}
k
k[1] <- sequence(ends[1])
k1
ends[1]
str(ends[1])
starts[1]:ends[1]
k[1] <- starts[1]:ends[1]
k[1] <- c(starts[1]:ends[1])
k[1]
k[1] <- as.vector(c(starts[1]:ends[1]))
k
str(k)
k = vector(mode = "numeric", length = length(starts))
k## (djb)
sequence(ends[1])>(starts[1]-1)
which(sequence(35)>10-1)
which(sequence(ends[1])>start[1]-1)
which(sequence(ends[1]>start[1]-1)
)
start[1]
starts[1]
which(sequence(ends[1])>starts[1]-1)
ends[1]
which(sequence(ends[1])>starts[1]-1)
k[1] <- which(sequence(ends[1])>starts[1]-1)
k[1] <- as.list(which(sequence(ends[1])>starts[1]-1))
k
k[[1]] <- as.list(which(sequence(ends[1])>starts[1]-1))
k[[1]]
k[[1]] <- which(sequence(ends[1])>starts[1]-1)
k[[1]]
for (i in seq_along(starts)) {
k[[i]] <- which(sequence(ends[i])>starts[i]-1)
}
k
kk <- do.call(rbind,k)
kk
kk <- do.call(c,k)
kk
rm(list=ls())
gc()


# summary(y)
x_y <- paste(y$wind,y$gust,sep="_")
# x_y
XY <- rle(x_y)
# XY
XY.cumsum <- cumsum(c(1, XY$lengths[-length(XY$lengths)]))
# XY.cumsum
# XY.1 <- XY[1]
# XY.2 <- XY[2]
# Z.XY <- cbind(unlist(XY.1),unlist(XY.2))
# colnames(Z.XY) <- c("length", "value")
# Z.XY
wind.speed <- rle(y$wind)
# wind.speed
wind.speed.cumsum <- cumsum(c(1, wind.speed$lengths[-length(wind.speed$lengths)]))
# wind.speed.cumsum
speed.0 <- which(!grepl("^0_.*", XY$values))
myruns.XY = which(!grepl("^0_.*", XY$values) & XY$lengths > 5)
any(myruns.XY)

#if statement

# Next, we can do a cumulative sum of the run lengths and extract the end positions of the runs with length of at least 5 using the above found indices.
XY.lengths.cumsum = cumsum(XY$lengths)
ends.XY = XY.lengths.cumsum[myruns.XY]

# Next, we find the start positions of these runs.
newindex.XY = ifelse(myruns.XY>1, myruns.XY-1, 0)
starts.XY = XY.lengths.cumsum[newindex.XY] + 1
if (0 %in% newindex.XY) starts.XY = c(1,starts.XY)

k.XY = vector(mode = "list", length = length(starts.XY))
for (i in seq_along(starts.XY)) {
  k.XY[[i]] <- which(sequence(ends.XY[i])>starts.XY[i]-1)
}
kk.XY <- do.call(c,k.XY)
kk.XY

dt2 <- dt[kk.XY,]
# jj.X = vector(mode = "list", length = length(dt$x))

mydf <- data.frame(
  V1 = c("a", "a", "a", "b", "c", "c", "d", "e", 
         "a", "a", "b", "b", "e", "e", "d", "d"),
  V2 = c(1, 2, 3, 2, 4, 1, 3, 9, 
         4, 8, 10, 199, 2, 5, 4, 10)
)

## Use rle, as before
### mydf$V1 needs to be converted from factor to charactor for this to work (djb)
X <- rle(mydf$V1)
## Identify the rows you want to keep
Y <- cumsum(c(1, X$lengths[-length(X$lengths)]))
Y
# [1]  1  4  5  7  8  9 11 13 15
mydf[Y, ]
#    V1 V2
# 1   a  1
# 4   b  2
# 5   c  4
# 7   d  3
# 8   e  9
# 9   a  4
# 11  b 10
# 13  e  2
# 15  d  4


# How to Find Consecutive Repeats in R
# MASTER R
# 
# Q: Given a sequence of random numbers, find the start and end positions of runs of five or more consecutive numbers that are greater than zero.
# 
# A: Use the rle() function.
# 
# For example, let's apply rle() to the following sequence of numbers.

seq3 = c(2,2,2,2,2,5,3,7,7,7,2,2,5,5,5,3,3,3)

## Run Length Encoding
rle.seq3 = rle(seq3)
##   lengths: int [1:7] 5 1 1 3 2 3 3
##   values : num [1:7] 2 5 3 7 2 5 3

# We see that rle() returns a list of two elements: lengths and values, where the latter gives the unique number of each run, and the former gives the run length, i.e. the number of consecutive repeats within each run. For example, the first run is the number 2 repeated 5 times, and the second run is the number 5 repeated once.

# Let's solve the original question. First, we set the seed, generate a sequence of normal random numbers greater than zero and apply rle() to it.

set.seed(201)
rnums = rnorm(100)
runs = rle(rnums > 0)
# Next, we find indices of the runs with length of at least 5.

myruns = which(runs$values == TRUE & runs$lengths >= 5)
# check if myruns has any value in it 
any(myruns) 
## [1] TRUE

# Next, we can do a cumulative sum of the run lengths and extract the end positions of the runs with length of at least 5 using the above found indices.

runs.lengths.cumsum = cumsum(runs$lengths)
ends = runs.lengths.cumsum[myruns]
# Next, we find the start positions of these runs.

newindex = ifelse(myruns>1, myruns-1, 0)
starts = runs.lengths.cumsum[newindex] + 1
if (0 %in% newindex) starts = c(1,starts)
# Lastly, we print out the start and end positions of these runs and use them to extract the runs themselves.

print(starts)
## [1] 10 68 75
print(ends)
## [1] 14 73 79
print(rnums[starts[1]:ends[1]])
## [1] 0.1890041 0.6932962 0.2238094 0.3984569 1.0134744
print(rnums[starts[2]:ends[2]])
## [1] 0.5311486 0.1588756 1.1229208 0.7904306 2.0994378 0.8786987
print(rnums[starts[3]:ends[3]])
## [1] 0.5789541 0.6795760 1.1309282 1.0107847 1.9778476

k = vector(mode = "list", length = length(starts))
## (djb)
for (i in seq_along(starts)) {
  k[[i]] <- which(sequence(ends[i])>starts[i]-1)
}
kk <- do.call(c,k)


# To see more examples of how to use rle(), read Winston Chang's post: http://www.cookbook-r.com/Manipulating_data/Finding_sequences_of_identical_values/