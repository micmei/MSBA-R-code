setwd("C:/Users/mlq/Desktop/BDA Project/DatasetsForBizAnalytics/PCA-SVD/ml-100k")
data <- read.table("u.data")

m <- 943
n <- 1682
A <- matrix(NA,m,n)

for (i in 1:nrow(data)) {
  a <- data[i,1]
  b <- data[i,2]
  c <- data[i,3]
  A[a,b] <- c
}

#
> sum(is.na(A))
#1486336
> sum(!is.na(A))
#99790

B <- A

# average ratings for movies and users
B.colmeans <- as.numeric(colMeans(B, na.rm = T))
B.rowmeans <- as.numeric(rowMeans(B, na.rm = T))
B.means <- as.numeric(mean(B, na.rm = T))

ratings.no <- list()
for (i in 1:ncol(A)) {
  ratings.no[i] <- length(which(A[,i]!=0))
}

ratings.no2 <- list()
ratings.no2 <- list()
for (j in 1:nrow(A)) {
  ratings.no2[j] <- length(which(A[j,]!=0))
}

par(mfrow=c(1,2))
hist(B.colmeans,breaks=5,col="blue",main = "Average movie ratings")
hist(B.rowmeans,breaks=5,col="red",main = "Average user ratings")

par(mfrow=c(1,2))
hist(as.numeric(unlist(ratings.no)),breaks=5,col="yellow",main="Number of ratings for each movie")
hist(as.numeric(unlist(ratings.no2)),breaks=5,col="violet",main="Number of ratings for each user")

par(mfrow=c(1,1))
image(A,main="sparse matrix of data")

require(plyr)
mydf <- data.frame(no1 = unlist(ratings.no), no2 = B.colmeans )
result <- ddply(mydf, .(cut(no2, breaks = seq(1,5,by=0.1))), summarize, number = mean(no1))

mydf2 <- data.frame(no1 = unlist(ratings.no2), no2 = B.rowmeans)
result2 <- ddply(mydf2, .(cut(no2, breaks = seq(1,5,by=0.1))), summarize, number = mean(no1))

par(mfrow=c(1,2))
plot(result,main = "Average movie ratings by \n movie count",
     xlab="Average movie ratings",ylab="Average number of ratings")
plot(result2,main = "Average movie ratings by \n user count",
     xlab="Average user ratings",ylab="Average number of ratings")

for (i in 1:length(B.rowmeans)) {
  for (j in 1:length(B.colmeans)) {
    f <- B.rowmeans[i] + B.colmeans[j] - 3.52
    B[i,j] <- f
  }
}

for (i in 1:nrow(data)) {
  a <- data[i,1]
  b <- data[i,2]
  c <- data[i,3]
  B[a,b] <- c
}

B <- scale(B)
B.svd <- svd(B)

u <- B.svd$u
d <- B.svd$d
v <- B.svd$v


u2 <- u[,1:2]
v2 <- v[,1:2]

u3 <- u[,1:3]
v3 <- v[,1:3]

par(mfrow=c(1,2))
q1 <- t(u2) %*% B
plot(q1[1,],q1[2,],main = "Projected Movie Vectors")

q2 <- t(v2) %*% t(B)
plot(q2[1,],q2[2,],main = "Projected users Vectors")
