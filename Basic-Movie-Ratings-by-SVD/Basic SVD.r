setwd("C:/Users/mlq/Desktop/BDA HW2")
data1 <- read.csv("data1.csv") 
A <- data1[2:11]
dataSum <- A*A
cSum <- sqrt(apply(dataSum,2,sum))

# Normalize the matrix
dataN <- matrix(data=NA,nrow=10,ncol=10)
for (i in 1:10) {
  dataN[i] <- A[i]/cSum[i]
}
dataN <- dataN[1:10]
B <- do.call(cbind,dataN)

# The result for question 1
Q1 <- B

# The result for question 2
Q2 <- t(B) %*% B

# The result for question 3
s <- svd(B)
leftSingularVector <- s$u
rightSingularVector <- s$v
singularValues <- s$d

# The result for question 4
par(mfrow=c(1,2))

plot(leftSingularVector[,1],type = "o", col="blue",
     ylim=c(-1,0.5),ann=F)
lines(leftSingularVector[,2],type = "b", col="red")
title("First two left-singular vectors")
title(ylab="u2",xlab="k=2")
legend(1,0.5,c("First","Second"),cex=0.8, 
       col=c("blue","red"), pch=21:22, lty=1:2)

plot(rightSingularVector[,1],type = "o", col="blue",
     ylim=c(-1,0.5),ann=F)
lines(rightSingularVector[,2],type = "b", col="red")
title("First two right-singular vectors")
title(ylab="v2",xlab="k=2")
legend(1,0.5,c("First","Second"),cex=0.8, 
       col=c("blue","red"), pch=21:22, lty=1:2)

# The result for question 5
library(calibrate)
u2 <- as.matrix(leftSingularVector[,1:2])
q5 <- t(u2) %*% B

names1 <- names(data1)[2:11])

plot(q5[1,],q5[2,],xlim=c(-1.2,0.3),ylim=c(-1.-0,0.2),main="DOCUMENTS projection", 
     xlab="First Singular Vector", 
     ylab="Second Singular vector")

textxy(q5[1,], q5[2,], labs=names1, cx = 0.5, dcol = "black", m = c(0, 0))

# The result for question 6
v2 <- as.matrix(rightSingularVector[1:2,])
q6 <- B %*% t(v2)

names2 <- data1$Term

plot(q6[,1],q6[,2],xlim=c(-0.8,0.3),ylim=c(-0.4,0.6),main="TERMS projection", 
     xlab="First Singular Vector", 
     ylab="Second Singular vector")

textxy(q6[,1], q6[,2], labs=names2, cx = 0.5, 
       dcol = "black", m = c(0, 0))
