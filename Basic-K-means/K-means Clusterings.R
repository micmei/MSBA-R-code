data2 <- read.csv("bankdata_1.csv") 
 
# Change nominal values to numeric values 
data21 <- data2 
data21$sex <- as.numeric(data2$sex) 
data21$married <- as.numeric(data2$married) 
data21$car <- as.numeric(data2$car) 
data21$save_act <- as.numeric(data2$save_act) 
data21$current_act <- as.numeric(data2$current_act) 
data21$mortgage <- as.numeric(data2$mortgage) 
data21$pep <- as.numeric(data2$pep)-1 
 
# scale the distance, formula is (x - min) / (max - min) 
data22 <- data21 
data22[2] <- scale(data22[2],center=min(data22[2]),scale=max(data22[2])-min(data22[2])) 
data22[3] <- scale(data22[3],center=min(data22[3]),scale=max(data22[3])-min(data22[3])) 
data22[4] <- scale(data22[4],center=min(data22[4]),scale=max(data22[4])-min(data22[4])) 
data22[5] <- scale(data22[5],center=min(data22[5]),scale=max(data22[5])-min(data22[5])) 
data22[6] <- scale(data22[6],center=min(data22[6]),scale=max(data22[6])-min(data22[6])) 
data22[7] <- scale(data22[7],center=min(data22[7]),scale=max(data22[7])-min(data22[7])) 
data22[8] <- scale(data22[8],center=min(data22[8]),scale=max(data22[8])-min(data22[8])) 
data22[9] <- scale(data22[9],center=min(data22[9]),scale=max(data22[9])-min(data22[9])) 
data22[10] <- scale(data22[10],center=min(data22[10]),scale=max(data22[10])-min(data22[10])) 
data22[11] <- scale(data22[11],center=min(data22[11]),scale=max(data22[11])-min(data22[11])) 
 
# combine the scale value with pep and their id 
data3 <- cbind(data21$id,data22[2:11],data21$pep) 
 
# initialize the centroid 
centroid <- list(runif(10,0,1),runif(10,0,1)) 
 
# split the data in data frame to list 
points <- split(data3, 1:nrow(data3)) 
n = 0 
 
# Two conditions to stop, one is over the max number of iteration(here I set it to 40) and the changes of centroid 
are   
# less than 0.0001 
 while (n <= 40) { 
     
    n <- n + 1 
     
    cluster <- list(c(),c()) 
     
    # set each point to nearest centroid 
    for (p in points) { 
        dist1 <- dist(rbind(p[2:11],centroid[[1]])) 
        dist2 <- dist(rbind(p[2:11],centroid[[2]])) 
        if (dist1 < dist2) { 
            cluster[[1]] <- c(cluster[[1]],list(p)) 
        } else { 
            cluster[[2]] <- c(cluster[[2]],list(p)) 
        } 
    } 
     
    centroid1 <- c(rep(0,10)) 
    centroid2 <- c(rep(0,10)) 
     
    # recalculate the centroids 
    for (p1 in cluster[[1]]) { 
        centroid1 <- centroid1 + p1[2:11] 
    } 
    centroid1 <- centroid1 / length(cluster[[1]]) 
     
    for (p2 in cluster[[2]]) { 
        centroid2 <- centroid2 + p2[2:11] 
    } 
    centroid2 <- centroid2 / length(cluster[[2]]) 
     
    # calculate the changes of centroids, and if the sum of the changes is less than threshold then we stop 
    diff <- (sqrt(sum((centroid1-centroid[[1]])^2)) + sqrt(sum((centroid2-centroid[[2]])^2))) 
     
    if (diff<0.0001) { 
        print(n) 
        print("We should break here") 
        break   
    } 
     
    # update the centroid 
    centroid[[1]] <- centroid1 
    centroid[[2]] <- centroid2 
     
} 
 
print("The first Centroid is") 
print(centroid[[1]]) 
 
print("The second Centroid is") 
print(centroid[[2]]) 
 
sum1 = 0 
sum2 = 0 
 
# calculate the average pep 
for (p1 in cluster[[1]]) { 
    sum1 <- sum1 + p1[12] 
} 
avg1 = sum1 / length(cluster[[1]]) 
 
for (p2 in cluster[[2]]) { 
    sum2 <- sum2 + p2[12] 
} 
avg2 = sum2 / length(cluster[[2]]) 
 
print("The average pep is the first cluster is") 
print(avg1) 
print("The average pep is the second cluster is") 
print(avg2) 
