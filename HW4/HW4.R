# ---- load-packages --------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(psych)
library(ggplot2)
library(reshape2)
library(progress)
library(knitr)
library(kableExtra)

# ---- problem-1 --------------------------------------------------------
## First declare the data
row.1 <- c(1, .17, .27, .34, .25)
row.2 <- c(.17, 1, .17, .21, .11)
row.3 <- c(.27, .17, 1, .15, .29)
row.4 <- c(.34, .21, .15, 1, .31)
row.5 <- c(.25, .11, .29, .31, 1)
all.dat <- rbind(row.1, row.2, row.3, row.4, row.5)
colnames(all.dat) <- c("P1", "P2", "P3", "P4", "P5")
rownames(all.dat) <- c("P1", "P2", "P3", "P4", "P5")
all.dat[upper.tri(all.dat)] <- NA
diag(all.dat) <- 0
all.dat.freeze <- as.dist(all.dat, diag=TRUE)
all.dat.order <- unique(all.dat)[order(unique(all.dat))]

# ---- problem-1-a2 --------------------------------------------------------
row.1 <- c(1, .17, .27, .34, .25)
row.2 <- c(.17, 1, .17, .21, .11)
row.3 <- c(.27, .17, 1, .15, .29)
row.4 <- c(.34, .21, .15, 1, .31)
row.5 <- c(.25, .11, .29, .31, 1)
all.dat <- rbind(row.1, row.2, row.3, row.4, row.5)
diag(all.dat) <- NA
colnames(all.dat) <- c("P1", "P2", "P3", "P4", "P5")
rownames(all.dat) <- c("P1", "P2", "P3", "P4", "P5")

## Visually inspect the new cluster and identify maximum column wise
all.dat.tmp <- all.dat[-c(2,5), -c(2,5)]
row.4 <- c(apply(all.dat[c(2,5),-c(2,5)], 2, function(x) min(x, na.rm=TRUE)), NA)
all.dat.tmp <- rbind(all.dat.tmp, row.4)
all.dat.tmp <- cbind(all.dat.tmp, row.4)
colnames(all.dat.tmp)[4] <- "P(2,5)"
rownames(all.dat.tmp)[4] <- "P(2,5)"
## Now print the new matrix
all.dat.tmp %>% kable()

# ---- problem-1-a3 --------------------------------------------------------
## Visually inspect the new cluster and identify maximum column wise
all.dat.tmp2 <- all.dat.tmp[-c(2,3), -c(2,3)]
row.3 <- apply(all.dat.tmp[c(2,3),-c(2)], 2, min)[c(1,3,2)]
all.dat.tmp2 <- rbind(all.dat.tmp2, row.3)
all.dat.tmp2 <- cbind(all.dat.tmp2, row.3)
colnames(all.dat.tmp2)[3] <- "P(3,4)"
rownames(all.dat.tmp2)[3] <- "P(3,4)"
## Now print the new matrix
all.dat.tmp2 %>% kable()

# ---- problem-1-plot1 --------------------------------------------------------
plot(hclust(all.dat.freeze, method="single"))

# ---- problem-1-s2 --------------------------------------------------------
row.1 <- c(1, .17, .27, .34, .25)
row.2 <- c(.17, 1, .17, .21, .11)
row.3 <- c(.27, .17, 1, .15, .29)
row.4 <- c(.34, .21, .15, 1, .31)
row.5 <- c(.25, .11, .29, .31, 1)
all.dat <- rbind(row.1, row.2, row.3, row.4, row.5)
colnames(all.dat) <- c("P1", "P2", "P3", "P4", "P5")
rownames(all.dat) <- c("P1", "P2", "P3", "P4", "P5")

## Visually inspect the new cluster and identify maximum column wise
all.dat.tmp <- all.dat[-c(2,5), -c(2,5)]
row.4 <- apply(all.dat[c(2,5),-2], 2, max)
all.dat.tmp <- rbind(all.dat.tmp, row.4)
all.dat.tmp <- cbind(all.dat.tmp, row.4)
colnames(all.dat.tmp)[4] <- "P(2,5)"
rownames(all.dat.tmp)[4] <- "P(2,5)"
## Now print the new matrix
all.dat.tmp %>% kable()

# ---- problem-1-s3 --------------------------------------------------------
## Visually inspect the new cluster and identify maximum column wise
all.dat.tmp2 <- all.dat.tmp[-c(2,3), -c(2,3)]
row.3 <- apply(all.dat.tmp[c(2,3),-c(3)], 2, max)[c(1,3,2)]
all.dat.tmp2 <- rbind(all.dat.tmp2, row.3)
all.dat.tmp2 <- cbind(all.dat.tmp2, row.3)
colnames(all.dat.tmp2)[3] <- "P(3,4)"
rownames(all.dat.tmp2)[3] <- "P(3,4)"
## Now print the new matrix
all.dat.tmp2 %>% kable()

# ---- problem-1-s4 --------------------------------------------------------
## Visually inspect the new cluster and identify maximum column wise
all.dat.tmp3 <- all.dat.tmp2[-c(2), -2]

row.2 <- max(all.dat.tmp3[-c(1,2),-c(2,3)])
colnames(all.dat.tmp3)[1] <- "P(1,2,5)"
rownames(all.dat.tmp3)[1] <- "P(1,2,5)"
## Now print the new matrix
all.dat.tmp3 %>% kable()

# ---- problem-1-s5 --------------------------------------------------------
plot(hclust(all.dat.freeze, method="complete"))

# ---- problem-2 --------------------------------------------------------
x <- c(1.6, 2.3, 1.4, 2.1, 1.9, 1.4, 1.8, 2.8, .1, .3, 2.4, 1.8, .15, .22, 1.1)
y <- c(-5.3, 4.5, 11, 4.1, 10.1, -5.1, 10.8, 4.3, .4, .5, 4.4, -5.5, .7, .8, -4.5)
in.dat <- as.matrix(cbind(x,y))
cent.one <- in.dat[c(3, 12),]
print(cent.one)

# ---- problem-2-1 --------------------------------------------------------
euclid <- function(points1, points2) {
  distanceMatrix <- matrix(NA, nrow=dim(points1)[1], ncol=dim(points2)[1])
  for(i in 1:nrow(points2)) {
    distanceMatrix[,i] <- sqrt(rowSums(t(t(points1)-points2[i,])^2))
  }
  distanceMatrix
}

euclid(in.dat, cent.one)

# ---- problem-2-2 --------------------------------------------------------
apply(euclid(in.dat, cent.one), 1, which.min)

# ---- problem-2-3 --------------------------------------------------------
## Now calculate the new mean values
cen.one <- apply(in.dat[which(apply(euclid(in.dat, cent.one), 1, which.min)==1),],2,mean)
cen.two <- apply(in.dat[which(apply(euclid(in.dat, cent.one), 1, which.min)==2),],2,mean)
print(cen.one)
print(cen.two)

# ---- problem-2-4 --------------------------------------------------------
euclid(in.dat, rbind(cen.one, cen.two))

# ---- problem-2-5 --------------------------------------------------------
assignment <- apply(euclid(in.dat, rbind(cen.one, cen.two)), 1, which.min)
apply(euclid(in.dat, rbind(cen.one, cen.two)), 1, which.min)

# ---- problem-2-6 --------------------------------------------------------
sum.square.one <- sum(euclid(in.dat, rbind(cen.one, cen.two))[which(assignment==1),1]^2)
sum.square.two <- sum(euclid(in.dat, rbind(cen.one, cen.two))[which(assignment==2),2]^2)
print(paste("Sum of squares for first cluster: ", sum.square.one))
print(paste("Sum of squares for second cluster: ", sum.square.two))

# ---- problem-2-7 --------------------------------------------------------
clust.one.dat <- in.dat[which(apply(euclid(in.dat, cent.one), 1, which.min)==1),]
clust.two.dat <- in.dat[which(apply(euclid(in.dat, cent.one), 1, which.min)==2),]

print(clust.one.dat[c(1,4),])

# ---- problem-2-8 --------------------------------------------------------
cent.one <- clust.one.dat[c(1,4),]
euclid(clust.one.dat, cent.one)

# ---- problem-2-9 --------------------------------------------------------
apply(euclid(clust.one.dat, cent.one), 1, which.min)

# ---- problem-2-10 --------------------------------------------------------
cen.one1 <- apply(clust.one.dat[which(apply(euclid(clust.one.dat, cent.one), 1, which.min)==1),],2,mean)
cen.two1 <- apply(clust.one.dat[which(apply(euclid(clust.one.dat, cent.one), 1, which.min)==2),],2,mean)
print(cen.one)
print(cen.two)

# ---- problem-2-11 --------------------------------------------------------
euclid(clust.one.dat, rbind(cen.one1, cen.two1))

# ---- problem-2-12 --------------------------------------------------------
assignment1 <- apply(euclid(clust.one.dat, rbind(cen.one1, cen.two1)), 1, which.min)
apply(euclid(clust.one.dat, rbind(cen.one1, cen.two1)), 1, which.min)

# ---- problem-2-13 --------------------------------------------------------
sum.square.one1 <- sum(euclid(clust.one.dat, rbind(cen.one1, cen.two1))[which(assignment1==1),1]^2)
sum.square.two1 <- sum(euclid(clust.one.dat, rbind(cen.one1, cen.two1))[which(assignment1==2),2]^2)
print(paste("Sum of squares for first cluster: ", sum.square.one1))
print(paste("Sum of squares for second cluster: ", sum.square.two1))
print(paste("Sum of squares for third cluster: ", sum.square.two))

# ---- problem-2-14 --------------------------------------------------------
print(clust.two.dat[c(2,5),])

# ---- problem-2-15 --------------------------------------------------------
cent.two <- clust.two.dat[c(2,5),]
euclid(clust.two.dat, cent.two)

# ---- problem-2-16 --------------------------------------------------------
apply(euclid(clust.two.dat, cent.two), 1, which.min)

# ---- problem-2-17 --------------------------------------------------------
cen.one2 <- apply(clust.two.dat[which(apply(euclid(clust.two.dat, cent.two), 1, which.min)==1),],2,mean)
cen.two2 <- apply(clust.two.dat[which(apply(euclid(clust.two.dat, cent.two), 1, which.min)==2),],2,mean)
print(cen.one2)
print(cen.two2)

# ---- problem-2-18 --------------------------------------------------------
euclid(clust.one.dat, rbind(cen.one2, cen.two2))

# ---- problem-2-19 --------------------------------------------------------
assignment2 <- apply(euclid(clust.two.dat, rbind(cen.one2, cen.two2)), 1, which.min)
apply(euclid(clust.two.dat, rbind(cen.one2, cen.two2)), 1, which.min)

# ---- problem-2-20 --------------------------------------------------------
sum.square.one2 <- sum(euclid(clust.two.dat, rbind(cen.one2, cen.two2))[which(assignment2==1),1]^2)
sum.square.two2 <- sum(euclid(clust.two.dat, rbind(cen.one2, cen.two2))[which(assignment2==2),2]^2)
print(paste("Sum of squares for first cluster: ", sum.square.one1))
print(paste("Sum of squares for second cluster: ", sum.square.two1))
print(paste("Sum of squares for third cluster: ", sum.square.one2))
print(paste("Sum of squares for fourth cluster: ", sum.square.two2))

# ---- problem-3-1 --------------------------------------------------------
## Read in the data
in.dat.orig <- read.csv("./Seed_Data.csv")
in.dat <- in.dat.orig[,1:7]

## In order to protect against differences in scale across the metrics - the data will be z scored
in.dat <- scale(in.dat)[,]

# ---- problem-3-c --------------------------------------------------------
euclid <- function(points1, points2) {
  distanceMatrix <- matrix(NA, nrow=dim(points1)[1], ncol=dim(points2)[1])
  for(i in 1:nrow(points2)) {
    distanceMatrix[,i] <- sqrt(rowSums(t(t(points1)-points2[i,])^2))
  }
  distanceMatrix
}

K_means <- function(x, centers, distFun =euclid, nItter=5) {
  clusterHistory <- list()
  centerHistory <- list()
  
  for(i in 1:nItter) {
    distsToCenters <- distFun(x, centers)
    clusters <- apply(distsToCenters, 1, which.min)
    centers <- apply(x, 2, tapply, clusters, mean)
    # Saving history
    clusterHistory[[i]] <- clusters
    centerHistory[[i]] <- centers
  }
  ## Now find the sum of squares
  sum.squared.within <- list()
  for(i in 1:dim(centers)[1]){
    ## Calculate the sum of squares within for this cluster
    index <- which(clusterHistory[[nItter]]==i)
    sum.tmp <- sum(euclid(x, centerHistory[[nItter]])[index,i]^2)
    sum.squared.within[[i]] <- sum.tmp
  }
  ## Now calculate the sum total
  sum.squared.total <- sum(unlist(sum.squared.within))
  
  list(clusters=clusterHistory[[nItter]], centers=centerHistory[[nItter]], sum.squared.within=sum.squared.within, sum.squared.total=sum.squared.total)
}

## Now run the code for 2,3,4,5, and 6 clusters
set.seed(16)
two.solution <- K_means(x = in.dat, centers = in.dat[sample(x = 1:dim(in.dat)[1], 2),])
print(two.solution$sum.squared.total)
print(two.solution$sum.squared.within)
print(two.solution$centers)
print(two.solution$clusters)
three.solution <- K_means(x = in.dat, centers = in.dat[sample(x = 1:dim(in.dat)[1], 3),])
print(three.solution$sum.squared.total)
print(three.solution$sum.squared.within)
print(three.solution$centers)
print(three.solution$clusters)
four.solution <- K_means(x = in.dat, centers = in.dat[sample(x = 1:dim(in.dat)[1], 4),])
print(four.solution$sum.squared.total)
print(four.solution$sum.squared.within)
print(four.solution$centers)
print(four.solution$clusters)
five.solution <- K_means(x = in.dat, centers = in.dat[sample(x = 1:dim(in.dat)[1], 5),])
print(five.solution$sum.squared.total)
print(five.solution$sum.squared.within)
print(five.solution$centers)
print(five.solution$clusters)
six.solution <- K_means(x = in.dat, centers = in.dat[sample(x = 1:dim(in.dat)[1], 6),])
print(six.solution$sum.squared.total)
print(six.solution$sum.squared.within)
print(six.solution$centers)
print(six.solution$clusters)


# ---- problem-3-c2 --------------------------------------------------------
all.ss.vals <- c(two.solution$sum.squared.total,
                 three.solution$sum.squared.total,
                 four.solution$sum.squared.total,
                 five.solution$sum.squared.total,
                 six.solution$sum.squared.total)
plot(y=all.ss.vals, x=2:6)


# ---- problem-3-d --------------------------------------------------------
in.dat.dist <- dist(in.dat)
agglom.clust.max <- hclust(in.dat.dist, method="complete")
agglom.clust.ward <- hclust(in.dat.dist, method="ward.D")
# Cut tree into 4 groups
max.grp<- cutree(agglom.clust.max, k = 4)
ward.grp <- cutree(agglom.clust.ward, k = 4)


## Now estimate the total sum of squares for each of the hclust clusters
# First assign clusters to the in data
kMeansClass <- four.solution$clusters
maxClass <- max.grp
wardClass <- ward.grp
in.dat <- cbind(in.dat, kMeansClass, maxClass, wardClass)
## Now estimate the total sum of squares for each of these 4 class solutions
# Do this in a loop for each cluster 
sse.max <- list()
sse.ward <- list()
for(i in 1:4){
  ## First grab the max class sum of squares within each class
  centroid <- apply(in.dat[which(in.dat[,"maxClass"]==i),1:7], 2, mean)
  ## Now estimate the distance from these
  sse.max[[i]] <- sum(euclid(in.dat[which(in.dat[,"maxClass"]==i),1:7], rbind(centroid, centroid))[,1]^2)
  
  ## Now do the same for the ward method
  centroid <- apply(in.dat[which(in.dat[,"wardClass"]==i),1:7], 2, mean)
  ## Now estimate the distance from these
  sse.ward[[i]] <- sum(euclid(in.dat[which(in.dat[,"wardClass"]==i),1:7], rbind(centroid, centroid))[,1]^2)
}

## Now obtain the total for each
total.max <- sum(unlist(sse.max))
total.ward <- sum(unlist(sse.ward))

# ---- problem-3-d-totalsse --------------------------------------------------------
out.table <- cbind(total.max, total.ward)
colnames(out.table) <- c("Complete Link Sum of squared error", "Ward's ICS Sum of squared error" )
out.table %>% kable()

# ---- problem-3-d-clusterAssign --------------------------------------------------------
ID <- seq(1, dim(in.dat)[1],1)
out.table <- cbind(ID, maxClass, wardClass)
for(i in 1:4){
  out.message1 <- paste("The class assignment is group: ", i)
  out.message2 <- paste("The max method SSE is: ", sse.max[[i]])
  out.message3 <- paste("The Ward's method SSE is: ", sse.ward[[i]])
  print(out.message1)
  print(out.message2)
  print(out.message3)
}
out.table %>% kable(.,
                    format    = "latex", 
                    longtable = T, 
                    digits = 3)

