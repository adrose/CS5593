# ---- load-packages --------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(psych)
library(ggplot2)
library(reshape2)
library(progress)
library(knitr)
library(kableExtra)

# ---- problem-1-a --------------------------------------------------------
in.dat <- c(152.36, 130.38, 101.54, 96.26, 88.03, 85.66, 83.62, 76.53, 74.36, 73.87, 73.36, 73.35,68.26, 65.25, 63.68, 63.05, 57.53)
hist(in.dat)
abline(v=mean(in.dat), col="black")
abline(v=median(in.dat), col="red")

# ---- problem-1-b --------------------------------------------------------
parametric.method <- function(x, threshold=3){
  ## First calculate the mean of the data
  mean.val <- sum(x)/length(x)
  ## Now calc the standard deviation
  stand.val <- sqrt(sum((x - mean.val)^2)/length(x))
  ## Now calculate the z-scores
  z.scores <- (x - mean.val) / stand.val
  ## Now identify p-values
  p.values <- dnorm(z.scores)
  ## Now identify outliers
  index <- which(abs(z.scores)>threshold)
  ## Return these values
  return(list(z.scores <- z.scores, p.values <- p.values, index <- index))
}

# ---- problem-1-c --------------------------------------------------------
nonparametric.method <- function(x, p=.05){
  ## In a loop calculate the z scores; 
  ## Then calculate the p values from a t distribution
  ## then flag and remove and observations below the alpha level
  ## repeat this until no more flags
  flag <- TRUE 
  orig.vals <- x
  out.index <- NULL
  while(flag == TRUE){
    z.scores <- parametric.method(abs(x))[[1]]
    max.val <- z.scores[which.max(z.scores)]
    ## Now calculate the threshold
    n <- length(z.scores)
    threshold <- ((n-1)/sqrt(n))*sqrt(qt((1-p)/n,n-2)^2/(n-2+qt((1-p)/n,n-2)^2))
    ## check if the z score is larger than the threshold
    if(max.val > threshold){
      out.index <- c(out.index, which.max(z.scores))
      x <- x[-which.max(z.scores)]
    }else{
      flag <- FALSE
    }
  }
  return(list(out.vals = x, out.index = out.index))
}

# ---- problem-1-d --------------------------------------------------------
euclid <- function(points1, points2) {
  if(is.null(dim(points1))){
    points1 <- cbind(points1,0)
  }
  if(is.null(dim(points2))){
    points2 <- cbind(points2,0)
  }
  distanceMatrix <- matrix(NA, nrow=dim(points1)[1], ncol=dim(points2)[1])
  for(i in 1:nrow(points2)) {
    distanceMatrix[,i] <- sqrt(rowSums(t(t(points1)-points2[i,])^2))
  }
  distanceMatrix
}

neighbor.method <- function(x, k=3){
  ## This function will calculate the distance for each object from it's kth nearest neighbor'
  ## First identify distance from all other points
  dist.mat <- euclid(x, x)
  diag(dist.mat) <- NA
  ## Now go through each column and find the k'th value
  dist.vals <- apply(dist.mat, 2, function(x) x[order(x, decreasing = F)][k])
  ## Now prepare the output
  out.vals <- cbind(x, dist.vals)
  ## Now return these
  return(out.vals)
}
# ---- problem-1-e-1 --------------------------------------------------------
# Run with w = 1
parametric.method(in.dat, threshold = 1)
# Run with w = 2
parametric.method(in.dat, threshold = 2)
# Run with w = 3
parametric.method(in.dat, threshold = 3)

# ---- problem-1-e-2 --------------------------------------------------------

# Nonparametric with alpha = .05
nonparametric.method(in.dat, p=.05)

# ---- problem-1-e-3 --------------------------------------------------------
# neighbor method w/ k = 2
neighbor.method(in.dat, k = 2)

# neighbor method w/ k = 3
neighbor.method(in.dat, k = 3)

# ---- problem-2 --------------------------------------------------------
accident <- c("no","no", "yes", "yes", "yes")
weather <- c("good", "good", "good", "bad", "bad")
construction <- c("no", "yes", "no", "no", "yes")
number.positive <- c(5,10,10,10,5)
number.negative <- c(30,20,5,5,0)

# ---- problem-2-a --------------------------------------------------------
## Conditional probability of an accident given congestion
# This will look across all cases where there was congestion (n=40)
# And report the instances where there was an accident
all.positive <- 40
all.accident.and.positive <- 25
conditional.prob.one <- 25/40


## Conditional probability of an accident given no congestion
all.negative <- 60
all.nonacident.and.negative <- 10
conditional.prob.two <- 10/60


## probability of good weather given congestion
all.positive <- 40
all.good.weather.and.positive <- 10
conditional.prob.three <- 10/40

## probability of good weather given no congestion
all.negative <- 60
all.good.weather.and.negative <- 50
conditional.prob.four <- 50/60

## probability of construction given congestion
all.positive <- 40
all.construction.and.positve <- 15
conditional.prob.five <- 15/40

## probability of construction given no congestion
all.negative <- 60
all.construction.and.negative <- 20
conditional.prob.six <- 20/60

# ---- problem-2-b --------------------------------------------------------
# Find the class probability for a instance where:
## accident == no; weather == bad; and construction == yes
## Calculate the probability of congestion == yes
cond.prob.yes <- conditional.prob.one * 