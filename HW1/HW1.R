# ---- load-packages --------------------------------------------------------
library("tidyverse")
library("knitr")
library("kableExtra")
library("pracma")


# ---- q-3-1 --------------------------------------------------------
x <- c(0,1,1,1,0,1)
y <- c(0,1,0,1,0,1)
## The SMC is calculated as the number of matches divided by the number of entries
f.zo <- length(which(x==0 & y==1))
f.oz <- length(which(x==1 & y==0))
f.zz <- length(which(x==0 & y==0))
f.oo <- length(which(x==1 & y==1))

# First calc the SMC
smc.one <- sum(f.zz, f.oo) / length(x)

# Now calc the jaccard coef
# number of 11 mathces versus number of non-zero attributes
jac.one <- f.oo / (f.oz + f.zo + f.oo)

## Now calc the cosine correlation
dot.prod <- pracma::dot(x,y)
distance.x <- dot(x,x)^.5
distance.y <- dot(y,y)^.5
cos.value <-  dot.prod / (distance.x * distance.y)
# now confirm
cos.value.lsa <- lsa::cosine(x,y)

## Now calculate the correlation
cor.val <- cor(x, y)

## Now calculate the euclidean distance
first.diff <- sum(x - y ^ 2) ^ 1

## Now do city block
second.diff <- sum(x - y ^ 2) ^ 1/2

## Now create a table with these data
metric <- c("SMC", "Jaccard Coefficient", "Cosine", "Correlation", "Minkowski (r=1)", "Minkowski (r=2)")
values <- c(smc.one, jac.one, cos.value, cor.val, first.diff, second.diff)
to.kable <- rbind(metric, round(values, 2))
to.kable %>% 
  kbl() %>% 
  kable_classic_2(full_width = F)

# ---- q-3-2 --------------------------------------------------------
x <- c(35,27,33,24,13,25,43)
y <- c(205401, -148497, -69783, 84958, -48436, 159102, 270076)



# ---- q-4 --------------------------------------------------------
# Load the data
in.data <- read.csv('./Data/auto-mpg.csv', na.strings = '?')

# ---- q-4-1 --------------------------------------------------------
# Load the data
in.data <- read.csv('../Data/auto-mpg.csv', na.strings = '?')

## Create a function to impute using the mean function given a vector
impute.mean <- function(x){
  # Check to see if the values are numeric - if they are run the imputation
  # if values are not numeric - reutnr the input data
  if(!is.numeric(x)){
    out.dat <- x
  }
  else{
    out.dat <- replace(x, is.na(x), mean(x, na.rm = TRUE))
  }
  return(out.dat)
}
orig.data <- in.data
# Now create the imputed dataset
in.data[] <- lapply(in.data, impute.mean)
## Now compare the complete cases between the two
table(complete.cases(orig.data), complete.cases(in.data)) %>% 
  kbl() %>% 
  kable_classic_2(full_width = F)



# ---- q-4-2 --------------------------------------------------------
# Draw a random sample without replacment
drawn.sample <- in.data[sample.int(dim(in.data)[1], size=50, replace=FALSE),]

# ---- q-4-3 --------------------------------------------------------
# Create a function which will do equal frequncy discretization
equalFreqBins <- function(x,n, na.rm=TRUE){
  ## Find the length
  nx <- length(x)
  ## Deal with uneven values
  nrepl <- floor(nx/n)
  ## Add to those for uneven lengths
  nplus <- sample(1:n,nx - nrepl*n)
  nrep <- rep(nrepl,n)
  nrep[nplus] <- nrepl+1
  ## Now assign the values
  x[order(x)] <- rep(seq.int(n),nrep)
  return(x)
}

# ---- q-4-4 --------------------------------------------------------
# Create a function which will create equal width bins
equalWidthBins <- function(x, n, na.rm=TRUE){
  ## Find the cut values
  widthVal <- (range(x, na.rm = na.rm)[2] - range(x, na.rm = na.rm)[1])/n
  cutVals <- seq(range(x, na.rm = na.rm)[1], range(x, na.rm = na.rm)[2], widthVal)
  ## Now assign the output
  x.out <- x
  for(i in 1:length(cutVals)-1){x.out[which(x>=cutVals[i])] <- i}
  return(x.out)
}

# ---- q-4-5 --------------------------------------------------------
in.data$random_value1 <- equalFreqBins(in.data$horsepower, 6)
in.data$random_value2 <- equalWidthBins(in.data$horsepower, 6)
## Create the first plot
plot(in.data$random_value1, in.data$horsepower)

## Now do the second plot
plot(in.data$random_value2, in.data$horsepower)

# ---- q-4-6 --------------------------------------------------------
pairs(in.data[,1:8])
