## Brief notes:
##  A free chapter three can be found here: https://www-users.cs.umn.edu/~kumar001/dmbook/ch3_classification.pdf
## Useful sources can be found here: https://github.com/mhahsler/Introduction_to_Data_Mining_R_Examples

# ---- load-packages --------------------------------------------------------
library("tidyverse")
library("knitr")
library("kableExtra")
library("pracma")
library("rpart")
library("caret")
library("RWeka")
WPM("load-package", "simpleEducationalLearningSchemes")
source("../../../adroseHelperScripts/R/afgrHelpFunc.R")

## Declare functions for plotting
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# ---- problem-1-a --------------------------------------------------------
# First declare the data
horse.surgery <- c(0,1,1,0,0,1,1,0,0,0,1,1)
horse.pulse <- c(92, 88, 64, 48, 76, 76, 88, 48, 92, 48, 64, 64)
adbominal.distension <- c("None", "Severe", "Severe", 
                          "Slight", "Slight", "None", 
                          "Severe", "Severe", "Severe", 
                          "Slight", "Slight", "Slight")
treat.applied <- c(3, 2, 2, 1, 4, 1, 3, 1, 4, 1, 1, 4)
treat.applied <- paste("Level", treat.applied)
horse.data <- data.frame(horse.surgery, horse.pulse, adbominal.distension, treat.applied)

# Now declare a function which will calculate the entropy of a given set of classes
calc.entropy <- function(tableVal){
  ## This function requires a table of the classes to be classified
  freqs = tableVal/sum(tableVal)
  out.val <- -sum(ifelse(freqs > 0, freqs * log(freqs), 0))
  out.val <- out.val / log(2)
  return(out.val)
}

## Now calculate the entire dataset entropy
entropy.treatment <- calc.entropy(table(horse.data$treat.applied)) 
# Total dataset entropy == 1.887919

### First explore first round cuts
## First calculate surgery here
entropy.treatment.surgery.no <- calc.entropy(table(horse.data$treat.applied[which(horse.data$horse.surgery==0)]))
entropy.treatment.surgery.yes <- calc.entropy(table(horse.data$treat.applied[which(horse.data$horse.surgery==1)]))
## Now calculate the weighted difference
weigthed.entropy.step1.1 <- (6/12 * entropy.treatment.surgery.no) + (6/12 * entropy.treatment.surgery.yes)
entropy.gain1.1 <- entropy.treatment - weigthed.entropy.step1.1
## Information gain for surgery == .1991966

## Now try information gain for abdominal distension
entropy.treatment.distenssion.none <- calc.entropy(table(horse.data$treat.applied[which(horse.data$adbominal.distension=="None")]))
entropy.treatment.distenssion.slight <- calc.entropy(table(horse.data$treat.applied[which(horse.data$adbominal.distension=="Slight")]))
entropy.treatment.distenssion.severe <- calc.entropy(table(horse.data$treat.applied[which(horse.data$adbominal.distension=="Severe")]))
## Now calculate the weighted difference
weigthed.entropy.step1.2 <- (2/12 * entropy.treatment.distenssion.none) + (5/12 * entropy.treatment.distenssion.slight)+ (5/12 * entropy.treatment.distenssion.severe)
entropy.gain1.2 <- entropy.treatment - weigthed.entropy.step1.2
## Information gain here ==.5158857


## Now explore HR with various cutoff points
for(i in unique(horse.data$horse.pulse)){
  horse.data$hrBin <- 0
  horse.data$hrBin[horse.data$horse.pulse>i] <- 1
  entropy.treatment.hr.bin.0 <- calc.entropy(table(horse.data$treat.applied[which(horse.data$hrBin==0)]))
  entropy.treatment.hr.bin.1 <- calc.entropy(table(horse.data$treat.applied[which(horse.data$hrBin==1)]))
  weigthed.entropy.step1.4 <- (sum(horse.data$hrBin==0)/12 * entropy.treatment.hr.bin.0) + (sum(horse.data$hrBin==1)/12 * entropy.treatment.hr.bin.1)
  entropy.gain1.4 <- entropy.treatment - weigthed.entropy.step1.4
  #print(paste(i, ":", entropy.gain1.4))
}
# [1] "92 : NA"
# [1] "88 : 0.253781796468065"
# [1] "64 : 0.302956001949977"
# [1] "48 : 0.406715376769688"
# [1] "76 : 0.522055208874201"

## The first step will be to create a binary variable using heartrate with a cutoff of > 76
## If HR is lower than 76 --> treatment group 1

horse.data$hrBin <- 0
horse.data$hrBin[horse.data$horse.pulse>76] <- 1

## First calculate the total entropy of the pulse <= 76 cohort
entropy.treatment.low.hr <- calc.entropy(table(horse.data$treat.applied[which(horse.data$hrBin==0)]))

## Now calculate entropy for surgery
entropy.treatment.low.hr.surgery.no <- calc.entropy(table(horse.data$treat.applied[which(horse.data$hrBin==0 & horse.data$horse.surgery==0)]))
entropy.treatment.low.hr.surgery.yes <- calc.entropy(table(horse.data$treat.applied[which(horse.data$hrBin==0 & horse.data$horse.surgery==1)]))

## Now calculate the weighted difference
weigthed.entropy.step2.1 <- (4/8 * entropy.treatment.low.hr.surgery.no) + (4/8 * entropy.treatment.low.hr.surgery.yes)
entropy.gain2.1 <- entropy.treatment.low.hr - weigthed.entropy.step2.1
## Information gain for surgery == 0.1431559

## Now try information gain for abdominal distension
entropy.treatment.low.none <- calc.entropy(table(horse.data$treat.applied[which(horse.data$adbominal.distension=="None" & horse.data$hrBin==0)])) # n=1
entropy.treatment.low.slight <- calc.entropy(table(horse.data$treat.applied[which(horse.data$adbominal.distension=="Slight"& horse.data$hrBin==0)])) # n=5
entropy.treatment.low.severe <- calc.entropy(table(horse.data$treat.applied[which(horse.data$adbominal.distension=="Severe"& horse.data$hrBin==0)])) # n=2

## Now calculate the information gain
weigthed.entropy.step2.2 <- (1/8 * entropy.treatment.low.none) + (5/8 * entropy.treatment.low.slight) + (2/8 * entropy.treatment.low.severe)
entropy.gain2.2 <- entropy.treatment.low.hr - weigthed.entropy.step2.2
## Information gain for surgery == 0.4419508

## The second step for those horses who have lower heart rate will be to flag for abdominal distension:
# for those with slight & none --> level 1 treatment
# for those with Severe abdominal distension --> level 2 treatment

## Now run through the same procedure for the high hr observations
## First calculate the total entropy of the pulse > 76 cohort
entropy.treatment.high.hr <- calc.entropy(table(horse.data$treat.applied[which(horse.data$hrBin==1)]))

## Now calculate entropy for surgery
entropy.treatment.high.hr.surgery.no <- calc.entropy(table(horse.data$treat.applied[which(horse.data$hrBin==1 & horse.data$horse.surgery==0)]))
entropy.treatment.high.hr.surgery.yes <- calc.entropy(table(horse.data$treat.applied[which(horse.data$hrBin==1 & horse.data$horse.surgery==1)]))

## Now calculate the weighted difference
weigthed.entropy.step2.1 <- (2/4 * entropy.treatment.high.hr.surgery.no) + (2/4 * entropy.treatment.high.hr.surgery.yes)
entropy.gain2.1 <- entropy.treatment.low.hr - weigthed.entropy.step2.1
## Information gain for surgery == 0.5

## Now try information gain for abdominal distension
entropy.treatment.high.none <- calc.entropy(table(horse.data$treat.applied[which(horse.data$adbominal.distension=="None" & horse.data$hrBin==1)])) # n=1
entropy.treatment.high.slight <- calc.entropy(table(horse.data$treat.applied[which(horse.data$adbominal.distension=="Slight"& horse.data$hrBin==1)])) # n=0
entropy.treatment.high.severe <- calc.entropy(table(horse.data$treat.applied[which(horse.data$adbominal.distension=="Severe"& horse.data$hrBin==1)])) # n=3

## Now calculate the information gain
weigthed.entropy.step2.2 <- (1/4 * entropy.treatment.high.none) +  (3/4 * entropy.treatment.high.severe)
entropy.gain2.2 <- entropy.treatment.low.hr - weigthed.entropy.step2.2
## Information gain for distension == 0.3112781

## The second step for those horses who have higher heart rate will be to flag for surgery:
# for those with surgery --> level 2 treatment
# for those without surgery --> level 4 treatment

# ---- problem-1-b --------------------------------------------------------
calc.gini <- function(counts){
  x <- c(1,2,3,4)
  x <- rep(x, counts)
  n <- length(x)
  x <- sort(x)
  n <- length(x)
  x <- sort(x)
  res <- 2 * sum(x * 1:n)/(n * sum(x)) - 1 - (1/n)
  res <- n/(n - 1) * res
  return(pmax(0, res))
}

## Now calculate the entire dataset gini
gini.treatment <- calc.gini(table(horse.data$treat.applied)) 
# Total dataset gini == 1.887919

### First explore first round cuts
## First calculate surgery here
gini.treatment.surgery.no <- calc.gini(table(horse.data$treat.applied[which(horse.data$horse.surgery==0)]))
gini.treatment.surgery.yes <- calc.gini(table(horse.data$treat.applied[which(horse.data$horse.surgery==1)]))
## Now calculate the weighted difference
weigthed.gini.step1.1 <- (6/12 * gini.treatment.surgery.no) + (6/12 * gini.treatment.surgery.yes)
gini.gain1.1 <- gini.treatment - weigthed.gini.step1.1
## Information gain for surgery == -0.02065342

## Now try information gain for abdominal distension
gini.treatment.distenssion.none <- calc.gini(table(horse.data$treat.applied[which(horse.data$adbominal.distension=="None")]))
gini.treatment.distenssion.slight <- calc.gini(table(horse.data$treat.applied[which(horse.data$adbominal.distension=="Slight")]))
gini.treatment.distenssion.severe <- calc.gini(table(horse.data$treat.applied[which(horse.data$adbominal.distension=="Severe")]))
## Now calculate the weighted difference
weigthed.gini.step1.2 <- (2/12 * gini.treatment.distenssion.none) + (5/12 * gini.treatment.distenssion.slight)+ (5/12 * gini.treatment.distenssion.severe)
gini.gain1.2 <- gini.treatment - weigthed.gini.step1.2
## Information gain here == -0.04871633


## Now explore HR with various cutoff points
for(i in unique(horse.data$horse.pulse)){
  horse.data$hrBin <- 0
  horse.data$hrBin[horse.data$horse.pulse>i] <- 1
  gini.treatment.hr.bin.0 <- calc.gini(table(horse.data$treat.applied[which(horse.data$hrBin==0)]))
  gini.treatment.hr.bin.1 <- calc.gini(table(horse.data$treat.applied[which(horse.data$hrBin==1)]))
  weigthed.gini.step1.4 <- (sum(horse.data$hrBin==0)/12 * gini.treatment.hr.bin.0) + (sum(horse.data$hrBin==1)/12 * gini.treatment.hr.bin.1)
  gini.gain1.4 <- gini.treatment - weigthed.gini.step1.4
  print(paste(i, ":", gini.gain1.4))
}
# [1] "92 : NaN"
# [1] "88 : 0.0157527657527657"
# [1] "64 : 0.0230699148346207"
# [1] "48 : 0.123474326599326"
# [1] "76 : 0.0234247234247234"

## The first step will be to create a binary variable using heartrate with a cutoff of > 49
## If HR is lower than 49 --> treatment group 1

horse.data$hrBin <- 0
horse.data$hrBin[horse.data$horse.pulse>48] <- 1

## Now run through this same classification procedure for the high hr horses
## First calculate the total entropy of the pulse > 76 cohort
gini.treatment.high.hr <- calc.gini(table(horse.data$treat.applied[which(horse.data$hrBin==1)]))

## Now calculate gini for surgery
gini.treatment.high.hr.surgery.no <- calc.gini(table(horse.data$treat.applied[which(horse.data$hrBin==1 & horse.data$horse.surgery==0)]))
gini.treatment.high.hr.surgery.yes <- calc.gini(table(horse.data$treat.applied[which(horse.data$hrBin==1 & horse.data$horse.surgery==1)]))

## Now calculate the weighted difference
weigthed.gini.step2.1 <- (3/9 * gini.treatment.high.hr.surgery.no) + (6/9 * gini.treatment.high.hr.surgery.yes)
gini.gain2.1 <- gini.treatment.high.hr - weigthed.gini.step2.1
## Information gain for surgery == 0.02514569

## Now try information gain for abdominal distension
gini.treatment.high.none <- calc.gini(table(horse.data$treat.applied[which(horse.data$adbominal.distension=="None" & horse.data$hrBin==1)])) # n=2
gini.treatment.high.slight <- calc.gini(table(horse.data$treat.applied[which(horse.data$adbominal.distension=="Slight"& horse.data$hrBin==1)])) # n=3
gini.treatment.high.severe <- calc.gini(table(horse.data$treat.applied[which(horse.data$adbominal.distension=="Severe"& horse.data$hrBin==1)])) # n=4

## Now calculate the information gain
weigthed.gini.step2.2 <- (2/9 * gini.treatment.high.none) +  (3/9 * gini.treatment.high.slight) + (4/9 * gini.treatment.high.severe)
gini.gain2.2 <- gini.treatment.high.hr - weigthed.gini.step2.2
## Information gain for surgery == -0.04566498

## The second step for those horses who have higher heart rate will be to flag for surgery:
# for those with surgery --> level 2 treatment
# for those without surgery --> level 4 treatment




# ---- problem-2-a --------------------------------------------------------
# Load the data
in.data <- read.csv('../Data/wine.csv')
# Now make a plot of the Magnesium; Color Intensity; and Malic Acid
out.plot.standard <- in.data[,c("Magnesium", "Color_intensity", "Malic_acid")] %>% 
  mutate(across(where(is.numeric), scale)) %>%  
  reshape2::melt(.) %>% 
  ggplot(., aes(x=variable, y=value)) +
  geom_boxplot() +
  theme_bw() +
  ggtitle("z-score values boxplot")


out.plot.raw <- in.data[,c("Magnesium", "Color_intensity", "Malic_acid")] %>% 
  #mutate(across(where(is.numeric), scale)) %>%  
  reshape2::melt(.) %>% 
  ggplot(., aes(x=variable, y=value)) +
  geom_boxplot() +
  theme_bw() +
  ggtitle("Raw values boxplot")

multiplot(out.plot.raw, out.plot.standard, cols = 2)

# ---- problem-2-b --------------------------------------------------------
## Now identify the outliers
out_ind1 <- which(in.data$Magnesium %in% boxplot.stats(in.data$Magnesium)$out)
out_ind2 <- which(in.data$Color_intensity %in% boxplot.stats(in.data$Color_intensity)$out)
out_ind3 <- which(in.data$Malic_acid %in% boxplot.stats(in.data$Malic_acid)$out)
out_ind <- union(out_ind1, out_ind2)
out_ind <- union(out_ind, out_ind3)
## Remove the outliers from the data
long_dt <- in.data[-out_ind,]
## Now print the data frame
long_dt_print <- long_dt
colnames(long_dt_print) <- substring(names(long_dt_print), 5)
kable(long_dt_print, 
  format    = "latex", 
  longtable = T, 
  digits = 3) %>%
  kable_styling(full_width = FALSE)

# ---- problem-2-c --------------------------------------------------------
# Now make a plot of the Magnesium; Color Intensity; and Malic Acid
out.plot.standard <- long_dt[,c("Magnesium", "Color_intensity", "Malic_acid")] %>% 
  mutate(across(where(is.numeric), scale)) %>%  
  reshape2::melt(.) %>% 
  ggplot(., aes(x=variable, y=value)) +
  geom_boxplot() +
  theme_bw() +
  ggtitle("z-score values boxplot")


out.plot.raw <- long_dt[,c("Magnesium", "Color_intensity", "Malic_acid")] %>% 
  #mutate(across(where(is.numeric), scale)) %>%  
  reshape2::melt(.) %>% 
  ggplot(., aes(x=variable, y=value)) +
  geom_boxplot() +
  theme_bw() +
  ggtitle("Raw values boxplot")

multiplot(out.plot.raw, out.plot.standard, cols = 2)

# ---- problem-2-d --------------------------------------------------------
long_dt$Cultivar <- factor(long_dt$Cultivar)
mod.1 <- rpart::rpart(Cultivar ~., data=long_dt)

## Now plot the tree
plot(mod.1)
text(mod.1)

## Now run through a cross validation using these same data with k=5
# First create the folds
set.seed(16)
folds <- caret::createFolds(long_dt$Cultivar, k = 5)

## Now prepare the output
confusion.matrix.out <- list()
model.error <- list()
## Now run a loop where in each loop a model is trained
## and then tested in the left out sample
for(i in 1:length(folds)){
  ## First train the model
  mod.tmp <- rpart::rpart(Cultivar~., data=long_dt[-folds[[i]],])
  ## Now get the confusion matrix
  pred.vals <- predict(mod.tmp, newdata = long_dt[folds[[i]],], type='class')
  true.vals <- long_dt[folds[[i]],"Cultivar"]
  confusion.matrix <- table(pred.vals, true.vals)
  ## Now get the error
  total.vals <- sum(confusion.matrix)
  correct.vals <- sum(diag(confusion.matrix))
  model.error.tmp <- 1 - correct.vals/total.vals
  ## Now output these values
  print(confusion.matrix)
  confusion.matrix.out[[i]] <- confusion.matrix
  model.error[[i]] <- model.error.tmp
}
model.error1 <- model.error

# ---- problem-2-e --------------------------------------------------------
ID3 <- make_Weka_classifier("weka/classifiers/trees/Id3")
long_dt$Cultivar <- factor(long_dt$Cultivar)

## ID3 requires nominal attributes so I am going to 
## convert all of my values into factors based on quartiles
long_dt_id3 <- long_dt
long_dt_id3[,2:14] <- apply(long_dt_id3[,2:14], 2, function(x)cut(x, breaks = 4))
for(i in 2:14){long_dt_id3[,i] <- factor(long_dt_id3[,i])}
mod.2 <- ID3(`Cultivar` ~ . , data=long_dt_id3)

## Now run through a cross validation using these same data with k=5
# First create the folds
set.seed(16)
folds <- caret::createFolds(long_dt_id3$Cultivar, k = 5)

## Now prepare the output
confusion.matrix.out <- list()
model.error <- list()
## Now run a loop where in each loop a model is trained
## and then tested in the left out sample
for(i in 1:length(folds)){
  ## First train the model
  mod.tmp <- ID3(Cultivar~., data=long_dt_id3[-folds[[i]],])
  ## Now get the confusion matrix
  pred.vals <- predict(mod.tmp, newdata = long_dt_id3[folds[[i]],], type='class')
  true.vals <- long_dt[folds[[i]],"Cultivar"]
  confusion.matrix <- table(pred.vals, true.vals)
  ## Now get the error
  total.vals <- sum(confusion.matrix)
  correct.vals <- sum(diag(confusion.matrix))
  model.error.tmp <- 1 - correct.vals/total.vals
  ## Now output these values
  print(confusion.matrix)
  confusion.matrix.out[[i]] <- confusion.matrix
  model.error[[i]] <- model.error.tmp
}
model.error2 <- model.error

# ---- problem-2-f --------------------------------------------------------
# First obtain the model mean errors from the 5-fold cross validation
mean.model.error.1 <- mean(unlist(model.error1))
mean.model.error.2 <- mean(unlist(model.error2))


## Now use these errors to estimate the error varaince
model.error.varaince = mean.model.error.1*(1-mean.model.error.1)/dim(long_dt)[1] + mean.model.error.2*(1-mean.model.error.2)/dim(long_dt)[1]
model.difference = mean.model.error.1 - mean.model.error.2

## Now calculate the confidence interval
upper.bound = model.difference+2.58*sqrt(model.error.varaince)
lower.bound = model.difference-2.58*sqrt(model.error.varaince)


## Now return this value
out.string <- paste("The estimated model difference is ", model.difference, "with a lower bound 99% confidence interval of ", lower.bound, "and an upper bound 99% confidence interval estimate of ", upper.bound)
print(out.string)
out.string2 <- paste("Because this confidence interval includes 0 within it's boundaries it leads us to conclude that there is no significant difference between the error of these models")
print(out.string2)

# ---- problem-2-g --------------------------------------------------------
## Now return the predicted class from in an input tuple
input.tuple.1  <- c(NA, apply(long_dt[,2:14], 2, function(x) sample(x, 1)))
input.tuple.1 <- as.data.frame(t(input.tuple.1))
## Now print the tuple to kable
input.tuple.1 %>% 
  kable(., , 
            format    = "latex", 
            longtable = T, 
            digits = 3) %>%
  kable_styling(full_width = FALSE)

## Now predict the class for this tuple
pred.class <- predict(mod.1, input.tuple.1, type='class')
## Now print this
print(paste("Predicted class input 1:", pred.class))

input.tuple.2  <- c(NA, apply(long_dt[,2:14], 2, function(x) sample(x, 1)))
input.tuple.2 <- as.data.frame(t(input.tuple.2))
## Now print the tuple to kable
input.tuple.2 %>% 
  kable(., , 
        format    = "latex", 
        longtable = T, 
        digits = 3) %>%
  kable_styling(full_width = FALSE)

## Now predict the class for this tuple
pred.class <- predict(mod.1, input.tuple.2, type='class')
## Now print this
print(paste("Predicted class input 2:", pred.class))

input.tuple.3  <- c(NA, apply(long_dt[,2:14], 2, function(x) sample(x, 1)))
input.tuple.3 <- as.data.frame(t(input.tuple.3))
## Now print the tuple to kable
input.tuple.3 %>% 
  kable(., , 
        format    = "latex", 
        longtable = T, 
        digits = 3) %>%
  kable_styling(full_width = FALSE)

## Now predict the class for this tuple
pred.class <- predict(mod.1, input.tuple.3, type='class')
## Now print this
print(paste("Predicted class input 3:", pred.class))
