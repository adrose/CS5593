## Brief notes:
##  A free chapter three can be found here: https://www-users.cs.umn.edu/~kumar001/dmbook/ch3_classification.pdf
## Useful sources can be found here: https://github.com/mhahsler/Introduction_to_Data_Mining_R_Examples

# ---- load-packages --------------------------------------------------------
library("tidyverse")
library("knitr")
library("kableExtra")
library("pracma")
source("../../../adroseHelperScripts/R/afgrHelpFunc.R")

# ---- problem-1-a --------------------------------------------------------
# First declare the data
horse.surgery <- c(0,1,1,0,0,1,1,0,0,0,1,1)
horse.pulse <- c(92, 88, 64, 48, 76, 76, 88, 48, 92, 48, 64, 64)
adbominal.distension <- c("None", "Severe", "Severe", "Slight", "Slight", "None", "Severe", "Severe", "Severe", "Slight", "Slight", "Slight")
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
entropy.treatment <- calc.entropy(table(horse.data$treat.applied)) # Total dataset entropy == 1.887919

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
gini.treatment <- calc.gini(table(horse.data$treat.applied)) # Total dataset gini == 1.887919

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
kable(
  long_dt, 
  format    = "latex", 
  longtable = T, 
  booktabs  = T, 
  caption   = "Longtable"
) %>%
  add_header_above(c(" ", "Group 1" = 5, "Group 2" = 6)) %>%
  kable_styling(latex_options = c("repeat_header"),
                repeat_header_continued = "\\textit{(Continued on Next Page...)}")

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