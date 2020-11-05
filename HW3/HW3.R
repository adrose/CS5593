# ---- load-packages --------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(psych)
library(foreach)
library(doParallel)
library(ggplot2)
library(reshape2)
library(caret)
library(progress)
library(arules)
library(arulesViz)
library(knitr)
library(kableExtra)
library(gtools)

# ---- problem-1-support --------------------------------------------------------
in.data <- read.csv('./hw3Q1.csv')
in.data.orig <- in.data
in.data[in.data==0] <- FALSE
in.data[in.data==1] <- TRUE
in.data <- apply(in.data, 2, as.logical)

## Now do it manually
# First create the item sets
one.pair <- c('a', 'b', 'c', 'd', 'e')
two.pair <- combinations(r = 2, v = c("a", "b", "c", "d", "e"), repeats.allowed = FALSE, n = 5)
thr.pair <- combinations(r = 3, v = c("a", "b", "c", "d", "e"), repeats.allowed = FALSE, n = 5)
fou.pair <- combinations(r = 4, v = c("a", "b", "c", "d", "e"), repeats.allowed = FALSE, n = 5)
fiv.pair <- combinations(r = 5, v = c("a", "b", "c", "d", "e"), repeats.allowed = FALSE, n = 5)

## Now go through these and calculate the support
out.support <- NULL
for(i in one.pair){
  ## Calculate the count of each of these columns in the data
  out.count <- sum(in.data[,i])
  out.sup <- out.count/dim(in.data)[1]
  out.row <- c(i, out.count, out.sup)
  out.support <- rbind(out.support, out.row)
}
## Now do two pairs
for(i in 1:dim(two.pair)[1]){
  ## Calc the support
  vals <- two.pair[i,]
  out.count <- length(which(in.data[,vals[1]] & in.data[,vals[2]]))
  out.sup <- out.count/dim(in.data)[1]
  out.row <- c(paste(vals, collapse ="_"), out.count, out.sup)
  out.support <- rbind(out.support, out.row)
}
## Now do 3 pair
for(i in 1:dim(thr.pair)[1]){
  ## Calc the support
  vals <- thr.pair[i,]
  out.count <- length(which(in.data[,vals[1]] & in.data[,vals[2]] & in.data[,vals[3]]))
  out.sup <- out.count/dim(in.data)[1]
  out.row <- c(paste(vals, collapse ="_"), out.count, out.sup)
  out.support <- rbind(out.support, out.row)
}
## Now do the 4 pair
for(i in 1:dim(fou.pair)[1]){
  ## Calc the support
  vals <- fou.pair[i,]
  out.count <- length(which(in.data[,vals[1]] & in.data[,vals[2]] & in.data[,vals[3]] & in.data[,vals[4]]))
  out.sup <- out.count/dim(in.data)[1]
  out.row <- c(paste(vals, collapse ="_"), out.count, out.sup)
  out.support <- rbind(out.support, out.row)
}
## Now the final 5 pair
for(i in 1:dim(fiv.pair)[1]){
  ## Calc the support
  vals <- fiv.pair[i,]
  out.count <- length(which(in.data[,vals[1]] & in.data[,vals[2]] & in.data[,vals[3]] & in.data[,vals[4]] & in.data[,vals[5]]))
  out.sup <- out.count/dim(in.data)[1]
  out.row <- c(paste(vals, collapse ="_"), out.count, out.sup)
  out.support <- rbind(out.support, out.row)
}
## Now print the support of these
out.support <- as.data.frame(out.support)
out.support$V3 <- as.numeric(as.character(out.support$V3))
kable(out.support, 
      format    = "latex", 
      longtable = T, 
      digits = 3) %>%
  kable_styling(full_width = FALSE)

# ---- problem-1-supportPass --------------------------------------------------------
out.support2 <- as.data.frame(out.support)
out.support2 <- out.support2[which(out.support2$V3>=.35),]
kable(out.support2, 
      format    = "latex", 
      longtable = T, 
      digits = 3) %>%
  kable_styling(full_width = FALSE)

# ---- problem-1-maximal --------------------------------------------------------
# fsets <- eclat(in.data, parameter = list(supp = 0.35))
# fsets.maximal <- fsets[is.maximal(fsets)]
# fsets.closed <- fsets[is.closed(fsets)]

out.support3 <- out.support2
out.support3$isMaximal <- FALSE
out.support3$isMaximal[c(10,8,7)] <- TRUE


out.support3$isClosedFrequnet <- FALSE
out.support3$isClosedFrequnet[c(5,6)] <- TRUE

out.support3$isFrequnetButNotClosedOrMaimxal <- FALSE
out.support3$isFrequnetButNotClosedOrMaimxal[9] <- TRUE

kable(out.support3, 
      format    = "latex", 
      longtable = T, 
      digits = 3) %>%
  kable_styling(full_width = FALSE)

# ---- problem-1-infrequent --------------------------------------------------------
out.support4 <- as.data.frame(out.support)
out.support4 <- out.support4[which(out.support4$V3<.35),]
kable(out.support4, 
      format    = "latex", 
      longtable = T, 
      digits = 3) %>%
  kable_styling(full_width = FALSE)

# ---- problem-2-contingency1 --------------------------------------------------------
out.tab <- table(in.data[,'b'], in.data[,'e'])
rownames(out.tab) <- c("B-", "B")
colnames(out.tab) <- c("E-", "E")
kable(out.tab, 
  format    = "latex", 
  longtable = T, 
  digits = 3)

# ---- problem-2-contingency2 --------------------------------------------------------
out.tab <- table(in.data[,'a'], in.data[,'d'])
rownames(out.tab) <- c("A-", "A")
colnames(out.tab) <- c("D-", "D")
kable(out.tab, 
      format    = "latex", 
      longtable = T, 
      digits = 3)

# ---- problem-2-contingency3 --------------------------------------------------------
out.tab <- table(in.data[,'e'], in.data[,'d'])
rownames(out.tab) <- c("E-", "E")
colnames(out.tab) <- c("D-", "D")
kable(out.tab, 
      format    = "latex", 
      longtable = T, 
      digits = 3)

# ---- problem-2-results1 --------------------------------------------------------
## First do support
row.one.s <- c("B>E", "Support", round(5/12, 2),3)
row.two.s <- c("A>D", "Support", round(6/12,2), 2)
row.thr.s <- c("E>D", "Support", round(8/12,2), 1)
out <- rbind(row.one.s, row.two.s, row.thr.s)
kable(out, 
      format    = "latex", 
      longtable = T, 
      digits = 3,
      col.names = c("Rule", "Measure", "Value", "Rank"))
# ---- problem-2-results2 --------------------------------------------------------
## Now do confidence
row.one.c <- c("B>E", "Confidence", round(5/7, 2),3)
row.two.c <- c("A>D", "Confidence", round(6/7, 2),1)
row.thr.c <- c("E>D", "Confidence", round(8/10,2), 2)
out <- rbind(row.one.c, row.two.c, row.thr.c)
kable(out, 
      format    = "latex", 
      longtable = T, 
      digits = 3,
      col.names = c("Rule", "Measure", "Value", "Rank"))

# ---- problem-2-results3 --------------------------------------------------------
# Now do the interest
row.one.i <- c("B>E", "Interest", round((5 / 12 ) / ((10 / 12) * (7/12)),2), 3)
row.two.i <- c("A>D", "Interest", round((6 / 12 ) / ((9 / 12) * (7/12)), 2),1)
row.thr.i <- c("E>D", "Interest", round((8 / 12 ) / ((9 / 12) * (10/12)),2), 2)
out <- rbind(row.one.i, row.two.i, row.thr.i)
kable(out, 
      format    = "latex", 
      longtable = T, 
      digits = 3,
      col.names = c("Rule", "Measure", "Value", "Rank"))


# ---- problem-2-results4 --------------------------------------------------------
# Now do the interest
row.one.i <- c("B>E", "IS", round((5/12)/sqrt((7/12) * (10/12)),2), 3)
row.two.i <- c("A>D", "IS", round((6/12)/sqrt((7/12) * (9/12)), 2),2)
row.thr.i <- c("E>D", "IS", round((8/12)/sqrt((10/12) * (9/12)),2), 1)
out <- rbind(row.one.i, row.two.i, row.thr.i)
kable(out, 
      format    = "latex", 
      longtable = T, 
      digits = 3,
      col.names = c("Rule", "Measure", "Value", "Rank"))

# ---- problem-3 --------------------------------------------------------
# Load the data
in.data <- read.table("./Data/HW3_data.txt", fill = TRUE)
in.data[in.data==""] <- NA
in.data <- apply(in.data, c(1,2), as.character)
# Cretae a parallel environment
registerDoParallel(6)
# ---- problem-3-bII --------------------------------------------------------
ModifiedApriori <- function(data, n=3, sup=0.003){
  ## This function will identify all possible candidate itemsets of sizes =< n
  ## The first thing necessary is to create all possible pairs
  # First identify all elements
  all.element <- unique(unlist(data))
  all.element <- names(table(all.element))
  all.element <- all.element
  threshold <- sup*nrow(data)
  ## Now go through every transaction and add it ato a set of all transactions if it hasn't been counted
  all.trans <- matrix(NA, nrow=length(all.element), ncol=2)
  index <- 1
  for(i in all.element){
    # Identify itemsets and their frequency
    count <- sum(data == i, na.rm=T)
    all.trans[index,] <- c(i, count)
    index <- index + 1
  }
  ## Now remove those who do not meet threshold
  all.trans <- all.trans[-which(as.numeric(all.trans[,2]) < threshold),]
  ## Now in a loop go through and compute all supersets from 2..n
  all.element.n <- list()
  all.element.n[[1]] <- all.trans[,1]
  all.trans.np <- list()
  all.trans.np[[1]] <- all.trans
  #all.trans.np[[1]] <- all.trans[order(as.numeric(all.trans[,2]), decreasing = T)[1:7],]
  data <- data[-which(apply(in.data, 1, function(x) sum(!is.na(x)))==1),]
  ## Now find rows w/o any of the items to search
  data <- data[-which(apply(data, 1, function(x) sum(all.trans[,1] %in% x))==0),]
  index <- 1
  for(q in 2:n){
    all.element.n[[q]] <- combn(all.trans[,1], q)
    ## Now remove those that are not in the trimmed 2 pair
    if(is.null(dim(all.trans.np[[q-1]]))){
      to.rm <- which(!t(all.element.n[[q]])[,1:q-1] %in% all.trans.np[[q-1]][seq(1,q-1,1)])
    }else{
      to.rm <- which(!t(all.element.n[[q]])[,1:q-1] %in% all.trans.np[[q-1]][,seq(1,q-1,1)])
    }
    if(length(to.rm)>0){
      all.element.n[[q]] <- all.element.n[[q]][,-to.rm]
    }
    #all.element.n[[q]] <- all.element.n[[q]][,which(all.element.n[[3]][1,] == "DAI62779" & all.element.n[[3]][2,] == "ELE17451")]
    all.trans.n <- foreach(i=1:ncol(all.element.n[[q]]), .combine=rbind) %dopar%{
      call <- NULL
      for(d in 1:q){
        if(d==1){
          call <- paste(call, "all.element.n[[q]][", d,",", i, "] %in% x")
        }else{
          call <- paste(call, "& all.element.n[[q]][", d,",", i, "] %in% x")
        }
      }
      count <- sum(apply(data[which(apply(data, 1, function(x) all.element.n[[q]][d,i] %in% x)),], 1, function(x) eval(parse(text=call))))
      # orig
      #  sum(apply(data, 1, function(x) eval(parse(text=call))))
      ## NOw add a completion check
      complet.check2 <- ncol(all.element.n[[q]]) * .1
      if( i %% floor((ncol(all.element.n[[q]]) / 10))  == 0 ){
        file.create(paste(i, ncol(all.element.n[[q]]), format(Sys.time(), "%X"), sep='_'))
      }
      to.write <-c(paste(all.element.n[[q]][,i]), count)
      if(count>threshold){
        to.write
      }
    }
    #all.trans.n <- all.trans.n[-which(as.numeric(all.trans.n[,q+1]) < threshold),]
    all.trans.np[[q]] <- all.trans.n[order(as.numeric(all.trans.n[[2]][,q+1]), decreasing=T)[1:10],]
    index <- index + 1
    data <- data[-which(apply(in.data, 1, function(x) sum(!is.na(x)))==q),]
  }
  ## Now return all of these
  out.vals <- list(out.items <- all.element.n, out.supp <- all.trans.np, n<-n, sup<-sup)
}

RuleGeneration <- function(out.vals, K=5){
  ## This function will organize the support and counts for all of the items
  ## from the output of the ModifiedApriori function
  ## First identify all of the top rules for each of the folds:
  # Create a output table
  out.table <- matrix(NA, nrow = K, ncol=length(out.vals[[2]])*2)
  col.index.rule <- 1
  col.index.sup <- 2
  for(i in 1:length(out.vals[[2]])){
    ## Make sure data frmat are correct
    tmp.vals <- out.supp <- out.vals[[2]][[i]]
    tmp.vals <- data.frame(tmp.vals)
    tmp.vals[,ncol(tmp.vals)] <- as.numeric(as.character(tmp.vals[,ncol(tmp.vals)]))
    tmp.vals <- tmp.vals[order(tmp.vals[,ncol(tmp.vals)], decreasing = T),]
    ## Now collapse the cols needed
    call <- NULL
    for(q in 1:ncol(tmp.vals)-1){
      if(q==1){
        call <- paste("paste(tmp.vals[,1:", K, ",", q, "]", ",")
      }else{
        call <- paste(call, "tmp.vals[,1:", K, ",", q, "]", ",")
      }
    }
    call <- paste(call, ")")
    rule.vec <- eval(parse(text=call))
    supp.vec <- tmp.vals[1:K, ncol(tmp.vals)]
    out.table[,col.index.rule] <- rule.vec
    out.table[,col.index.sup] <- supp.vec
    col.index.rule <- col.index.rule+2
    col.index.sup <- col.index.sup+2
  }
  return(out.table)
}

# ---- problem-3-bIII --------------------------------------------------------
# Load the data
in.data <- read.table("./Data/HW3_data.txt", fill = TRUE)
in.data[in.data==""] <- NA
in.data <- apply(in.data, c(1,2), as.character)
# Crete a parallel environment
registerDoParallel(6)
## Run the apriori function
time.start <- Sys.time()
all.dat <- ModifiedApriori(in.data, n = 3, sup = .003)
time.end <- Sys.time()
time.end - time.start

## Now grab the top rules
all.rul <- RuleGeneration(all.dat, K = 5)
print(all.rul)
#write.csv(all.rul, "all12.csv", quote=F, row.names=F)

# ---- problem-3-4 --------------------------------------------------------
to.print <- read.csv("./all12.csv")
to.print <- data.frame(to.print)
kable(to.print[1:5,], format = "latex", digits = 3,row.names=FALSE, col.names = c("Rule", "Support"))

kable(to.print[6:10,], format = "latex", digits = 3, row.names=FALSE, col.names = c("Rule", "Support"))

kable(to.print[11:15,], format = "latex", digits = 3, row.names=FALSE, col.names = c("Rule", "Support"))
