##Kappa agreement

library(dplyr)
library(tidyverse)

## Set the working directory
setwd("C:/Users/laura/Documents/")

rev1 <- read.csv("CAMH/DIABETES/search/SIMAH_workplace/ReviewerB_clean_TC.csv")
rev0 <- read.csv("CAMH/DIABETES/search/SIMAH_workplace/ReviewerB_clean_LLF.csv")

rev0 <- rev0[1:500,]
rev1 <- rev1[1:500,]

rev0 <- rename(rev0, rev0 = DECISION)
rev1 <- rename(rev1, rev1 = DECISION)

merge_set1 <- merge(rev0,rev1, by = "TI", all=TRUE )

set1 <- merge_set1[, c("TI","AB.x","rev0", "rev1")]

set1$y_y <- NA
set1$n_n <- NA
set1$y_n <- NA
set1$n_y <- NA

for(i in 1:dim(set1)[1]) {
    if(set1$rev0[i] == 1 & set1$rev1[i] == 1) {
      set1$y_y[i] <- 1
    } else {
      set1$y_y[i] <- 0
       }
  }

for(i in 1:dim(set1)[1]) {
  if(set1$rev0[i] == 0 & set1$rev1[i] == 0) {
    set1$n_n[i] <- 1
  } else {
    set1$n_n[i] <- 0
  }
}

for(i in 1:dim(set1)[1]) {
  if(set1$rev0[i] == 1 & set1$rev1[i] == 0) {
    set1$y_n[i] <- 1
  } else {
    set1$y_n[i] <- 0
  }
}

for(i in 1:dim(set1)[1]) {
  if(set1$rev0[i] == 0 & set1$rev1[i] == 1) {
    set1$n_y[i] <- 1
  } else {
    set1$n_y[i] <- 0
  }
}

sum(set1$y_y == 1)
sum(set1$n_n == 1)
sum(set1$y_n == 1)
sum(set1$n_y == 1)

##Cohen's Kappa test

#po = (y_y=1 + n_n=1)/total
(8+489)/500

#pe(yes) = (y_y=1 + y_n=1)/total * (y_y=1+n_y=1)/total
((8+2)/500)*((8+1)/500)

#pe(no) = (n_y=1 + n_n=1)/total * (y_n=1+n_n=1)/total
((1+489)/500)*((2+489)/500)

#pe = pe(yes)+ pe(no)
0.00036 + 0.96236

#k= (po-pe)/(1-pe)
(0.994-0.96272)/(1-0.96272)

##select conflicts from both datasets
con1 <- set1 %>%
  filter(y_n == 1)
con2 <- set1 %>%
  filter(n_y == 1)

conflicts <- rbind(con1, con2)

rev2 <- read.csv("CAMH/DIABETES/search/SIMAH_workplace/ReviewerA_clean_CK.csv")
rev3 <- read.csv("CAMH/DIABETES/search/SIMAH_workplace/ReviewerA_clean_LLF.csv")

rev2 <- rev2[1:500,]
rev3 <- rev3[1:500,]

rev3 <- rename(rev3, rev0 = DECISION)
rev2 <- rename(rev2, rev1 = DECISION)

merge_set2 <- merge(rev3,rev2, by = "TI", all=TRUE )

set2 <- merge_set2[, c("TI","AB.x","rev0", "rev1")]

set2$y_y <- NA
set2$n_n <- NA
set2$y_n <- NA
set2$n_y <- NA

for(i in 1:dim(set2)[1]) {
  if(set2$rev0[i] == 1 & set2$rev1[i] == 1) {
    set2$y_y[i] <- 1
  } else {
    set2$y_y[i] <- 0
  }
}

for(i in 1:dim(set2)[1]) {
  if(set2$rev0[i] == 0 & set2$rev1[i] == 0) {
    set2$n_n[i] <- 1
  } else {
    set2$n_n[i] <- 0
  }
}

for(i in 1:dim(set2)[1]) {
  if(set2$rev0[i] == 1 & set2$rev1[i] == 0) {
    set2$y_n[i] <- 1
  } else {
    set2$y_n[i] <- 0
  }
}

for(i in 1:dim(set2)[1]) {
  if(set2$rev0[i] == 0 & set2$rev1[i] == 1) {
    set2$n_y[i] <- 1
  } else {
    set2$n_y[i] <- 0
  }
}

sum(set2$y_y == 1)
sum(set2$n_n == 1)
sum(set2$y_n == 1)
sum(set2$n_y == 1)

con3 <- set2 %>%
  filter(y_n == 1)
con4 <- set2 %>%
  filter(n_y == 1)

conflicts1 <- rbind(con3, con4)

Review_conflicts <- rbind(conflicts, conflicts1)

Review_conflicts$DECISION <- NA
Review_conflicts$COMMENT <- NA

Review_conflicts <- rename(Review_conflicts, AB = AB.x)
Review_conflicts <- Review_conflicts[,c("TI", "AB", "DECISION", "COMMENT")]
write.csv(Review_conflicts, "CAMH/DIABETES/search/SIMAH_workplace/Review_conflicts.csv", na = "", row.names = F)
