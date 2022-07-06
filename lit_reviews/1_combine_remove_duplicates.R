# This code reads raw files containing references, merges them and removes duplicates
# Author: Charlotte Probst
# Project: SIMAH

library(stringr)

## Set the working directory
setwd("C:/Users/marie/Dropbox/NIH2020/")

## Read in ovid
dOVID <- read.csv("SIMAH_workplace/lit_reviews/OVID_update_2019-2020.csv", skip = 1)
names(dOVID)

# Replace journal with source if journal is missing
dOVID$JN[which(is.na(dOVID$JN))] <- dOVID$SO[which(is.na(dOVID$JN))]
dOVID <- dOVID[,!(names(dOVID) %in% c("SO"))]

dOVID$DP <- NA
dOVID$PG<-  gsub("-"," to ", dOVID$PG)

## Read in WOS
dWOS <- read.csv("SIMAH_workplace/lit_reviews/WOS_update_2019-2020.csv")
names(dWOS)

## generate page variable
dWOS$PG <- paste0(dWOS$BP," to ",dWOS$EP)
dWOS$PG[which(dWOS$PG == " to ")] <- NA
dWOS <- dWOS[,!(names(dWOS) %in% c("BP", "EP"))]

dWOS$DB <- NA
dWOS$DB <- "Web of Science"

## combine databases 
v.vars <- c("TI", "AB", "AU", "YR", "JN", "PT", "VO", "IP", "PG", "DB", "DP") 
dWOS <- dWOS[,v.vars]
dOVID <- dOVID[,v.vars]

dDB <- rbind(dWOS, dOVID)

write.csv(dDB, "SIMAH_workplace/lit_reviews/COMPLETE_clean.csv", na = "", row.names = F)

## Remove/Indicate duplicates

#Author loop - with change in WoS it doesn't work
#dDB$AU <- as.character(dDB$AU) 
#dDB$AU1 <- NA
#for(i in 1:dim(dDB)[1]) {
#  if(dDB$DB[i] == "OVID") {
#    dDB$AU1[i] <- strsplit(dDB$AU, " ")[[i]][1]
#  } else {
#    dDB$AU1[i] <- strsplit(dDB$AU, ",")[[i]][1]
#
#  }
#  
#}

#remove duplicates by title
dDB$TI_3 <-  dDB$TI

c.seps1 <- c("-")
for (j in c.seps1){
  dDB$TI_3 <-  gsub(j," ", dDB$TI_3)  
}

c.seps2 <- c(";", ",", ":")
for (j in c.seps2){
  dDB$TI_3 <-  gsub(j,"", dDB$TI_3)  
}

dDB$TI_3 <- tolower(word(dDB$TI_3, start = 1, end = 7, sep=" "))

dDB$DUPLICATE <- NA
ind <- duplicated(dDB[,c("YR", "TI_3", "VO")])
dDB$DUPLICATE[ind] <- 1

table(dDB$DUPLICATE) ## Duplicates to be removed

dDB <- dDB[-(which(dDB$DUPLICATE==1)),]

table(dDB$YR, exclude = NULL)

## Remove books
levels(as.factor(dDB$PT))

v.type.exclude <- c("Book\n\nEdited Book", "B", "Conference Abstract", "Conference Review","Conference Paper", "Letter\n\nComment", "Note", "Review")

dDB$EXCLUDE_TYPE <- NA
for (k in 1:dim(dDB)[1]) {
  if(dDB$PT[k] %in% v.type.exclude){
    dDB$EXCLUDE_TYPE[k] <- 1
  }  
}

table(dDB$EXCLUDE_TYPE) # display table for types to be excluded

dDB <- dDB[-(which(dDB$EXCLUDE_TYPE == 1)),]

dDB$DECISION <- NA
dDB$COMMENT <- NA

v.vars <- c("TI", "AB", "DECISION", "COMMENT", "AU" , "YR", "JN",  "VO", "IP", "PG", "DB") 


dDB <- dDB[,v.vars]

#set.seed(7)
#dDB$REVIEWER <- sample(c("A", "B", "C"), length(dDB$TI), replace = T)
#table(dDB$REVIEWER)

#dReviewerA <- dDB[which(dDB$REVIEWER == "A"),]
#dReviewerB <- dDB[which(dDB$REVIEWER == "B"),]
#dReviewerC <- dDB[which(dDB$REVIEWER == "C"),]


write.csv(dDB, "SIMAH_workplace/lit_reviews/COMPLETE_clean.csv", na = "", row.names = F)
#write.csv(dReviewerA, "SIMAH_workplace/lit_reviews/ReviewerA_clean.csv", na = "", row.names = F)
#write.csv(dReviewerB, "SIMAH_workplace/lit_reviews/ReviewerB_clean.csv", na = "", row.names = F)
#write.csv(dReviewerC, "SIMAH_workplace/lit_reviews/ReviewerC_clean.csv", na = "", row.names = F)
