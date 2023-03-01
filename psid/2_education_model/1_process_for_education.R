
library(splitstackshape)
library(foreign)
library(dplyr)
library(tidyr)
library(readxl)
library(readr)
setwd("~/Google Drive/SIMAH Sheffield")
alldata <- read_csv("SIMAH_workplace/PSID/alldata_new_1999_2019.csv")

# remove nonsample individuals and latino / immigrant samples 
alldata <- alldata %>% 
  filter(relationship!="born after this year or nonresponse") %>% 
  filter(relationship!="Immigrant/Latino")

# unify sample weights (model can't cope with different weights per year)
alldata <- alldata %>% group_by(uniqueID) %>% mutate(sampleweight = mean(weight)) %>% filter(sampleweight!=0)

# filter on just younger ages for education model
alldata <- alldata %>% filter(age>=18 & age<=34)

length(unique(alldata$uniqueID))

alldata$timeperiod <- ifelse(alldata$year<=2009, 1,
                             ifelse(alldata$year>=2009, 0, NA))

alldata %>% group_by(timeperiod) %>% summarise(n=length(unique(uniqueID)))

alldata %>% group_by(timeperiod) %>% summarise(meanage = mean(age))

alldata %>% group_by(timeperiod, year, individualrace) %>% tally() %>% ungroup() %>% 
  group_by(timeperiod, year) %>% 
  mutate(n/sum(n))

alldata <- expandRows(alldata, "sampleweight")

write.csv(alldata, "SIMAH_workplace/education_transitions/new_PSID_processed_weighted.csv")

alldata <- alldata %>% group_by(uniqueID) %>% mutate(consecID=1:n())

# # # # # # # function for assigning IDs - for each replication of an individual append a number to the end of the original ID
IDfunction <- function(data){
  n <- nrow(data)
  data$ID <- 1:n
  data$ID <- ifelse(data$ID==10, 172,
                    ifelse(data$ID==100, 173,
                           ifelse(data$ID==20, 174,
                                  ifelse(data$ID==30, 175,
                                         ifelse(data$ID==40, 176,
                                                ifelse(data$ID==50, 161,
                                                       ifelse(data$ID==60, 162,
                                                              ifelse(data$ID==70, 163,
                                                                     ifelse(data$ID==80, 164,
                                                                            ifelse(data$ID==90, 165,
                                                                                   ifelse(data$ID==110, 166,
                                                                                          ifelse(data$ID==120, 167,
                                                                                                 ifelse(data$ID==130, 168,
                                                                                                        ifelse(data$ID==140, 169,
                                                                                                               ifelse(data$ID==150, 171,
                                                                                   data$ID)))))))))))))))
  data$newID <- paste(data$uniqueID2,data$ID,sep=".")
  return(data)
}
# # # # # # apply the ID function to each original individual
data <- alldata %>% mutate(uniqueID2 = uniqueID) %>% group_by(uniqueID,year) %>%
  group_modify(~IDfunction(.))
# #
data$newID <- as.numeric(data$newID)
# # # # # # # # check that there are no duplicate newIDs for different original IDs
test <- data %>% group_by(newID,year) %>% tally()
# #
write.csv(data, "SIMAH_workplace/education_transitions/new_PSID_weighted_IDs.csv", row.names=F)
