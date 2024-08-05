# calculating the total people in each educational category 
# note this script is best run on a server because of the large ACS dataset

#  SIMAH project 2022 
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc()
suppressPackageStartupMessages(library("dplyr"))
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(readr)
library(R.utils)
library(ipumsr)
library(data.table)
options(scipen=999)

####EDIT ONLY BELOW HERE ### 
###set working directory to the main "Microsimulation" folder in your directory 
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "/home/cbuckley/"
setwd(paste(WorkingDirectory))

ddi <- read_ipums_ddi("SIMAH_workplace/ACS/usa_00044.xml")

data <- read_ipums_micro(ddi)
library(labelled)
data <- remove_attributes(data, "var_desc")
data <- remove_attributes(data, "label")
data <- remove_attributes(data, "labels")
data <- remove_attributes(data, "lbl")
data <- remove_attributes(data, "int+lbl")
data <- zap_ipums_attributes(data)

list <- list()
# 
for(i in unique(data$YEAR)){
  list[[paste(i)]] <- data %>% filter(YEAR==i)
  list[[paste(i)]] <- list[[paste(i)]] %>% 
    filter(AGE>=18 & AGE<=79) %>% 
    mutate(AGECAT = cut(AGE, breaks = c(0, 24, 34, 44, 54, 64, 79),
                        labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65-79")),
           RACE = ifelse(RACE == 1, "White",
                         ifelse(RACE == 2, "Black", "Other")),
           RACE = ifelse(HISPAN == 0, RACE, "Hispanic"),
           EDUC = ifelse(EDUC <= 6, "LEHS",
                         ifelse(EDUC > 6 & EDUC <= 9, "SomeC", "College")),
           SEX = ifelse(SEX == 1, "Men", "Women"),
           n=1) %>% 
    group_by(YEAR, AGE, RACE, EDUC, SEX) %>% 
    summarise(TPop = sum(PERWT),
              OrigSample = sum(n))
}
summary <- do.call(rbind, list)

# save targets - individual ages 
write.csv(summary, "SIMAH_workplace/microsim/education_calibration/education_targets_indage.csv", row.names=F)

# now generate targets for age categories 
summary <- summary %>% 
  mutate(AGECAT = cut(AGE, breaks = c(0, 24, 34, 44, 54, 64, 79),
                      labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65-79"))) %>% 
  group_by(YEAR, AGECAT, RACE, EDUC, SEX) %>% 
  summarise(TPop=sum(TPop),
            OrigSample=sum(OrigSample))
write.csv(summary, "SIMAH_workplace/microsim/education_calibration/education_targets.csv", row.names=F)

# generate targets for specific years of some college - just for year 2000
somecollege <- data %>% filter(YEAR==2000) %>% 
  filter(AGE>=18 & AGE<=79) %>% 
  mutate(RACE = ifelse(RACE == 1, "White",
                       ifelse(RACE == 2, "Black", "Other")),
         RACE = ifelse(HISPAN == 0, RACE, "Hispanic"),
         EDUC_cat = ifelse(EDUC <= 6, "LEHS",
                       ifelse(EDUC > 6 & EDUC <= 9, "SomeC", "College")),
         EDUCdetailed = ifelse(EDUC<=6, "LEHS",
                                ifelse(EDUC==7, "SomeC1",
                                       ifelse(EDUC==8, "SomeC2",
                                              ifelse(EDUC==9, "SomeC3",
                                                     ifelse(EDUC>=10, "College", NA))))),
         SEX = ifelse(SEX == 1, "Men", "Women")) %>% 
  filter(EDUC_cat=="SomeC") %>% 
  group_by(AGE, SEX, RACE, EDUCdetailed) %>% 
  tally(name="sum") %>% 
  ungroup() %>% 
  group_by(AGE, SEX, RACE) %>% 
  mutate(total=sum(sum),
         percent=sum/total)
write.csv(somecollege, "SIMAH_workplace/microsim/education_calibration/somecollege_ACS.csv", row.names=F)

