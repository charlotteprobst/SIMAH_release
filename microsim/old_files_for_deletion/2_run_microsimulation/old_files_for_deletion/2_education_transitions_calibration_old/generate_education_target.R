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
# WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
WorkingDirectory <- "/home/cbuckley/"
setwd(paste(WorkingDirectory))

# first read in the data 2000 to 2021
#
# gunzip("SIMAH_workplace/ACS/usa_00042.dat.gz", remove=FALSE)

# now read in the data for 2021 
ddi <- read_ipums_ddi("SIMAH_workplace/ACS/usa_00042.xml")

data <- read_ipums_micro(ddi)
library(labelled)
data <- remove_attributes(data, "var_desc")
data <- remove_attributes(data, "label")
data <- remove_attributes(data, "labels")
data <- remove_attributes(data, "lbl")
data <- remove_attributes(data, "int+lbl")
data <- zap_ipums_attributes(data)

# datatest <- data %>% group_by(YEAR) %>% sample_n(1000)

# 
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

# save targets
write.csv(summary, "SIMAH_workplace/microsim/2_output_data/education_calibration/education_targets_indage.csv", row.names=F)
