# SIMAH June 2021
# Calculating population totals from ACS for US and States 
# Split by sex, age group, race/ethnicity and education 

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
options(scipen=999)

####EDIT ONLY BELOW HERE ### 
###set working directory to the main "Microsimulation" folder in your directory 
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
setwd(paste(WorkingDirectory))

# first read in the data 2000 to 2022
#
# gunzip("SIMAH_workplace/ACS/usa_00043.dat.gz", remove=FALSE)

ddi <- read_ipums_ddi("SIMAH_workplace/ACS/usa_00043.xml")

data <- read_ipums_micro(ddi)
library(labelled)
data <- remove_attributes(data, "var_desc")
data <- remove_attributes(data, "label")
data <- remove_attributes(data, "labels")
data <- remove_attributes(data, "lbl")
data <- remove_attributes(data, "int+lbl")
data <- zap_ipums_attributes(data)

# create a lookup table for mapping state FIP to state name 
lookup_table <- data.frame(
  STATEFIP = c(
    "01", "02", "04", "05", "06", "08", "09", "10", "11", "12",
    "13", "15", "16", "17", "18", "19", "20", "21", "22", "23",
    "24", "25", "26", "27", "28", "29", "30", "31", "32", "33",
    "34", "35", "36", "37", "38", "39", "40", "41", "42", "44",
    "45", "46", "47", "48", "49", "50", "51", "53", "54", "55",
    "56", "61", "62", "63", "64", "65", "66", "67", "68", "72",
    "97", "99"
  ),
  STATE = c(
    "Alabama", "Alaska", "Arizona", "Arkansas", "California",
    "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida",
    "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas",
    "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan",
    "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada",
    "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina",
    "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island",
    "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",
    "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming",
    "Maine-New Hampshire-Vermont", "Massachusetts-Rhode Island",
    "Minnesota-Iowa-Missouri-Kansas-Nebraska-S.Dakota-N.Dakota",
    "Maryland-Delaware", "Montana-Idaho-Wyoming", "Utah-Nevada",
    "Arizona-New Mexico", "Alaska-Hawaii", "Puerto Rico", "Military/Mil. Reservation",
    "State not identified"
  )
)

lookup_table$STATEFIP <- as.integer(lookup_table$STATEFIP)

data <- left_join(data, lookup_table)

summary <- data %>% 
  filter(AGE>=18) %>% 
  mutate(AGECAT = cut(AGE, breaks=c(0,24,34,44,54,64,79),
                      labels=c("18-24","25-34","35-44","45-54",
                               "55-64","65-79")),
                      RACE = ifelse(RACE==1, "White",
                                    ifelse(RACE==2,"Black",
                                           "Other")),
                      RACE = ifelse(HISPAN==0, RACE,
                                    "Hispanic"),
                      RACE = as.factor(RACE),
                      EDUC = ifelse(EDUC<=6, "LEHS",
                                    ifelse(EDUC>6 & EDUC<=9, "SomeC","College")),
                      SEX = SEX) %>% 
  group_by(YEAR, STATE, SEX, RACE, AGECAT, EDUC) %>%
  summarise(TPop=sum(PERWT),
            OrigSample=n())

summaryUSA <- data %>% 
  filter(AGE>=18) %>% 
  mutate(AGECAT = cut(AGE, breaks=c(0,24,34,44,54,64,79),
                      labels=c("18-24","25-34","35-44","45-54",
                               "55-64","65-79")),
         RACE = ifelse(RACE==1, "White",
                       ifelse(RACE==2,"Black",
                              "Other")),
         RACE = ifelse(HISPAN==0, RACE,
                       "Hispanic"),
         RACE = as.factor(RACE),
         EDUC = ifelse(EDUC<=6, "LEHS",
                       ifelse(EDUC>6 & EDUC<=9, "SomeC","College")),
         SEX = SEX) %>% 
  group_by(YEAR, SEX, RACE, AGECAT, EDUC) %>%
  summarise(TPop=sum(PERWT),
            OrigSample=n()) %>% mutate(STATE="USA")

summary <- rbind(summary, summaryUSA)

summary <- summary %>% filter(YEAR>=2000)

# generate the education targets
education_targets <- summary %>% 
  mutate(SEX=ifelse(SEX==1, "Men","Women")) %>% 
  group_by(YEAR, STATE, SEX, RACE, AGECAT) %>% 
  mutate(target=TPop/sum(TPop),
         SE = sqrt(target*(1-target)/sum(OrigSample)))

# save ACS population counts 2000 to 2021 
write.csv(education_targets, "SIMAH_workplace/ACS/ACS_education_targets.csv", row.names=F)
