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

# first read in the data 2000 to 2021
#
gunzip("SIMAH_workplace/ACS/usa_00035.dat.gz", remove=FALSE)

# now read in the data for 2021 
ddi <- read_ipums_ddi("SIMAH_workplace/ACS/usa_00035.xml")

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
  state = c(
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
  mutate(age_gp = cut(AGE, breaks=c(0,24,29,34,39,44,49,54,59,64,69,74,79,10000),
                      labels=c("18-24","25-29","30-34","35-39","40-44","45-49",
                               "50-54","55-59","60-64","65-69",
                               "70-74","75-79","80+")),
                      race = ifelse(RACE==1, "White",
                                    ifelse(RACE==2,"Black",
                                           "Other")),
                      race = ifelse(HISPAN==0, race,
                                    "Hispanic"),
                      race = as.factor(race),
                      edclass = ifelse(EDUC<=6, "LEHS",
                                    ifelse(EDUC>6 & EDUC<=9, "SomeC","College")),
                      sex = SEX) %>% 
  group_by(YEAR, state, sex, AGE, edclass) %>%
  summarise(TPop=sum(PERWT)) %>% rename(year=YEAR, ind_age=AGE)

summaryUSA <- summary %>% group_by(year, sex, ind_age, edclass) %>% 
  summarise(TPop = sum(TPop)) %>% mutate(state="USA")

summary <- rbind(summary, summaryUSA)

# save ACS population counts 2000 to 2021 
write.csv(summary, "SIMAH_workplace/ACS/ACS_popcounts_2000_2021_bystate_indage.csv", row.names=F)

totalpop <- summary %>% filter(state=="USA") %>% 
  group_by(year) %>% summarise(TPop=sum(TPop))

# draw a plot of all of all age groups
allages <- summaryUSA %>% filter(state=="USA") %>% 
  group_by(year, sex, edclass) %>% summarise(TPop=sum(TPop)) %>% 
  mutate(sex = ifelse(sex==1, "Men","Women"), edclass = factor(edclass, levels=c("LEHS","SomeC","College")))

ggplot(data=allages, aes(x=year, y=TPop, colour=sex)) + geom_line(size=1) + facet_grid(cols=vars(edclass), scales="free") +
  theme_bw() + theme(legend.title=element_blank(), legend.position="bottom") + ylim(0,NA) + xlim(2010, 2021)
ggsave("SIMAH_workplace/ACS/popcounts_sex_edclass_with2020.png",dpi=300, width=33, height=19, units="cm")

ggplot(data=subset(allages,year!=2020), aes(x=year, y=TPop, colour=sex)) + geom_line(size=1) + facet_grid(cols=vars(edclass), scales="free") +
  theme_bw() + theme(legend.title=element_blank(), legend.position="bottom") + ylim(0,NA) + xlim(2010, 2021)
ggsave("SIMAH_workplace/ACS/popcounts_sex_edclass_without2020.png",dpi=300, width=33, height=19, units="cm")
