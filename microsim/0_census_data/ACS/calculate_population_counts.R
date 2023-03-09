# SIMAH June 2021
# Calculating population totals from ACS for US and States 
# Split by sex, age group, racee/ethnicity and education 

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
# gunzip("SIMAH_workplace/ACS/usa_00034.dat.gz", remove=FALSE)

# now read in the data for 2021 
ddi <- read_ipums_ddi("SIMAH_workplace/ACS/usa_00034.xml")

data <- read_ipums_micro(ddi)
library(labelled)
data <- remove_attributes(data, "var_desc")
data <- remove_attributes(data, "label")
data <- remove_attributes(data, "labels")
data <- remove_attributes(data, "lbl")
data <- remove_attributes(data, "int+lbl")
data <- zap_ipums_attributes(data)


summaryUSA <- data %>% 
  filter(AGE>=18) %>% 
  mutate(age_gp = cut(AGE, breaks=c(0,24,29,34,39,44,49,54,59,64,69,74,79,10000),
                      labels=c("18","25","30","35","40","45","50","55","60","65",
                               "70","75","80")),
                      race = ifelse(RACE==1, "White",
                                    ifelse(RACE==2,"Black",
                                           "Other")),
                      race = ifelse(HISPAN==0, race,
                                    "Hispanic"),
                      race = as.factor(race),
                      edclass = ifelse(EDUC<=6, "LEHS",
                                    ifelse(EDUC>6 & EDUC<=9, "SomeC","College")),
                      sex = SEX) %>% 
  group_by(YEAR, sex, edclass) %>%
  summarise(TPop=sum(PERWT)) %>% rename(year=YEAR)

# save ACS population counts 2000 to 2021 
write.csv(summaryUSA, "SIMAH_workplace/ACS/ACS_popcounts_2000_2021_updated_educsex.csv", row.names=F)

# draw a plot of all of all age groups
allages <- summaryUSA %>% group_by(year, sex, edclass) %>% summarise(TPop=sum(TPop)) %>% 
  mutate(sex = ifelse(sex==1, "Men","Women"), edclass = factor(edclass, levels=c("LEHS","SomeC","College")))

ggplot(data=allages, aes(x=year, y=TPop, colour=sex)) + geom_line(size=1) + facet_grid(cols=vars(edclass), scales="free") +
  theme_bw() + theme(legend.title=element_blank(), legend.position="bottom") + ylim(0,NA) + xlim(2010, 2021)
ggsave("SIMAH_workplace/ACS/popcounts_sex_edclass_with2020.png",dpi=300, width=33, height=19, units="cm")

ggplot(data=subset(allages,year!=2020), aes(x=year, y=TPop, colour=sex)) + geom_line(size=1) + facet_grid(cols=vars(edclass), scales="free") +
  theme_bw() + theme(legend.title=element_blank(), legend.position="bottom") + ylim(0,NA) + xlim(2010, 2021)
ggsave("SIMAH_workplace/ACS/popcounts_sex_edclass_without2020.png",dpi=300, width=33, height=19, units="cm")
