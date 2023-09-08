library(foreign)
library(dplyr)
library(tidyr)
library(readxl)
library(readr)
setwd("~/Google Drive/SIMAH Sheffield")
alldata <- read_csv("SIMAH_workplace/PSID/alldata_new_1999_2019.csv")


# first remove all nonresponders and people born before years they were in the sample
# and remove latino / immigrant sample - they didn't answer education questions
alldata <- alldata %>% 
  filter(relationship!="born after this year or nonresponse") %>% 
  filter(relationship!="Immigrant/Latino")
  
# how many possible observations in each year? 
obs <- alldata %>% group_by(year) %>% tally()

# approx 18000 - 22000 per year

# now how much missing data for education in each year 
ed <- alldata %>% group_by(year, education_cat) %>% tally() %>% 
  ungroup() %>% group_by(year) %>% 
  mutate(prop = n/sum(n)) %>% 
  filter(is.na(education_cat))

# 5% in early years - 20-27% in later years (not sure what is going on in later years!)

# how much missing data for race in each year 
ed <- alldata %>% group_by(year, individualrace) %>% tally() %>% 
  ungroup() %>% group_by(year) %>% 
  mutate(prop = n/sum(n)) %>% 
  filter(is.na(individualrace))
# around 10% 






