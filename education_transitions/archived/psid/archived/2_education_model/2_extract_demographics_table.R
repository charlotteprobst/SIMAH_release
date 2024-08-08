# script to extract summary tables 
library(splitstackshape)
library(foreign)
library(dplyr)
library(tidyr)
library(readxl)
library(readr)
library(skimr)      # descriptive statistics
library(gmodels)    # CrossTable command
library(tableone)   # create table one
setwd("~/Google Drive/SIMAH Sheffield")

alldata <- read_csv("SIMAH_workplace/PSID/alldata_new_1999_2019.csv")

# remove nonsample individuals and latino / immigrant samples 

# unify sample weights (model can't cope with different weights per year)
alldata <- alldata %>% group_by(uniqueID) %>% mutate(sampleweight = mean(weight,na.rm=T))

# filter on just younger ages for education model
alldata <- alldata %>% filter(age>=16 & age<=34)

length(unique(alldata$uniqueID))
nrow(alldata)

# # missing hispanic latino oversample 
# missingoversample <- alldata %>% filter(is.na(education) & is.na(race_new) & is.na(total_fam_income) & relationship=="Immigrant/Latino")
# ids <- missingoversample$uniqueID
# 
# alldata <- alldata %>% filter(!uniqueID %in% ids)

summaryperyear <- alldata %>% group_by(year) %>% tally()

missing <- alldata %>% filter(is.na(education)) %>% filter(is.na(race_new_unique)) %>% 
  filter(is.na(total_fam_income))

summary(as.factor(missing$relationship))/nrow(missing)

missingeducation <- alldata %>% filter(is.na(education))
nrow(missingeducation)
length(unique(missingeducation$uniqueID))

missingincome <- alldata %>% filter(is.na(total_fam_income))
length(unique(missingincome$uniqueID))


missingrace <- alldata %>% filter(is.na(race_new_unique))
length(unique(missingrace$uniqueID))

alldata <- alldata %>% drop_na(sex, race_new_unique, education, total_fam_income)

length(unique(alldata$uniqueID))
nrow(alldata)

alldata <- alldata %>% 
  drop_na(sex, education, age, race_new_unique, total_fam_income) %>% 
  mutate(educ = ifelse(education<=11, "LHS",
                       ifelse(education==12, "HS",
                              ifelse(education>=13 & education<=15, "SomeC",
                                     ifelse(education>=16, "College",NA)))),
         race_new=ifelse(race_new_unique=="Native","others",
                         ifelse(race_new_unique=="Asian/PI","others",
                                ifelse(race_new_unique=="other","others",race_new_unique))),
         educ = as.factor(educ), race_new=as.factor(race_new),
         sex = as.factor(sex))

library(survey)

svy <- svydesign(ids= ~1, weights=~sampleweight, data=alldata)

# Table 1: Participant characteristics - STRATIFIED BY SEX
tab1 <-svyCreateTableOne(vars= c("age","educ", "race_new", "total_fam_income"),
                      factorVars = c("educ","race_new"),
                      strata= c("year","sex"), addOverall = TRUE, data=svy)
table1_v1 <- print(tab1, noSpaces = TRUE, catDigits = 1, contDigits = 1, printToggle = FALSE, test=FALSE, format="f") %>%
  data.frame(.) %>% 
  dplyr::select(Overall, X1999.male, X1999.female, X2009.male, X2009.female, X2019.male, X2019.female) 

table1_v1[c(1,4:7,9:12),1:7] <- round(as.numeric(unlist(table1_v1[c(1,4:7,9:12),1:7]))*0.06299793,digits=0)

table1_v2 <- print(tab1, noSpaces = TRUE, catDigits = 1, contDigits = 1, printToggle = FALSE, test=FALSE, format="p")  %>% 
  data.frame(.) %>% 
  dplyr::select(Overall, X1999.male, X1999.female, X2009.male, X2009.female, X2019.male, X2019.female)
names(table1_v2) <- paste0("pct", names(table1_v2))

table1 <- cbind(table1_v1, table1_v2) %>% 
  mutate(Overall = paste0(Overall, " (", pctOverall, ")"),
         X1999.male = paste0(X1999.male, " (", pctX1999.male, ")"),
         X1999.female = paste0(X1999.female, " (", pctX1999.female, ")"),
         X2009.male = paste0(X2009.male, " (", pctX2009.male, ")"),
         X2009.female = paste0(X2009.female, " (", pctX2009.female, ")"),
         X2019.male = paste0(X2019.male, " (", pctX2019.male, ")"),
         X2019.female = paste0(X2019.female, " (", pctX2019.female, ")")) %>% 
  dplyr::select(Overall, X1999.male, X1999.female, X2009.male, X2009.female, X2019.male, X2019.female)


write.csv(table1, "SIMAH_workplace/education_transitions/final_models/Table1_demographics_weighted.csv", row.names=T)
  