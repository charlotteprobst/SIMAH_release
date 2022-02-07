# SIMAH June 2021
# Calculating population totals from ACS for US and States 
# Split by sex, age group, racee/ethnicity and education 

library(dplyr)
library(readr)
library(ipumsr)
library(tidyr)

setwd("~/Google Drive/SIMAH Sheffield")

# read in the data 
ddi <- read_ipums_ddi("SIMAH_workplace/ACS/usa_00031.xml")

library(R.utils)
# gunzip("SIMAH_workplace/ACS/usa_00031.dat.gz")
data <- read_ipums_micro(ddi)
library(labelled)
data <- remove_attributes(data, "var_desc")
data <- remove_attributes(data, "label")
data <- remove_attributes(data, "labels")
data <- remove_attributes(data, "lbl")

data$STATEFIP <- as.integer(data$STATEFIP)

data <- data %>% 
  mutate(SEX = ifelse(SEX==1, "M","F"),
         RACE = ifelse(RACE==1, "White",
                       ifelse(RACE==2,"Black",
                              "Other")),
         RACE = ifelse(HISPAN==0, RACE,
                       "Hispanic"),
         EDUC = ifelse(EDUC<=6, "LEHS",
                       ifelse(EDUC>6 & EDUC<=9, "SomeC","College")),
         AGECAT=cut(AGE,
                    breaks=c(0,17,24,29,34,39,44,49,54,59,64,69,74,79,1000),
                    labels=c("0-17","18-24","25-29","30-34","35-39","40-44",
                    "45-49","50-54","55-59","60-64","65-69","70-74","75-79",
                    "80+")),
                    STATE = recode(STATEFIP, "06"="California", "08"="Colorado", "12"="Florida", "18"="Indiana",
                                   "21"="Kentucky","22"="Louisiana","25"="Massachusetts","26"="Michigan",
                                   "27"="Minnesota","29"="Missouri", "36"="New York", "41"="Oregon",
                                   "42"="Pennsylvania", "47"="Tennessee", "48"="Texas",
                                   .default="NA"))

summary(as.factor(data$STATE))

data <- data %>% filter(YEAR==2020) %>% pivot_longer(cols=REPWTP1:REPWTP80) %>% 
  dplyr::select(YEAR, STATE, SEX, RACE, AGECAT, EDUC, PERWT, name, value)

summary2020 <- data %>% group_by(YEAR, SEX, RACE, AGECAT, EDUC, name) %>% 
  summarise(TPop = sum(value)) %>% 
  dplyr::rename(year=YEAR, sex=SEX, race=RACE, edclass=EDUC, age_gp = AGECAT) %>% 
  mutate(age_gp = ifelse(age_gp=="80+", "80",
                         substr(age_gp,1,2)),
         sex = ifelse(sex=="M",1,2))

weights <- unique(summary2020$name)

summary2020list <- list()

for(i in unique(weights)){
  summary2020list[[paste(i)]] <- summary2020 %>% filter(name==i)
}

saveRDS(summary2020list, "SIMAH_workplace/ACS/rep_weights_2020.RDS")

# write.csv(summary2020, "SIMAH_workplace/ACS/rep_weights_2020.csv", row.names=F)
