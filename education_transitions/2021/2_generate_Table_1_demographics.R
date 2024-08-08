# Script to generate Table 1 - Demographics of sample

library(tidyverse)
setwd("C:/Users/cmp21seb/Documents/SIMAH/")

source("SIMAH_code/education_transitions/2021/functions/0_setup_education_model_2021.R")

data <- read_csv("SIMAH_workplace/education_transitions/2021/data_to_model/new_PSID_weighted_IDs_2021.csv")

data$agecat <- ifelse(data$age==18, "18",
                      ifelse(data$age==19, "19",
                             ifelse(data$age==20, "20",
                                    ifelse(data$age>=21 & data$age<=25, "21-25","26+"))))

# Convert 'agecat' to a factor with specified levels
data$agecat <- factor(data$agecat, levels = c("18", "19", "20", "21-25", "26+"))

# COVID period 2013-2018
data_2013_2018 <- data %>% filter(year>=2013 & year <=2018)
data_2013_2018_setup <- setup_education_model_2021(data_2013_2018)

data_2019_2021 <- data %>% filter(year>=2019 & year <=2021)
data_2019_2021_setup <- setup_education_model_2021(data_2019_2021)

# Summarise samples by individauls

# Pre-covid model
mean(data_2013_2018_setup$age, na.rm=TRUE)
sd(data_2013_2018_setup$age, na.rm=TRUE)
age_summary <- data_2013_2018_setup %>%
  distinct(uniqueID, agecat) %>%
  group_by(agecat) %>%
  summarise(count = n(), .groups = 'drop')

sex_summary <- data_2013_2018_setup %>%
  distinct(uniqueID, sex) %>%
  group_by(sex) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(percentage = (count / sum(count)) * 100)

race_summary <- data_2013_2018_setup %>%
  distinct(uniqueID, racefinal2) %>%
  group_by(racefinal2) %>%
  summarise(count = n(), .groups = 'drop')

# Covid model
mean(data_2019_2021_setup$age, na.rm=TRUE)
sd(data_2019_2021_setup$age, na.rm=TRUE)
age_summary_2 <- data_2019_2021_setup %>%
  distinct(uniqueID, agecat) %>%
  group_by(agecat) %>%
  summarise(count = n(), .groups = 'drop')

sex_summary_2 <- data_2019_2021_setup %>%
  distinct(uniqueID, sex) %>%
  group_by(sex) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(percentage = (count / sum(count)) * 100)

race_summary_2 <- data_2019_2021_setup %>%
  distinct(uniqueID, racefinal2) %>%
  group_by(racefinal2) %>%
  summarise(count = n(), .groups = 'drop')

# Summarise samples by observations
age_summary_observations <- data_2013_2018_setup %>%
  group_by(agecat) %>%
  summarise(count = n(), .groups = 'drop')

sex_summary_observations <- data_2013_2018_setup %>%
  group_by(sex) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(percentage = (count / sum(count)) * 100)

race_summary_observations <- data_2013_2018_setup %>%
  group_by(race) %>%
  summarise(count = n(), .groups = 'drop')

age_summary_observations_2 <- data_2019_2021_setup %>%
  group_by(agecat) %>%
  summarise(count = n(), .groups = 'drop')

sex_summary_observations_2 <- data_2019_2021_setup %>%
  group_by(sex) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(percentage = (count / sum(count)) * 100)

race_summary_observations_2 <- data_2019_2021_setup %>%
  group_by(race) %>%
  summarise(count = n(), .groups = 'drop')



