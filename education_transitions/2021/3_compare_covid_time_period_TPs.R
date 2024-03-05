############################################################################################
# Compare the COVID TPs from the standalone COVID timeperiod model (2019-2021) with those from the
# 2005-2021 model with a covariate for time period
############################################################################################
library(tidyverse)
setwd("C:/Users/cmp21seb/Documents/SIMAH/")

TPs_2012_2021 <- read_csv("SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_6.csv")
TPs_2019_2021 <- read_csv("SIMAH_workplace/education_transitions/2021/annual_education_TPs_2019_2021_detail.csv")

# Calculate the average TP for each group during the COVID time period
TPs_2012_2021_model_covid_only <- TPs_2012_2021 %>% filter(
  time_period=="2019-2021"
) %>% dplyr::select(-(time_period))

TP_covariate <- TPs_2012_2021_model_covid_only %>% group_by(
  age, sex, race, StateFrom, StateTo) %>% summarise(TP_covariate = prob)
TP_separate_model <- TPs_2019_2021 %>% group_by(
  age, sex, race, StateFrom, StateTo) %>% summarise(TP_seperate_model = prob)

# Merge
All_covid_TPs <- full_join(TP_covariate, TP_separate_model) 

results <- All_covid_TPs %>% mutate(
  difference = TP_covariate - TP_seperate_model,
  abs_difference = abs(difference)
) 
results <- results %>% mutate_if(is.numeric, ~ round(., 2))

write_csv(results, "SIMAH_workplace/education_transitions/2021/comparisson between COVID TPs (covariate v seperate model).csv")

allowed_results <- results %>% dplyr::filter(StateFrom=="State 1" & (StateTo=="State 1"|StateTo=="State 2")|
                                      StateFrom=="State 2" & (StateTo=="State 2"|StateTo=="State 3")|
                                      age!=18 & StateFrom=="State 3" & (StateTo=="State 3"|StateTo=="State 4")|
                                      age!=18 & age!=19 & StateFrom=="State 4" & (StateTo=="State 4"|StateTo=="State 5")|
                                      age!=18 & age!=19 & age!=20 & (StateFrom=="State 5" & StateTo=="State 5"))

write_csv(allowed_results, "SIMAH_workplace/education_transitions/2021/comparisson between COVID TPs (covariate v seperate model)_logical TPs only.csv")
