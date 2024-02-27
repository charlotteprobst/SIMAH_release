############################################################################################
# Compare the TPs from the existing education model (2005-2019) pre- and post- calibration,
# to the TPs generated with the new model (2005-2021 with a covariate for time period)
############################################################################################
library(tidyverse)
setwd("C:/Users/cmp21seb/Documents/SIMAH/")

Original_TPs_wave_1 <- readRDS("SIMAH_workplace/microsim/2_output_data/education_calibration/newagecat30/transitionsList-1_converted.RDS")
Original_TPs_wave_10 <- readRDS("SIMAH_workplace/microsim/2_output_data/education_calibration/newagecat30/transitionsList-10_converted.RDS")
New_TPs_uncalibrated <- read_csv("SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_6.csv")

# Calculate the average TP for each group
Original_TPs_wave_1_mean <- Original_TPs_wave_1 %>% group_by(
  agecat, sex, race, StateFrom, StateTo) %>% summarise(mean_TP_wave_1 = mean(probability))
Original_TPs_wave_10_mean <- Original_TPs_wave_10 %>% group_by(
  agecat, sex, race, StateFrom, StateTo) %>% summarise(mean_TP_wave_10 = mean(probability))
# nb. The original TPs include a different number of transitions as has an additional age category,
# and new model includes a covariate for time period

# Prep data for merging
New_TPs_uncalibrated <- New_TPs_uncalibrated %>% rename(TP_covid = prob)
New_TPs_uncalibrated$sex <- as.character(New_TPs_uncalibrated$sex)
New_TPs_uncalibrated <- New_TPs_uncalibrated %>%
  mutate(StateFrom = as.numeric(gsub("State ", "", StateFrom)),
         StateTo = as.numeric(gsub("State ", "", StateTo)),
         sex = ifelse(sex==0, "Men", "Women"),
         race = case_when(race=="black" ~ "Black",
                race=="white" ~ "White",
                race=="hispanic"~ "Hispanic",
                race=="other" ~ "Others")) %>%
  rename(agecat = age)

New_TPs_2012_2018_only <- New_TPs_uncalibrated %>% filter(
  time_period=="2012-2018"
  ) %>% dplyr::select(-(time_period))

New_TPs_under_21 <- New_TPs_2012_2018_only %>% filter(agecat==18| agecat==19| agecat==20)

Original_TPs <- full_join(Original_TPs_wave_1_mean, Original_TPs_wave_10_mean) 
Original_TPs_under_21 <- Original_TPs %>% filter(agecat==18| agecat==19| agecat==20)

TPs_all_models_under_21 <- Original_TPs_under_21 %>% 
  inner_join(., New_TPs_under_21, by=c("agecat", "sex", "race", "StateFrom", "StateTo"))

results <- TPs_all_models_under_21 %>% mutate(
  abs_difference_wave1_v_covid = abs(mean_TP_wave_1 - TP_covid),
  abs_difference_wave10_v_covid = abs(mean_TP_wave_10 - TP_covid),
  abs_difference_wave1_v_wave10 = abs(mean_TP_wave_1 - mean_TP_wave_10)
) 

results <- results %>% mutate_if(is.numeric, ~ round(., 2))
write_csv(results, "SIMAH_workplace/education_transitions/2021/comparisson between original SIMAH TPs and new COVID TPs.csv")
