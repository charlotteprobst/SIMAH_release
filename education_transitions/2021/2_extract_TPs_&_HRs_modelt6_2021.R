## Education transitions analysis to 2021

# Setup
setwd("C:/Users/cmp21seb/Documents/SIMAH/")
library(tidyverse)  # data management
library(skimr)      # descriptive statistics
library(janitor)    # data management
library(msm)        # model transition probabilities
library(tableone)   # create descriptives table
library(knitr)      # create descriptives table
options(scipen = 999)

# Source functions
source("SIMAH_code/education_transitions/2021/functions/0_adjust_CIs_2021.R")
source("SIMAH_code/education_transitions/2021/functions/0_extractTPs_2021.R")
source("SIMAH_code/education_transitions/2021/functions/0_predict_HRs_2021.R")
source("SIMAH_code/education_transitions/2021/functions/0_setup_education_model_2021.R")
source("SIMAH_code/education_transitions/2021/functions/0_transition_population_2021.R")

# Load model data
data <- readRDS("SIMAH_workplace/education_transitions/2021/data_to_model/prepped_data_for_markov_2021.rds")
data <- data %>% filter(year>=2012)
men <- data %>% filter(sex==0)
women <- data %>% filter(sex==1)
white <- data %>% filter(racefinal2=="white")
black <- data %>% filter(racefinal2=="black")
hispanic <- data %>% filter(racefinal2=="hispanic")
other <- data %>% filter(racefinal2=="other")
black_men <- data %>% filter(racefinal2=="black", sex==0)

# Load MSM Models of interest
modelt6 <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/covid_modelt6.RDS")
modelt6_men <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/modelt6_men.RDS")
modelt6_women <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/modelt6_women.RDS")
modelt6_white <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/modelt6_white.RDS")
modelt6_black <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/modelt6_black.RDS")
modelt6_hispanic <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/modelt6_hispanic.RDS")
modelt6_other <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/modelt6_other.RDS")
modelt6_white_men <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/modelt6_white_men.RDS")
modelt6_black_men <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/modelt6_black_men.RDS")
modelt6_hispanic_men <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/modelt6_hispanic_men.RDS")
modelt6_other_men <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/modelt6_other_men.RDS")
modelt6_white_women <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/modelt6_white_women.RDS")
modelt6_black_women <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/modelt6_black_women.RDS")
modelt6_hispanic_women <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/modelt6_hispanic_women.RDS")
modelt6_other_women <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/modelt6_other_women.RDS")
modelt6_interaction_race <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/modelt6_interaction_race.RDS")
modelt6_interaction_sex <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/modelt6_interaction_sex.RDS")


# Extract TPs

# all years (covariate for all time periods)
modelt6_TPs <- extractTPs_basic(modelt6, 1)
modelt6_TPs_men <- extractTPs_basic(modelt6_men, 1) # interaction for sex (men only)
modelt6_TPs_women <- extractTPs_basic(modelt6_women, 1) # interaction for sex (women only)
modelt6_TPs_white <- extractTPs_basic(modelt6_white, 1) # interaction for race (white only)
modelt6_TPs_black <- extractTPs_basic(modelt6_black, 1) # interaction for race (black only)
modelt6_TPs_hispanic <- extractTPs_basic(modelt6_hispanic, 1) # interaction for race (hispanic only)
modelt6_TPs_other <- extractTPs_basic(modelt6_other, 1) # interaction for race (other only)
modelt6_TPs_black_men <- extractTPs_basic(modelt6_black_men, 1) # interaction for race and sex (Black men only)
modelt6_TPs_interaction_race <- extractTPs_basic(modelt6_interaction_race, 1) 
modelt6_TPs_interaction_sex <- extractTPs_basic(modelt6_interaction_sex, 1) 

# Save results
write_csv(modelt6_TPs, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_6.csv")
write_csv(modelt6_TPs_men, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_6_men.csv")
write_csv(modelt6_TPs_women, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_6_women.csv")
write_csv(modelt6_TPs_white, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_6_white.csv")
write_csv(modelt6_TPs_black, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_6_black.csv")
write_csv(modelt6_TPs_hispanic, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_6_hispanic.csv")
write_csv(modelt6_TPs_other, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_6_other.csv")
write_csv(modelt6_TPs_black_men, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_6_black_men.csv")
write_csv(modelt6_TPs_interaction_race, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_6_interaction_race.csv")
write_csv(modelt6_TPs_interaction_sex, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_6_interaction_sex.csv")

##################################################################

# Cut 'year' variable into two time periods 
cutYearIntoIntervals <- function(dataset) {
  dataset$time_1 <- cut(dataset$year,
                        breaks=c(0,2018, 2021),
                        labels=c("2012-2018", "2019-2021"))
  return(dataset)
}

data <- cutYearIntoIntervals(data)
men <- cutYearIntoIntervals(men)
women <- cutYearIntoIntervals(women)
white <- cutYearIntoIntervals(white)
black <- cutYearIntoIntervals(black)
hispanic <- cutYearIntoIntervals(hispanic)
other <- cutYearIntoIntervals(other)
black_men <- cutYearIntoIntervals(black_men)

# Extract all possible combos of individuals to transition
combo_2 <- expand.grid(timevary = unique(data$time_1), agecat = unique(data$agecat), sex = unique(data$sex), racefinal2=unique(data$racefinal2))
combo_2_men <- expand.grid(timevary = unique(men$time_1), agecat = unique(men$agecat), racefinal2=unique(men$racefinal2))
combo_2_women <- expand.grid(timevary = unique(women$time_1), agecat = unique(women$agecat), racefinal2=unique(women$racefinal2))
combo_2_white <- expand.grid(timevary = unique(white$time_1), agecat = unique(white$agecat), sex=unique(white$sex))
combo_2_black <- expand.grid(timevary = unique(black$time_1), agecat = unique(black$agecat), sex=unique(black$sex))
combo_2_hispanic <- expand.grid(timevary = unique(hispanic$time_1), agecat = unique(hispanic$agecat), sex=unique(hispanic$sex))
combo_2_other <- expand.grid(timevary = unique(other$time_1), agecat = unique(other$agecat), sex=unique(other$sex))
combo_2_black_men <- expand.grid(timevary = unique(black_men$time_1), agecat = unique(black_men$agecat))

modelt6_TPs_detail <- extractTP_incl_time(modelt6, combo_2)
modelt6_TPs_men_detail <- extractTP_interaction_sex(modelt6_men,combo_2_men)
modelt6_TPs_women_detail <- extractTP_interaction_sex(modelt6_women,combo_2_women)
modelt6_TPs_white_detail <- extractTP_interaction_race(modelt6_white,combo_2_white)
modelt6_TPs_black_detail <- extractTP_interaction_race(modelt6_black,combo_2_black)
modelt6_TPs_hispanic_detail <- extractTP_interaction_race(modelt6_hispanic,combo_2_hispanic)
modelt6_TPs_other_detail <- extractTP_interaction_race(modelt6_other,combo_2_other)
modelt6_TPs_black_men_detail <- extractTP_interaction_race_sex(modelt6_black_men,combo_2_black_men)

# Save results
write_csv(modelt6_TPs_detail, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_6.csv")
write_csv(modelt6_TPs_men_detail, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_6_men_detail.csv")
write_csv(modelt6_TPs_women_detail, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_6_women_detail.csv")
write_csv(modelt6_TPs_white_detail, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_6_white_detail.csv")
write_csv(modelt6_TPs_black_detail, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_6_black_detail.csv")
write_csv(modelt6_TPs_hispanic_detail, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_6_hispanic_detail.csv")
write_csv(modelt6_TPs_other_detail, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_6_other_detail.csv")
write_csv(modelt6_TPs_black_men_detail, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_6_black_men_detail.csv")

########################################
# Hazard ratios

modelt6_HRs <- predict_HRs(modelt6)
modelt6_HRs_men <- predict_HRs(modelt6_men)
modelt6_HRs_women <- predict_HRs(modelt6_women)
modelt6_HRs_white <- predict_HRs(modelt6_white)
modelt6_HRs_black <- predict_HRs(modelt6_black)
modelt6_HRs_hispanic <- predict_HRs(modelt6_hispanic)
modelt6_HRs_other <- predict_HRs(modelt6_other)
modelt6_HRs_white_men <- predict_HRs(modelt6_white_men)
modelt6_HRs_black_men <- predict_HRs(modelt6_black_men)
modelt6_HRs_hispanic_men <- predict_HRs(modelt6_hispanic_men)
modelt6_HRs_other_men <- predict_HRs(modelt6_other_men)
modelt6_HRs_white_women <- predict_HRs(modelt6_white_women)
modelt6_HRs_black_women <- predict_HRs(modelt6_black_women)
modelt6_HRs_hispanic_women <- predict_HRs(modelt6_hispanic_women)
modelt6_HRs_other_women <- predict_HRs(modelt6_other_women) 
modelt6_HRs_interaction_race <- predict_HRs(modelt6_interaction_race)
modelt6_HRs_interaction_sex <- predict_HRs(modelt6_interaction_sex)

# Adjust the CIs to reflect the true (rather than replicated) population size
modelt6_HRs_adjusted <- adjust_CIs(modelt6, "2012-2021", data)
modelt6_HRs_adjusted_men <- adjust_CIs(modelt6_men, "2012-2021", data)
modelt6_HRs_adjusted_women <- adjust_CIs(modelt6_women, "2012-2021", data)
modelt6_HRs_adjusted_white <- adjust_CIs(modelt6_white, "2012-2021", data)
modelt6_HRs_adjusted_black <- adjust_CIs(modelt6_black, "2012-2021", data)
modelt6_HRs_adjusted_hispanic <- adjust_CIs(modelt6_hispanic, "2012-2021", data)
modelt6_HRs_adjusted_other <- adjust_CIs(modelt6_other, "2012-2021", data)
modelt6_HRs_adjusted_white_men <- adjust_CIs(modelt6_white_men, "2012-2021", data)
modelt6_HRs_adjusted_black_men <- adjust_CIs(modelt6_black_men, "2012-2021", data)
modelt6_HRs_adjusted_hispanic_men <- adjust_CIs(modelt6_hispanic_men, "2012-2021", data)
modelt6_HRs_adjusted_other_men <- adjust_CIs(modelt6_other_men, "2012-2021", data)
modelt6_HRs_adjusted_white_women <- adjust_CIs(modelt6_white_women, "2012-2021", data)
modelt6_HRs_adjusted_black_women <- adjust_CIs(modelt6_black_women, "2012-2021", data)
modelt6_HRs_adjusted_hispanic_women <- adjust_CIs(modelt6_hispanic_women, "2012-2021", data)
modelt6_HRs_adjusted_other_women <- adjust_CIs(modelt6_other_women, "2012-2021", data)
modelt6_HRs_adjusted_interaction_race <- adjust_CIs(modelt6_interaction_race, "2012-2021", data)
modelt6_HRs_adjusted_interaction_sex <- adjust_CIs(modelt6_interaction_sex, "2012-2021", data)

write_csv(modelt6_HRs_adjusted, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_model6.csv")
write_csv(modelt6_HRs_adjusted_men, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_model6_men.csv")
write_csv(modelt6_HRs_adjusted_women, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_model6_women.csv")
write_csv(modelt6_HRs_adjusted_white, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_model6_white.csv")
write_csv(modelt6_HRs_adjusted_black, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_model6_black.csv")
write_csv(modelt6_HRs_adjusted_hispanic, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_model6_hispanic.csv")
write_csv(modelt6_HRs_adjusted_other, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_model6_other.csv")
write_csv(modelt6_HRs_adjusted_white_men, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_model6_white_men.csv")
write_csv(modelt6_HRs_adjusted_black_men, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_model6_black_men.csv")
write_csv(modelt6_HRs_adjusted_hispanic_men, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_model6_hispanic_men.csv")
write_csv(modelt6_HRs_adjusted_other_men, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_model6_other_men.csv")
write_csv(modelt6_HRs_adjusted_white_women, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_model6_white_women.csv")
write_csv(modelt6_HRs_adjusted_black_women, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_model6_black_women.csv")
write_csv(modelt6_HRs_adjusted_hispanic_women, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_model6_hispanic_women.csv")
write_csv(modelt6_HRs_adjusted_other_women, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_model6_other_women.csv")
write_csv(modelt6_HRs_adjusted_interaction_race, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_model6_interaction_race.csv")
write_csv(modelt6_HRs_adjusted_interaction_sex, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_model6_interaction_sex.csv")
