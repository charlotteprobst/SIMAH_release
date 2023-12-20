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
men <- data %>% filter(sex==0)
women <- data %>% filter(sex==1)
white <- data %>% filter(racefinal2=="white")
black <- data %>% filter(racefinal2=="black")
hispanic <- data %>% filter(racefinal2=="hispanic")
other <- data %>% filter(racefinal2=="other")
black_men <- data %>% filter(racefinal2=="black", sex==0)

# Load MSM Models 
modelt5 <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/modelt5_alt_ref_cats.RDS")
modelt5_men <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/modelt5_men_alt_ref_cats.RDS")
modelt5_women <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/modelt5_women_alt_ref_cats.RDS")
modelt5_white <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/modelt5_white_alt_ref_cats.RDS")
modelt5_black <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/modelt5_black_alt_ref_cats.RDS")
modelt5_hispanic <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/modelt5_hispanic_alt_ref_cats.RDS")
modelt5_other <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/modelt5_other_alt_ref_cats.RDS")
modelt5_white_men <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/modelt5_white_men_alt_ref_cats.RDS")
modelt5_black_men <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/modelt5_black_men_alt_ref_cats.RDS")
modelt5_hispanic_men <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/modelt5_hispanic_men_alt_ref_cats.RDS")
modelt5_other_men <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/modelt5_other_men_alt_ref_cats.RDS")
modelt5_white_women <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/modelt5_white_women_alt_ref_cats.RDS")
modelt5_black_women <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/modelt5_black_women_alt_ref_cats.RDS")
modelt5_hispanic_women <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/modelt5_hispanic_women_alt_ref_cats.RDS")
modelt5_other_women <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/modelt5_other_women_alt_ref_cats.RDS")

# Extract TPs

# all years (covariate for all time periods)
modelt5_TPs <- extractTPs_basic(modelt5, 1)
modelt5_TPs_men <- extractTPs_basic(modelt5_men, 1) # interaction for sex (men only)
modelt5_TPs_women <- extractTPs_basic(modelt5_women, 1) # interaction for sex (women only)
modelt5_TPs_white <- extractTPs_basic(modelt5_white, 1) # interaction for race (white only)
modelt5_TPs_black <- extractTPs_basic(modelt5_black, 1) # interaction for race (black only)
modelt5_TPs_hispanic <- extractTPs_basic(modelt5_hispanic, 1) # interaction for race (hispanic only)
modelt5_TPs_other <- extractTPs_basic(modelt5_other, 1) # interaction for race (other only)
modelt5_TPs_black_men <- extractTPs_basic(modelt5_black_men, 1) # interaction for race and sex (Black men only)

# Save results
write_csv(modelt5_TPs, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_5_alt_ref_cats.csv")
write_csv(modelt5_TPs_men, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_5_alt_ref_cats_men.csv")
write_csv(modelt5_TPs_women, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_5_alt_ref_cats_women.csv")
write_csv(modelt5_TPs_white, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_5_alt_ref_cats_white.csv")
write_csv(modelt5_TPs_black, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_5_alt_ref_cats_black.csv")
write_csv(modelt5_TPs_hispanic, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_5_alt_ref_cats_hispanic.csv")
write_csv(modelt5_TPs_other, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_5_alt_ref_cats_other.csv")
write_csv(modelt5_TPs_black_men, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_5_alt_ref_cats_black_men.csv")

##################################################################

# Extract TPs for specific groups

##### include covariates for time period (for models 5 & 6)
data$time_1 <- cut(data$year,
                       breaks=c(0,2005,2011,2018, 2021),
                       labels=c("1999-2005","2006-2011","2012-2018", "2019-2021"))
men$time_1 <- cut(men$year,
                  breaks=c(0,2005,2011,2018, 2021),
                  labels=c("1999-2005","2006-2011","2012-2018", "2019-2021"))
women$time_1 <- cut(women$year,
                  breaks=c(0,2005,2011,2018, 2021),
                  labels=c("1999-2005","2006-2011","2012-2018", "2019-2021"))
white$time_1 <- cut(white$year,
                  breaks=c(0,2005,2011,2018, 2021),
                  labels=c("1999-2005","2006-2011","2012-2018", "2019-2021"))
black$time_1 <- cut(black$year,
                    breaks=c(0,2005,2011,2018, 2021),
                    labels=c("1999-2005","2006-2011","2012-2018", "2019-2021"))
hispanic$time_1 <- cut(hispanic$year,
                  breaks=c(0,2005,2011,2018, 2021),
                  labels=c("1999-2005","2006-2011","2012-2018", "2019-2021"))
other$time_1 <- cut(other$year,
                    breaks=c(0,2005,2011,2018, 2021),
                    labels=c("1999-2005","2006-2011","2012-2018", "2019-2021"))
black_men$time_1 <- cut(black_men$year,
                        breaks=c(0,2005,2011,2018, 2021),
                        labels=c("1999-2005","2006-2011","2012-2018", "2019-2021"))

combo_2 <- expand.grid(timevary = unique(data$time_1), agecat = unique(data$agecat), sex = unique(data$sex), racefinal2=unique(data$racefinal2))
combo_2_men <- expand.grid(timevary = unique(men$time_1), agecat = unique(men$agecat), racefinal2=unique(men$racefinal2))
combo_2_women <- expand.grid(timevary = unique(women$time_1), agecat = unique(women$agecat), racefinal2=unique(women$racefinal2))
combo_2_white <- expand.grid(timevary = unique(white$time_1), agecat = unique(white$agecat), sex=unique(white$sex))
combo_2_black <- expand.grid(timevary = unique(black$time_1), agecat = unique(black$agecat), sex=unique(black$sex))
combo_2_hispanic <- expand.grid(timevary = unique(hispanic$time_1), agecat = unique(hispanic$agecat), sex=unique(hispanic$sex))
combo_2_other <- expand.grid(timevary = unique(other$time_1), agecat = unique(other$agecat), sex=unique(other$sex))
combo_2_black_men <- expand.grid(timevary = unique(black_men$time_1), agecat = unique(black_men$agecat))

modelt5_TPs_detail <- extractTP_incl_time(modelt5, combo_2)
modelt5_TPs_men_detail <- extractTP_interaction_sex(modelt5_men,combo_2_men)
modelt5_TPs_women_detail <- extractTP_interaction_sex(modelt5_women,combo_2_women)
modelt5_TPs_white_detail <- extractTP_interaction_race(modelt5_white,combo_2_white)
modelt5_TPs_black_detail <- extractTP_interaction_race(modelt5_black,combo_2_black)
modelt5_TPs_hispanic_detail <- extractTP_interaction_race(modelt5_hispanic,combo_2_hispanic)
modelt5_TPs_other_detail <- extractTP_interaction_race(modelt5_other,combo_2_other)
modelt5_TPs_black_men_detail <- extractTP_interaction_race_sex(modelt5_black_men,combo_2_black_men)

# Save results
write_csv(modelt5_TPs_detail, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_5_alt_ref_cats.csv")
write_csv(modelt5_TPs_men_detail, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_5_alt_ref_cats_men_detail.csv")
write_csv(modelt5_TPs_women_detail, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_5_alt_ref_cats_women_detail.csv")
write_csv(modelt5_TPs_white_detail, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_5_alt_ref_cats_white_detail.csv")
write_csv(modelt5_TPs_black_detail, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_5_alt_ref_cats_black_detail.csv")
write_csv(modelt5_TPs_hispanic_detail, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_5_alt_ref_cats_hispanic_detail.csv")
write_csv(modelt5_TPs_other_detail, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_5_alt_ref_cats_other_detail.csv")
write_csv(modelt5_TPs_black_men_detail, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_5_alt_ref_cats_black_men_detail.csv")

########################################
# Hazard ratios

modelt5_HRs <- predict_HRs(modelt5)
modelt5_HRs_men <- predict_HRs(modelt5_men)
modelt5_HRs_women <- predict_HRs(modelt5_women)
modelt5_HRs_white <- predict_HRs(modelt5_white)
modelt5_HRs_black <- predict_HRs(modelt5_black)
modelt5_HRs_hispanic <- predict_HRs(modelt5_hispanic)
modelt5_HRs_other <- predict_HRs(modelt5_other)
modelt5_HRs_white_men <- predict_HRs(modelt5_white_men)
modelt5_HRs_black_men <- predict_HRs(modelt5_black_men)
modelt5_HRs_hispanic_men <- predict_HRs(modelt5_hispanic_men)
modelt5_HRs_other_men <- predict_HRs(modelt5_other_men)
modelt5_HRs_white_women <- predict_HRs(modelt5_white_women)
modelt5_HRs_black_women <- predict_HRs(modelt5_black_women)
modelt5_HRs_hispanic_women <- predict_HRs(modelt5_hispanic_women)
modelt5_HRs_other_women <- predict_HRs(modelt5_other_women)


# Adjust the CIs to reflect the true (rather than replicated) population size
modelt5_HRs_adjusted <- adjust_CIs(modelt5, "1999-2009", data)
modelt5_HRs_adjusted_men <- adjust_CIs(modelt5_men, "1999-2009", data)
modelt5_HRs_adjusted_women <- adjust_CIs(modelt5_women, "1999-2009", data)
modelt5_HRs_adjusted_white <- adjust_CIs(modelt5_white, "1999-2009", data)
modelt5_HRs_adjusted_black <- adjust_CIs(modelt5_black, "1999-2009", data)
modelt5_HRs_adjusted_hispanic <- adjust_CIs(modelt5_hispanic, "1999-2009", data)
modelt5_HRs_adjusted_other <- adjust_CIs(modelt5_other, "1999-2009", data)
modelt5_HRs_adjusted_white_men <- adjust_CIs(modelt5_white_men, "1999-2009", data)
modelt5_HRs_adjusted_black_men <- adjust_CIs(modelt5_black_men, "1999-2009", data)
modelt5_HRs_adjusted_hispanic_men <- adjust_CIs(modelt5_hispanic_men, "1999-2009", data)
modelt5_HRs_adjusted_other_men <- adjust_CIs(modelt5_other_men, "1999-2009", data)
modelt5_HRs_adjusted_white_women <- adjust_CIs(modelt5_white_women, "1999-2009", data)
modelt5_HRs_adjusted_black_women <- adjust_CIs(modelt5_black_women, "1999-2009", data)
modelt5_HRs_adjusted_hispanic_women <- adjust_CIs(modelt5_hispanic_women, "1999-2009", data)
modelt5_HRs_adjusted_other_women <- adjust_CIs(modelt5_other_women, "1999-2009", data)

write_csv(modelt5_HRs_adjusted, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_model5_alt_ref_cats.csv")
write_csv(modelt5_HRs_adjusted_men, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_model5_alt_ref_cats_men.csv")
write_csv(modelt5_HRs_adjusted_women, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_model5_alt_ref_cats_women.csv")
write_csv(modelt5_HRs_adjusted_white, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_model5_alt_ref_cats_white.csv")
write_csv(modelt5_HRs_adjusted_black, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_model5_alt_ref_cats_black.csv")
write_csv(modelt5_HRs_adjusted_hispanic, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_model5_alt_ref_cats_hispanic.csv")
write_csv(modelt5_HRs_adjusted_other, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_model5_alt_ref_cats_other.csv")
write_csv(modelt5_HRs_adjusted_white_men, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_model5_alt_ref_cats_white_men.csv")
write_csv(modelt5_HRs_adjusted_black_men, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_model5_alt_ref_cats_black_men.csv")
write_csv(modelt5_HRs_adjusted_hispanic_men, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_model5_alt_ref_cats_hispanic_men.csv")
write_csv(modelt5_HRs_adjusted_other_men, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_model5_alt_ref_cats_other_men.csv")
write_csv(modelt5_HRs_adjusted_white_women, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_model5_alt_ref_cats_white_women.csv")
write_csv(modelt5_HRs_adjusted_black_women, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_model5_alt_ref_cats_black_women.csv")
write_csv(modelt5_HRs_adjusted_hispanic_women, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_model5_alt_ref_cats_hispanic_women.csv")
write_csv(modelt5_HRs_adjusted_other_women, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_model5_alt_ref_cats_other_women.csv")