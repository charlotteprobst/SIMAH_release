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

# Load MSM Models 
modelt1 <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/formodel_modelt1_newn.RDS")
modelt2 <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/formodel_modelt2_newn.RDS")
modelt3 <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/formodel_modelt3_newn.RDS")
modelt4 <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/formodel_modelt4_newn.RDS")
modelt5 <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/formodel_modelt5_newn.RDS")
modelt6 <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/formodel_modelt6_newn.RDS")
modelt5_men <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/modelt5_men.RDS")
modelt5_women <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/modelt5_women.RDS")

# Extract TPs for moving between states, overall

# 1999-2005
modelt1_TPs <- extractTPs_basic(modelt1, 1)
# 2006-2011
modelt2_TPs <- extractTPs_basic(modelt2, 1)
# 2012-2018
modelt3_TPs <- extractTPs_basic(modelt3, 1)
# 2019-2021
modelt4_TPs <- extractTPs_basic(modelt4, 1)
# all years (covariate for all time periods)
modelt5_TPs <- extractTPs_basic(modelt5, 1)
modelt5_TPs_men <- extractTPs_basic(modelt5_men, 1) # plus interaction for sex (men only)
modelt5_TPs_women <- extractTPs_basic(modelt5_women, 1) # plus interaction for sex (women only)
# all years (covariate of 2012-2018 vs 2019-201)
modelt6_TPs <- extractTPs_basic(modelt6, 1)


# Save results
# write_csv(modelt1_TPs, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_1999_2005.csv")
# write_csv(modelt2_TPs, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_2006_2011.csv")
# write_csv(modelt3_TPs, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_2012_2018.csv")
# write_csv(modelt4_TPs, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_2019_2021.csv")
# write_csv(modelt5_TPs, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_all_years_a.csv")
write_csv(modelt5_TPs_men, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_all_years_a_men.csv")
write_csv(modelt5_TPs_women, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_all_years_a_women.csv")
# write_csv(modelt6_TPs, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_all_years_b.csv")

##################################################################

# Extract TPs for specific groups
combo <- expand.grid(agecat = unique(data$agecat), sex = unique(data$sex), racefinal2=unique(data$racefinal2))

modelt1_TPs_detail <- extractTPs_subgroups(modelt1, combo)
modelt2_TPs_detail <- extractTPs_subgroups(modelt2, combo)
modelt3_TPs_detail <- extractTPs_subgroups(modelt3, combo)
modelt4_TPs_detail <- extractTPs_subgroups(modelt4, combo)

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
combo_2 <- expand.grid(timevary = unique(data$time_1), agecat = unique(data$agecat), sex = unique(data$sex), racefinal2=unique(data$racefinal2))
combo_2_men <- expand.grid(timevary = unique(men$time_1), agecat = unique(men$agecat), racefinal2=unique(men$racefinal2))
combo_2_women <- expand.grid(timevary = unique(women$time_1), agecat = unique(women$agecat), racefinal2=unique(women$racefinal2))
 
# data$time_2 <- cut(data$year,
#                        breaks=c(0,2018, 2021),
#                        labels=c("2012-2018", "2019-2021"))
# combo_3 <- expand.grid(timevary = unique(data$time_2), agecat = unique(data$agecat), sex = unique(data$sex), racefinal2=unique(data$racefinal2))

modelt5_TPs_detail <- extractTP_incl_time(modelt5, combo_2)
modelt5_TPs_men_detail <- extractTP_interaction_sex(modelt5_men,combo_2_men)
modelt5_TPs_women_detail <- extractTP_interaction_sex(modelt5_women,combo_2_women)

modelt6_TPs_detail <- extractTP_incl_time(modelt6, combo_3)

# Save results
write_csv(modelt1_TPs_detail, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_1999_2005_detail.csv")
write_csv(modelt2_TPs_detail, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_2006_2011_detail.csv")
write_csv(modelt3_TPs_detail, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_2012_2018_detail.csv")
write_csv(modelt4_TPs_detail, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_2019_2021_detail.csv")
write_csv(modelt5_TPs_detail, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_all_years_a_detail.csv")
write_csv(modelt5_TPs_men_detail, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_all_years_a_men_detail.csv")
write_csv(modelt5_TPs_women_detail, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_all_years_a_women_detail.csv")
write_csv(modelt6_TPs_detail, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_all_years_b_detail.csv")


########################################
# Hazard ratios

modelt1_HRs <- predict_HRs(modelt1)
modelt2_HRs <- predict_HRs(modelt2)
modelt3_HRs <- predict_HRs(modelt3)
modelt4_HRs <- predict_HRs(modelt4)
modelt5_HRs <- predict_HRs(modelt5)
modelt5_HRs_men <- predict_HRs(modelt5_men)
modelt5_HRs_women <- predict_HRs(modelt5_women)
modelt6_HRs <- predict_HRs(modelt6)

# Adjust the CIs to reflect the true (rather than replicated) population size
modelt1_HRs_adjusted <- adjust_CIs_2021(modelt1, "1999-2005", data)
modelt2_HRs_adjusted <- adjust_CIs_2021(modelt2, "2006-2011", data)
modelt3_HRs_adjusted <- adjust_CIs_2021(modelt3, "2012-2018", data)
modelt4_HRs_adjusted <- adjust_CIs_2021(modelt4, "2019-2021", data)
modelt5_HRs_adjusted <- adjust_CIs_2021(modelt5, "1999-2009", data)
modelt5_HRs_adjusted_men <- adjust_CIs(modelt5_men, "1999-2009", data)
modelt5_HRs_adjusted_women <- adjust_CIs(modelt5_women, "1999-2009", data)
modelt6_HRs_adjusted <- adjust_CIs_2021(modelt6, "1999-2009", data)

write_csv(modelt1_HRs_adjusted, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_1999_2005.csv")
write_csv(modelt2_HRs_adjusted, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_2006_2011.csv")
write_csv(modelt3_HRs_adjusted, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_2012_2018.csv")
write_csv(modelt4_HRs_adjusted, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_2019_2021.csv")
write_csv(modelt5_HRs_adjusted, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_all_years_a.csv")
write_csv(modelt5_HRs_adjusted_men, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_all_years_a_men.csv")
write_csv(modelt5_HRs_adjusted_women, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_all_years_women.csv")
write_csv(modelt6_HRs_adjusted, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_all_years_b.csv")
