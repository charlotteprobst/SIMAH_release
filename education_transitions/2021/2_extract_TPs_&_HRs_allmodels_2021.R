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

# Set up date to align with model variables
data$racefinal2 <- data$final_race_using_method_hierarchy
data$racefinal2 <- as.character(data$final_race_using_method_hierarchy) 
data$racefinal2 <- ifelse(data$racefinal2=="Asian/PI","other",data$racefinal2) 
data$racefinal2 <- ifelse(data$racefinal2=="Native","other",data$racefinal2) 
data$racefinal2 <- as.factor(data$racefinal2) 

# Load MSM Models 
modelt1 <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/covid_modelt1.RDS")
modelt2 <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/covid_modelt2.RDS")
modelt3 <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/covid_modelt3.RDS")
modelt3_2019 <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/covid_modelt3_2019.RDS")
modelt4 <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/covid_modelt4.RDS")
modelt5 <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/covid_modelt5.RDS")

# Extract TPs for moving between states, overall

# 1999-2005
modelt1_TPs <- extractTPs_basic(modelt1, 1)
# 2006-2011
modelt2_TPs <- extractTPs_basic(modelt2, 1)
# 2013-2018
modelt3_TPs <- extractTPs_basic(modelt3, 1)
# 2013-2019
modelt3_2019_TPs <- extractTPs_basic(modelt3_2019, 1)
# 2019-2021
modelt4_TPs <- extractTPs_basic(modelt4, 1)
# all years (covariate for all time periods)
modelt5_TPs <- extractTPs_basic(modelt5, 1)

# Save results
write_csv(modelt1_TPs, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_1999_2005.csv")
write_csv(modelt2_TPs, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_2006_2011.csv")
write_csv(modelt3_TPs, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_2013_2018.csv")
write_csv(modelt3_2019_TPs, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_2013_2019.csv")
write_csv(modelt4_TPs, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_2019_2021.csv")
write_csv(modelt5_TPs, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_four_time_periods.csv")

##################################################################

# Extract TPs for specific groups
combo <- expand.grid(agecat = unique(data$agecat), sex = unique(data$sex), racefinal2=unique(data$racefinal2))

datat5 <- data
datat5$timevary <- cut(datat5$year,
                       breaks=c(0,2005,2011,2018, 2021),
                       labels=c("1999-2005","2006-2011","2012-2018", "2019-2021"))
combo_2 <- expand.grid(timevary = unique(datat5$timevary), agecat = unique(datat5$agecat), sex = unique(datat5$sex), racefinal2=unique(datat5$racefinal2))

modelt1_TPs_detail <- extractTPs_subgroups(modelt1, combo)
modelt2_TPs_detail <- extractTPs_subgroups(modelt2, combo)
modelt3_TPs_detail <- extractTPs_subgroups(modelt3, combo)
modelt3_2019_TPs_detail <- extractTPs_subgroups(modelt3_2019, combo)
modelt4_TPs_detail <- extractTPs_subgroups(modelt4, combo)
modelt5_TPs_detail <- extractTP_incl_time(modelt5, combo_2) # something about the time periods

# Save results
write_csv(modelt1_TPs_detail, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_1999_2005_detail.csv")
write_csv(modelt2_TPs_detail, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_2006_2011_detail.csv")
write_csv(modelt3_TPs_detail, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_2012_2018_detail.csv")
write_csv(modelt3_2019_TPs_detail, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_2012_2019_detail.csv")
write_csv(modelt4_TPs_detail, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_2019_2021_detail.csv")
write_csv(modelt5_TPs_detail, "SIMAH_workplace/education_transitions/2021/annual_education_TPs_four_time_periods_detail.csv")

########################################
# Hazard ratios
modelt1_HRs <- predict_HRs(modelt1)
modelt2_HRs <- predict_HRs(modelt2)
modelt3_HRs <- predict_HRs(modelt3)
modeltt3_2019_HRs <- predict_HRs(modelt3_2019)
modelt4_HRs <- predict_HRs(modelt4)
modelt5_HRs <- predict_HRs(modelt5)

# Adjust the CIs to reflect the true (rather than replicated) population size
modelt1_HRs_adjusted <- adjust_CIs(modelt1, "1999-2005", data)
modelt2_HRs_adjusted <- adjust_CIs(modelt2, "2006-2011", data)
modelt3_HRs_adjusted <- adjust_CIs(modelt3, "2012-2018", data)
modelt3_2019_HRs_adjusted <- adjust_CIs(modelt3_2019, "2012-2019", data)
modelt4_HRs_adjusted <- adjust_CIs(modelt4, "2019-2021", data)
modelt5_HRs_adjusted <- adjust_CIs(modelt5, "1999-2021", data)

write_csv(modelt1_HRs_adjusted, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_1999_2005.csv")
write_csv(modelt2_HRs_adjusted, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_2006_2011.csv")
write_csv(modelt3_HRs_adjusted, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_2012_2018.csv")
write_csv(modelt3_2019_HRs_adjusted, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_2012_2019.csv")
write_csv(modelt4_HRs_adjusted, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_2019_2021.csv")
write_csv(modelt5_HRs_adjusted, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_four_time_periods.csv")
