# Setup
setwd("C:/Users/cmp21seb/Documents/SIMAH/")
library(tidyverse)  # data management
library(skimr)      # descriptive statistics
library(janitor)    # data management
library(msm)        # model transition probabilities
library(tableone)   # create descriptives table
library(knitr)      # create descriptives table
library(stringr)
options(scipen = 999)

# Source functions
source("SIMAH_code/alcohol_transitions_psid/functions/0_extractTPs_alc.R")
# source("SIMAH_code/alcohol_transitions_psid/functions/0_predict_HRs_alc.R")
source("SIMAH_code/alcohol_transitions_psid/functions/0_setup_alcohol_model.R")

# Load model data
data <- readRDS("SIMAH_workplace/alcohol_transitions_psid/prepped_data_for_markov_alc.rds")

# Set up data to align with model variables
data$race <- data$final_race_using_method_hierarchy
data$race <- as.character(data$final_race_using_method_hierarchy) 
data$race <- ifelse(data$race=="Asian/PI","other",data$race) 
data$race <- ifelse(data$race=="Native","other",data$race) 
data$race <- as.factor(data$race) 
data <- data %>% 
  mutate(education = case_when(
    education <= 12 ~ "Less than or equal to high school",
    education > 12 & education < 16 ~ "Some college",
    education >= 16 ~ "College +"
  ))

data <- data %>% drop_na(race,education,sex,age_cat)

# Load MSM Models 
model_2005_2010 <- readRDS("SIMAH_workplace/alcohol_transitions_psid/markov_models/psid_alcohol_model_2005_2010.RDS")
model_2011_2019 <- readRDS("SIMAH_workplace/alcohol_transitions_psid/markov_models/psid_alcohol_model_2011_2019.RDS")
model_2019_2021 <- readRDS("SIMAH_workplace/alcohol_transitions_psid/markov_models/psid_alcohol_model_2019_2021.RDS")
model_timevary <- readRDS("SIMAH_workplace/alcohol_transitions_psid/markov_models/psid_model_4_timeperiod.RDS")

# Extract TPs for moving between states, overall
model_2005_2010_TPs <- extractTPs_basic(model_2005_2010, 1)
model_2011_2019_TPs <- extractTPs_basic(model_2011_2019, 1)
model_2019_2021_TPs <- extractTPs_basic(model_2019_2021, 1)
# model_timevary_TPs <- extractTPs_basic(model_timevary, 1)
# Error in normboot.msm(x, function(x) pmatrix.msm(x = x, t = t, t1 = t1,  : 
# Asymptotic standard errors not available in fitted model

# Save results
write_csv(model_2005_2010_TPs, "SIMAH_workplace/alcohol_transitions_psid/model_2005_2010_TPs.csv")
write_csv(model_2011_2019_TPs, "SIMAH_workplace/alcohol_transitions_psid/model_2011_2019_TPs.csv")
write_csv(model_2019_2021_TPs, "SIMAH_workplace/alcohol_transitions_psid/model_2019_2021_TPs.csv")
# write_csv(model_timevary_TPs, "SIMAH_workplace/alcohol_transitions_psid/model_timevary_TPs.csv")


##################################################################

# Extract TPs for specific groups
combo <- expand.grid(agecat = unique(data$age_cat), sex = unique(data$sex), race=unique(data$race), education=unique(data$education))

model_2005_2010_TPs_detail <- extractTPs_subgroups(model_2005_2010, combo) 
model_2011_2019_TPs_detail <- extractTPs_subgroups(model_2011_2019, combo)

# Save results
write_csv(model_2005_2010_TPs_detail, "SIMAH_workplace/alcohol_transitions_psid/model_2005_2010_TPs_detail.csv")
write_csv(model_2011_2019_TPs_detail, "SIMAH_workplace/alcohol_transitions_psid/model_2011_2019_TPs_detail.csv")

# CONTINUE FROM HERE

########################################
# Hazard ratios
model_2005_2010_HRs <- predict_HRs(model_2005_2010)
model_2011_2019_HRs <- predict_HRs(model_2011_2019)

write_csv(model_2005_2010_HRs, "SIMAH_workplace/alcohol_transitions_psid/model_2005_2010_HRs.csv")
write_csv(model_2011_2019_HRs, "SIMAH_workplace/alcohol_transitions_psid/model_2011_2019_HRs.csv")
