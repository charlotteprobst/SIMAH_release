# NHIS Data Management File

library(haven)      # Read SAS file
library(tidyverse)  # data management
library(janitor)    # clean variable names
library(skimr)      # descriptive statistics


# Specify the data file location
data_orig <- "~/Dropbox/Mac/Desktop/Public Health Institute/Alcohol Research Group/SIMAH/Original data/"
data_new  <- "~/Dropbox/Mac/Desktop/Public Health Institute/Alcohol Research Group/SIMAH/SIMAH_workplace/nhis/Restricted access data/Data/"


# Import data form SAS and edit/re-categorize variables 
nhis_all <- read_sas (paste0(data_orig, "nhis_mort_clean.sas7bdat")) %>%
  zap_formats() %>% zap_label() %>% clean_names() %>%   # removes labels/formats form SAS and clean names
  mutate (# Recode and create variables
    bl_age = age,                  # Baseline age
    end_age = age + yrs_followup,  # Age at death/censor
    alcohol5v2 = recode(alcohol6, `1`=1, `2`=2, `3`=3, `4`=4, `5`=5, `6`=5),  # separate former/never drinkers and merge high risk and very high risk group
    smk = recode(smoking, `0`=1, `1`=2, `2`=3, `3`=4),                   # recoded such that '1' is the first/smallest category
    income4 = recode(income, `1`=1, `2`=2, `3`=2, `4`=3, `5`=4),              # merge the two middle categories
    allcause_death = mortstat,
    
    # LABEL VARIABLES 
    # Exposures
    edu3 = factor(edu, levels=c(3,1,2), labels = c("Bachelors", "Highschool", "Some college")),
    income.factor = factor(income, levels=c(4,1,2,3,5), labels = c("High income", "Poor","Near poor","Middle income", "Missing")),
    income4.factor = factor(income4, levels=c(3,1,2,4), labels = c( "High", "Low","Medium", "Missing")),
  
    # Mediator 1 - Alcohol
    alcohol5v2.factor = factor(alcohol5v2, levels=c(3,1,2,4,5), labels = c("Category I","Never Drinker", "Former Drinker", "Category II","Category III")), 

    # Mediator 2 - BMI
    bmi.factor = factor(bmi_cat, levels=c(2,1,3,4), labels = c("Healthy weight","Underweight", "Overweight", "Obese")),
    
    # Mediator 3 - Smoking
    smk.factor = factor(smk, levels=c(1,2,3,4), labels = c("Never smoker", "Former smoker", "Current some day smoker", "Current everyday smoker")),
    
    # Mediator 4 - Physical Activity
    phy.factor = factor(phy_act3, levels=c(3,1,2), labels = c("Active", "Sedentary", "Somewhat active")),
    
    # Covariates / other
    ethnicity.factor = factor(ethnicity, levels=c(1,2,3,4), labels = c("Non-Hispanic White", "Non-Hispanic Black","Hispanic", "Other")),
    female.factor = factor(female, levels=c(0,1), labels = c("Male", "Female")),
    married2 = factor(married, levels=c(0,1), labels = c("Not married/living togeter", "Married/cohabitating")),

    
    # Outcome        
    allcause_death.factor = factor(allcause_death, levels=c(0,1), labels = c("Alive","Deceased")))

str(nhis_all)
# Check recoding                                     
# count(nhis_all, alcohol5, alcohol4)
# count(nhis_all, alcohol6, alcohol5_2)



# Create subset of data with relevant participants (Ages 25 - 85 years)
nhis_ages25_85 <- filter (nhis_all, age>=25 & age<85) %>% # Remove those outside our age range
  mutate(lost = factor(ifelse(complete.cases(yrs_followup, allcause_death, alcohol5v2, bmi_cat, smk, phy_act3, edu3, age, female, married2, ethnicity), "Complete", "Lost")))

nhis_cc_ages25_85 <- filter(nhis_ages25_85, lost=="Complete") # Data with complete cases (CC)


# Ages 25-85; Descriptives of how many were lost to follow-up
nrow(nhis_all)                        # n completed NHIS
nrow(nhis_all) - nrow(nhis_ages25_85)      # Aged <25 or >85
nrow(nhis_ages25_85)                       # n eligible 
count(nhis_all, allcause_death)       # No mortality data
nrow(nhis_ages25_85) - nrow(nhis_cc_ages25_85)          # missing any covariates
nrow(nhis_ages25_85) - nrow(nhis_cc_ages25_85) - 23474  # of those with mortality data, n missing covariates
nrow(nhis_cc_ages25_85)                            # final sample size


# Create subset of data with relevant participants (Ages 18 - 85 years)
nhis_ages18_85 <- filter (nhis_all, age>=18 & age<85) %>% # Remove those outside our age range
  mutate(lost = factor(ifelse(complete.cases(yrs_followup, allcause_death, alcohol5v2, bmi_cat, smk, phy_act3, edu3, age, female, married2, ethnicity), "Complete", "Lost")))

nhis_cc_ages18_85 <- filter(nhis_ages18_85, lost=="Complete") # Data with complete cases (CC)


# Ages 18-85; Descriptives of how many were lost to follow-up
nrow(nhis_all)                                          # n completed NHIS
nrow(nhis_all) - nrow(nhis_ages18_85)                   # Aged <18 or >85
nrow(nhis_ages18_85)                                    # n eligible 
count(nhis_all, allcause_death)                         # No mortality data
nrow(nhis_ages18_85) - nrow(nhis_cc_ages18_85)          # missing any covariates
nrow(nhis_ages18_85) - nrow(nhis_cc_ages18_85) - 23474  # of those with mortality data, n missing covariates
nrow(nhis_cc_ages18_85)                                 # final sample size


# Save copy of final datasets  
saveRDS(nhis_ages25_85,     paste0(data_new, "nhis_all25_85.rds"))      # NHIS data with all participants ages 25-85
saveRDS(nhis_cc_ages25_85,  paste0(data_new, "nhis25_85.rds"))          # NHIS data to be analyzed (ages 25-85)

saveRDS(nhis_ages18_85,     paste0(data_new, "nhis_all18_85.rds"))      # NHIS data with all participants  ages 18-85
saveRDS(nhis_cc_ages18_85,  paste0(data_new, "nhis18_85.rds"))          # NHIS data to be analyzed (ages 18-85)

nhis18_female <- filter(nhis_cc_ages18_85, female==1)
nhis18_male <- filter(nhis_cc_ages18_85, female==0)

saveRDS(nhis18_female, paste0(data_new, "nhis18_female.rds"))
saveRDS(nhis18_male, paste0(data_new, "nhis18_male.rds"))
