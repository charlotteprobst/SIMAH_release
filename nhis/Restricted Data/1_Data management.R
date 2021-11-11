
# SIMAH - Restricted Access Data 
# Data Management File

library(haven)      # Read SAS file
library(tidyverse)  # data management
library(janitor)    # clean variable names
library(skimr)      # descriptive statistics
library(survey)     # to accommodate survey weights


# Specify the data file location
data  <- "C:/..."


# Import data and edit/recode variables 

nhis_all <- read_sas ("C:/...") 

      # Variables needed (categories and labels shown below):
          # allcause_deaths
          # edu
          # income
          # alc_daily_g
          # alc
          # hed
          # smk
          # bmi_cat
          # phy
          # race
          # female
          # married
          # employed
          # bl_age
          # end_age
          # yrs_followup
          # PsyDistr



# Label variables
## NOTE 1: the 'factor' variables end with a number indicating the number of categories they contain
## NOTE 2: The first level listed in the reference level

    # Outcome        
    nhis_all$allcause_deaths2 <- factor(nhis_all$allcause_deaths, levels=c(0,1),
                                    labels = c("Alive","Deceased"))
    
    # Exposures
    nhis_all$edu3 <- factor(nhis_all$edu, levels=c(3,1,2), 
                              labels = c("Bachelors", "Highschool", "Some college"))

    nhis_all$income5 <- factor(nhis_all$income, levels=c(4,1,2,3,5), 
                                  labels = c( "Higher income", "Poor","Near poor", "Middleincome", "Missing"))
    
    nhis_all$race4 <- factor(nhis_all$race, levels=c(1,2,3, 4),
      labels = c("Non-Hispanic White", "Non-Hispanic Black","Hispanic", "Other"))
    
    nhis_all$PsyDistr3 <-factor(nhis_all$PsyDistr, levels = c(1,2,3),
                                    labels = c("None/low", "Moderate", "Severe"))
    
    
    
    # Lifestyle factors (Note, the versions with a # at the end of the variable name are factors)
    nhis_all$alc5 <- factor(nhis_all$alc, levels=c(3,1,2,4,5),
                                    labels = c("Low risk","Never Drinker", "Former Drinker", "Medium risk","High risk")) 
    
    nhis_all$hed4 <- factor(nhis_all$hed, levels=c(1,2,3,4),
                              labels = c("No HED", "HED <1/month", "HED >1/month, <1/week", "HED >=1/week"))
    
    nhis_all$bmi4 <- factor(nhis_all$bmi_cat, levels=c(2,1,3,4),
                                  labels = c("Healthy weight","Underweight", "Overweight", "Obese"))
                                  
    nhis_all$smk4 <- factor(nhis_all$smk, levels=c(1,2,3,4),
                                  labels = c("Never smoker", "Former smoker", "Current some day smoker", "Current everyday smoker"))
                                 
    nhis_all$phy3 <-factor(nhis_all$phy, levels=c(3,1,2),
                                  labels = c("Active", "Sedentary", "Somewhat active"))
    
    
    # Covariates
    nhis_all$female2 <- factor(nhis_all$female, levels=c(0,1),
                                labels = c("Male", "Female"))
    
    nhis_all$married2 <- factor(nhis_all$married, levels=c(0,1),
                                  labels = c("Not married/living togeter", "Married/cohabitating"))
    
    nhis_all$employed3 <- factor(nhis_all$employed, levels=c(0,1),
                                    labels = c("Not employed", "Paid employment, student or retired"))
                
        
        
# Create subset of data with relevant participants        
# Remove those outside our age range
nhis_age25_85 <- filter (nhis_all, age>=25 & age <85)
        
    # remove those with missing data 
    nhis <- nhis_age25_85 %>%
      filter(complete.cases(yrs_followup, allcause_deaths, alc5, bmi4, smk4, phy3, edu, age, female, married, ethnicity))
       
    # Create database specific to males or females
    nhis_female <- filter(nhis, female==1)
    nhis_male <- filter(nhis, female==0)
    
        
  

# Save copy of final datasets  
saveRDS(nhis_all, paste0(data, "nhis_all.rds"))         # NHIS data with all participants
saveRDS(nhis, paste0(data, "nhis.rds"))                 # NHIS data to be analyzed
saveRDS(nhis_male, paste0(data, "nhis_male.rds"))       # NHIS data to be analyzed (males only)
saveRDS(nhis_female, paste0(data, "nhis_female.rds"))   # NHIS data to be analyzed (females only)

