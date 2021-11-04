
# SES x Lifestyle Differential Vulnerability & Exposure Project
# Data Management File

library(haven)      # Read SAS file
library(tidyverse)  # data management
library(janitor)    # clean variable names
library(skimr)      # descriptive statistics
library(survey)     # to accomodate survey weights


# Specify the data file location
data  <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nhis/Data/"


# Import data form SAS and edit/recategorize variables 

nhis_all <- read_sas ("SIMAH_workspace/nhis/Data/nhis_mort_clean.sas7bdat") %>%
  zap_formats() %>% zap_label() %>% clean_names() %>%   # removes labels/formats form SAS and clean names
  filter(!is.na(mortstat) & !is.na(new_weight)) %>%     # remove people for whom mortality data is not available and the one person that doesn't have a mortality weight
  # Recode and create variables
  mutate (bl_age = age,                  # Baseline age
          end_age = age + yrs_followup,  # Age at death/censor
          income4 = recode(income, `1`=1, `2`=2, `3`=2, `4`=3, `5`=4),              # merge the two middle categories
          alcohol4 = recode(alcohol5, `1`=1, `2`=2, `3`=3, `4`=4, `5`=4),           # merge high risk and very high risk group
          alcohol5v2 = recode(alcohol6, `1`=1, `2`=2, `3`=3, `4`=4, `5`=5, `6`=5),  # separate former/never drinkers and merge high risk and very high risk group
          smoking4 = recode(smoking, `0`=1, `1`=2, `2`=3, `3`=4),                   # recoded such that '1' is the first/smallest category
          smoking3 = recode(smoking, `0`=1, `1`=2, `2`=3, `3`=3),                   # merge current everyday and some day smokers
          ucod_leading = if_else(mortstat==0, "000",  ucod_leading),                # Assign 'alive' category to the cause of death variable
          death_type = case_when(ucod_leading=="000" ~ 0,                           # alive
                                ucod_leading=="004" ~ 1,                            # accident
                                ucod_leading%in% c("001") ~ 2,                      # Cardiovascular disease (CVD)
                                ucod_leading=="003" ~ 3,                            # Chronic respiratory diseases (CRDs)
                                ucod_leading %in% c("002","005", "006", "007", "008", "009", "010") ~ 4), #other deaths
          allcause_death = mortstat,
          heart_death = ifelse(ucod_leading=="001", 1, 0),  # death from 'diseases of heart'
          neoplasm_death = ifelse(ucod_leading=="002", 1, 0),  # death from 'malignant neoplasms'
          cvd_death = ifelse(ucod_leading=="005", 1, 0),  # death from 'Cerebrovascular diseases'
          diabetes_death = ifelse(ucod_leading=="007", 1, 0))  # death from 'Diabetes mellitus'
    
                                
         str(nhis_all)
          # Check recoding                                     
          # count(nhis_all, alcohol5, alcohol4)
          # count(nhis_all, alcohol6, alcohol5_2)
          # count(nhis_all, smoking, smoking3)
          # count(nhis_all, mortstat, death_type, accid_death, cvd_death, crd_death, other_death)


         
         
# Label variables 

        # Exposures
        nhis_all$edu.factor <- factor(nhis_all$edu, levels=c(1,2,3),
                                  labels = c("Highschool", "Some college", "Bachelors"))
                  nhis_all$edu.factor <- relevel(nhis_all$edu.factor, ref = "Bachelors")    # specifies the reference category      
        
        nhis_all$income.factor <- factor(nhis_all$income, levels=c(1,2,3,4,5), labels = c("Poor","Near poor","Middle income", "High income", "Missing"))
                  nhis_all$income.factor <- relevel(nhis_all$income.factor, ref = "High income")    # specifies the reference category 
        
        nhis_all$income4.factor <- factor(nhis_all$income4, levels=c(1,2,3,4), labels = c("Low","Medium", "High", "Missing"))
                  nhis_all$income4.factor <- relevel(nhis_all$income4.factor, ref = "High")    # specifies the reference category 
                  
                  
        # Mediator 1 - Alcohol
        nhis_all$alcohol5.factor <- factor(nhis_all$alcohol5, levels=c(1,2,3,4,5),
                                       labels = c("Abstinence", "Low risk","Medium risk","High risk","Very high risk")) 
        
        nhis_all$alcohol6.factor <- factor(nhis_all$alcohol6, levels=c(1,2,3,4,5,6),
                                       labels = c("Never Drinker", "Former Drinker", "Low risk","Medium risk","High risk","Very high risk")) 
        
        nhis_all$alcohol4.factor <- factor(nhis_all$alcohol4, levels=c(1,2,3,4),
                                       labels = c("Abstinence", "Low risk","Medium risk","High risk")) 
        
        nhis_all$alcohol5v2.factor <- factor(nhis_all$alcohol5v2, levels=c(1,2,3,4,5),
                                        labels = c("Never Drinker", "Former Drinker", "Low risk","Medium risk","High risk")) 
                                        nhis_all$alcohol5v2.factor <- relevel(nhis_all$alcohol5v2.factor, ref = "Low risk")    # specifies the reference category
        
       
        nhis_all$drink_hist.factor <- factor(nhis_all$drink_hist, levels=c(0,1,2),
                                         labels = c("Never Drinker", "Former Drinker", "Current Drinker"))
         
        
        nhis_all$hed.factor <- factor(nhis_all$hed, levels=c(1,2,3,4),
                                  labels = c("No HED", "HED <1/month", "HED >1/month, <1/week", "HED >=1/week"))
        
        
        # Mediator 2 - BMI
        nhis_all$bmi_cat.factor <- factor(nhis_all$bmi_cat, levels=c(1,2,3,4),
                                      labels = c("Underweight", "Healthy weight","Overweight", "Obese"))
                                      nhis_all$bmi_cat.factor <- relevel(nhis_all$bmi_cat.factor, ref = "Healthy weight")    # specifies the reference category
        
                    
                    
        # Mediator  - Smoking
        nhis_all$smoking4.factor <- factor(nhis_all$smoking4, levels=c(1,2,3,4),
                                      labels = c("Never smoker", "Former smoker", "Current some day smoker", "Current everyday smoker"))
                                      nhis_all$smoking4.factor <- relevel(nhis_all$smoking4.factor, ref = "Never smoker")    # specifies the reference category
        
        nhis_all$smoking3.factor <- factor(nhis_all$smoking3, levels=c(1,2,3),
                                      labels = c("Never smoker", "Former smoker", "Current smoker"))
                                      nhis_all$smoking3.factor <- relevel(nhis_all$smoking3.factor, ref = "Never smoker")    # specifies the reference category
        
        
        # Mediator 4 - Physicial Activity
        nhis_all$phy_act3.factor <-factor(nhis_all$phy_act3,levels=c(1,2,3),
                                      labels = c("Sedentary", "Somewhat active", "Active"))
                                      nhis_all$phy_act3.factor <- relevel(nhis_all$phy_act3.factor, ref = "Active")    # specifies the reference category
        
        
        
        # Covariates
        nhis_all$ethnicity.factor <- factor(nhis_all$ethnicity, levels=c(1,2,3, 4),
                                        labels = c("Non-Hispanic White", "Non-Hispanic Black","Hispanic", "Other"))
                                         nhis_all$ethnicity.factor <- relevel(nhis_all$ethnicity.factor, ref = "Non-Hispanic White")    # specifies the reference category      
                                      
                                      
        nhis_all$female.factor <- factor(nhis_all$female, levels=c(0,1),
                                    labels = c("Male", "Female"))
        
        nhis_all$married.factor <- factor(nhis_all$married, levels=c(0,1),
                                      labels = c("Not married/living togeter", "Married/cohabitating"))
                
        nhis_all$employed.factor <- factor(nhis_all$employed, levels=c(0,1),
                                       labels = c("Not employed", "Paid employment, student or retired"))
                
        nhis_all$diabet.factor <- factor(nhis_all$diabet, levels=c(0,1,2),
                                     labels = c("No","Borderline","Yes"))
        
        
        
        
        
        # Outcome        
        nhis_all$mortstat.factor <- factor(nhis_all$mortstat, levels=c(0,1),
                                       labels = c("Alive","Deceased"))
        
        nhis_all$allcause_death.factor <- factor(nhis_all$allcause_death, levels=c(0,1),
                                                labels = c("Alive","Deceased"))
                
        nhis_all$ucod_leading <- factor(nhis_all$ucod_leading, levels=c("000", '001', '002', '003', '004', '005', '006', '007', '008', '009', '010'),
                                    labels = c("Assumed alive",
                                                "Diseases of heart (I00-I09, I11, I13, I20-I51)",
                                                "Malignant neoplasms (C00-C97)",
                                                "Chronic lower respiratory diseases (J40-J47)",
                                                "Accidents (unintentional injuries) (V01-X59, Y85-Y86)",
                                                "Cerebrovascular diseases (I60-I69)",
                                                "Alzheimer's disease (G30)",
                                                "Diabetes mellitus (E10-E14)",
                                                "Influenza and pneumonia (J09-J18)",
                                                "Nephritis, nephrotic syndrome and nephrosis (N00-N07, N17-N19, N25-N27)",
                                                "All other causes (residual)"))
        nhis_all$death_type.factor <- factor(nhis_all$death_type, levels=c(0, 1, 2),
                                         labels = c("Alive","Accidental", "Other death"))
        
        
        
        # Create an 'interaction' variable, combining the SES and Health behavior variables
        # For main analyses
        nhis_all$edu.alc <- interaction(nhis_all$edu.factor, nhis_all$alcohol5v2.factor)
        nhis_all$edu.smk <- interaction(nhis_all$edu.factor, nhis_all$smoking4.factor)
        nhis_all$edu.bmi <- interaction(nhis_all$edu.factor, nhis_all$bmi_cat.factor)
        nhis_all$edu.phy <- interaction(nhis_all$edu.factor, nhis_all$phy_act3.factor)
        
        # For sensitivity analyses
        nhis_all$inc.alc <- interaction(nhis_all$income4.factor, nhis_all$alcohol5v2.factor)
        nhis_all$inc.smk <- interaction(nhis_all$income4.factor, nhis_all$smoking4.factor)
        nhis_all$inc.bmi <- interaction(nhis_all$income4.factor, nhis_all$bmi_cat.factor)
        nhis_all$inc.phy <- interaction(nhis_all$income4.factor, nhis_all$phy_act3.factor)
        
        nhis_all$edu.hed <- interaction(nhis_all$edu.factor, nhis_all$hed.factor)
        
        
       
# Create subset of data with relevant participants        
# Remove those outside our age range
nhis_age25_85 <- filter (nhis_all, age>=25 & age <85)
        
        # remove those with missing data 
        nhis <- nhis_age25_85 %>%
          filter(complete.cases(yrs_followup, allcause_death, alcohol5v2, bmi_cat, smoking4, phy_act3, edu, age, female, married, ethnicity))
           
        
        # Create database specific to males or females
        nhis_female <- filter(nhis, female==1)
        nhis_male <- filter(nhis, female==0)
        
        

  

# Save copy of final datasets  
saveRDS(nhis_all, paste0(data, "nhis_all.rds"))         # NHIS data with all participants
saveRDS(nhis, paste0(data, "nhis.rds"))                 # NHIS data to be analyzed
saveRDS(nhis_male, paste0(data, "nhis_male.rds"))       # NHIS data to be analyzed (males only)
saveRDS(nhis_female, paste0(data, "nhis_female.rds"))   # NHIS data to be analyzed (females only)

