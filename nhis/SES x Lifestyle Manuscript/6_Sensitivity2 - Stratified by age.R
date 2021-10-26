
# SES x Lifestyle Differential Vulnerability & Exposure Project
# Sensitivity Analyses 2: Stratified by age


# LOAD DATA AND SET FILE LOCATIONS

# load libraries
library(tidyverse)  # data management
library(skimr)      # descriptive statistics
library(gmodels)    # CrossTable command
library(tableone)   # create table one
library(survival)   # surivval analyses
library(survminer)  # surivval analyses
library(timereg)    # additive survival models
library(survey)     # for survey weighted cox model
library(VGAM)       # multinomial regression, needed for causal mediation
library(MASS)       # needed for causal mediation functions


# Specify the data and output file locations
data    <- "SIMAH_workspace/nhis/Data/"
output  <- "C:/Users/klajd/Documents/2021-Present CAMH/NHIS Data/Model Outputs - SES x Lifestyle manuscript/Sensitivity/"
source("Function - Format Results.R")
source("Function - CausalMed Results.R")


    
# Load data
nhis        <- readRDS (file.path(data, "nhis.rds"))
nhis_male   <- readRDS (file.path(data, "nhis_male.rds"))
nhis_female <- readRDS (file.path(data, "nhis_female.rds"))



# Aalen Models (Stratified by age) --------------------------------------------------------------------------------------------------

# FIRST: Run all models and save results  *******************************************************************************************

### ALCOHOL * Education  ************************************************************************************************************

# WOMEN AGES 25-60: Interaction model
model <- "_alc_f_25_60"   # Used to name the files appropriately: specify health behavior and sex strata
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(alcohol5v2.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female,
  start.time=25, max.time=59.999)
saveRDS(aalen, paste0(output, "aalen", model, ".rds"))  ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()

      # WOMEN AGES 25-60: Joint effect model
      model <- "_alc_f2_25_60"   # Used to name the files appropriately: specify health behavior and sex strata
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.alc) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female, 
        start.time=25, max.time=59.999)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds"))  ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  


# WOMEN AGES 60-70: Interaction model
model <- "_alc_f_60_70"
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(alcohol5v2.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female,
    start.time=60, max.time=69.999)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()
      
      # WOMEN AGES 60-70: joint effects model
      model <- "_alc_f2_60_70"   
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.alc) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female, 
        start.time=60, max.time=69.999)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  



# WOMEN AGES 70-85: Interaction model
model <- "_alc_f_70_85"
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(alcohol5v2.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female,
    start.time=70, max.time=84.999)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()

      # WOMEN AGES 70-85: joint effects model
      model <- "_alc_f2_70_85"   
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.alc) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female, 
        start.time=70, max.time=84.999)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  




# MEN AGES 25-60: Interaction model
model <- "_alc_m_25_60"   # Used to name the files appropriately: specify health behavior and sex strata
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(alcohol5v2.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male,
    start.time=25, max.time=59.999)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()

      # MEN AGES 25-60: Interaction model
      model <- "_alc_m2_25_60"   # Used to name the files appropriately: specify health behavior and sex strata
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.alc) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male, 
        start.time=25, max.time=59.999)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  



# MEN AGES 60-70: Interaction model
model <- "_alc_m_60_70"
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(alcohol5v2.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male,
    start.time=60, max.time=69.999)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()

      # MEN AGES 60-70: joint effects model
      model <- "_alc_m2_60_70"   
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.alc) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male, 
        start.time=60, max.time=69.999)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  



# MEN AGES 70-85: Interaction model
model <- "_alc_m_70_85"
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(alcohol5v2.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male,
    start.time=70, max.time=84.999)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()


      # MEN AGES 70-85: joint effects model
      model <- "_alc_m2_70_85"   
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.alc) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male, 
        start.time=70, max.time=84.999)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  





### SMOKING * Education ************************************************************************************************************

# WOMEN AGES 25-60: Interaction model
model <- "_smk_f_25_60"   # Used to name the files appropriately: specify health behavior and sex strata
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(smoking4.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female,
  start.time=25, max.time=59.999)
saveRDS(aalen, paste0(output, "aalen", model, ".rds"))  ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()
      
      # WOMEN AGES 25-60: joint effects model
      model <- "_smk_f2_25_60"   # Used to name the files appropriately: specify health behavior and sex strata
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.smk) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female, 
        start.time=25, max.time=59.999)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds"))  ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  



# WOMEN AGES 60-70: Interaction model
model <- "_smk_f_60_70"
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(smoking4.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female,
  start.time=60, max.time=69.999)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()

      # WOMEN AGES 60-70: joint effects  model
      model <- "_smk_f2_60_70"   
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.smk) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female, 
        start.time=60, max.time=69.999)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  



# WOMEN AGES 70-85: Interaction model
model <- "_smk_f_70_85"
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(smoking4.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female,
  start.time=70, max.time=84.999)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()

      # WOMEN AGES 70-85: joint effects  model
      model <- "_smk_f2_70_85"   
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.smk) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female, 
        start.time=70, max.time=84.999)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  



# MEN AGES 25-60: Interaction model
model <- "_smk_m_25_60"   # Used to name the files appropriately: specify health behavior and sex strata
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(smoking4.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male,
start.time=25, max.time=59.999)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()

      # MEN AGES 25-60: joint effects  model
      model <- "_smk_m2_25_60"   # Used to name the files appropriately: specify health behavior and sex strata
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.smk) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male, 
        start.time=25, max.time=59.999)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  



# MEN AGES 60-70: Interaction model
model <- "_smk_m_60_70"
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(smoking4.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male,
  start.time=60, max.time=69.999)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()

      # MEN AGES 60-70: joint effects  model
      model <- "_smk_m2_60_70"   
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.smk) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male, 
        start.time=60, max.time=69.999)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  



# MEN AGES 70-85: Interaction model
model <- "_smk_m_70_85"
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(smoking4.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male,
  start.time=70, max.time=84.999)
saveRDS(aalen, paste0(output, "aalen", model, ".rds"))  ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()
      
      # MEN AGES 70-85: joint effects  model
      model <- "_smk_m2_70_85"   
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.smk) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male, 
        start.time=70, max.time=84.999)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds"))  ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  



### BMI * Education ************************************************************************************************************

# WOMEN AGES 25-60: Interaction model
model <- "_bmi_f_25_60"   # Used to name the files appropriately: specify health behavior and sex strata
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(bmi_cat.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female,
  start.time=25, max.time=59.999)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()

      # WOMEN AGES 25-60: joint effects  model
      model <- "_bmi_f2_25_60"   # Used to name the files appropriately: specify health behavior and sex strata
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.bmi) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female, 
        start.time=25, max.time=59.999)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  



# WOMEN AGES 60-70: Interaction model    
model <- "_bmi_f_60_70"
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(bmi_cat.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female,
  start.time=60, max.time=69.999)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()

      # WOMEN AGES 60-70: joint effects  model    
      model <- "_bmi_f2_60_70"   
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.bmi) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female, 
        start.time=60, max.time=69.999)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  



# WOMEN AGES 70-85: Interaction model
model <- "_bmi_f_70_85"
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(bmi_cat.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female,
  start.time=70, max.time=84.999)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()
      
      # WOMEN AGES 70-85: joint effects  model
      model <- "_bmi_f2_70_85"   
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.bmi) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female, 
        start.time=70, max.time=84.999)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  





# MEN AGES 25-60: Interaction model
model <- "_bmi_m_25_60"   # Used to name the files appropriately: specify health behavior and sex strata
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(bmi_cat.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male,
start.time=25, max.time=59.999)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()

      # MEN AGES 25-60: joint effects  model
      model <- "_bmi_m2_25_60"   # Used to name the files appropriately: specify health behavior and sex strata
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.bmi) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male, 
        start.time=25, max.time=59.999)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  



# MEN AGES 60-70: Interaction model
model <- "_bmi_m_60_70"
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(bmi_cat.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male,
  start.time=60, max.time=69.999)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()

      # MEN AGES 60-70: joint effects  model
      model <- "_bmi_m2_60_70"   
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.bmi) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male, 
        start.time=60, max.time=69.999)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  



# MEN AGES 70-85: Interaction model
model <- "_bmi_m_70_85"
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(bmi_cat.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male,
  start.time=70, max.time=84.999)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()

      # MEN AGES 70-85: joint effects  model
      model <- "_bmi_m2_70_85"   
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.bmi) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male, 
        start.time=70, max.time=84.999)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  



      
### PHYSICAL ACTIVITY * Education ************************************************************************************************************

# WOMEN AGES 25-60: Interaction model
model <- "_phy_f_25_60"   # Used to name the files appropriately: specify health behavior and sex strata
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(phy_act3.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female,
  start.time=25, max.time=59.999)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()
      
      # WOMEN AGES 25-60: joint effects model
      model <- "_phy_f2_25_60"   # Used to name the files appropriately: specify health behavior and sex strata
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.phy) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female, 
        start.time=25, max.time=59.999)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  



# WOMEN AGES 60-70: Interaction model
model <- "_phy_f_60_70"
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(phy_act3.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female,
  start.time=60, max.time=69.999)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()

      # WOMEN AGES 60-70: joint effects model
      model <- "_phy_f2_60_70"   
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.phy) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female, 
        start.time=60, max.time=69.999)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  



# WOMEN AGES 70-85: Interaction model
model <- "_phy_f_70_85"
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(phy_act3.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female,
  start.time=70, max.time=84.999)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()

      # WOMEN AGES 70-85: joint effects model
      model <- "_phy_f2_70_85"   
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.phy) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female, 
        start.time=70, max.time=84.999)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  



# MEN AGES 25-60: Interaction model
model <- "_phy_m_25_60"   # Used to name the files appropriately: specify health behavior and sex strata
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(phy_act3.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male,
start.time=25, max.time=59.999)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()

      # MEN AGES 25-60: joint effects model
      model <- "_phy_m2_25_60"   # Used to name the files appropriately: specify health behavior and sex strata
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.phy) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male, 
        start.time=25, max.time=59.999)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  



# MEN AGES 60-70: Interaction model
model <- "_phy_m_60_70"
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(phy_act3.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male,
  start.time=60, max.time=69.999)
saveRDS(aalen, paste0(output, "aalen", model, ".rds"))  ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()
      
      # MEN AGES 60-70: joint effects model
      model <- "_phy_m2_60_70"   
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.phy) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male, 
        start.time=60, max.time=69.999)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds"))  ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  



# MEN AGES 70-85: Interaction model
model <- "_phy_m_70_85"
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(phy_act3.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male,
  start.time=70, max.time=84.999)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()
      
      # MEN AGES 70-85: joint effects model
      model <- "_phy_m2_70_85"   
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.phy) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male, 
        start.time=70, max.time=84.999)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  





# Second: Load and view model results  ***************************************************************************************

# Alcohol, Women
model <- "_alc_f_25_60"  ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))   # Name and load the model            
model <- "_alc_f2_25_60"  ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))
aalen_10000py(aalen, 1); aalen_10000py(aalen, 6); aalen_10000py(aalen2, 13); aalen_10000py(aalen, 31) # print results of interest

      model <- "_alc_f_60_70"  ;   aalen <-readRDS(paste0(output, "aalen", model, ".rds"))                                                      
      model <- "_alc_f2_60_70"  ;   aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))       
      aalen_10000py(aalen, 1); aalen_10000py(aalen, 6); aalen_10000py(aalen2, 13); aalen_10000py(aalen, 31)   
      
      model <- "_alc_f_70_85" ;  aalen <-readRDS(paste0(output, "aalen", model, ".rds"))        
      model <- "_alc_f2_70_85" ;  aalen2 <-readRDS(paste0(output, "aalen", model, ".rds")) 
      aalen_10000py(aalen, 1); aalen_10000py(aalen, 6); aalen_10000py(aalen2, 13); aalen_10000py(aalen, 31)   


 
# Smoking, Women
model <- "_smk_f_25_60"   ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))             
model <- "_smk_f2_25_60"   ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))                                
aalen_10000py(aalen, 1); aalen_10000py(aalen, 5); aalen_10000py(aalen2, 10); aalen_10000py(aalen, 28)   

      model <- "_smk_f_60_70" ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))    
      model <- "_smk_f2_60_70" ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))    
      aalen_10000py(aalen, 1); aalen_10000py(aalen, 5); aalen_10000py(aalen2, 10); aalen_10000py(aalen, 28)   
      
      model <- "_smk_f_70_85"  ;  aalen <-readRDS(paste0(output, "aalen", model, ".rds"))      
      model <- "_smk_f2_70_85"  ;  aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))      
      aalen_10000py(aalen, 1); aalen_10000py(aalen, 5); aalen_10000py(aalen2, 10); aalen_10000py(aalen, 28)   


# BMI, Women
model <- "_bmi_f_25_60" ;  aalen <-readRDS(paste0(output, "aalen", model, ".rds"))   
model <- "_bmi_f2_25_60" ;  aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))                                
aalen_10000py(aalen, 1); aalen_10000py(aalen, 5); aalen_10000py(aalen2, 10); aalen_10000py(aalen, 28)   

      model <- "_bmi_f_60_70" ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))  
      model <- "_bmi_f2_60_70" ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))                                
            aalen_10000py(aalen, 1); aalen_10000py(aalen, 5); aalen_10000py(aalen2, 10); aalen_10000py(aalen, 28)   
      
      model <- "_bmi_f_70_85"  ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))  
      model <- "_bmi_f2_70_85"  ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))  
      aalen_10000py(aalen, 1); aalen_10000py(aalen, 5); aalen_10000py(aalen2, 10); aalen_10000py(aalen, 28)   
      

# Physical Activity, Women
model <- "_phy_f_25_60" ;  aalen <-readRDS(paste0(output, "aalen", model, ".rds"))       
model <- "_phy_f2_25_60" ;  aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))   
aalen_10000py(aalen, 1); aalen_10000py(aalen, 3);aalen_10000py(aalen2, 4);  aalen_10000py(aalen, 23)   
      
      model <- "_phy_f_60_70"  ;  aalen <-readRDS(paste0(output, "aalen", model, ".rds"))    
      model <- "_phy_f2_60_70"  ;  aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))  
      aalen_10000py(aalen, 1); aalen_10000py(aalen, 3);aalen_10000py(aalen2, 4);  aalen_10000py(aalen, 23)   
      
      model <- "_phy_f_70_85" ;  aalen <-readRDS(paste0(output, "aalen", model, ".rds"))
      model <- "_phy_f2_70_85" ;  aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))  
      aalen_10000py(aalen, 1); aalen_10000py(aalen, 3);aalen_10000py(aalen2, 4);  aalen_10000py(aalen, 23)   






      
      
      

# Alcohol, Men
model <- "_alc_m_25_60" ;  aalen <-readRDS(paste0(output, "aalen", model, ".rds"))    
model <- "_alc_m2_25_60" ;  aalen2 <-readRDS(paste0(output, "aalen", model, ".rds")) 
aalen_10000py(aalen, 1); aalen_10000py(aalen, 6); aalen_10000py(aalen2, 13); aalen_10000py(aalen, 31)   
      
      model <- "_alc_m_60_70" ; aalen <-readRDS(paste0(output, "aalen", model, ".rds")) 
      model <- "_alc_m2_60_70" ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))  
      aalen_10000py(aalen, 1); aalen_10000py(aalen, 6); aalen_10000py(aalen2, 13); aalen_10000py(aalen, 31)   
      
      model <- "_alc_m_70_85"  ;   aalen <-readRDS(paste0(output, "aalen", model, ".rds"))   
      model <- "_alc_m2_70_85"  ;   aalen2 <-readRDS(paste0(output, "aalen", model, ".rds")) 
      aalen_10000py(aalen, 1); aalen_10000py(aalen, 6); aalen_10000py(aalen2, 13); aalen_10000py(aalen, 31)   
      
      

# Smoking, Men
model <- "_smk_m_25_60" ;  aalen <-readRDS(paste0(output, "aalen", model, ".rds"))        
model <- "_smk_m2_25_60" ;  aalen2 <-readRDS(paste0(output, "aalen", model, ".rds")) 
aalen_10000py(aalen, 1); aalen_10000py(aalen, 5); aalen_10000py(aalen2, 10); aalen_10000py(aalen, 28)   
      
      model <- "_smk_m_60_70" ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))             
      model <- "_smk_m2_60_70" ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))  
      aalen_10000py(aalen, 1); aalen_10000py(aalen, 5); aalen_10000py(aalen2, 10); aalen_10000py(aalen, 28)   
      
      model <- "_smk_m_70_85" ;   aalen <-readRDS(paste0(output, "aalen", model, ".rds"))  
      model <- "_smk_m2_70_85" ;   aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))    
      aalen_10000py(aalen, 1); aalen_10000py(aalen, 5); aalen_10000py(aalen2, 10); aalen_10000py(aalen, 28)   
      
      
# BMI, Men
model <- "_bmi_m_25_60" ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))
model <- "_bmi_m2_25_60" ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))
aalen_10000py(aalen, 1); aalen_10000py(aalen, 5); aalen_10000py(aalen2, 10); aalen_10000py(aalen, 28)

      model <- "_bmi_m_60_70" ;aalen <-readRDS(paste0(output, "aalen", model, ".rds"))
      model <- "_bmi_m2_60_70" ;aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))
      aalen_10000py(aalen, 1); aalen_10000py(aalen, 5); aalen_10000py(aalen2, 10); aalen_10000py(aalen, 28)   
      
      model <- "_bmi_m_70_85" ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))
      model <- "_bmi_m2_70_85" ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))
      aalen_10000py(aalen, 1); aalen_10000py(aalen, 5); aalen_10000py(aalen2, 10); aalen_10000py(aalen, 28)   
      
      
# Physical Activity, Men
model <- "_phy_m_25_60" ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))       
model <- "_phy_m2_25_60" ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))      
aalen_10000py(aalen, 1); aalen_10000py(aalen, 3);aalen_10000py(aalen2, 4);  aalen_10000py(aalen, 23)   

      model <- "_phy_m_60_70" ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))   
      model <- "_phy_m2_60_70" ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))   
      aalen_10000py(aalen, 1); aalen_10000py(aalen, 3);aalen_10000py(aalen2, 4);  aalen_10000py(aalen, 23)   
      
      model <- "_phy_m_70_85" ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))        
      model <- "_phy_m2_70_85" ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))  
      aalen_10000py(aalen, 1); aalen_10000py(aalen, 3);aalen_10000py(aalen2, 4);  aalen_10000py(aalen, 23)   





      
# WOMEN - Causal Mediation Analysis (Stratified by age) ----------------------------------------------------------------------------------

# Load data (used the same data as that generated from the main analysis)
expandedData <-readRDS(file.path(output, "expandedData_fem.rds"))

# AGES 25 - 59 Years - Run Causal Mediation Model
CMed_f_25_59 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(A.edu) * const(edu_M1.alc) + 
                                                              const(A.edu) * const(edu_M2.smk) +
                                                              const(A.edu) * const(edu_M3.bmi) +
                                                              const(A.edu) * const(edu_M4.phy) +
                                                              const(married) + factor(ethnicity) + const(factor(srvy_yr)),
                            start.time=25, max.time=59.999,
                            data=expandedData, weights=expandedData$weightM, clusters=expandedData$ID, robust=0)  
          saveRDS(CMed_f_25_59, file.path(output, "CMed_f_25_59.rds"))       # Save model results
          CMed_f_model_25_59 <-readRDS(file.path(output, "CMed_f_25_59.rds"))  # Load model results
          
          
          
          # Get final results. NOTE: THE NUMBERS BELOW MAY HAVE TO BE CHANGED IF A DIFFERENT MODEL IS USED
          summary(CMed_f_model_25_59)   
          getTE_NotRobust(CMed_f_model_25_59, c(1,3,5,7,9,29,33,37,41))  
          getIE_NotRobust(CMed_f_model_25_59, c(3,5,7,9,29,33,37,41))    
          getTE_IE_NotRobust(CMed_f_model_25_59, c(1,3,5,7,9,29,33,37,41), c(3,5,7,9,29,33,37,41)) 



# AGES 60 - 69 Years - Run Causal Mediation Model
CMed_f_60_69 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(A.edu) * const(edu_M1.alc) + 
                                                              const(A.edu) * const(edu_M2.smk) +
                                                              const(A.edu) * const(edu_M3.bmi) +
                                                              const(A.edu) * const(edu_M4.phy) +
                                                              const(married) + factor(ethnicity) + const(factor(srvy_yr)),
                        start.time=60, max.time=69.999,
                        data=expandedData, weights=expandedData$weightM, clusters=expandedData$ID, robust=0)  
          saveRDS(CMed_f_60_69, file.path(output, "CMed_f_60_69.rds"))       # Save model results
          CMed_f_model_60_69 <-readRDS(file.path(output, "CMed_f_60_69.rds"))  # Load model results
          
          
          # Get final results. NOTE: THE NUMBERS BELOW MAY HAVE TO BE CHANGED IF A DIFFERENT MODEL IS USED
          summary(CMed_f_model_25_59)   
          getTE_NotRobust(CMed_f_model_25_59, c(1,3,5,7,9,29,33,37,41))  
          getIE_NotRobust(CMed_f_model_25_59, c(3,5,7,9,29,33,37,41))    
          getTE_IE_NotRobust(CMed_f_model_25_59, c(1,3,5,7,9,29,33,37,41), c(3,5,7,9,29,33,37,41)) 




# AGES 70 - 85 Years - Run Causal Mediation Model
CMed_f_70_85 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(A.edu) * const(edu_M1.alc) + 
                                                              const(A.edu) * const(edu_M2.smk) +
                                                              const(A.edu) * const(edu_M3.bmi) +
                                                              const(A.edu) * const(edu_M4.phy) +
                                                              const(married) + factor(ethnicity) + const(factor(srvy_yr)),
                        start.time=70, max.time=84.999,
                        data=expandedData, weights=expandedData$weightM, clusters=expandedData$ID, robust=0)  
          saveRDS(CMed_f_70_85, file.path(output, "CMed_f_70_85.rds"))       # Save model results
          CMed_f_model_70_85 <-readRDS(file.path(output, "CMed_f_70_85.rds"))  # Load model results
          
          
          
          # Get final results. NOTE: THE NUMBERS BELOW MAY HAVE TO BE CHANGED IF A DIFFERENT MODEL IS USED
          summary(CMed_f_model_25_59)   
          getTE_NotRobust(CMed_f_model_25_59, c(1,3,5,7,9,29,33,37,41))  
          getIE_NotRobust(CMed_f_model_25_59, c(3,5,7,9,29,33,37,41))    
          getTE_IE_NotRobust(CMed_f_model_25_59, c(1,3,5,7,9,29,33,37,41), c(3,5,7,9,29,33,37,41)) 






# MEN - Causal Mediation Analysis (Stratified by age) ----------------------------------------------------------------------------------

# Load data (used the same data as that generated from the main analysis)
expandedData <-readRDS(file.path(output, "expandedData_male.rds"))

          
          
# AGES 25 - 59 Years - Run Causal Mediation Model   
CMed_m_25_59 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(A.edu) * const(edu_M1.alc) + 
                                                              const(A.edu) * const(edu_M2.smk) +
                                                              const(A.edu) * const(edu_M3.bmi) +
                                                              const(A.edu) * const(edu_M4.phy) +
                                                              const(married) + factor(ethnicity) + const(factor(srvy_yr)),
                        start.time=25, max.time=59.999,
                        data=expandedData, weights=expandedData$weightM, clusters=expandedData$ID, robust=0)  
          saveRDS(CMed_m_25_59, file.path(output, "CMed_m_25_59.rds"))       # Save model results
          CMed_m_model_25_59 <-readRDS(file.path(output, "CMed_m_25_59.rds"))  # Load model results
          
          
          
          # Get final results. NOTE: THE NUMBERS BELOW MAY HAVE TO BE CHANGED IF A DIFFERENT MODEL IS USED
          summary(CMed_m_model_25_59)  
          getTE_NotRobust(CMed_m_model_25_59, c(1,3,5,7,9,29,33,37,41))  
          getIE_NotRobust(CMed_m_model_25_59, c(3,5,7,9,29,33,37,41))    
          getTE_IE_NotRobust(CMed_m_model_25_59, c(1,3,5,7,9,29,33,37,41), c(3,5,7,9,29,33,37,41)) 



          

# AGES 60 - 69 Years - Run Causal Mediation Model
CMed_m_60_69 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(A.edu) * const(edu_M1.alc) + 
                                                              const(A.edu) * const(edu_M2.smk) +
                                                              const(A.edu) * const(edu_M3.bmi) +
                                                              const(A.edu) * const(edu_M4.phy) +
                                                              const(married) + factor(ethnicity) + const(factor(srvy_yr)),
                          start.time=60, max.time=69.999,
                          data=expandedData, weights=expandedData$weightM, clusters=expandedData$ID, robust=0)  
          saveRDS(CMed_m_60_69, file.path(output, "CMed_m_60_69.rds"))       # Save model results
          CMed_m_model_60_69 <-readRDS(file.path(output, "CMed_m_60_69.rds"))  # Load model results
          
          
          # Get final results. NOTE: THE NUMBERS BELOW MAY HAVE TO BE CHANGED IF A DIFFERENT MODEL IS USED
          summary(CMed_m_model_25_59)  
          getTE_NotRobust(CMed_m_model_25_59, c(1,3,5,7,9,29,33,37,41))  
          getIE_NotRobust(CMed_m_model_25_59, c(3,5,7,9,29,33,37,41))    
          getTE_IE_NotRobust(CMed_m_model_25_59, c(1,3,5,7,9,29,33,37,41), c(3,5,7,9,29,33,37,41)) 





# AGES 70 - 85 Years - Run Causal Mediation Model
CMed_m_70_85 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(A.edu) * const(edu_M1.alc) + 
                                                              const(A.edu) * const(edu_M2.smk) +
                                                              const(A.edu) * const(edu_M3.bmi) +
                                                              const(A.edu) * const(edu_M4.phy) +
                                                              const(married) + factor(ethnicity) + const(factor(srvy_yr)),
                        start.time=70, max.time=84.999,
                        data=expandedData, weights=expandedData$weightM, clusters=expandedData$ID, robust=0)  
          saveRDS(CMed_m_70_85, file.path(output, "CMed_m_70_85.rds"))       # Save model results
          CMed_m_model_70_85 <-readRDS(file.path(output, "CMed_m_70_85.rds"))  # Load model results
          
          
          
          # Get final results. NOTE: THE NUMBERS BELOW MAY HAVE TO BE CHANGED IF A DIFFERENT MODEL IS USED
          summary(CMed_m_model_25_59)  
          getTE_NotRobust(CMed_m_model_25_59, c(1,3,5,7,9,29,33,37,41))  
          getIE_NotRobust(CMed_m_model_25_59, c(3,5,7,9,29,33,37,41))    
          getTE_IE_NotRobust(CMed_m_model_25_59, c(1,3,5,7,9,29,33,37,41), c(3,5,7,9,29,33,37,41)) 




