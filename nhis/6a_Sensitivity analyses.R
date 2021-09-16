

### SIMAH - NHIS Data
### SES x Health Behavior interaction and mediation


# LOAD DATA AND SET FILE LOCATIONS -----------------------------------------------------------------------------

# load libraries
library(tidyverse)  # data management
library(skimr)      # descriptive statistics
library(gmodels)    # CrossTable command
library(tableone)   # create table one
library(survival)   # surivval analyses
library(survminer)  # surivval analyses
library(timereg)    # additive survival models
library(survey)     # for survey weighted cox model



# Set the working directory and other file locations

# # Personal Computer:
kp <- "C:/Users/klajd/OneDrive/SIMAH"
setwd(kp)
data    <- "SIMAH_workspace/nhis/Data/"
output  <- "SIMAH_workspace/nhis/SES x Behavior/Output/Sensitivity/"
source("SIMAH_code/nhis/0_Function_Formatted_results.R")


    
# Load data
nhis        <- readRDS (file.path(data, "nhis.rds"))
nhis_male   <- readRDS (file.path(data, "nhis_male.rds"))
nhis_female <- readRDS (file.path(data, "nhis_female.rds"))
nhis_svyWeights <- readRDS (file.path(data, "nhis_svyWeights.rds"))
nhis_svyWeights_female <- readRDS (file.path(data, "nhis_svyWeights_female.rds"))
nhis_svyWeights_male <- readRDS (file.path(data, "nhis_svyWeights_male.rds"))



# Sensitivity 1: Aalen Models, Income as SES measure ------------------------------------------------------------------------------------------------------------------------------

# The effect estimates from the model can be directly interpreted as the number of additional events (deaths) per 1 person year at risk
  # Two different versions of the model were ran identify the interaction effect (model with the interaction term) and the joint effect (model with interacting variable) 
  # A "aalen_10000py" function was created to extract the coefficients and multiple them by 10,000 to get estimes per 10,000 person years
  # The results from each model pertain to the effect of "Low SES & healthy behavior", "Bad behavior & high SES", "Low SES & Bad behavior", "Interaction effect"


# FIRST: Run all models and save results  ***************************************************************************************

### Income * Alcohol ************************************************************************************************************

# WOMEN: Interaction model
model <- "_inc_alc_f"   # Used to name the files appropriately: specify health behavior and sex strata
    aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(income.factor)*const(alcohol5v2.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
    saveRDS(aalen, paste0(output, "aalen", model, ".rds"))  ;  pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  
   
      # WOMEN: Joint effect model
      model <- "_inc_alc_f2"   # Used to name the file
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(inc.alc) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds"))  ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  
    
    
    
# MEN: Interaction model
model <- "_inc_alc_m"   # Used to name the file
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(income.factor)*const(alcohol5v2.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()    

      # MEN: Joint effect model
      model <- "_inc_alc_m2"   # Used to name the file
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(inc.alc) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds"))  ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()    


    
    
### Smoking * Income *********************************************************************************************************
    
# WOMEN: Interaction model
model <- "_inc_smk_f"   # Used to name the file
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(income.factor)*const(smoking4.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  

      # WOMEN: Joint effect model
      model <- "_inc_smk_f2"   # Used to name the file
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(inc.smk) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ;  pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  


      
# MEN: Interaction model
model <- "_inc_smk_m"   # Used to name the file
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(income.factor)*const(smoking4.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()    

      # MEN: Joint effect model
      model <- "_inc_smk_m2"   # Used to name the file
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(inc.smk) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds"))  ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()    




    
### Income * BMI *********************************************************************************************************
    
# WOMEN: Interaction model
model <- "_inc_bmi_f"   # Used to name the file
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(income.factor)*const(bmi_cat.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ;  pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  
    
      # WOMEN: Joint effect model
      model <- "_inc_bmi_f2"   # Used to name the file
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(inc.bmi) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds"))  ;  pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  
    
     
    
# MEN: Interaction model
model <- "_inc_bmi_m"   # Used to name the file
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(income.factor)*const(bmi_cat.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()    
    
      # MEN: Joint effect model
      model <- "_inc_bmi_m2"   # Used to name the file
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(inc.bmi) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds"))  ;  pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()    
    
    
      
      
### Income * Physical Activity *********************************************************************************************************
    
# WOMEN: Interaction model
model <- "_inc_phy_f"   # Used to name the file
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(income.factor)*const(phy_act3.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  
    
    # WOMEN: Joint effect model
    model <- "_inc_phy_f2"   # Used to name the file
    aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(inc.phy) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
    saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ;  pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  
    
    
    
# MEN: Interaction model
model <- "_inc_phy_m"   # Used to name the file
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(income.factor)*const(phy_act3.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()    
    
    # MEN: Joint effect model
    model <- "_inc_phy_m2"   # Used to name the file
    aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(inc.phy) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
    saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()    
    
    

# Second: Load and view model results  ***************************************************************************************


### Income * Alcohol 
model <- "_inc_alc_f" ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))     # Name and load the interaction model 
model <- "_inc_alc_f2" ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))   # Name and load the joint effect model  
aalen_10000py(aalen, 2); aalen_10000py(aalen, 8); aalen_10000py(aalen2, 21); aalen_10000py(aalen, 40)          # print results of interest

model <- "_inc_alc_m"  ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))        
model <- "_inc_alc_m2"  ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))        
aalen_10000py(aalen, 2); aalen_10000py(aalen, 8); aalen_10000py(aalen2, 21); aalen_10000py(aalen, 40)



#### Income * Smoking 
model <- "_inc_smk_f" ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))           
model <- "_inc_smk_f2" ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))           
aalen_10000py(aalen, 2); aalen_10000py(aalen, 7); aalen_10000py(aalen2, 16); aalen_10000py(aalen, 35)

model <- "_inc_smk_m" ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))        
model <- "_inc_smk_m2" ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))        
aalen_10000py(aalen, 2); aalen_10000py(aalen, 7); aalen_10000py(aalen2, 16); aalen_10000py(aalen, 35)


#### Income * BMI 
model <- "_inc_bmi_f" ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))           
model <- "_inc_bmi_f2" ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))           
aalen_10000py(aalen, 2); aalen_10000py(aalen, 7); aalen_10000py(aalen2, 16); aalen_10000py(aalen, 35)

model <- "_inc_bmi_m"  ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))        
model <- "_inc_bmi_m2"  ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))        
aalen_10000py(aalen, 2); aalen_10000py(aalen, 7); aalen_10000py(aalen2, 16); aalen_10000py(aalen, 35)



#### Income * Physical Activity 
model <- "_inc_phy_f" ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))           
model <- "_inc_phy_f2" ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))           
aalen_10000py(aalen, 2); aalen_10000py(aalen, 5); aalen_10000py(aalen2, 6); aalen_10000py(aalen, 26)

model <- "_inc_phy_m"  ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))        
model <- "_inc_phy_m2"  ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))        
aalen_10000py(aalen, 2); aalen_10000py(aalen, 5); aalen_10000py(aalen2, 6); aalen_10000py(aalen, 26)







# Sensitivity 2: Aalen Models, HED as alcohol measure -------------------------------

# The effect estimates from the model can be directly interpreted as the number of additional events (deaths) per 1 person year at risk
# Two different versions of the model were ran identify the interaction effect (model with the interaction term) and the joint effect (model with interacting variable) 
# A "aalen_10000py" function was created to extract the coefficients and multiple them by 10,000 to get estimes per 10,000 person years
# The results from each model pertain to the effect of "Low SES & healthy behavior", "Bad behavior & high SES", "Low SES & Bad behavior", "Interaction effect"


# FIRST: Run all models and save results

# WOMEN: Interaction model
model <- "_hed_f"   # Used to name the files appropriately: specify health behavior and sex strata
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(hed.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
saveRDS(aalen, paste0(output, "aalen", model, ".rds"))  ;  pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  

      # WOMEN: Joint effect model
      model <- "_hed_f2"   # Used to name the file
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.hed) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  


      
# MEN: Interaction model
model <- "_hed_m"   # Used to name the files appropriately: specify health behavior and sex strata
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(hed.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ;  pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()    

      # MEN: Joint effect model
      model <- "_hed_m2"   # Used to name the file
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.hed) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()    




# SECOND: load the models and view the results
model <- "_hed_f" ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))     # Name and load the interaction model 
model <- "_hed_f2" ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))   # Name and load the joint effect model
aalen_10000py(aalen, 1); aalen_10000py(aalen, 5); aalen_10000py(aalen, 28)      # Print results of interest


model <- "_hed_m"  ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))        
model <- "_hed_m2"  ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))  
aalen_10000py(aalen, 1); aalen_10000py(aalen, 5); aalen_10000py(aalen, 28)
      
      







# Sensitivity 3: Stratified by age ------------------------------------------------------------------------------------------------------------------------------


# FIRST: Run all models and save results  ***************************************************************************************

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





    
    
# Sensitivity 4: Aalen Models among entire sample---------------------------------------------------------------------------------------------------

# The effect estimates from the model can be directly interpreted as the number of additional events (deaths) per 1 person year at risk
# Two different versions of the model were ran identify the interaction effect (model with the interaction term) and the joint effect (model with interacting variable) 
# A "aalen_10000py" function was created to extract the coefficients and multiple them by 10,000 to get estimes per 10,000 person years
# The results from each model pertain to the effect of "Low SES & healthy behavior", "Bad behavior & high SES", "Low SES & Bad behavior", "Interaction effect"


# FIRST: Run all models and save results  ***************************************************************************************

## Alcohol x Education : Interaction model
model <- "_alc_all"   # Used to name the files appropriately: specify health behavior and sex strata
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(alcohol5v2.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)) + female.factor,  data = nhis)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()

      ## Alcohol x Education: Joint effect model
      model <- "_alc_all2"   # Used to name the file
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.alc) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)) + female.factor,  data = nhis)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  

      

## Smoking x Education : Interaction model
model <- "_smk_all"   # Used to name the file
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(smoking4.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)) + female.factor,  data = nhis)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()

      ## Smoking x Education : Joint effect model
      model <- "_smk_all2"   # Used to name the file
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.smk) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)) + female.factor,  data = nhis)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  

    
      
      
# BMI x Education : Interaction model
model <- "_bmi_all"   # Used to name the file
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(bmi_cat.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)) + female.factor,  data = nhis)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()

      # BMI x Education : Joint effect model
      model <- "_bmi_all2"   # Used to name the file
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.bmi) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)) + female.factor,  data = nhis)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  

    
      
      
# Physical Activity x Education : Interaction model
model <- "_phy_all"   # Used to name the file
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(phy_act3.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)) + female.factor,  data = nhis)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()

      # Physical Activity x Education : Joint effect model
      model <- "_phy_all2"   # Used to name the file
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.phy) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)) + female.factor,  data = nhis)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  

        


## SECOND: Load and view results ***************************************************************************************    

## Alcohol x Education: All participants
model <- "_alc_all" ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))  
model <- "_alc_all2" ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))  
aalen_10000py(aalen, 1); aalen_10000py(aalen, 6); aalen_10000py(aalen2, 13); aalen_10000py(aalen, 31)


## Smoking x Education: All participants 
model <- "_smk_all" ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))   
model <- "_smk_all2" ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))   
aalen_10000py(aalen, 1); aalen_10000py(aalen, 5); aalen_10000py(aalen2, 10); aalen_10000py(aalen, 28)


# BMI x Education: All participants
model <- "_bmi_all" ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))   
model <- "_bmi_all2" ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))             
aalen_10000py(aalen, 1); aalen_10000py(aalen, 5); aalen_10000py(aalen2, 10); aalen_10000py(aalen, 28)


# Physical Activity x Education: All participants
model <- "_phy_all" ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))   
model <- "_phy_all2" ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))             
aalen_10000py(aalen, 1); aalen_10000py(aalen, 3); aalen_10000py(aalen2, 4); aalen_10000py(aalen, 23)

    
    
    
    


# Sensitivity 5: Multiplicative Hazard Ratios---------------------------------------------------------------------------------------------------

# Alcohol * Education ***********************************************************************************************************************************************************

# WOMEN: No survey weights - interaction model and joint effect model
cox <- coxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*alcohol5v2.factor + married.factor + ethnicity.factor + factor(srvy_yr), data=nhis_female)
cox2 <- coxph(Surv(bl_age, end_age, allcause_death) ~ edu.alc + married.factor + ethnicity.factor + factor(srvy_yr), data=nhis_female)
cox_HR(cox, 1) ; cox_HR(cox, 6) ; cox_HR(cox2, 13); cox_HR(cox, 34)
    
          # Assessing proportional hazards
          cox_check <- cox.zph(cox)
          print(cox_check)
          plot(cox_check, col = "red")
          # ggcoxzph(cox_check) # is slower to print

# WOMEN: With survey weights 
cox_wt <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*alcohol5v2.factor + married.factor + ethnicity.factor + factor(srvy_yr), design=nhis_svyWeights_female)
cox_wt2 <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ edu.alc + married.factor + ethnicity.factor + factor(srvy_yr), design=nhis_svyWeights_female)
cox_HR(cox_wt, 1) ; cox_HR(cox_wt, 6) ; cox_HR(cox_wt2, 13); cox_HR(cox_wt, 34)

          # Assessing proportional hazards
          cox_wt_check <- cox.zph(cox_wt)
          print(cox_wt_check)
          plot(cox_wt_check, col = "red")
          # ggcoxzph(cox_wt_check) # is slower to print


          
# MEN: No survey weights - interaction model and joint effect model
cox <- coxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*alcohol5v2.factor + married.factor + ethnicity.factor + factor(srvy_yr), data=nhis_male)
cox2 <- coxph(Surv(bl_age, end_age, allcause_death) ~ edu.alc + married.factor + ethnicity.factor + factor(srvy_yr), data=nhis_male)
cox_HR(cox, 1) ; cox_HR(cox, 6) ; cox_HR(cox2, 13); cox_HR(cox, 34)
          
          # Assessing proportional hazards
          cox_check <- cox.zph(cox)
          print(cox_check)
          plot(cox_check, col = "red")
          # ggcoxzph(cox_check) # is slower to print
          
# MEN: With survey weights 
cox_wt <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*alcohol5v2.factor + married.factor + ethnicity.factor + factor(srvy_yr), design=nhis_svyWeights_male)
cox_wt2 <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ edu.alc + married.factor + ethnicity.factor + factor(srvy_yr), design=nhis_svyWeights_male)
cox_HR(cox_wt, 1) ; cox_HR(cox_wt, 6) ; cox_HR(cox_wt2, 13); cox_HR(cox_wt, 34)
          
          # Assessing proportional hazards
          cox_wt_check <- cox.zph(cox_wt)
          print(cox_wt_check)
          plot(cox_wt_check, col = "red")
          # ggcoxzph(cox_wt_check) # is slower to print
          
          
          

          
# Smoking * Education ***********************************************************************************************************************************************************
# WOMEN: No survey weights - interaction model and joint effect model
cox <- coxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*smoking4.factor + married.factor + ethnicity.factor + factor(srvy_yr), data=nhis_female)
cox2 <- coxph(Surv(bl_age, end_age, allcause_death) ~ edu.smk + married.factor + ethnicity.factor + factor(srvy_yr), data=nhis_female)
cox_HR(cox, 1) ; cox_HR(cox, 5) ; cox_HR(cox2, 10); cox_HR(cox, 31)
         
          # Assessing proportional hazards
          cox_check <- cox.zph(cox)
          print(cox_check)
          plot(cox_check, col = "red")
          # ggcoxzph(cox_check) # is slower to print
          
# WOMEN: With survey weights 
cox_wt <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*smoking4.factor + married.factor + ethnicity.factor + factor(srvy_yr), design=nhis_svyWeights_female)
cox_wt2 <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ edu.smk + married.factor + ethnicity.factor + factor(srvy_yr), design=nhis_svyWeights_female)
cox_HR(cox_wt, 1) ; cox_HR(cox_wt, 5) ; cox_HR(cox_wt2, 10); cox_HR(cox_wt, 31)
          
          # Assessing proportional hazards
          cox_wt_check <- cox.zph(cox_wt)
          print(cox_wt_check)
          plot(cox_wt_check, col = "red")
          # ggcoxzph(cox_wt_check) # is slower to print
          
          
        
# MEN: No survey weights - interaction model and joint effect model
cox <- coxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*smoking4.factor + married.factor + ethnicity.factor + factor(srvy_yr), data=nhis_male)
cox2 <- coxph(Surv(bl_age, end_age, allcause_death) ~ edu.smk + married.factor + ethnicity.factor + factor(srvy_yr), data=nhis_male)
cox_HR(cox, 1) ; cox_HR(cox, 5) ; cox_HR(cox2, 10); cox_HR(cox, 31)
          
          # Assessing proportional hazards
          cox_check <- cox.zph(cox)
          print(cox_check)
          plot(cox_check, col = "red")
          # ggcoxzph(cox_check) # is slower to print
          
# MEN: With survey weights 
cox_wt <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*smoking4.factor + married.factor + ethnicity.factor + factor(srvy_yr), design=nhis_svyWeights_male)
cox_wt2 <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ edu.smk + married.factor + ethnicity.factor + factor(srvy_yr), design=nhis_svyWeights_male)
cox_HR(cox_wt, 1) ; cox_HR(cox_wt, 5) ; cox_HR(cox_wt2, 10); cox_HR(cox_wt, 31)
          
          # Assessing proportional hazards
          cox_wt_check <- cox.zph(cox_wt)
          print(cox_wt_check)
          plot(cox_wt_check, col = "red")
          # ggcoxzph(cox_wt_check) # is slower to print
          
          
          
        

          
          
# BMI * Education ***********************************************************************************************************************************************************
# WOMEN: No survey weights - interaction model and joint effect model
cox <- coxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*bmi_cat.factor + married.factor + ethnicity.factor + factor(srvy_yr), data=nhis_female)
cox2 <- coxph(Surv(bl_age, end_age, allcause_death) ~ edu.bmi + married.factor + ethnicity.factor + factor(srvy_yr), data=nhis_female)
cox_HR(cox, 1) ; cox_HR(cox, 5) ; cox_HR(cox2, 10); cox_HR(cox, 31)

          # Assessing proportional hazards
          cox_check <- cox.zph(cox)
          print(cox_check)
          plot(cox_check, col = "red")
          # ggcoxzph(cox_check) # is slower to print
          
# WOMEN: With survey weights 
cox_wt <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*bmi_cat.factor + married.factor + ethnicity.factor + factor(srvy_yr), design=nhis_svyWeights_female)
cox_wt2 <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ edu.bmi + married.factor + ethnicity.factor + factor(srvy_yr), design=nhis_svyWeights_female)
cox_HR(cox_wt, 1) ; cox_HR(cox_wt, 5) ; cox_HR(cox_wt2, 10); cox_HR(cox_wt, 31)
          
          # Assessing proportional hazards
          cox_wt_check <- cox.zph(cox_wt)
          print(cox_wt_check)
          plot(cox_wt_check, col = "red")
          # ggcoxzph(cox_wt_check) # is slower to print
          
          
          
# MEN: No survey weights - interaction model and joint effect model
cox <- coxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*bmi_cat.factor + married.factor + ethnicity.factor + factor(srvy_yr), data=nhis_male)
cox2 <- coxph(Surv(bl_age, end_age, allcause_death) ~ edu.bmi + married.factor + ethnicity.factor + factor(srvy_yr), data=nhis_male)
cox_HR(cox, 1) ; cox_HR(cox, 5) ; cox_HR(cox2, 10); cox_HR(cox, 31)
          
          # Assessing proportional hazards
          cox_check <- cox.zph(cox)
          print(cox_check)
          plot(cox_check, col = "red")
          # ggcoxzph(cox_check) # is slower to print
          
# MEN: With survey weights 
cox_wt <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*bmi_cat.factor + married.factor + ethnicity.factor + factor(srvy_yr), design=nhis_svyWeights_male)
cox_wt2 <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ edu.bmi + married.factor + ethnicity.factor + factor(srvy_yr), design=nhis_svyWeights_male)
cox_HR(cox_wt, 1) ; cox_HR(cox_wt, 5) ; cox_HR(cox_wt2, 10); cox_HR(cox_wt, 31)
          
          # Assessing proportional hazards
          cox_wt_check <- cox.zph(cox_wt)
          print(cox_wt_check)
          plot(cox_wt_check, col = "red")
          # ggcoxzph(cox_wt_check) # is slower to print
          
          
          
          
          
          
# Physical activity * Education ****************************************************************************************************************************************************
# WOMEN: No survey weights - interaction model and joint effect model
cox <- coxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*phy_act3.factor + married.factor + ethnicity.factor + factor(srvy_yr), data=nhis_female)
cox2 <- coxph(Surv(bl_age, end_age, allcause_death) ~ edu.phy + married.factor + ethnicity.factor + factor(srvy_yr), data=nhis_female)
cox_HR(cox, 1) ; cox_HR(cox, 3) ; cox_HR(cox2, 4); cox_HR(cox, 26)

          # Assessing proportional hazards
          cox_check <- cox.zph(cox)
          print(cox_check)
          plot(cox_check, col = "red")
          # ggcoxzph(cox_check) # is slower to print
          
# WOMEN: With survey weights 
cox_wt <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*phy_act3.factor + married.factor + ethnicity.factor + factor(srvy_yr), design=nhis_svyWeights_female)
cox_wt2 <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ edu.phy + married.factor + ethnicity.factor + factor(srvy_yr), design=nhis_svyWeights_female)
cox_HR(cox_wt, 1) ; cox_HR(cox_wt, 3) ; cox_HR(cox_wt2, 4); cox_HR(cox_wt, 26)
          
          # Assessing proportional hazards
          cox_wt_check <- cox.zph(cox_wt)
          print(cox_wt_check)
          plot(cox_wt_check, col = "red")
          # ggcoxzph(cox_wt_check) # is slower to print
          
          
          
# MEN: No survey weights - interaction model and joint effect model
cox <- coxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*phy_act3.factor + married.factor + ethnicity.factor + factor(srvy_yr), data=nhis_male)
cox2 <- coxph(Surv(bl_age, end_age, allcause_death) ~ edu.phy + married.factor + ethnicity.factor + factor(srvy_yr), data=nhis_male)
cox_HR(cox, 1) ; cox_HR(cox, 3) ; cox_HR(cox2, 4); cox_HR(cox, 26)
          
          # Assessing proportional hazards
          cox_check <- cox.zph(cox)
          print(cox_check)
          plot(cox_check, col = "red")
          # ggcoxzph(cox_check) # is slower to print
          
# MEN: With survey weights 
cox_wt <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*phy_act3.factor + married.factor + ethnicity.factor + factor(srvy_yr), design=nhis_svyWeights_male)
cox_wt2 <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ edu.phy + married.factor + ethnicity.factor + factor(srvy_yr), design=nhis_svyWeights_male)
cox_HR(cox_wt, 1) ; cox_HR(cox_wt, 3) ; cox_HR(cox_wt2, 4); cox_HR(cox_wt, 26)
          
          # Assessing proportional hazards
          cox_wt_check <- cox.zph(cox_wt)
          print(cox_wt_check)
          plot(cox_wt_check, col = "red")
          # ggcoxzph(cox_wt_check) # is slower to print
          
          

