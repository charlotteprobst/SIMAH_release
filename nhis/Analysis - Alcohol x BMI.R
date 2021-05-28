
### SIMAH - NHIS Data
### Alcohol x BMI Interaction 

library(tidyverse)  # data management
library(skimr)      # descriptive statistics
library(gmodels)    # CrossTable command
library(tableone)   # create table one
library(survival)   # surivval analyses
library(survminer)  # surivval analyses
library(timereg)    # additive survival models
library(biostat3)   # survRate command
memory.limit(size=1e+13)


# Set the working directory and load data
kp <- "C:/Users/klajd/OneDrive/SIMAH"
setwd(kp)

nhis <-readRDS ("SIMAH_workspace/nhis/Data/nhis.rds")
nhis_male <- readRDS ("SIMAH_workspace/nhis/Data/nhis_male.rds")
nhis_female <- readRDS("SIMAH_workspace/nhis/Data/nhis_female.rds")



### Sensitivity 5: 
          
## DESCRIPTIVES ----------------------------------------------------------------------
table1_alc_bmi <-CreateTableOne(vars= c("allcause_death", "heart_death", "neoplasm_death", "cvd_death", "diabetes_death"),
                  factorVars = c("allcause_death", "heart_death", "neoplasm_death", "cvd_death", "diabetes_death"), 
                  strata= c("alcohol5v2.factor","bmi_cat.factor"), addOverall = TRUE, data=nhis)
    table1_alc_bmi <- print(table1_alc_bmi, noSpaces = TRUE, catDigits = 0, contDigits = 1, printToggle = FALSE, test=FALSE)
    write.csv(table1_alc_bmi, file="SIMAH_workspace/nhis/Alcohol x BMI/Output/Table1 Alcohol x BMI.csv")
                

## Additive Hazard Models  ----------------------------------------------------------------------

# Change the death type and re-run analyses; causes of death:
# allcause_death 
# heart_death 
# neoplasm_death
# cvd_death
# diabetes_death
                     
# Model 1:Adjusted for age (as timescale), education, ethnicity/race, and marital status. 

                
        ## All Participants
        bmi_alc_m1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol5v2.factor) + 
                          const(edu.factor) + const(married.factor) + female.factor + ethnicity.factor,  data = nhis, robust=0)
                summary(bmi_alc_m1)
                
                
                # Joint effects
                nhis$interact <- interaction(nhis$bmi_cat.factor, nhis$alcohol5v2.factor)
                bmi_alc_m1b <- aalen(Surv(bl_age, end_age, diabetes_death) ~ const(interact) + 
                    const(edu.factor) + const(married.factor) + female.factor + ethnicity.factor,  data = nhis, robust=0)
                    summary(bmi_alc_m1b)
              
                
        ## MEN
        bmi_alc_m1_m <- aalen(Surv(bl_age, end_age, diabetes_death) ~ const(bmi_cat.factor)*const(alcohol5v2.factor) + 
                    const(edu.factor) + const(married.factor) + ethnicity.factor,  data = nhis_male, robust=0)
                summary(bmi_alc_m1_m)
                
                
        ## WOMEN
        bmi_alc_m1_f <- aalen(Surv(bl_age, end_age, diabetes_death) ~ const(bmi_cat.factor)*const(alcohol5v2.factor) + 
                    const(edu.factor) + const(married.factor) + ethnicity.factor,  data = nhis_female, robust=0)
                summary(bmi_alc_m1_f)            
                
                
                
                ggsurvplot_facet(fit = survfit(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor, data = nhis), 
                  data=nhis, facet.by="alcohol5v2.factor", censor = FALSE,xlim = c(25, 100), 
                  conf.int = TRUE, 
                  xlab = "Age (years)", 
                  ylab = "Overall survival probability") 
                
                
                ggsurvplot_facet(fit = survfit(Surv(bl_age, end_age, allcause_death) ~ alcohol5v2.factor, data = nhis), 
                  data=nhis, facet.by="bmi_cat.factor", censor = FALSE,xlim = c(25, 100), 
                  conf.int = TRUE, 
                  xlab = "Age (years)", 
                  ylab = "Overall survival probability") 
                

                
                
# Change the death type and re-run analyses; causes of death:
# allcause_death 
# heart_death 
# neoplasm_death
# cvd_death
# diabetes_death
                
                
# Model 2:Adjusted for age (as timescale), education, and marital status. 
        ## All Participants
        bmi_alc_m2 <- aalen(Surv(bl_age, end_age, diabetes_death) ~ const(bmi_cat.factor)*const(alcohol5v2.factor) + 
                    const(married.factor) + const(edu.factor) + female.factor,  data = nhis, robust=0)
                summary(bmi_alc_m2)
                
                # Joint effects
                nhis$interact <- interaction(nhis$bmi_cat.factor, nhis$alcohol5v2.factor)
                bmi_alc_m2b <- aalen(Surv(bl_age, end_age, cvd_death) ~ const(interact) + 
                    const(married.factor) + const(edu.factor) + female.factor,  data = nhis, robust=0)
                summary(bmi_alc_m2b)
        
                
        ## MEN
        bmi_alc_m2_m <- aalen(Surv(bl_age, end_age, diabetes_death) ~ const(bmi_cat.factor)*const(alcohol5v2.factor) + 
                    const(edu.factor) + const(married.factor),  data = nhis_male, robust=0)
                summary(bmi_alc_m2_m)   
                
                
                
        ## WOMEN
        bmi_alc_m2_f <- aalen(Surv(bl_age, end_age, diabetes_death) ~ const(bmi_cat.factor)*const(alcohol5v2.factor) + 
                    const(edu.factor) + const(married.factor),  data = nhis_female, robust=0)
                summary(bmi_alc_m2_f)
                
       
                
     
                
                           
                
# Change the death type and re-run analyses; causes of death:
# allcause_death 
# heart_death 
# neoplasm_death
# cvd_death
# diabetes_death            

             
# Model 3: Stratified: Non-Hispanic White Only; Adjusted for age (as timescale), education, and marital status. 
       nhis_white <- filter(nhis, ethnicity.factor=="Non-Hispanic White")
       nhis_female_white <- filter(nhis_female, ethnicity.factor=="Non-Hispanic White")
       nhis_male_white <- filter(nhis_male, ethnicity.factor=="Non-Hispanic White")
                
       
        ## All Participants
        bmi_alc_m3 <- aalen(Surv(bl_age, end_age, diabetes_death) ~ const(bmi_cat.factor)*const(alcohol5v2.factor) + 
                    const(edu.factor) + const(married.factor) + female.factor,  data = nhis_white, robust=0)
                summary(bmi_alc_m3)
                
                
                # Joint effects
                nhis$interact <- interaction(nhis$bmi_cat.factor, nhis$alcohol5v2.factor)
                bmi_alc_m3b <- aalen(Surv(bl_age, end_age, diabetes_death) ~ const(interact) + 
                    const(married.factor) + const(edu.factor) + female.factor,  data = nhis_white, robust=0)
                summary(bmi_alc_m3b)
                

        ## MEN
        bmi_alc_m3_m <- aalen(Surv(bl_age, end_age, diabetes_death) ~ const(bmi_cat.factor)*const(alcohol5v2.factor) + 
                    const(edu.factor) + const(married.factor),  data = nhis_male_white, robust=0)
                summary(bmi_alc_m3_m)
                
                
        ## WOMEN
        bmi_alc_m3_f <- aalen(Surv(bl_age, end_age, diabetes_death) ~ const(bmi_cat.factor)*const(alcohol5v2.factor) + 
                    const(edu.factor) + const(married.factor),  data = nhis_female_white, robust=0)
                summary(bmi_alc_m3_f)                
                