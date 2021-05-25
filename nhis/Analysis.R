
### SIMAH Objective 1
### Impact of SES and Alcohol use on mortality


library(haven)      # Read SAS file
library(tidyverse)  # data management
library(janitor)    # clean variable names
library(skimr)      # descriptive statistics
library(gmodels)    # CrossTable command
library(tableone)   # create table one
library(survival)   # surivval analyses
library(survminer)  # surivval analyses
library(timereg)    # additive survival models
library(survey)     # for survey weighted cox model
library(cmprsk)     # Competing risk cox model 
library(biostat3)   # survRate command
library(tictoc)     # to track track R's time to complete tasks
library(beepr)      # Plays sound once process is complete
library(medflex)    # causal mediation
library(VGAM)       # multinomial regression, needed for causal mediation
memory.limit(size=1e+13)


# DATA IMPORT & MANAGEMENT----------------------------------------------------------------------------------------------------------------
nhis_all <- read_sas ("data/nhis_mort_clean.sas7bdat") %>%
  zap_formats() %>% zap_label() %>% clean_names() %>%   # removes labels/formats form SAS and clean names
  filter(!is.na(mortstat) & !is.na(new_weight)) %>%     # remove people for whom mortality data is not available and the one person that doesn't have a mortality weight
  # Recode and create variables
  mutate (bl_age = age,                  # Baseline age
          end_age = age + yrs_followup,  # Age at death/censor
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
          accid_death = if_else(death_type==1, 1, 0),
          cvd_death = if_else(death_type==2, 1, 0),
          crd_death = if_else(death_type==3, 1, 0),
          other_death = if_else(death_type==4, 1, 0))
                                
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
        
        nhis_all$income.factor <- factor(nhis_all$income, levels=c(0, 1,2,3,4), labels = c("Missing", "Poor","Near poor","Middle income", "Higher income"))
        
        
        
        
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
        
        

        
       
        
        # Remove those outside our age range
        nhis_age25_85 <- nhis_all %>%
          filter (age>=25 & age <85)
        
        # remove those with missing data 
        nhis <- nhis_age25_85 %>%
          filter(complete.cases(yrs_followup, allcause_death, alcohol5v2, bmi_cat, smoking4, phy_act3, edu, age, female, married, ethnicity))
           
        
        # Create database specific to males or females
        nhis_female <- filter(nhis, female==1)
        nhis_male <- filter(nhis, female==0)
        
        
        
        # Create a database utilizing survey weights and design variables
            # See the Survey Desctiption - Appendix III (Variance Estimation Method for Public Use Data) and Appendix IV (Merging Data Files and Combining Years of Data in the NHIS)
        
          nhis_svyWeights_all <- svydesign(id = ~new_psu,
                                  strata  = ~new_stratum,
                                  weights = ~new_weight,
                                  nest    = TRUE,
                                  data    = nhis_all)
        
          # Create subset with no missing data, correcting for survey weights
          nhis_svyWeights <- subset(nhis_svyWeights_all, 
                                    !is.na(yrs_followup) & !is.na(mortstat) & 
                                    !is.na(alcohol5v2) & !is.na(bmi_cat) & !is.na(smoking4) & !is.na(phy_act3) &
                                    !is.na(edu) & !is.na(age) & !is.na(female) & !is.na(married) & !is.na(ethnicity) & 
                                    (age>=25 & age <85))

        




# DESCRIPTIVES -----------------------------------------------------------------------------------------------------------

# Participant characteristics 
tab1 <-CreateTableOne(vars= c("yrs_followup","allcause_death.factor", "age",
                              "alcohol5v2.factor","smoking4.factor",  "bmi_cat.factor", "phy_act3.factor",
                               "ethnicity.factor",  "married.factor", "employed.factor", "income.factor"),
                      factorVars = c("allcause_death.factor",
                                      "alcohol5v2.factor", "smoking4.factor", "bmi_cat.factor", "phy_act3.factor",
                                     "ethnicity.factor",  "married.factor", "employed.factor", "income.factor"), 
                      strata= c("edu.factor", "female.factor"), addOverall = TRUE, data=nhis)
  table1_v1 <- print(tab1, noSpaces = TRUE, catDigits = 0, contDigits = 1, printToggle = FALSE, test=FALSE)
  table1_v2 <- print(tab1, noSpaces = TRUE, catDigits = 0, contDigits = 1, printToggle = FALSE, test=FALSE, format="p")
  write.csv(table1_v1, file="output/Table1 Demographics_V1.csv")
  write.csv(table1_v2, file="output/Table1 Demographics_V2.csv")
  kableone(table1_v1)

  
  
# Person years and death rate: tstop = person years; event = # events; rate = events per person year 
survRate(Surv(yrs_followup, allcause_death)~1, data=nhis)          # overall 
survRate(Surv(yrs_followup, allcause_death)~edu, data=nhis)        # for each SES category 
survRate(Surv(yrs_followup, allcause_death)~female+edu, data=nhis) # for each SES * Sex category 
 

# Survival plot 
ggsurvplot_facet(fit = survfit(Surv(bl_age, end_age, allcause_death) ~ edu, data = nhis), 
  data=nhis, facet.by="female.factor", censor = FALSE,xlim = c(25, 100), 
  conf.int = TRUE, 
  legend.labs = c("Low SES", "Medium SES", "High SES"),
  xlab = "Age (years)", 
  ylab = "Overall survival probability") 

      # Age Medium Survival:
      survfit(Surv(bl_age, end_age, allcause_death) ~ edu, data = nhis)
      survfit(Surv(bl_age, end_age, allcause_death) ~ edu, data = nhis_female)
      survfit(Surv(bl_age, end_age, allcause_death) ~ edu, data = nhis_male)


# ASSUMPTIONS: Additive Hazard Models ------------------------------------------------------------------------------------------------------------------

# First, check time-invariant assumption; whether the impact of covariates is time-varying or constant with time (similar to proportional hazard assumption in Cox models).
# To make a variable constant with time, use the wrapper "const()" around the variable; without this wrapper the varying will be time-varying.
# Start by fitting the model where all components of the model have time-varying effects (i.e. don't use the const() wrapper)  
# Then start to simply model by a number of successive tests to making variables age-invariant; those that have a straight line in the plot and/or are not significant in the Kolmogorov-Smirnov / Cramer von Mises test
# Ultimately, the variables that are part of an interaction have to have a age-invariant effect (use const() wrapper).
      # and sensitivity analyses (stratifying by age group) will be ran to examine the potential impact of violating the assumption (if the assumption was violated)

# For more details and theoretical justification/description see:
      # Rod et al. 2012 https://doi.org/10.1097/EDE.0b013e31825fa218
      # Scheike TH, Martinussen T. Dynamic Regression models for survival data: Springer, NY.; 2006.
      
# Assumption: Alcohol x Education *********************************************************************************************************************
# *****************************************************************************************************************************************************
      
## WOMEN: Checking assumptions for Alcohol x Education model 
# Start with all variables as age-varying
aalen_alc_f_assump1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ alcohol5v2.factor + edu.factor + married.factor + ethnicity.factor, data = nhis_female)
    saveRDS(aalen_alc_f_assump1, "Output/Assumptions/Interaction/alc/aalen_alc_f_assump1.rds");               # Save model results
    pdf("Output/Assumptions/Interaction/alc/aalen_alc_f_assump1.pdf"); plot(aalen_alc_f_assump1); dev.off()   # save plot 
    aalen_alc_f_assump1 <-readRDS("Output/Assumptions/Interaction/alc/aalen_alc_f_assump1.rds")               # load model results
    summary(aalen_alc_f_assump1)
    # RESULT: Married should be made age-invariant

    
        # Iteration 2 
        aalen_alc_f_assump2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ alcohol5v2.factor + edu.factor + const(married.factor) + ethnicity.factor, data = nhis_female)
              saveRDS(aalen_alc_f_assump2, "Output/Assumptions/Interaction/alc/aalen_alc_f_assump2.rds");               # Save model results
              pdf("Output/Assumptions/Interaction/alc/aalen_alc_f_assump2.pdf"); plot(aalen_alc_f_assump2); dev.off()   # save plot 
              aalen_alc_f_assump2 <-readRDS("Output/Assumptions/Interaction/alc/aalen_alc_f_assump2.rds")               # load model results
              summary(aalen_alc_f_assump2)
              # RESULT: Education should be made age-invariant
        
            
        # Iteration 3
        aalen_alc_f_assump3 <- aalen(Surv(bl_age, end_age, allcause_death) ~ alcohol5v2.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, data = nhis_female)
              saveRDS(aalen_alc_f_assump3, "Output/Assumptions/Interaction/alc/aalen_alc_f_assump3.rds");               # Save model results
              pdf("Output/Assumptions/Interaction/alc/aalen_alc_f_assump3.pdf"); plot(aalen_alc_f_assump3); dev.off()   # save plot 
              aalen_alc_f_assump3 <-readRDS("Output/Assumptions/Interaction/alc/aalen_alc_f_assump3.rds")               # load model results
              summary(aalen_alc_f_assump3)
              # RESULT: alcohol (former, low risk) and ethnicity should be kept age-varying  
        
    
              
              
              
## MEN: Checking assumptions for Alcohol x Education model 
# Start with all variables as age-varying
aalen_alc_m_assump1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ alcohol5v2.factor + edu.factor + married.factor + ethnicity.factor, data = nhis_male)
        saveRDS(aalen_alc_m_assump1, "Output/Assumptions/Interaction/alc/aalen_alc_m_assump1.rds");               # Save model results
        pdf("Output/Assumptions/Interaction/alc/aalen_alc_m_assump1.pdf"); plot(aalen_alc_m_assump1); dev.off()   # save plot 
        aalen_alc_m_assump1 <-readRDS("Output/Assumptions/Interaction/alc/aalen_alc_m_assump1.rds")               # load model results
        summary(aalen_alc_m_assump1)
        # RESULT: Education should be made age-invariant
              
        
              # Iteration 2 
              aalen_alc_m_assump2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ alcohol5v2.factor + const(edu.factor) + married.factor + ethnicity.factor, data = nhis_male)
              saveRDS(aalen_alc_m_assump2, "Output/Assumptions/Interaction/alc/aalen_alc_m_assump2.rds");               # Save model results
              pdf("Output/Assumptions/Interaction/alc/aalen_alc_m_assump2.pdf"); plot(aalen_alc_m_assump2); dev.off()   # save plot 
              aalen_alc_m_assump2 <-readRDS("Output/Assumptions/Interaction/alc/aalen_alc_m_assump2.rds")               # load model results
              summary(aalen_alc_m_assump2)
              # RESULT: Marital Status should be made age-invariant
              
              
              # Iteration 3
              aalen_alc_m_assump3 <- aalen(Surv(bl_age, end_age, allcause_death) ~ alcohol5v2.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, data = nhis_male)
              saveRDS(aalen_alc_m_assump3, "Output/Assumptions/Interaction/alc/aalen_alc_m_assump3.rds");               # Save model results
              pdf("Output/Assumptions/Interaction/alc/aalen_alc_m_assump3.pdf"); plot(aalen_alc_m_assump3); dev.off()   # save plot 
              aalen_alc_m_assump3 <-readRDS("Output/Assumptions/Interaction/alc/aalen_alc_m_assump3.rds")               # load model results
              summary(aalen_alc_m_assump3)
              # RESULT: alcohol (former, low risk) and ethnicity should be kept age-varying 
              
              
                      
              
              
                                 
           
# Assumption: Smoking x Education **********************************************************************************************************************
# ******************************************************************************************************************************************************
              
## WOMEN: Checking assumptions for Alcohol x Education model 
# Start with all variables as age-varying
aalen_smk_f_assump1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor + edu.factor + married.factor + ethnicity.factor, data = nhis_female)
      saveRDS(aalen_smk_f_assump1, "Output/Assumptions/Interaction/smk/aalen_smk_f_assump1.rds");               # Save model results
      pdf("Output/Assumptions/Interaction/smk/aalen_smk_f_assump1.pdf"); plot(aalen_smk_f_assump1); dev.off()   # save plot 
      aalen_smk_f_assump1 <-readRDS("Output/Assumptions/Interaction/smk/aalen_smk_f_assump1.rds")               # load model results
      summary(aalen_smk_f_assump1)
      # RESULT: Married should be made age-invariant
              
      
              # Iteration 2 
              aalen_smk_f_assump2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor + edu.factor + const(married.factor) + ethnicity.factor, data = nhis_female)
              saveRDS(aalen_smk_f_assump2, "Output/Assumptions/Interaction/smk/aalen_smk_f_assump2.rds");               # Save model results
              pdf("Output/Assumptions/Interaction/smk/aalen_smk_f_assump2.pdf"); plot(aalen_smk_f_assump2); dev.off()   # save plot 
              aalen_smk_f_assump2 <-readRDS("Output/Assumptions/Interaction/smk/aalen_smk_f_assump2.rds")               # load model results
              summary(aalen_smk_f_assump2)
              # RESULT: Smoking, Education (highschool) and ethnicity should be age-varying
             
              
              
                      
              
## MEN: Checking assumptions for Alcohol x Education model 
# Start with all variables as age-varying
aalen_smk_m_assump1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor + edu.factor + married.factor + ethnicity.factor, data = nhis_male)
        saveRDS(aalen_smk_m_assump1, "Output/Assumptions/Interaction/smk/aalen_smk_m_assump1.rds");               # Save model results
        pdf("Output/Assumptions/Interaction/smk/aalen_smk_m_assump1.pdf"); plot(aalen_smk_m_assump1); dev.off()   # save plot 
        aalen_smk_m_assump1 <-readRDS("Output/Assumptions/Interaction/smk/aalen_smk_m_assump1.rds")               # load model results
        summary(aalen_smk_m_assump1)
        # RESULT: Education should be made age-invariant

                      
              # Iteration 2 
              aalen_smk_m_assump2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor + const(edu.factor) + married.factor + ethnicity.factor, data = nhis_male)
              saveRDS(aalen_smk_m_assump2, "Output/Assumptions/Interaction/smk/aalen_smk_m_assump2.rds");               # Save model results
              pdf("Output/Assumptions/Interaction/smk/aalen_smk_m_assump2.pdf"); plot(aalen_smk_m_assump2); dev.off()   # save plot 
              aalen_smk_m_assump2 <-readRDS("Output/Assumptions/Interaction/smk/aalen_smk_m_assump2.rds")               # load model results
              summary(aalen_smk_m_assump2)
              # RESULT: Marital Status should be made age-invariant
              
              
              # Iteration 3
              aalen_smk_m_assump3 <- aalen(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, data = nhis_male)
              saveRDS(aalen_smk_m_assump3, "Output/Assumptions/Interaction/smk/aalen_smk_m_assump3.rds");               # Save model results
              pdf("Output/Assumptions/Interaction/smk/aalen_smk_m_assump3.pdf"); plot(aalen_smk_m_assump3); dev.off()   # save plot 
              aalen_smk_m_assump3 <-readRDS("Output/Assumptions/Interaction/smk/aalen_smk_m_assump3.rds")               # load model results
              summary(aalen_smk_m_assump3)
              # RESULT: Smoking (former, everyday) and ethnicity should be kept age-varying 
              
           
              
              
 
              
              
              
                           
# Assumption: BMI x Education ********************************************************************************************************************
# ************************************************************************************************************************************************
              
## WOMEN: Checking assumptions for Alcohol x Education model 
# Start with all variables as age-varying
aalen_bmi_f_assump1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor + edu.factor + married.factor + ethnicity.factor, data = nhis_female)
      saveRDS(aalen_bmi_f_assump1, "Output/Assumptions/Interaction/bmi/aalen_bmi_f_assump1.rds");               # Save model results
      pdf("Output/Assumptions/Interaction/bmi/aalen_bmi_f_assump1.pdf"); plot(aalen_bmi_f_assump1); dev.off()   # save plot 
      aalen_bmi_f_assump1 <-readRDS("Output/Assumptions/Interaction/bmi/aalen_bmi_f_assump1.rds")               # load model results
      summary(aalen_bmi_f_assump1)
      # RESULT: Education should be made age-invariant
              
      
              # Iteration 2 
              aalen_bmi_f_assump2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor + const(edu.factor) + married.factor + ethnicity.factor, data = nhis_female)
              saveRDS(aalen_bmi_f_assump2, "Output/Assumptions/Interaction/bmi/aalen_bmi_f_assump2.rds");               # Save model results
              pdf("Output/Assumptions/Interaction/bmi/aalen_bmi_f_assump2.pdf"); plot(aalen_bmi_f_assump2); dev.off()   # save plot 
              aalen_bmi_f_assump2 <-readRDS("Output/Assumptions/Interaction/bmi/aalen_bmi_f_assump2.rds")               # load model results
              summary(aalen_bmi_f_assump2)
              # RESULT: Marital Status should be made age-invariant
              
              
              # Iteration 3
              aalen_bmi_f_assump3 <- aalen(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, data = nhis_female)
              saveRDS(aalen_bmi_f_assump3, "Output/Assumptions/Interaction/bmi/aalen_bmi_f_assump3.rds");               # Save model results
              pdf("Output/Assumptions/Interaction/bmi/aalen_bmi_f_assump3.pdf"); plot(aalen_bmi_f_assump3); dev.off()   # save plot 
              aalen_bmi_f_assump3 <-readRDS("Output/Assumptions/Interaction/bmi/aalen_bmi_f_assump3.rds")               # load model results
              summary(aalen_bmi_f_assump3)
              # RESULT: BMI (Obese, Overweight) can be made age-invariant
              
              
              
              
              
## MEN: Checking assumptions for Alcohol x Education model 
# Start with all variables as age-varying
aalen_bmi_m_assump1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor + edu.factor + married.factor + ethnicity.factor, data = nhis_male)
      saveRDS(aalen_bmi_m_assump1, "Output/Assumptions/Interaction/bmi/aalen_bmi_m_assump1.rds");               # Save model results
      pdf("Output/Assumptions/Interaction/bmi/aalen_bmi_m_assump1.pdf"); plot(aalen_bmi_m_assump1); dev.off()   # save plot 
      aalen_bmi_m_assump1 <-readRDS("Output/Assumptions/Interaction/bmi/aalen_bmi_m_assump1.rds")               # load model results
      summary(aalen_bmi_m_assump1)
      # RESULT: Education should be made age-invariant

                    
              # Iteration 2 
              aalen_bmi_m_assump2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor + const(edu.factor) + married.factor + ethnicity.factor, data = nhis_male)
              saveRDS(aalen_bmi_m_assump2, "Output/Assumptions/Interaction/bmi/aalen_bmi_m_assump2.rds");               # Save model results
              pdf("Output/Assumptions/Interaction/bmi/aalen_bmi_m_assump2.pdf"); plot(aalen_bmi_m_assump2); dev.off()   # save plot 
              aalen_bmi_m_assump2 <-readRDS("Output/Assumptions/Interaction/bmi/aalen_bmi_m_assump2.rds")               # load model results
              summary(aalen_bmi_m_assump2)
              # RESULT: Marital Status should be made age-invariant
              
              
              # Iteration 3
              aalen_bmi_m_assump3 <- aalen(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, data = nhis_male)
              saveRDS(aalen_bmi_m_assump3, "Output/Assumptions/Interaction/bmi/aalen_bmi_m_assump3.rds");               # Save model results
              pdf("Output/Assumptions/Interaction/bmi/aalen_bmi_m_assump3.pdf"); plot(aalen_bmi_m_assump3); dev.off()   # save plot 
              aalen_bmi_m_assump3 <-readRDS("Output/Assumptions/Interaction/bmi/aalen_bmi_m_assump3.rds")               # load model results
              summary(aalen_bmi_m_assump3)
              # RESULT: BMI (Obese) can be made age-invariant
              
              
           
              
              
              
              
              
              
              
# Assumption: Physical Activity x Education **********************************************************************************************************
# ****************************************************************************************************************************************************
              
## WOMEN: Checking assumptions for Alcohol x Education model 
# Start with all variables as age-varying
aalen_phy_f_assump1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ phy_act3.factor + edu.factor + married.factor + ethnicity.factor, data = nhis_female)
      saveRDS(aalen_phy_f_assump1, "Output/Assumptions/Interaction/phy/aalen_phy_f_assump1.rds");               # Save model results
      pdf("Output/Assumptions/Interaction/phy/aalen_phy_f_assump1.pdf"); plot(aalen_phy_f_assump1); dev.off()   # save plot 
      aalen_phy_f_assump1 <-readRDS("Output/Assumptions/Interaction/phy/aalen_phy_f_assump1.rds")               # load model results
      summary(aalen_phy_f_assump1)
      # RESULT: Education should be made age-invariant
              
              # Iteration 2 
              aalen_phy_f_assump2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ phy_act3.factor + const(edu.factor) + married.factor + ethnicity.factor, data = nhis_female)
              saveRDS(aalen_phy_f_assump2, "Output/Assumptions/Interaction/phy/aalen_phy_f_assump2.rds");               # Save model results
              pdf("Output/Assumptions/Interaction/phy/aalen_phy_f_assump2.pdf"); plot(aalen_phy_f_assump2); dev.off()   # save plot 
              aalen_phy_f_assump2 <-readRDS("Output/Assumptions/Interaction/phy/aalen_phy_f_assump2.rds")               # load model results
              summary(aalen_phy_f_assump2)
              # RESULT: Marital Status should be made age-invariant
              
              
              # Iteration 3
              aalen_phy_f_assump3 <- aalen(Surv(bl_age, end_age, allcause_death) ~ phy_act3.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, data = nhis_female)
              saveRDS(aalen_phy_f_assump3, "Output/Assumptions/Interaction/phy/aalen_phy_f_assump3.rds");               # Save model results
              pdf("Output/Assumptions/Interaction/phy/aalen_phy_f_assump3.pdf"); plot(aalen_phy_f_assump3); dev.off()   # save plot 
              aalen_phy_f_assump3 <-readRDS("Output/Assumptions/Interaction/phy/aalen_phy_f_assump3.rds")               # load model results
              summary(aalen_phy_f_assump3)
              # RESULT: Physical activity should be made age-invariant
              
              
              
              
## MEN: Checking assumptions for Alcohol x Education model 
# Start with all variables as age-varying
aalen_phy_m_assump1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ phy_act3.factor + edu.factor + married.factor + ethnicity.factor, data = nhis_male)
      saveRDS(aalen_phy_m_assump1, "Output/Assumptions/Interaction/phy/aalen_phy_m_assump1.rds");               # Save model results
      pdf("Output/Assumptions/Interaction/phy/aalen_phy_m_assump1.pdf"); plot(aalen_phy_m_assump1); dev.off()   # save plot 
      aalen_phy_m_assump1 <-readRDS("Output/Assumptions/Interaction/phy/aalen_phy_m_assump1.rds")               # load model results
      summary(aalen_phy_m_assump1)
      # RESULT: Education should be made age-invariant
              
      
              # Iteration 2 
              aalen_phy_m_assump2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ phy_act3.factor + const(edu.factor) + married.factor + ethnicity.factor, data = nhis_male)
              saveRDS(aalen_phy_m_assump2, "Output/Assumptions/Interaction/phy/aalen_phy_m_assump2.rds");               # Save model results
              pdf("Output/Assumptions/Interaction/phy/aalen_phy_m_assump2.pdf"); plot(aalen_phy_m_assump2); dev.off()   # save plot 
              aalen_phy_m_assump2 <-readRDS("Output/Assumptions/Interaction/phy/aalen_phy_m_assump2.rds")               # load model results
              summary(aalen_phy_m_assump2)
              # RESULT: Marital Status should be made age-invariant
              
              
              # Iteration 3
              aalen_phy_m_assump3 <- aalen(Surv(bl_age, end_age, allcause_death) ~ phy_act3.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, data = nhis_male)
              saveRDS(aalen_phy_m_assump3, "Output/Assumptions/Interaction/phy/aalen_phy_m_assump3.rds");               # Save model results
              pdf("Output/Assumptions/Interaction/phy/aalen_phy_m_assump3.pdf"); plot(aalen_phy_m_assump3); dev.off()   # save plot 
              aalen_phy_m_assump3 <-readRDS("Output/Assumptions/Interaction/phy/aalen_phy_m_assump3.rds")               # load model results
              summary(aalen_phy_m_assump3)
              # RESULT: Physical Activity (Sedentary) and ethnicity should be kept age-varying 
              
             
              

              
              
                            
# Assumption: Causal Mediation ********************************************************************************************************************
# *************************************************************************************************************************************************

# To check the time-invariant assumption, run the full model with all variables as age-varying; then simplify by making variables
# age-invariant (those not significant in the Kolmogorov-Smirnov / Cramer von Mises test)


# FEMALES
CM_allcause_f_assump_1 <- aalen(Surv(bl_age, end_age, allcause_death) ~  edu.factor + alcohol5v2.factor + smoking4.factor +
                bmi_cat.factor + phy_act3.factor + married.factor + ethnicity.factor, data=nhis_female)    
        saveRDS(CM_allcause_f_assump_1, "output/Assumptions/CM_allcause_f_assump_1.rds")                      # Save model results
        pdf("output/Assumptions/CM_allcause_f_assump_1_plot.pdf"); plot(CM_allcause_f_assump_1); dev.off()    # save plot
        CM_allcause_f_assump_1 <-readRDS("output/Assumptions/CM_allcause_f_assump_1.rds")                     # load model results
        summary(CM_allcause_f_assump_1)
        # Result: Marital status should be made age-invariant
        
        
        # Iteration 2 
        CM_allcause_f_assump_2 <- aalen(Surv(bl_age, end_age, allcause_death) ~  edu.factor + alcohol5v2.factor + smoking4.factor +
              bmi_cat.factor + phy_act3.factor + const(married.factor) + ethnicity.factor, data=nhis_female)
              saveRDS(CM_allcause_f_assump_2, "output/Assumptions/CM_allcause_f_assump_2.rds")                     # Save model results
              pdf("output/Assumptions/CM_allcause_f_assump_2_plot.pdf"); plot(CM_allcause_f_assump_2); dev.off()   # save plot
              CM_allcause_f_assump_2 <-readRDS("output/Assumptions/CM_allcause_f_assump_2.rds")                    # load model results
              summary(CM_allcause_f_assump_2)
              # Result: education should be made age-invariant
        
        
        
        # Iteration 3
        CM_allcause_f_assump_3 <- aalen(Surv(bl_age, end_age, allcause_death) ~  const(edu.factor) + alcohol5v2.factor + smoking4.factor +
                        bmi_cat.factor + phy_act3.factor + const(married.factor) + ethnicity.factor, data=nhis_female)
                saveRDS(CM_allcause_f_assump_3, "output/Assumptions/CM_allcause_f_assump_3.rds")                    # Save model results
                pdf("output/Assumptions/CM_allcause_f_assump_3_plot.pdf"); plot(CM_allcause_f_assump_3); dev.off()  # save plot
                CM_allcause_f_assump_3 <-readRDS("output/Assumptions/CM_allcause_f_assump_3.rds")                   # load model results
                summary(CM_allcause_f_assump_3)
                # Result: BMI should be made age-invariant
        
        
        # Iteration 4 
        CM_allcause_f_assump_4 <- aalen(Surv(bl_age, end_age, allcause_death) ~  const(edu.factor) + alcohol5v2.factor + smoking4.factor +
                              const(bmi_cat.factor) + phy_act3.factor + const(married.factor) + ethnicity.factor, data=nhis_female)
                saveRDS(CM_allcause_f_assump_4, "output/Assumptions/CM_allcause_f_assump_4.rds")                    # Save model results
                pdf("output/Assumptions/CM_allcause_f_assump_4_plot.pdf"); plot(CM_allcause_f_assump_4); dev.off()  # save plot
                CM_allcause_f_assump_4 <-readRDS("output/Assumptions/CM_allcause_f_assump_4.rds")                   # load model results
                summary(CM_allcause_f_assump_4)
                # Result: alcohol should be made age-invariant
        
        
        # Iteration 5
        CM_allcause_f_assump_5 <- aalen(Surv(bl_age, end_age, allcause_death) ~  const(edu.factor) + const(alcohol5v2.factor) + smoking4.factor +
                                  const(bmi_cat.factor) + phy_act3.factor + const(married.factor) + ethnicity.factor, data=nhis_female)
                saveRDS(CM_allcause_f_assump_5, "output/Assumptions/CM_allcause_f_assump_5.rds")                    # Save model results
                pdf("output/Assumptions/CM_allcause_f_assump_5_plot.pdf"); plot(CM_allcause_f_assump_5); dev.off()  # save plot
                CM_allcause_f_assump_5 <-readRDS("output/Assumptions/CM_allcause_f_assump_5.rds")                   # load model results
                summary(CM_allcause_f_assump_5)
        
        
        # Final result: 
        # Age-invariant variables: marital status, education, BMI, alcohol
        # Age-varying variables: smoking4, physical activity, race/ethnicity




# MALES
CM_allcause_m_assump_1 <- aalen(Surv(bl_age, end_age, allcause_death) ~  edu.factor + alcohol5v2.factor + smoking4.factor +
                            bmi_cat.factor + phy_act3.factor + married.factor + ethnicity.factor, data=nhis_male)
        saveRDS(CM_allcause_m_assump_1, "output/Assumptions/CM_allcause_m_assump_1.rds")                    # Save model results
        pdf("output/Assumptions/CM_allcause_m_assump_1_plot.pdf"); plot(CM_allcause_m_assump_1); dev.off()  # save plot
        CM_allcause_m_assump_1 <-readRDS("output/Assumptions/CM_allcause_m_assump_1.rds")                   # load model results
        summary(CM_allcause_m_assump_1)
        # Result: marital status should be made age-invariant
        
        
        # Iteration 2 
        CM_allcause_m_assump_2 <- aalen(Surv(bl_age, end_age, allcause_death) ~  edu.factor + alcohol5v2.factor + smoking4.factor +
                   bmi_cat.factor + phy_act3.factor + const(married.factor) + ethnicity.factor, data=nhis_male)
                saveRDS(CM_allcause_m_assump_2, "output/Assumptions/CM_allcause_m_assump_2.rds")                    # Save model results
                pdf("output/Assumptions/CM_allcause_m_assump_2_plot.pdf"); plot(CM_allcause_m_assump_2); dev.off()  # save plot
                CM_allcause_m_assump_2 <-readRDS("output/Assumptions/CM_allcause_m_assump_2.rds")                   # load model results
                summary(CM_allcause_m_assump_2)
                # Result: Education should be made age-invariant
        
        
        
        # Iteration 3   
        CM_allcause_m_assump_3 <- aalen(Surv(bl_age, end_age, allcause_death) ~  const(edu.factor) + alcohol5v2.factor + smoking4.factor +
                  bmi_cat.factor + phy_act3.factor + const(married.factor) + ethnicity.factor, data=nhis_male)
              saveRDS(CM_allcause_m_assump_3, "output/Assumptions/CM_allcause_m_assump_3.rds")                    # Save model results
              pdf("output/Assumptions/CM_allcause_m_assump_3_plot.pdf"); plot(CM_allcause_m_assump_3); dev.off()  # save plot
              CM_allcause_m_assump_3 <-readRDS("output/Assumptions/CM_allcause_m_assump_3.rds")                   # load model results
              summary(CM_allcause_m_assump_3)
              # Result: BMI should be made age-invariant
        
        
              
        # Iteration 4 
        CM_allcause_m_assump_4 <- aalen(Surv(bl_age, end_age, allcause_death) ~  const(edu.factor) + alcohol5v2.factor + smoking4.factor +
                      const(bmi_cat.factor) + phy_act3.factor + const(married.factor) + ethnicity.factor, data=nhis_male)
                saveRDS(CM_allcause_m_assump_4, "output/Assumptions/CM_allcause_m_assump_4.rds")                    # Save model results
                pdf("output/Assumptions/CM_allcause_m_assump_4_plot.pdf"); plot(CM_allcause_m_assump_4); dev.off()  # save plot
                CM_allcause_m_assump_4 <-readRDS("output/Assumptions/CM_allcause_m_assump_4.rds")                   # load model results
                summary(CM_allcause_m_assump_4)
                # Result: Alcohol should be made age-invariant
        
        
        # Iteration 5  
        CM_allcause_m_assump_5 <- aalen(Surv(bl_age, end_age, allcause_death) ~  const(edu.factor) + const(alcohol5v2.factor) + smoking4.factor +
                        const(bmi_cat.factor) + phy_act3.factor + const(married.factor) + ethnicity.factor, data=nhis_male)
                saveRDS(CM_allcause_m_assump_5, "output/Assumptions/CM_allcause_m_assump_5.rds")                    # Save model results
                pdf("output/Assumptions/CM_allcause_m_assump_5_plot.pdf"); plot(CM_allcause_m_assump_5); dev.off()  # save plot
                CM_allcause_m_assump_5 <-readRDS("output/Assumptions/CM_allcause_m_assump_5.rds")                   # load model results
                summary(CM_allcause_m_assump_5)
        
                
        # Final result: 
        # Age-invariant variables: marital status, education, BMI, alcohol
        # Age-varying variables: smoking, physical activity, race/ethnicity




# OBJECTIVE 1: Joint Effects, Hazard Models - Stratified by Sex---------------------------------------------------------------------------------------------------------------------

# The effect estimates from the model can be directly interpreted as the number of additional events (deaths) per 1 person year at risk
      # Multiple the estimate by, say 10,000, to get the number of additional events per 10,000 person years
      # For more details and theoretical justification/description see:
            # Rod et al. 2012 https://doi.org/10.1097/EDE.0b013e31825fa218
            # Scheike TH, Martinussen T. Dynamic Regression models for survival data: Springer, NY.; 2006.         
                
                
## Education * Alcohol 
    ## WOMEN
    aalen_alc_f <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(alcohol5v2.factor) + 
                                const(married.factor) + ethnicity.factor,  data = nhis_female)
             saveRDS(aalen_alc_f, "Output/Interaction/alc/aalen_alc_f.rds")               # Save model results
             pdf("Output/Interaction/alc/aalen_alc_f.pdf"); plot(aalen_alc_f); dev.off()  # Save plot
             aalen_alc_f <-readRDS("Output/Interaction/alc/aalen_alc_f.rds")              # Load model results
             summary(aalen_alc_f)
             
             
    ## MEN
   aalen_alc_m <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(alcohol5v2.factor) + 
                              const(married.factor) + ethnicity.factor,  data = nhis_male)
             saveRDS(aalen_alc_m, "Output/Interaction/alc/aalen_alc_m.rds")               # Save model results
             pdf("Output/Interaction/alc/aalen_alc_m.pdf"); plot(aalen_alc_m); dev.off()  # Save plot
             aalen_alc_m <-readRDS("Output/Interaction/alc/aalen_alc_m.rds")              # Load model results
             summary(aalen_alc_m)
     
   
             
                           
##Smoking * Education 
             
      ## WOMEN
      aalen_smk_f <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(smoking4.factor) + 
                 const(married.factor) + ethnicity.factor,  data = nhis_female)
             saveRDS(aalen_smk_f, "Output/Interaction/smk/aalen_smk_f.rds")               # Save model results
             pdf("Output/Interaction/smk/aalen_smk_f.pdf"); plot(aalen_smk_f); dev.off()  # Save plot
             aalen_smk_f <-readRDS("Output/Interaction/smk/aalen_smk_f.rds")              # Load model results
             summary(aalen_smk_f)
             
             
      ## MEN
      aalen_smk_m <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(smoking4.factor) + 
                 const(married.factor) + ethnicity.factor,  data = nhis_male)
             saveRDS(aalen_smk_m, "Output/Interaction/smk/aalen_smk_m.rds")               # Save model results
             pdf("Output/Interaction/smk/aalen_smk_m.pdf"); plot(aalen_smk_m); dev.off()  # Save plot
             aalen_smk_m <-readRDS("Output/Interaction/smk/aalen_smk_m.rds")              # Load model results
             summary(aalen_smk_m)
    

             
             
            
## BMI * Education
             
      ## WOMEN
      aalen_bmi_f <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(bmi_cat.factor) + 
                 const(married.factor) + ethnicity.factor,  data = nhis_female)
             saveRDS(aalen_bmi_f, "Output/Interaction/bmi/aalen_bmi_f.rds")               # Save model results
             pdf("Output/Interaction/bmi/aalen_bmi_f.pdf"); plot(aalen_bmi_f); dev.off()  # Save plot
             aalen_bmi_f <-readRDS("Output/Interaction/bmi/aalen_bmi_f.rds")              # Load model results
             summary(aalen_bmi_f)
             
             
      ## MEN
      aalen_bmi_m <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(bmi_cat.factor) + 
                 const(married.factor) + ethnicity.factor,  data = nhis_male)
             saveRDS(aalen_bmi_m, "Output/Interaction/bmi/aalen_bmi_m.rds")               # Save model results
             pdf("Output/Interaction/bmi/aalen_bmi_m.pdf"); plot(aalen_bmi_m); dev.off()  # Save plot
             aalen_bmi_m <-readRDS("Output/Interaction/bmi/aalen_bmi_m.rds")              # Load model results
             summary(aalen_bmi_m)
             
             
             
             
             
## Physical Activity * Education
  
      ## WOMEN
      aalen_phy_f <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(phy_act3.factor) + 
                 const(married.factor) + ethnicity.factor,  data = nhis_female)
             saveRDS(aalen_phy_f, "Output/Interaction/phy/aalen_phy_f.rds")               # Save model results
             pdf("Output/Interaction/phy/aalen_phy_f.pdf"); plot(aalen_phy_f); dev.off()  # Save plot
             aalen_phy_f <-readRDS("Output/Interaction/phy/aalen_phy_f.rds")              # Load model results
             summary(aalen_phy_f)
             
             
      ## MEN
      aalen_phy_m <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(phy_act3.factor) + 
                 const(married.factor) + ethnicity.factor,  data = nhis_male)
             saveRDS(aalen_phy_m, "Output/Interaction/phy/aalen_phy_m.rds")               # Save model results
             pdf("Output/Interaction/phy/aalen_phy_m.pdf"); plot(aalen_phy_m); dev.off()  # Save plot
             aalen_phy_m <-readRDS("Output/Interaction/phy/aalen_phy_m.rds")              # Load model results
             summary(aalen_phy_m)
      
             


# OBJECTIVE 2: Causal Mediation - FEMALES -------------------------------------------------------------------------------------------------------------

# For more details and theoretical justification/description see:
    # Lange et al. 2014 https//doi.org/10.1093/aje/kwt270
    # Lange et al. 2012 https//doi.org/10.1093/aje/kwr525
    # Lange et al. 2011 https//doi.org/10.1097/EDE.0b013e31821c680c
             
             
### Step 0: Select data and load functions **************************************************************************************************************
# *******************************************************************************************************************************************************
mydata <- nhis %>%
  mutate(A.edu = edu,
    M1.alc = alcohol5v2,
    M2.smk = smoking4,
    M3.bmi = bmi_cat,
    M4.phy = phy_act3) %>%
  filter (female.factor=="Female") %>%
  dplyr::select(A.edu, M1.alc, M2.smk, M3.bmi, M4.phy,
    allcause_death, bl_age, end_age, married, ethnicity)

    # specifies the reference category
    mydata$A.edu <- factor(mydata$A.edu, levels=c(1,2,3), labels = c("Low", "Med", "High"))
    mydata$A.edu <- relevel(mydata$A.edu, ref = "High")

    # NOTE: For technical reasons, the mediators should be coded as integers starting with 1
    

# Select random subset of the sample (if needed to improve speed of analyses)
# set.seed(1234)
# mydata <- sample_frac(mydata, .70) # selects X% of sample at random






### Step 1: Fit a model for each mediator ***************************************************************************************************************
# *******************************************************************************************************************************************************

# Fit model for each mediator, conditioning on exposure (education) and all confounders

mydata$ATemp <- mydata$A.edu # first, create and use a copy of the exposure variable (for technical reasons related to R)
fitM1 <- vglm(M1.alc ~ ATemp + bl_age + married + ethnicity, data = mydata, family=multinomial(refLevel = 3))
fitM2 <- vglm(M2.smk ~ ATemp + bl_age + married + ethnicity, data = mydata, family=multinomial(refLevel = 1))
fitM3 <- vglm(M3.bmi ~ ATemp + bl_age + married + ethnicity, data = mydata, family=multinomial(refLevel = 2))
fitM4 <- vglm(M4.phy ~ ATemp + bl_age + married + ethnicity, data = mydata, family=multinomial(refLevel = 3))






### Step 2: Construct copies of ID and exposure *********************************************************************************************************
# *******************************************************************************************************************************************************

#Create ID Variable
mydata$ID <- 1:nrow(mydata) # construct id variable

# Create counterfactual version of exposure (education); repeated 4 times because there are 4 mediators
levelsOfEDU <- unique(mydata$A.edu)
myData1 <- mydata
myData2 <- mydata
myData3 <- mydata
myData1$eduSTAR1 <- levelsOfEDU[1]
myData2$eduSTAR1 <- levelsOfEDU[2]
myData3$eduSTAR1 <- levelsOfEDU[3]
tempMyData <- rbind(myData1, myData2, myData3)

myData1 <- tempMyData
myData2 <- tempMyData
myData3 <- tempMyData
myData1$eduSTAR2 <- levelsOfEDU[1]
myData2$eduSTAR2 <- levelsOfEDU[2]
myData3$eduSTAR2 <- levelsOfEDU[3]
tempMyData <- rbind(myData1, myData2, myData3)

myData1 <- tempMyData
myData2 <- tempMyData
myData3 <- tempMyData
myData1$eduSTAR3 <- levelsOfEDU[1]
myData2$eduSTAR3 <- levelsOfEDU[2]
myData3$eduSTAR3 <- levelsOfEDU[3]
tempMyData <- rbind(myData1, myData2, myData3)

myData1 <- tempMyData
myData2 <- tempMyData
myData3 <- tempMyData
myData1$eduSTAR4 <- levelsOfEDU[1]
myData2$eduSTAR4 <- levelsOfEDU[2]
myData3$eduSTAR4 <- levelsOfEDU[3]
newMyData <- rbind(myData1, myData2, myData3)






### Step 3: Construct weights  *********************************************************************************************************************
# **************************************************************************************************************************************************

# M1: alcohol
newMyData$ATemp <- newMyData$A.edu
tempDir1 <- as.matrix(predict(fitM1,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M1.alc)]

newMyData$ATemp <- newMyData$eduSTAR1
tempIndir1 <- as.matrix(predict(fitM1,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M1.alc)]

newMyData$weight1 <- tempIndir1/tempDir1


#M2: Smoking
newMyData$ATemp <- newMyData$A.edu
tempDir2 <- as.matrix(predict(fitM2,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M2.smk)]

newMyData$ATemp <- newMyData$eduSTAR2
tempIndir2 <- as.matrix(predict(fitM2,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M2.smk)]

newMyData$weight2 <- tempIndir2/tempDir2


#M3: BMI
newMyData$ATemp <- newMyData$A.edu
tempDir3 <- as.matrix(predict(fitM3,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M3.bmi)]

newMyData$ATemp <- newMyData$eduSTAR3
tempIndir3 <- as.matrix(predict(fitM3,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M3.bmi)]

newMyData$weight3 <- tempIndir3/tempDir3


#M4: Physical activity
newMyData$ATemp <- newMyData$A.edu
tempDir4 <- as.matrix(predict(fitM4,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M4.phy)]

newMyData$ATemp <- newMyData$eduSTAR4
tempIndir4 <- as.matrix(predict(fitM4,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M4.phy)]

newMyData$weight4 <- tempIndir4/tempDir4



# Final weight
newMyData$weightM <- newMyData$weight1 * newMyData$weight2 * newMyData$weight3 * newMyData$weight4
hist(newMyData$weightM)


# Remove temporary items
rm(fitM1, fitM2, fitM3, fitM4,
   tempIndir1, tempIndir2, tempIndir3, tempIndir4, 
   tempDir1, tempDir2, tempDir3, tempDir4)


## save expanded data
saveRDS(newMyData, "data/expandedData_fem.rds") 





### Step 4: Fit model *****************************************************************************************************************************
# *************************************************************************************************************************************************

## FEMALES
expandedData <-readRDS("data/expandedData_fem.rds")
CMed_f <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(A.edu) * const(eduSTAR1) + 
                                                          const(A.edu) * const(eduSTAR2) +
                                                          const(A.edu) * const(eduSTAR3) +
                                                          const(A.edu) * const(eduSTAR4) +
                                                          const(married) + ethnicity,
                            data=expandedData, weights=expandedData$weightM, clusters=expandedData$ID, robust=0)  # robust=0 is set for now to speed processing time; remember to change function below if this setting is changed
                  saveRDS(CMed_f, "output/causal mediation/CMed_f.rds")       # Save model results
                  CMed_model <-readRDS("output/causal mediation/CMed_f.rds")  # Load model results


# Direct, indirect and mediated interactive effects and standard errors are derived directly from the summary() command below
# Total effect is obtained by the sum of the three separate effects
# Confidence intervals for total effects and mediated proportions are computed using the code below:

### Load functions
# Function to obtain 95% CI of total effect and mediated proportion
getTE <- function(CMed_model, v)
{
                    TE <- sum(CMed_model$gamma[v])
                    mu <- CMed_model$gamma[v]
                    # Omega <- CMed_model$robvar.gamma[v,v] # To obtain robust estimates
                    Omega <- CMed_model$var.gamma[v,v]    # To obtain non-robust estimates
                    temp <- mvrnorm(n=10^4, mu=mu, Sigma=Omega)
                    temp_TE <- apply(temp,1,sum)
                    med_prop <- c(mu/TE,1)
                    med_prop_CI <- rbind(t(apply(temp/temp_TE, 2, quantile, c(0.025, 0.975))), c(1,1))
                    output <- cbind(c(mu,TE), c(apply(temp,2,sd),sd(temp_TE)), med_prop, med_prop_CI)
                    colnames(output) <- c("Est.", "SE", "med_prop", "lowerCI", "UpperCI")
                    rownames(output) <- c(rownames(CMed_model$gamma)[v],"TE")
                    return(output)
                  }
                  
# Function to obtain SE for indirect effect
getIE <- function(CMed_model, v)
{
                    IE <- sum(CMed_model$gamma[v])
                    mu <- CMed_model$gamma[v]
                    # Omega <- CMed_model$robvar.gamma[v,v] # To obtain robust estimates
                    Omega <- CMed_model$var.gamma[v,v]    # To obtain non-robust estimates
                    require(MASS)
                    temp <- mvrnorm(n=10^4, mu=mu, Sigma=Omega)
                    temp_IE <- apply(temp,1,sum)
                    med_prop <- c(mu/IE,1)
                    med_prop_CI <- rbind(t(apply(temp/temp_IE, 2, quantile, c(0.025, 0.975))), c(1,1))
                    output <- cbind(c(mu,IE), c(apply(temp,2,sd),sd(temp_IE)), med_prop, med_prop_CI)
                    colnames(output) <- c("Est.", "SE", "med_prop", "lowerCI", "UpperCI")
                    rownames(output) <- c(rownames(CMed_model$gamma)[v],"IE")
                    return(output)
                  }
                  
# Function to obtain 95% CI of mediated proportion of the indirect effect
getTE_IE <- function(CMed_model, v, z)
{
                    #total effect
                    TE <- sum(CMed_model$gamma[v])
                    mu <- CMed_model$gamma[v]
                    # Omega <- CMed_model$robvar.gamma[v,v]  # To obtain robust estimates
                    Omega <- CMed_model$var.gamma[v,v]     # To obtain non-robust estimates
                    require(MASS)
                    temp <- mvrnorm(n=10^4, mu=mu, Sigma=Omega)
                    temp_TE <- apply(temp,1,sum)
                    IE <- sum(CMed_model$gamma[z])
                    muIE <- CMed_model$gamma[z]
                    #OmegaIE <- CMed_model$robvar.gamma[z,z] # To obtain robust estimates
                    OmegaIE <- CMed_model$var.gamma[z,z]    # To obtain non-robust estimates
                    require(MASS)
                    tempIE <- mvrnorm(n=10^4, mu=muIE, Sigma=OmegaIE)
                    temp_IE <- apply(tempIE,1,sum)
                    med_prop <- c(IE/TE,1)
                    med_prop_CI <- (temp_IE/temp_TE)
                    output <- cbind(IE, med_prop, quantile)
                    quantile <- quantile(med_prop_CI, c(0.025, 0.975))
                    output <- cbind(IE, med_prop, quantile)
                    return(output)
                  }
                  
                  
                  
                  
# Get final results. NOTE: THE NUMBERS BELOW MAY HAVE TO BE CHANGED IF A DIFFERENT MODEL IS USED
summary(CMed_model)   #Estimates and SE
getTE(CMed_model, c(1,3,5,7,9,12,16,20,24))  # Simulated estimate and SE for total effect and mediated proportions for other effects
getIE(CMed_model, c(3,5,7,9,12,16,20,24))    # Estimate and simulated SE for indirect combined effect
getTE_IE(CMed_model, c(1,3,5,7,9,12,16,20,24), c(3,5,7,9,12,16,20,24)) # Mediated proportion and simulated 95% CI for mediated proportion of indirect combined effect




# OBJECTIVE 2: Causal Mediation - MALES -------------------------------------------------------------------------------------------------------------

# For more details and theoretical justification/description see:
    # Lange et al. 2014 https//doi.org/10.1093/aje/kwt270
    # Lange et al. 2012 https//doi.org/10.1093/aje/kwr525
    # Lange et al. 2011 https//doi.org/10.1097/EDE.0b013e31821c680c


### Step 0: Select data and load functions **************************************************************************************************************
# *******************************************************************************************************************************************************
mydata <- nhis %>%
  mutate(A.edu = edu,
    M1.alc = alcohol5v2,
    M2.smk = smoking4,
    M3.bmi = bmi_cat,
    M4.phy = phy_act3) %>%
  filter (female.factor=="Male") %>%
  dplyr::select(A.edu, M1.alc, M2.smk, M3.bmi, M4.phy,
    allcause_death, bl_age, end_age, married, ethnicity)

# specifies the reference category
mydata$A.edu <- factor(mydata$A.edu, levels=c(1,2,3), labels = c("Low", "Med", "High"))
mydata$A.edu <- relevel(mydata$A.edu, ref = "High")

# NOTE: For technical reasons, the mediators should be coded as integers starting with 1


# Select random subset of the sample
set.seed(1234)
mydata <- sample_frac(mydata, .70) # selects X% of sample at random







### Step 1: Fit a model for each mediator ***************************************************************************************************************
# *******************************************************************************************************************************************************

# Fit model for each mediator, conditioning on exposure (education) and confounders

mydata$ATemp <- mydata$A.edu # first, create and use a copy of the exposure variable (for technical reasons related to R)
fitM1 <- vglm(M1.alc ~ ATemp + bl_age + married + ethnicity, data = mydata, family=multinomial(refLevel = 3))
fitM2 <- vglm(M2.smk ~ ATemp + bl_age + married + ethnicity, data = mydata, family=multinomial(refLevel = 1))
fitM3 <- vglm(M3.bmi ~ ATemp + bl_age + married + ethnicity, data = mydata, family=multinomial(refLevel = 2))
fitM4 <- vglm(M4.phy ~ ATemp + bl_age + married + ethnicity, data = mydata, family=multinomial(refLevel = 3))





### Step 2: Construct copies of ID and exposure *********************************************************************************************************
# *******************************************************************************************************************************************************

#Create ID Variable
mydata$ID <- 1:nrow(mydata) # construct id variable

# Create counterfactual version of exposure (education); repeated 4 times because there are 4 mediators
levelsOfEDU <- unique(mydata$A.edu)
myData1 <- mydata
myData2 <- mydata
myData3 <- mydata
myData1$eduSTAR1 <- levelsOfEDU[1]
myData2$eduSTAR1 <- levelsOfEDU[2]
myData3$eduSTAR1 <- levelsOfEDU[3]
tempMyData <- rbind(myData1, myData2, myData3)

myData1 <- tempMyData
myData2 <- tempMyData
myData3 <- tempMyData
myData1$eduSTAR2 <- levelsOfEDU[1]
myData2$eduSTAR2 <- levelsOfEDU[2]
myData3$eduSTAR2 <- levelsOfEDU[3]
tempMyData <- rbind(myData1, myData2, myData3)

myData1 <- tempMyData
myData2 <- tempMyData
myData3 <- tempMyData
myData1$eduSTAR3 <- levelsOfEDU[1]
myData2$eduSTAR3 <- levelsOfEDU[2]
myData3$eduSTAR3 <- levelsOfEDU[3]
tempMyData <- rbind(myData1, myData2, myData3)

myData1 <- tempMyData
myData2 <- tempMyData
myData3 <- tempMyData
myData1$eduSTAR4 <- levelsOfEDU[1]
myData2$eduSTAR4 <- levelsOfEDU[2]
myData3$eduSTAR4 <- levelsOfEDU[3]
newMyData <- rbind(myData1, myData2, myData3)






### Step 3: Construct weights  *********************************************************************************************************************
# **************************************************************************************************************************************************

# M1: alcohol
newMyData$ATemp <- newMyData$A.edu
tempDir1 <- as.matrix(predict(fitM1,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M1.alc)]

newMyData$ATemp <- newMyData$eduSTAR1
tempIndir1 <- as.matrix(predict(fitM1,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M1.alc)]

newMyData$weight1 <- tempIndir1/tempDir1


#M2: Smoking
newMyData$ATemp <- newMyData$A.edu
tempDir2 <- as.matrix(predict(fitM2,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M2.smk)]

newMyData$ATemp <- newMyData$eduSTAR2
tempIndir2 <- as.matrix(predict(fitM2,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M2.smk)]

newMyData$weight2 <- tempIndir2/tempDir2


#M3: BMI
newMyData$ATemp <- newMyData$A.edu
tempDir3 <- as.matrix(predict(fitM3,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M3.bmi)]

newMyData$ATemp <- newMyData$eduSTAR3
tempIndir3 <- as.matrix(predict(fitM3,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M3.bmi)]


newMyData$weight3 <- tempIndir3/tempDir3


#M4: Physical activity
newMyData$ATemp <- newMyData$A.edu
tempDir4 <- as.matrix(predict(fitM4,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M4.phy)]

newMyData$ATemp <- newMyData$eduSTAR4
tempIndir4 <- as.matrix(predict(fitM4,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M4.phy)]

newMyData$weight4 <- tempIndir4/tempDir4



# Final weight
newMyData$weightM <- newMyData$weight1 * newMyData$weight2 * newMyData$weight3 * newMyData$weight4
hist(newMyData$weightM)


# Remove temporary items
rm(fitM1, fitM2, fitM3, fitM4,
  tempIndir1, tempIndir2, tempIndir3, tempIndir4, 
  tempDir1, tempDir2, tempDir3, tempDir4)


## save expanded data
saveRDS(newMyData, "data/expandedData_male.rds") 





### Step 4: Fit model *****************************************************************************************************************************
# *************************************************************************************************************************************************

## MALES
expandedData <-readRDS("data/expandedData_male.rds")
CMed_m <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(A.edu) * const(eduSTAR1) +
                                                          const(A.edu) * const(eduSTAR2) +
                                                          const(A.edu) * const(eduSTAR3) +
                                                          const(A.edu) * const(eduSTAR4) +
                                                          const(married) + ethnicity,
                                data=expandedData, weights=expandedData$weightM, clusters=expandedData$ID, robust=0)  # robust=0 is set for now to speed processing time; remember to change function below if this setting is changed
                      saveRDS(CMed_m, "output/causal mediation/CMed_m.rds")       # Save model results
                      CMed_model <-readRDS("output/causal mediation/CMed_m.rds")  # Load model results


# Direct, indirect and mediated interactive effects and standard errors are derived directly from the summary() command
# Total effect is obtained by the sum of the three separate effects
# Confidence intervals for total effects and mediated proportions are computed using the code below:

### Load functions
# Function to obtain 95% CI of total effect and mediated proportion
getTE <- function(CMed_model, v)
{
                        TE <- sum(CMed_model$gamma[v])
                        mu <- CMed_model$gamma[v]
                        # Omega <- CMed_model$robvar.gamma[v,v] # To obtain robust estimates
                        Omega <- CMed_model$var.gamma[v,v]    # To obtain non-robust estimates
                        temp <- mvrnorm(n=10^4, mu=mu, Sigma=Omega)
                        temp_TE <- apply(temp,1,sum)
                        med_prop <- c(mu/TE,1)
                        med_prop_CI <- rbind(t(apply(temp/temp_TE, 2, quantile, c(0.025, 0.975))), c(1,1))
                        output <- cbind(c(mu,TE), c(apply(temp,2,sd),sd(temp_TE)), med_prop, med_prop_CI)
                        colnames(output) <- c("Est.", "SE", "med_prop", "lowerCI", "UpperCI")
                        rownames(output) <- c(rownames(CMed_model$gamma)[v],"TE")
                        return(output)
                      }
                      
# Function to obtain SE for indirect effect
getIE <- function(CMed_model, v)
{
                        IE <- sum(CMed_model$gamma[v])
                        mu <- CMed_model$gamma[v]
                        # Omega <- CMed_model$robvar.gamma[v,v] # To obtain robust estimates
                        Omega <- CMed_model$var.gamma[v,v]    # To obtain non-robust estimates
                        require(MASS)
                        temp <- mvrnorm(n=10^4, mu=mu, Sigma=Omega)
                        temp_IE <- apply(temp,1,sum)
                        med_prop <- c(mu/IE,1)
                        med_prop_CI <- rbind(t(apply(temp/temp_IE, 2, quantile, c(0.025, 0.975))), c(1,1))
                        output <- cbind(c(mu,IE), c(apply(temp,2,sd),sd(temp_IE)), med_prop, med_prop_CI)
                        colnames(output) <- c("Est.", "SE", "med_prop", "lowerCI", "UpperCI")
                        rownames(output) <- c(rownames(CMed_model$gamma)[v],"IE")
                        return(output)
                      }
                      
# Function to obtain 95% CI of mediated proportion of the indirect effect
getTE_IE <- function(CMed_model, v, z)
{
                        #total effect
                        TE <- sum(CMed_model$gamma[v])
                        mu <- CMed_model$gamma[v]
                        # Omega <- CMed_model$robvar.gamma[v,v]  # To obtain robust estimates
                        Omega <- CMed_model$var.gamma[v,v]     # To obtain non-robust estimates
                        require(MASS)
                        temp <- mvrnorm(n=10^4, mu=mu, Sigma=Omega)
                        temp_TE <- apply(temp,1,sum)
                        IE <- sum(CMed_model$gamma[z])
                        muIE <- CMed_model$gamma[z]
                        #OmegaIE <- CMed_model$robvar.gamma[z,z] # To obtain robust estimates
                        OmegaIE <- CMed_model$var.gamma[z,z]    # To obtain non-robust estimates
                        require(MASS)
                        tempIE <- mvrnorm(n=10^4, mu=muIE, Sigma=OmegaIE)
                        temp_IE <- apply(tempIE,1,sum)
                        med_prop <- c(IE/TE,1)
                        med_prop_CI <- (temp_IE/temp_TE)
                        output <- cbind(IE, med_prop, quantile)
                        quantile <- quantile(med_prop_CI, c(0.025, 0.975))
                        output <- cbind(IE, med_prop, quantile)
                        return(output)
                      }
                      
                      
# Get final results. NOTE: THE NUMBERS BELOW MAY HAVE TO BE CHANGED IF A DIFFERENT MODEL IS USED
summary(CMed_model)   #Estimates and SE
getTE(CMed_model, c(1,3,5,7,9,12,16,20,24))  #Simulated estimate and SE for total effect and mediated proportions for other effects
getIE(CMed_model, c(3,5,7,9,12,16,20,24))    #Estimate and simulated SE for indirect combined effect
getTE_IE(CMed_model, c(1,3,5,7,9,12,16,20,24), c(3,5,7,9,12,16,20,24)) #Mediated proportion and simulated 95% CI for mediated proportion of indirect combined effect




# SENSITIVITY ANALYSES----------------------------------------------------------------------------------------------------------------------------
### Sensitivity 1: Analyses on entire sample---------------------------------------------------------------------------------------------------
########### Hazard Models - All Participants ------------------------------------------------------------------------

## Alcohol x Education 
aalen_allcause_alc_edu <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(alcohol5v2.factor) + 
    female.factor + const(married.factor) + ethnicity.factor,  data = nhis, robust=0)
    saveRDS(aalen_allcause_alc_edu, "output/interaction/alc/aalen_allcause_alc_edu.rds")                # Save model results
    pdf("output/interaction/alc/aalen_allcause_alc_edu.rds"); plot(aalen_allcause_alc_edu); dev.off()   # save plot
    aalen_allcause_alc_edu <-readRDS("output/interaction/alc/aalen_allcause_alc_edu.rds")               # load model results
    summary(aalen_allcause_alc_edu)

    
## Smoking x Education 
aalen_allcause_smk_edu <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(smoking4.factor) + 
        female.factor + const(married.factor) + ethnicity.factor,  data = nhis, robust=0)
    saveRDS(aalen_allcause_smk_edu, "output/interaction/smk/aalen_allcause_smk_edu.rds")
    pdf("output/interaction/smk/aalen_allcause_smk_edu.rds"); plot(aalen_allcause_smk_edu); dev.off()
    aalen_allcause_smk_edu <-readRDS("output/interaction/smk/aalen_allcause_smk_edu.rds")
    summary(aalen_allcause_smk_edu)
    
    
    
# BMI x Education
aalen_allcause_bmi_edu <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(bmi_cat.factor) + 
        female.factor + const(married.factor) + ethnicity.factor,  data = nhis, robust=0)
    saveRDS(aalen_allcause_bmi_edu, "output/interaction/bmi/aalen_allcause_bmi_edu.rds")
    pdf("output/interaction/bmi/aalen_allcause_bmi_edu.rds"); plot(aalen_allcause_bmi_edu); dev.off()
    aalen_allcause_bmi_edu <-readRDS("output/interaction/bmi/aalen_allcause_bmi_edu.rds")
    summary(aalen_allcause_bmi_edu)
    
    
    
    
# Physical Activity x Education
aalen_allcause_phy_edu <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(phy_act3.factor) + 
        female.factor + const(married.factor) + ethnicity.factor,  data = nhis, robust=0)
    saveRDS(aalen_allcause_phy_edu, "output/interaction/phy/aalen_allcause_phy_edu.rds")
    pdf("output/interaction/phy/aalen_allcause_phy_edu.rds"); plot(aalen_allcause_phy_edu); dev.off()
    aalen_allcause_phy_edu <-readRDS("output/interaction/phy/aalen_allcause_phy_edu.rds")
    summary(aalen_allcause_phy_edu)
    
    

########### Causal Mediation - All Participants -------------------------------------------------------------------------------------------------------------
    
    ### Step 0: Select data and load functions **************************************************************************************************************
    # *******************************************************************************************************************************************************
    mydata <- nhis %>%
      mutate(A.edu = edu,
        M1.alc = alcohol5v2,
        M2.smk = smoking4,
        M3.bmi = bmi_cat,
        M4.phy = phy_act3) %>%
      dplyr::select(A.edu, M1.alc, M2.smk, M3.bmi, M4.phy,
        allcause_death, bl_age, end_age, female, married, ethnicity)
    
    # specifies the reference category
    mydata$A.edu <- factor(mydata$A.edu, levels=c(1,2,3), labels = c("Low", "Med", "High"))
    mydata$A.edu <- relevel(mydata$A.edu, ref = "High")
    
    # NOTE: For technical reasons, the mediators should be coded as integers starting with 1
    
    
    # Select random subset of the sample
    set.seed(1234)
    mydata <- sample_frac(mydata, .70) # selects X% of sample at random
    
    ### Load functions
    #Function to obtain 95% CI of total effect and mediated proportion
    getTE <- function(CMed_model, v)
    {
      TE <- sum(CMed_model$gamma[v])
      mu <- CMed_model$gamma[v]
      # Omega <- CMed_model$robvar.gamma[v,v] # To obtain robust estimates
      Omega <- CMed_model$var.gamma[v,v]    # To obtain non-robust estimates
      temp <- mvrnorm(n=10^4, mu=mu, Sigma=Omega)
      temp_TE <- apply(temp,1,sum)
      med_prop <- c(mu/TE,1)
      med_prop_CI <- rbind(t(apply(temp/temp_TE, 2, quantile, c(0.025, 0.975))), c(1,1))
      output <- cbind(c(mu,TE), c(apply(temp,2,sd),sd(temp_TE)), med_prop, med_prop_CI)
      colnames(output) <- c("Est.", "SE", "med_prop", "lowerCI", "UpperCI")
      rownames(output) <- c(rownames(CMed_model$gamma)[v],"TE")
      return(output)
    }
    
    #Function to obtain SE for indirect effect
    getIE <- function(CMed_model, v)
    {
      IE <- sum(CMed_model$gamma[v])
      mu <- CMed_model$gamma[v]
      # Omega <- CMed_model$robvar.gamma[v,v] # To obtain robust estimates
      Omega <- CMed_model$var.gamma[v,v]    # To obtain non-robust estimates
      require(MASS)
      temp <- mvrnorm(n=10^4, mu=mu, Sigma=Omega)
      temp_IE <- apply(temp,1,sum)
      med_prop <- c(mu/IE,1)
      med_prop_CI <- rbind(t(apply(temp/temp_IE, 2, quantile, c(0.025, 0.975))), c(1,1))
      output <- cbind(c(mu,IE), c(apply(temp,2,sd),sd(temp_IE)), med_prop, med_prop_CI)
      colnames(output) <- c("Est.", "SE", "med_prop", "lowerCI", "UpperCI")
      rownames(output) <- c(rownames(CMed_model$gamma)[v],"IE")
      return(output)
    }
    
    #Function to obtain 95% CI of mediated proportion of the indirect effect
    getTE_IE <- function(CMed_model, v, z)
    {
      #total effect
      TE <- sum(CMed_model$gamma[v])
      mu <- CMed_model$gamma[v]
      # Omega <- CMed_model$robvar.gamma[v,v]  # To obtain robust estimates
      Omega <- CMed_model$var.gamma[v,v]     # To obtain non-robust estimates
      require(MASS)
      temp <- mvrnorm(n=10^4, mu=mu, Sigma=Omega)
      temp_TE <- apply(temp,1,sum)
      IE <- sum(CMed_model$gamma[z])
      muIE <- CMed_model$gamma[z]
      #OmegaIE <- CMed_model$robvar.gamma[z,z] # To obtain robust estimates
      OmegaIE <- CMed_model$var.gamma[z,z]    # To obtain non-robust estimates
      require(MASS)
      tempIE <- mvrnorm(n=10^4, mu=muIE, Sigma=OmegaIE)
      temp_IE <- apply(tempIE,1,sum)
      med_prop <- c(IE/TE,1)
      med_prop_CI <- (temp_IE/temp_TE)
      output <- cbind(IE, med_prop, quantile)
      quantile <- quantile(med_prop_CI, c(0.025, 0.975))
      output <- cbind(IE, med_prop, quantile)
      return(output)
    }
    
    
    
    
    
    
    ### Step 1: Fit a model for each mediator ***************************************************************************************************************
    # *******************************************************************************************************************************************************
    
    # Fit model for each mediator, conditioning on exposure (education) and confounders
    
    mydata$ATemp <- mydata$A.edu # first, create and use a copy of the exposure variable (for technical reasons related to R)
    fitM1 <- vglm(M1.alc ~ ATemp + bl_age + female + married + ethnicity, data = mydata, family=multinomial(refLevel = 3))
    fitM2 <- vglm(M2.smk ~ ATemp + bl_age + female + married + ethnicity, data = mydata, family=multinomial(refLevel = 1))
    fitM3 <- vglm(M3.bmi ~ ATemp + bl_age + female + married + ethnicity, data = mydata, family=multinomial(refLevel = 2))
    fitM4 <- vglm(M4.phy ~ ATemp + bl_age + female + married + ethnicity, data = mydata, family=multinomial(refLevel = 3))
    
    
    
    
    
    ### Step 2: Construct copies of ID and exposure *********************************************************************************************************
    # *******************************************************************************************************************************************************
    
    #Create ID Variable
    mydata$ID <- 1:nrow(mydata) # construct id variable
    
    # Create counterfactual version of exposure (education); repeated 4 times because there are 4 mediators
    levelsOfEDU <- unique(mydata$A.edu)
    myData1 <- mydata
    myData2 <- mydata
    myData3 <- mydata
    myData1$eduSTAR1 <- levelsOfEDU[1]
    myData2$eduSTAR1 <- levelsOfEDU[2]
    myData3$eduSTAR1 <- levelsOfEDU[3]
    tempMyData <- rbind(myData1, myData2, myData3)
    
    myData1 <- tempMyData
    myData2 <- tempMyData
    myData3 <- tempMyData
    myData1$eduSTAR2 <- levelsOfEDU[1]
    myData2$eduSTAR2 <- levelsOfEDU[2]
    myData3$eduSTAR2 <- levelsOfEDU[3]
    tempMyData <- rbind(myData1, myData2, myData3)
    
    myData1 <- tempMyData
    myData2 <- tempMyData
    myData3 <- tempMyData
    myData1$eduSTAR3 <- levelsOfEDU[1]
    myData2$eduSTAR3 <- levelsOfEDU[2]
    myData3$eduSTAR3 <- levelsOfEDU[3]
    tempMyData <- rbind(myData1, myData2, myData3)
    
    myData1 <- tempMyData
    myData2 <- tempMyData
    myData3 <- tempMyData
    myData1$eduSTAR4 <- levelsOfEDU[1]
    myData2$eduSTAR4 <- levelsOfEDU[2]
    myData3$eduSTAR4 <- levelsOfEDU[3]
    newMyData <- rbind(myData1, myData2, myData3)
    
    
    
    
    
    
    ### Step 3: Construct weights  *********************************************************************************************************************
    # **************************************************************************************************************************************************
    
    # M1: alcohol
    newMyData$ATemp <- newMyData$A.edu
    tempDir1 <- as.matrix(predict(fitM1,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M1.alc)]
    
    newMyData$ATemp <- newMyData$eduSTAR1
    tempIndir1 <- as.matrix(predict(fitM1,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M1.alc)]
    
    newMyData$weight1 <- tempIndir1/tempDir1
    
    
    #M2: Smoking
    newMyData$ATemp <- newMyData$A.edu
    tempDir2 <- as.matrix(predict(fitM2,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M2.smk)]
    
    newMyData$ATemp <- newMyData$eduSTAR2
    tempIndir2 <- as.matrix(predict(fitM2,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M2.smk)]
    
    newMyData$weight2 <- tempIndir2/tempDir2
    
    
    #M3: BMI
    newMyData$ATemp <- newMyData$A.edu
    tempDir3 <- as.matrix(predict(fitM3,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M3.bmi)]
    
    newMyData$ATemp <- newMyData$eduSTAR3
    tempIndir3 <- as.matrix(predict(fitM3,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M3.bmi)]
    
    
    newMyData$weight3 <- tempIndir3/tempDir3
    
    
    #M4: Physical activity
    newMyData$ATemp <- newMyData$A.edu
    tempDir4 <- as.matrix(predict(fitM4,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M4.phy)]
    
    newMyData$ATemp <- newMyData$eduSTAR4
    tempIndir4 <- as.matrix(predict(fitM4,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M4.phy)]
    
    newMyData$weight4 <- tempIndir4/tempDir4
    
    
    
    # Final weight
    newMyData$weightM <- newMyData$weight1 * newMyData$weight2 * newMyData$weight3 * newMyData$weight4
    hist(newMyData$weightM)
    
    
    # Remove temporary items
    rm(fitM1, fitM2, fitM3, fitM4,
      tempIndir1, tempIndir2, tempIndir3, tempIndir4, 
      tempDir1, tempDir2, tempDir3, tempDir4)
    
    
    ## save expanded data
    saveRDS(newMyData, "data/expandedData_all.rds") 
    
    
    
    
    
    ### Step 4: Fit model *****************************************************************************************************************************
    # *************************************************************************************************************************************************
    
    ## ALL Participants
    expandedData <-readRDS("data/expandedData_f.rds")
    CMed_all <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(A.edu) * const(eduSTAR1) +
                                                            const(A.edu) * const(eduSTAR2) +
                                                            const(A.edu) * const(eduSTAR3) +
                                                            const(A.edu) * const(eduSTAR4) +
                                                            const(married) + female + ethnicity,
                data=expandedData, weights=expandedData$weightM, clusters=expandedData$ID)
              saveRDS(CMed_all, "output/causal mediation/CMed_all.rds")    # Save model results
              CMed_model <-readRDS("output/causal mediation/CMed_all.rds")  # Load model results
    
    
    # Direct, indirect and mediated interactive effects and standard errors are derived directly from the summary() command
    # Total effect is obtained by the sum of the three separate effects
    # Confidence intervals for total effects and mediated proportions are computed using the code below:
    
    # Get final results NOTE: THE NUMBERS BELOW HAVE TO BE CHANGED*********************
    summary(CMed_model)   #Estimates and SE
    getTE(CMed_model, c(1,3,5,7,9,12,16,20,24))  #Simulated estimate and SE for total effect and mediated proportions for other effects
    getIE(CMed_model, c(3,5,7,9,12,16,20,24))    #Estimate and simulated SE for indirect combined effect
    getTE_IE(CMed_model, c(1,3,5,7,9,12,16,20,24), c(3,5,7,9,12,16,20,24)) #Mediated proportion and simulated 95% CI for mediated proportion of indirect combined effect
    
    
    
    
### Sensitivity 2: XXXXX ------------------------------------------------------------------------------------------------------------------------------
### Sensitivity 3: Stratified by age ------------------------------------------------------------------------------------------------------------------------------
############ Assumptions: Stratified by age ------------------------------------------------------------------------------------------------------------------------------------------ 
    
# Assumption: Alcohol x Education ********************************************************************************************************************
# ****************************************************************************************************************************************************

# FEMALES
    
    # Time-varying covariates
    # Iteration 1 - Ages 25-65
    aalen_alc_f_assump1_tvc2565 <- aalen(Surv(bl_age, end_age, allcause_death) ~ alcohol5v2.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
      start.time=25, max.time=64.999, data = nhis_female)
    saveRDS(aalen_alc_f_assump1_tvc2565, "Output/Assumptions/Interaction/alc/aalen_alc_f_assump1_tvc2565.rds");               # Save model results
    pdf("Output/Assumptions/Interaction/alc/aalen_alc_f_assump1_tvc2565.pdf"); plot(aalen_alc_f_assump1_tvc2565); dev.off()   # save plot 
    aalen_alc_f_assump1_tvc2565 <-readRDS("Output/Assumptions/Interaction/alc/aalen_alc_f_assump1_tvc2565.rds")               # load model results
    summary(aalen_alc_f_assump1_tvc2565)
    # RESULT: ??  
    
    
    # Iteration 1 - Ages 65+
    aalen_alc_f_assump1_tvc65up <- aalen(Surv(bl_age, end_age, allcause_death) ~ alcohol5v2.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
      start.time=65, data = nhis_female)
    saveRDS(aalen_alc_f_assump1_tvc65up, "Output/Assumptions/Interaction/alc/aalen_alc_f_assump1_tvc65up.rds");               # Save model results
    pdf("Output/Assumptions/Interaction/alc/aalen_alc_f_assump1_tvc65up.pdf"); plot(aalen_alc_f_assump1_tvc65up); dev.off()   # save plot 
    aalen_alc_f_assump1_tvc65up <-readRDS("Output/Assumptions/Interaction/alc/aalen_alc_f_assump1_tvc65up.rds")               # load model results
    summary(aalen_alc_f_assump1_tvc65up)
    # RESULT: ??  
    
    
  
    
# MALES
    
    # Iteration 1 - Ages 25-65
    aalen_alc_m_assump1_tvc2565 <- aalen(Surv(bl_age, end_age, allcause_death) ~ alcohol5v2.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
      start.time=25, max.time=64.999, data = nhis_male)
    saveRDS(aalen_alc_m_assump1_tvc2565, "Output/Assumptions/Interaction/alc/aalen_alc_m_assump1_tvc2565.rds");               # Save model results
    pdf("Output/Assumptions/Interaction/alc/aalen_alc_m_assump1_tvc2565.pdf"); plot(aalen_alc_m_assump1_tvc2565); dev.off()   # save plot 
    aalen_alc_m_assump1_tvc2565 <-readRDS("Output/Assumptions/Interaction/alc/aalen_alc_m_assump1_tvc2565.rds")               # load model results
    summary(aalen_alc_m_assump1_tvc2565)
    # RESULT: ??  
    
    
    # Iteration 1 - Ages 65+
    aalen_alc_m_assump1_tvc65up <- aalen(Surv(bl_age, end_age, allcause_death) ~ alcohol5v2.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
      start.time=65, data = nhis_male)
    saveRDS(aalen_alc_m_assump1_tvc65up, "Output/Assumptions/Interaction/alc/aalen_alc_m_assump1_tvc65up.rds");               # Save model results
    pdf("Output/Assumptions/Interaction/alc/aalen_alc_m_assump1_tvc65up.pdf"); plot(aalen_alc_m_assump1_tvc65up); dev.off()   # save plot 
    aalen_alc_m_assump1_tvc65up <-readRDS("Output/Assumptions/Interaction/alc/aalen_alc_m_assump1_tvc65up.rds")               # load model results
    summary(aalen_alc_m_assump1_tvc65up)
    # RESULT: ??  
    
    
    
# Assumption: Smoking x Education **********************************************************************************************************
# ****************************************************************************************************************************************************
    
# FEMALES
    
    # Iteration 1 - Ages 25-65
    aalen_smk_f_assump1_tvc2565 <- aalen(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor + edu.factor + const(married.factor) + ethnicity.factor, 
      start.time=25, max.time=64.999, data = nhis_female)
    saveRDS(aalen_smk_f_assump1_tvc2565, "Output/Assumptions/Interaction/alc/aalen_smk_f_assump1_tvc2565.rds");               # Save model results
    pdf("Output/Assumptions/Interaction/alc/aalen_smk_f_assump1_tvc2565.pdf"); plot(aalen_smk_f_assump1_tvc2565); dev.off()   # save plot 
    aalen_smk_f_assump1_tvc2565 <-readRDS("Output/Assumptions/Interaction/alc/aalen_smk_f_assump1_tvc2565.rds")               # load model results
    summary(aalen_smk_f_assump1_tvc2565)
    # RESULT: ??  
    
    
    # Iteration 1 - Ages 65+
    aalen_smk_f_assump1_tvc65up <- aalen(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor + edu.factor + const(married.factor) + ethnicity.factor, 
      start.time=65, data = nhis_female)
    saveRDS(aalen_smk_f_assump1_tvc65up, "Output/Assumptions/Interaction/alc/aalen_smk_f_assump1_tvc65up.rds");               # Save model results
    pdf("Output/Assumptions/Interaction/alc/aalen_smk_f_assump1_tvc65up.pdf"); plot(aalen_smk_f_assump1_tvc65up); dev.off()   # save plot 
    aalen_smk_f_assump1_tvc65up <-readRDS("Output/Assumptions/Interaction/alc/aalen_smk_f_assump1_tvc65up.rds")               # load model results
    summary(aalen_smk_f_assump1_tvc65up)
    # RESULT: ??  
    
    
    
    
# MALES
    # Iteration 1 - Ages 25-65
    aalen_smk_m_assump1_tvc2565 <- aalen(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
      start.time=25, max.time=64.999, data = nhis_male)
    saveRDS(aalen_smk_m_assump1_tvc2565, "Output/Assumptions/Interaction/alc/aalen_smk_m_assump1_tvc2565.rds");               # Save model results
    pdf("Output/Assumptions/Interaction/alc/aalen_smk_m_assump1_tvc2565.pdf"); plot(aalen_smk_m_assump1_tvc2565); dev.off()   # save plot 
    aalen_smk_m_assump1_tvc2565 <-readRDS("Output/Assumptions/Interaction/alc/aalen_smk_m_assump1_tvc2565.rds")               # load model results
    summary(aalen_smk_m_assump1_tvc2565)
    # RESULT: ??  
    
    
    # Iteration 1 - Ages 65+
    aalen_smk_m_assump1_tvc65up <- aalen(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
      start.time=65, data = nhis_male)
    saveRDS(aalen_smk_m_assump1_tvc65up, "Output/Assumptions/Interaction/alc/aalen_smk_m_assump1_tvc65up.rds");               # Save model results
    pdf("Output/Assumptions/Interaction/alc/aalen_smk_m_assump1_tvc65up.pdf"); plot(aalen_smk_m_assump1_tvc65up); dev.off()   # save plot 
    aalen_smk_m_assump1_tvc65up <-readRDS("Output/Assumptions/Interaction/alc/aalen_smk_m_assump1_tvc65up.rds")               # load model results
    summary(aalen_smk_m_assump1_tvc65up)
    # RESULT: ??  
    
    
    
    
    
# Assumption: BMI x Education **********************************************************************************************************
# ****************************************************************************************************************************************************
    
# FEMALES
    # Iteration 1 - Ages 25-65
    aalen_bmi_f_assump1_tvc2565 <- aalen(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
      start.time=25, max.time=64.999, data = nhis_female)
    saveRDS(aalen_bmi_f_assump1_tvc2565, "Output/Assumptions/Interaction/bmi/aalen_bmi_f_assump1_tvc2565.rds");               # Save model results
    pdf("Output/Assumptions/Interaction/bmi/aalen_bmi_f_assump1_tvc2565.pdf"); plot(aalen_bmi_f_assump1_tvc2565); dev.off()   # save plot 
    aalen_bmi_f_assump1_tvc2565 <-readRDS("Output/Assumptions/Interaction/bmi/aalen_bmi_f_assump1_tvc2565.rds")               # load model results
    summary(aalen_bmi_f_assump1_tvc2565)  
    # RESULT: ??
    
    
    # Iteration 1 - Ages 65+
    aalen_bmi_f_assump1_tvc65up <- aalen(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
      start.time=65, data = nhis_female)
    saveRDS(aalen_bmi_f_assump1_tvc65up, "Output/Assumptions/Interaction/bmi/aalen_bmi_f_assump1_tvc65up.rds");               # Save model results
    pdf("Output/Assumptions/Interaction/bmi/aalen_bmi_f_assump1_tvc65up.pdf"); plot(aalen_bmi_f_assump1_tvc65up); dev.off()   # save plot 
    aalen_bmi_f_assump1_tvc65up <-readRDS("Output/Assumptions/Interaction/bmi/aalen_bmi_f_assump1_tvc65up.rds")               # load model results
    summary(aalen_bmi_f_assump1_tvc65up) 
    # RESULT: ??
    
    
    
    
    
# MALES
    # Iteration 1 - Ages 25-65
    aalen_bmi_m_assump1_tvc2565 <- aalen(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
      start.time=25, max.time=64.999,  data = nhis_male)
    saveRDS(aalen_bmi_m_assump1_tvc2565, "Output/Assumptions/Interaction/bmi/aalen_bmi_m_assump1_tvc2565.rds");               # Save model results
    pdf("Output/Assumptions/Interaction/bmi/aalen_bmi_m_assump1_tvc2565.pdf"); plot(aalen_bmi_m_assump1_tvc2565); dev.off()   # save plot 
    aalen_bmi_m_assump1_tvc2565 <-readRDS("Output/Assumptions/Interaction/bmi/aalen_bmi_m_assump1_tvc2565.rds")               # load model results
    summary(aalen_bmi_m_assump1_tvc2565)
    # RESULT: ??
    
    # Iteration 1 - Ages 65+
    aalen_bmi_m_assump1_tvc65up <- aalen(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
      start.time=65, data = nhis_male)
    saveRDS(aalen_bmi_m_assump1_tvc65up, "Output/Assumptions/Interaction/bmi/aalen_bmi_m_assump1_tvc65up.rds");               # Save model results
    pdf("Output/Assumptions/Interaction/bmi/aalen_bmi_m_assump1_tvc65up.pdf"); plot(aalen_bmi_m_assump1_tvc65up); dev.off()   # save plot 
    aalen_bmi_m_assump1_tvc65up <-readRDS("Output/Assumptions/Interaction/bmi/aalen_bmi_m_assump1_tvc65up.rds")               # load model results
    summary(aalen_bmi_m_assump1_tvc65up)
    # RESULT: ??
    
    
    
# Assumption: Physical Activity x Education **********************************************************************************************************
# ****************************************************************************************************************************************************

# FEMALE
    # Iteration 1 - Ages 25-65
    aalen_phy_f_assump1_tvc2565 <- aalen(Surv(bl_age, end_age, allcause_death) ~ phy_act3.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
      start.time=25, max.time=64.999, data = nhis_female)
    saveRDS(aalen_phy_f_assump1_tvc2565, "Output/Assumptions/Interaction/phy/aalen_phy_f_assump1_tvc2565.rds");               # Save model results
    pdf("Output/Assumptions/Interaction/phy/aalen_phy_f_assump1_tvc2565.pdf"); plot(aalen_phy_f_assump1_tvc2565); dev.off()   # save plot 
    aalen_phy_f_assump1_tvc2565 <-readRDS("Output/Assumptions/Interaction/phy/aalen_phy_f_assump1_tvc2565.rds")               # load model results
    summary(aalen_phy_f_assump1_tvc2565)
    # RESULT: ??
    
    # Iteration 1 - Ages 65+
    aalen_phy_f_assump1_tvc65up <- aalen(Surv(bl_age, end_age, allcause_death) ~ phy_act3.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
      start.time=65, data = nhis_female)
    saveRDS(aalen_phy_f_assump1_tvc65up, "Output/Assumptions/Interaction/phy/aalen_phy_f_assump1_tvc65up.rds");               # Save model results
    pdf("Output/Assumptions/Interaction/phy/aalen_phy_f_assump1_tvc65up.pdf"); plot(aalen_phy_f_assump1_tvc65up); dev.off()   # save plot 
    aalen_phy_f_assump1_tvc65up <-readRDS("Output/Assumptions/Interaction/phy/aalen_phy_f_assump1_tvc65up.rds")               # load model results
    summary(aalen_phy_f_assump1_tvc65up)
    # RESULT: ??
    
    
    
# MALE
    # Iteration 1 - Ages 25-65
    aalen_phy_m_assump1_tvc2565 <- aalen(Surv(bl_age, end_age, allcause_death) ~ phy_act3.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
      start.time=25, max.time=64.999, data = nhis_male)
    saveRDS(aalen_phy_m_assump1_tvc2565, "Output/Assumptions/Interaction/phy/aalen_phy_m_assump1_tvc2565.rds");               # Save model results
    pdf("Output/Assumptions/Interaction/phy/aalen_phy_m_assump1_tvc2565.pdf"); plot(aalen_phy_m_assump1_tvc2565); dev.off()   # save plot 
    aalen_phy_m_assump1_tvc2565 <-readRDS("Output/Assumptions/Interaction/phy/aalen_phy_m_assump1_tvc2565.rds")               # load model results
    summary(aalen_phy_m_assump1_tvc2565)
    # RESULT: ??
    
    # Iteration 1 - Ages 65+
    aalen_phy_m_assump1_tvc65up <- aalen(Surv(bl_age, end_age, allcause_death) ~ phy_act3.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
      start.time=65, data = nhis_male)
    saveRDS(aalen_phy_m_assump1_tvc65up, "Output/Assumptions/Interaction/phy/aalen_phy_m_assump1_tvc65up.rds");               # Save model results
    pdf("Output/Assumptions/Interaction/phy/aalen_phy_m_assump1_tvc65up.pdf"); plot(aalen_phy_m_assump1_tvc65up); dev.off()   # save plot 
    aalen_phy_m_assump1_tvc65up <-readRDS("Output/Assumptions/Interaction/phy/aalen_phy_m_assump1_tvc65up.rds")               # load model results
    summary(aalen_phy_m_assump1_tvc65up)
    # RESULT: ??
    
    
    
### Sensitivity 4: Multiplicative Hazard Ratios---------------------------------------------------------------------------------------------------

#### Alcohol * Education

      # No survey weights 
      cox_alc_noweight <- coxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*alcohol5v2.factor + 
                                female.factor + married.factor + ethnicity.factor, data=nhis)
            summary(cox_alc_noweight)
      
      
      # Using survey weights
      cox_alc_weights <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*alcohol5v2.factor + 
                                  female.factor + married.factor + ethnicity.factor, design=nhis_svyWeights)
              summary(cox_allcause_alc_edu)


                # Assessing proportional hazards
                cox_alc_weights_check <- cox.zph(cox_alc_weights)
                print(cox_alc_weights_check)
                plot(cox_alc_weights_check, col = "red")
                ggcoxzph(cox_alc_weights_check, var=1, point.alpha = 0.5)
                ggcoxzph(cox_alc_weights_check, var=2)
                ggcoxzph(cox_alc_weights_check, var=3)
                ggcoxzph(cox_alc_weights_check, var=4)
                ggcoxzph(cox_alc_weights_check, var=5)

                



#### Smoking * Education
                
          # No survey weights 
          cox_smk_noweight <- coxph(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor*alcohol5v2.factor + 
                                    female.factor + married.factor + ethnicity.factor, data=nhis)
                summary(cox_smk_noweight)
                
                
          # Using survey weights
          cox_smk_weights <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor*alcohol5v2.factor + 
                                female.factor + married.factor + ethnicity.factor, design=nhis_svyWeights)
                summary(cox_allcause_smk_edu)
                
                
                # Assessing proportional hazards
                cox_smk_weights_check <- cox.zph(cox_smk_weights)
                print(cox_smk_weights_check)
                plot(cox_smk_weights_check, col = "red")






#### BMI * Education
                
        # No survey weights 
        cox_bmi_noweight <- coxph(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor*alcohol5v2.factor + 
                              female.factor + married.factor + ethnicity.factor, data=nhis)
                summary(cox_bmi_noweight)
                
                
        # Using survey weights
        cox_bmi_weights <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor*alcohol5v2.factor + 
                                  female.factor + married.factor + ethnicity.factor, design=nhis_svyWeights)
                summary(cox_allcause_bmi_edu)
                
                
                # Assessing proportional hazards
                cox_bmi_weights_check <- cox.zph(cox_bmi_weights)
                print(cox_bmi_weights_check)
                plot(cox_bmi_weights_check, col = "red")




                
#### Physical Activity * Education
                
          # No survey weights 
          cox_phy_noweight <- coxph(Surv(bl_age, end_age, allcause_death) ~ phy_act3.factor*alcohol5v2.factor + 
                                female.factor + married.factor + ethnicity.factor, data=nhis)
                summary(cox_phy_noweight)
                
                
          # Using survey weights
          cox_phy_weights <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor*alcohol5v2.factor + 
                                female.factor + married.factor + ethnicity.factor, design=nhis_svyWeights)
                summary(cox_allcause_phy_edu)
                
                
                # Assessing proportional hazards
                cox_phy_weights_check <- cox.zph(cox_phy_weights)
                print(cox_phy_weights_check)
                plot(cox_phy_weights_check, col = "red")

### Sensitivity 5: Alcohol x BMI ----------------------------------------------------------------------
               
# Model 1:Adjusted for age (as timescale), education, ethnicity/race, and marital status. 
        ## All Participants
        bmi_alc_m1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol5v2.factor) + 
                          const(edu.factor) + const(married.factor) + female.factor + ethnicity.factor,  data = nhis, robust=0)
                summary(bmi_alc_m1)
                
                
                # Joint effects
                nhis$interact <- interaction(nhis$bmi_cat.factor, nhis$alcohol5v2.factor)
                table(nhis$interact)
                bmi_alc_m1b <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(interact) + 
                    const(edu.factor) + const(married.factor) + female.factor + ethnicity.factor,  data = nhis, robust=0)
                summary(bmi_alc_m1b)
              
                count(nhis, interact)
                nhis %>%
                  filter(allcause_death == 1) %>%
                  count(interact)
                
        ## WOMEN
        bmi_alc_m1_f <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol5v2.factor) + 
                  const(edu.factor) + const(married.factor) + ethnicity.factor,  data = nhis_female, robust=0)
                summary(bmi_alc_m1_f)
                
                
        ## MEN
        bmi_alc_m1_m <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol5v2.factor) + 
                    const(edu.factor) + const(married.factor) + ethnicity.factor,  data = nhis_male, robust=0)
                summary(bmi_alc_m1_m)
                
                
                
                
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
                
                
                
                
# Model 2a:Adjusted for age (as timescale), education, and marital status. 
        ## All Participants
        bmi_alc_m2a <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol5v2.factor) + 
                    const(ethnicity.factor) + const(married.factor) + female.factor,  data = nhis, robust=0)
                summary(bmi_alc_m2a)
                
        ## WOMEN
        bmi_alc_m2a_f <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol5v2.factor) + 
                    const(edu.factor) + const(married.factor),  data = nhis_female, robust=0)
                summary(bmi_alc_m2a_f)
                
        ## MEN
        bmi_alc_m2a_m <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol5v2.factor) + 
                    const(edu.factor) + const(married.factor),  data = nhis_male, robust=0)
                summary(bmi_alc_m2a_m)
                
                
                
                
# Model 2b:Adjusted for age (as timescale), ethnicity, and marital status. 
        ## All Participants
        bmi_alc_m2b <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol5v2.factor) + 
                ethnicity.factor + const(married.factor) + female.factor,  data = nhis, robust=0)
                summary(bmi_alc_m2b)
                
        ## WOMEN
        bmi_alc_m2b_f <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol5v2.factor) + 
                ethnicity.factor + const(married.factor),  data = nhis_female, robust=0)
                summary(bmi_alc_m2b_f)
                
                
        ## MEN
        bmi_alc_m2b_m <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol5v2.factor) + 
                ethnicity.factor + const(married.factor),  data = nhis_male, robust=0)
                summary(bmi_alc_m2b_m)
                
                
                
                
                
# Model 2c:Adjusted for age (as timescale) and marital status. 
        ## All Participants
        bmi_alc_m2c <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol5v2.factor) + 
                    const(married.factor) + female.factor,  data = nhis, robust=0)
                summary(bmi_alc_m2c)
                
        ## WOMEN
        bmi_alc_m2c_f <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol5v2.factor) + 
                    const(married.factor),  data = nhis_female, robust=0)
                summary(bmi_alc_m2c_f)
                
                
        ## MEN
        bmi_alc_m2c_m <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol5v2.factor) + 
                    const(married.factor),  data = nhis_male, robust=0)
                summary(bmi_alc_m2c_m)
                
                
                
                
# Model 3: Stratified: Non-Hispanic White Only; Adjusted for age (as timescale), education, and marital status. 
       nhis_white <- filter(nhis, ethnicity.factor=="Non-Hispanic White")
       nhis_female_white <- filter(nhis_female, ethnicity.factor=="Non-Hispanic White")
       nhis_male_white <- filter(nhis_male, ethnicity.factor=="Non-Hispanic White")
                
        ## All Participants
        bmi_alc_m2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol5v2.factor) + 
                    const(edu.factor) + const(married.factor) + female.factor,  data = nhis_white, robust=0)
                summary(bmi_alc_m2)
                
        ## WOMEN
        bmi_alc_m2_f <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol5v2.factor) + 
                    const(edu.factor) + const(married.factor),  data = nhis_female_white, robust=0)
                summary(bmi_alc_m2_f)
                
                
        ## MEN
        bmi_alc_m2_m <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol5v2.factor) + 
                    const(edu.factor) + const(married.factor),  data = nhis_male_white, robust=0)
                summary(bmi_alc_m2_m)
                
                
                