

### SIMAH - NHIS Data
### SES x Health Behavior interaction and mediation


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
library(biostat3)   # survRate command


# Set the working directory and other file locations
# # Personal Computer:
kp <- "C:/Users/klajd/OneDrive/SIMAH"
setwd(kp)
data    <- "SIMAH_workspace/nhis/Data"
output  <- "SIMAH_workspace/nhis/SES x Behavior/Output/"


# Load data
nhis        <- readRDS (file.path(data, "nhis.rds"))
nhis_male   <- readRDS (file.path(data, "nhis_male.rds"))
nhis_female <- readRDS (file.path(data, "nhis_female.rds"))






# DESCRIPTIVE STATISTICS 

# Participant characteristics - STRATIFIED BY SEX
tab1 <-CreateTableOne(vars= c("age", "yrs_followup","allcause_death.factor", 
                              "alcohol5v2.factor","smoking4.factor",  "bmi_cat.factor", "phy_act3.factor",
                               "ethnicity.factor", "income.factor",  "married.factor"),
                      factorVars = c("allcause_death.factor",
                                      "alcohol5v2.factor", "smoking4.factor", "bmi_cat.factor", "phy_act3.factor",
                                     "ethnicity.factor", "income.factor",  "married.factor"), 
                      strata= c("edu.factor", "female.factor"), addOverall = TRUE, data=nhis)
  table1_v1 <- print(tab1, noSpaces = TRUE, catDigits = 0, contDigits = 1, printToggle = FALSE, test=FALSE)  # Shows sample size and %
  table1_v2 <- print(tab1, noSpaces = TRUE, catDigits = 0, contDigits = 1, printToggle = FALSE, test=FALSE, format="p") # shows % only
  write.csv(table1_v1, file = file.path(output, "Table1 Demographics_V1.csv"))
  write.csv(table1_v2, file = file.path(output, "Table1 Demographics_V2.csv"))
  kableone(table1_v1)

  
  

# Participant characteristics - AMONG ALL PARTICIPANTS
tab_e1 <-CreateTableOne(vars= c("female.factor", "age", "yrs_followup","allcause_death.factor", 
      "alcohol5v2.factor","smoking4.factor",  "bmi_cat.factor", "phy_act3.factor",
      "ethnicity.factor", "income.factor",  "married.factor"),
  factorVars = c("female.factor","allcause_death.factor", 
      "alcohol5v2.factor", "smoking4.factor", "bmi_cat.factor", "phy_act3.factor",
      "ethnicity.factor", "income.factor",  "married.factor"), 
  strata= c("edu.factor"), addOverall = TRUE, data=nhis)
table_e1_v1 <- print(tab_e1, noSpaces = TRUE, catDigits = 0, contDigits = 1, printToggle = FALSE, test=FALSE)  # Shows sample size and %
table_e1_v2 <- print(tab_e1, noSpaces = TRUE, catDigits = 0, contDigits = 1, printToggle = FALSE, test=FALSE, format="p") # shows % only
write.csv(table_e1_v1, file = file.path(output, "Table_e1 Demographics_V1.csv"))
write.csv(table_e1_v2, file = file.path(output, "Table_e1 Demographics_V2.csv"))
kableone(table_e1_v1)
  
  

# Person years and death rate: tstop = person years; event = # events; rate = events per person year 
survRate(Surv(yrs_followup, allcause_death)~1, data=nhis)          # overall 
survRate(Surv(yrs_followup, allcause_death)~edu, data=nhis)        # for each SES category 
survRate(Surv(yrs_followup, allcause_death)~female+edu, data=nhis) # for each SES * Sex category 
 


skim(nhis$alcohol_daily_grams)

nhis %>%
  group_by(female.factor, edu.factor) %>%
  dplyr::select(alcohol_daily_grams) %>%
  skim()

nhis %>%
  group_by(edu.factor) %>%
  dplyr::select(alcohol_daily_grams) %>%
  skim()


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


      
      
# Descriptives of HED (heavy episodic drinking)
tab_hed <-CreateTableOne(vars= c("hed.factor"),
                        factorVars = c("hed.factor"), 
                        strata= c("edu.factor", "female.factor"), addOverall = TRUE, data=nhis)
table_hed <- print(tab_hed, noSpaces = TRUE, catDigits = 0, contDigits = 1, printToggle = FALSE, test=FALSE)
kableone(table_hed)
      
      