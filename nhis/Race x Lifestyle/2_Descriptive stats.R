

# Race x Lifestyle Differential Vulnerability & Exposure Project
# Descriptive Statistics

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


# Specify the data and output file locations
data    <- "C:/Users/klajd/Documents/2021 CAMH/SIMAH/SIMAH_workplace/nhis/Processed data/"   # Location of data
output  <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nhis/Race x Lifestyle/Output/"      # Location of figures/tables


# Load data
nhis        <- readRDS (paste0(data, "nhis.rds"))
nhis_male   <- readRDS (paste0(data, "nhis_male.rds"))
nhis_female <- readRDS (paste0(data, "nhis_female.rds"))



# DESCRIPTIVE STATISTICS 
# Table 1: Participant characteristics - STRATIFIED BY SEX
tab1 <-CreateTableOne(vars= c("age", "yrs_followup","allcause_death.factor", 
                              "alcohol5v2.factor","smoking4.factor",  "bmi_cat.factor", "phy_act3.factor",
                               "edu.factor", "income4.factor",  "married.factor"),
                      factorVars = c("allcause_death.factor",
                                      "alcohol5v2.factor", "smoking4.factor", "bmi_cat.factor", "phy_act3.factor",
                                     "edu.factor", "income4.factor",  "married.factor"), 
                      strata= c("ethnicity.factor", "female.factor"), data=nhis)
  table1_v1 <- print(tab1, noSpaces = TRUE, catDigits = 0, contDigits = 1, printToggle = FALSE, test=FALSE)  # Shows sample size and %
  table1_v2 <- print(tab1, noSpaces = TRUE, catDigits = 0, contDigits = 1, printToggle = FALSE, test=FALSE, format="p") # shows % only
  write.csv(table1_v1, file = file.path(output, "Table1 Demographics_V1.csv"))
  write.csv(table1_v2, file = file.path(output, "Table1 Demographics_V2.csv"))
  kableone(table1_v1)

  
# Person years and death rate: tstop = person years; event = # events; rate = events per person year 
survRate(Surv(yrs_followup, allcause_death)~1, data=nhis)          # overall 
survRate(Surv(yrs_followup, allcause_death)~ethnicity, data=nhis)        # for each SES category 
survRate(Surv(yrs_followup, allcause_death)~female+ethnicity, data=nhis) # for each SES * Sex category 
 




# Figure 2: Survival plot 
ggsurvplot_facet(fit = survfit(Surv(bl_age, end_age, allcause_death) ~ ethnicity, data = nhis), 
  data=nhis, facet.by="female.factor", censor = FALSE,xlim = c(25, 100), 
  conf.int = TRUE, 
  legend.labs = c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Non-Hispanic Other"),
  xlab = "Age (years)", 
  ylab = "Overall survival probability") 

      # Age Medium Survival:
      survfit(Surv(bl_age, end_age, allcause_death) ~ ethnicity, data = nhis)
      survfit(Surv(bl_age, end_age, allcause_death) ~ ethnicity, data = nhis_female)
      survfit(Surv(bl_age, end_age, allcause_death) ~ ethnicity, data = nhis_male)


    
      