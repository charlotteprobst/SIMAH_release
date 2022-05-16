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
library(broom)


# Specify the data and output file locations
data    <- "C:/Users/klajd/Documents/2021 CAMH/SIMAH/SIMAH_workplace/nhis/Processed data/"    # Location of data
output  <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nhis/Race x Lifestyle/Output/"      # Location of figures/tables

# Load data (participants aged 25-85 years)
nhis_all    <- readRDS (paste0(data, "nhis_all18_85.rds"))
nhis        <- readRDS (paste0(data, "nhis18_85.rds"))
nhis_male   <- filter(nhis, female==0)
nhis_female <- filter(nhis, female==1)


# DESCRIPTIVE STATISTICS 
# Table 1: Participant characteristics - STRATIFIED BY SEX
tab1 <-CreateTableOne(vars= c("age", "yrs_followup","allcause_death.factor", 
                              "alcohol5v2.factor","smoking4.factor",  "bmi_cat.factor", "phy_act3.factor",
                               "edu.factor",  "married.factor"),
                      factorVars = c("allcause_death.factor",
                                      "alcohol5v2.factor", "smoking4.factor", "bmi_cat.factor", "phy_act3.factor",
                                     "edu.factor",  "married.factor"), 
                      strata= c("ethnicity.factor", "female.factor"), addOverall = TRUE, data=nhis)
  table1_v1 <- print(tab1, noSpaces = TRUE, catDigits = 0, contDigits = 1, printToggle = FALSE, test=FALSE)  # Shows sample size and %
  table1_v2 <- print(tab1, noSpaces = TRUE, catDigits = 0, contDigits = 1, printToggle = FALSE, test=FALSE, format="p") # shows % only
  write.csv(table1_v1, file = file.path(output, "Table1 Demographics_V1.csv"))
  write.csv(table1_v2, file = file.path(output, "Table1 Demographics_V2.csv"))
  kableone(table1_v1)

  
# Person years and death rate: tstop = person years; event = # events; rate = events per person year 
survRate(Surv(yrs_followup, allcause_death)~female.factor+ethnicity.factor, data=nhis) %>%
  mutate(group = paste(female.factor, ethnicity.factor, sep=", "),
         rate_10000py = rate * 10000) %>% remove_rownames() %>%
  dplyr::select (group, tstop, rate_10000py) %>% t()%>%
  write.csv(file = file.path(output, "Table1 Demographics_V3.csv"))
  
survRate(Surv(yrs_followup, allcause_death)~ethnicity.factor, data=nhis) %>%
  mutate(rate_10000py = round(rate * 10000, 0)) %>% 
  dplyr::select (ethnicity.factor, rate_10000py)



# Race/ethnicity proportions
nhis %>% group_by (ethnicity.factor) %>%
  summarise (n = n()) %>%
  mutate (perc = round(n / sum(n) *100 ,0))

nhis %>% filter (ethnicity.factor == "Other") %>% 
  group_by (ethnicity_detail) %>%
  summarise (n = n()) %>%
  mutate (perc = round(n / sum(n) *100 ,0))


# Figure 2a: Survival plot 
ggsurvplot_facet(fit = survfit(Surv(bl_age, end_age, allcause_death) ~ ethnicity.factor, data = nhis), 
  data=nhis, facet.by="female.factor", censor = FALSE,xlim = c(18, 100), 
  conf.int = TRUE, 
  xlab = "Age (years)", 
  ylab = "Overall survival probability")

      # Age Medium Survival:
      survfit(Surv(bl_age, end_age, allcause_death) ~ ethnicity, data = nhis)
      survfit(Surv(bl_age, end_age, allcause_death) ~ ethnicity, data = nhis_female)
      survfit(Surv(bl_age, end_age, allcause_death) ~ ethnicity, data = nhis_male)

      
# Figure 2b: Survival plot 
ggsurvplot_facet(fit = survfit(Surv(bl_age, end_age, allcause_death) ~ ethnicity_detail, data = nhis), 
  data=nhis, facet.by="female.factor", censor = FALSE,xlim = c(18, 100), 
  conf.int = TRUE, 
  xlab = "Age (years)", 
  ylab = "Overall survival probability") 
      
      # Age Medium Survival:
      survfit(Surv(bl_age, end_age, allcause_death) ~ ethnicity_detail, data = nhis)
      survfit(Surv(bl_age, end_age, allcause_death) ~ ethnicity_detail, data = nhis_female)
      survfit(Surv(bl_age, end_age, allcause_death) ~ ethnicity_detail, data = nhis_male)
    
      