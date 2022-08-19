
# Race x Lifestyle Differential Vulnerability & Exposure Project
# Causal Mediation File 


# LOAD DATA AND SET FILE LOCATIONS

# load libraries
library(tidyverse)  # data management
library(timereg)    # additive survival models
library(VGAM)       # multinomial regression, needed for causal mediation
library(MASS)       # needed for causal mediation functions
memory.limit(size=1e+13)


# Personal computer; specify locations 
data   <- "C:/Users/klajd/Documents/2021 CAMH/SIMAH/SIMAH_workplace/nhis/Processed data/"            # Location of data
output <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nhis/Race x Lifestyle/Output/"
source("Function - CausalMed sensitivity 2.R")

# Load data
nhis        <- readRDS (paste0(data, "nhis18_85.rds"))
nhis_male   <- filter(nhis, female==0)
nhis_female <- filter(nhis, female==1)


# MEN: Causal mediation with one mediator: 
alcohol_men <- CMed_oneVar_noINT (nhis_male, alcohol5v2, smoking4, bmi_cat, phy_act3, 3) %>% mutate (mediator = "Alcohol") %>% relocate (mediator)
smoking_men <- CMed_oneVar_noINT (nhis_male, smoking4, alcohol5v2, bmi_cat, phy_act3, 1) %>% mutate (mediator = "Smoking") %>% relocate (mediator)
bmi_men     <- CMed_oneVar_noINT (nhis_male, bmi_cat, alcohol5v2, smoking4, phy_act3, 2) %>% mutate (mediator = "BMI") %>% relocate (mediator)
phy_men     <- CMed_oneVar_noINT (nhis_male, phy_act3, alcohol5v2, smoking4, bmi_cat, 3) %>% mutate (mediator = "Physical") %>% relocate (mediator)

CMed_men <- rbind(alcohol_men, smoking_men, bmi_men, phy_men)
view(CMed_men)


# WOMEN: Causal mediation with one mediator: 
alcohol_women <- CMed_oneVar_noINT (nhis_female, alcohol5v2, smoking4, bmi_cat, phy_act3, 3) %>% mutate (mediator = "Alcohol") %>% relocate (mediator)
smoking_women <- CMed_oneVar_noINT (nhis_female, smoking4, alcohol5v2, bmi_cat, phy_act3, 1) %>% mutate (mediator = "Smoking") %>% relocate (mediator)
bmi_women     <- CMed_oneVar_noINT (nhis_female, bmi_cat, alcohol5v2, smoking4, phy_act3, 2) %>% mutate (mediator = "BMI") %>% relocate (mediator)
phy_women     <- CMed_oneVar_noINT (nhis_female, phy_act3, alcohol5v2, smoking4, bmi_cat, 3) %>% mutate (mediator = "Physical") %>% relocate (mediator)

CMed_women <- rbind(alcohol_women, smoking_women, bmi_women, phy_women)
view(CMed_women)


# Combined
colnames(CMed_men)   <- paste0("men_", colnames(CMed_men))
colnames(CMed_women) <- paste0("women_", colnames(CMed_women))

CMed_table <- cbind(CMed_men, CMed_women) 
CMed_table <- CMed_table[c(1,3:5,9:10)] %>% 
  rename(mediator = men_mediator, label = men_label)

view(CMed_table)
write.csv(CMed_table, file=paste0(output, "Table_e3 Causal Mediation, one mediator, no interaction.csv")) # save results

