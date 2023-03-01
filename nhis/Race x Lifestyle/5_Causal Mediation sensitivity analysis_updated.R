# SIMAH Restricted-access Data
# Causal Mediation  


# LOAD DATA AND SET FILE LOCATIONS

# load libraries
library(tidyverse)  # data management
library(timereg)    # additive survival models
library(VGAM)       # multinomial regression, needed for causal mediation
library(MASS)       # needed for causal mediation functions
library(knitr)      # formatted table


# Specify the data and output file locations

data <- "~/Dropbox/Mac/Desktop/Public Health Institute/Alcohol Research Group/Bash/SIMAH/SIMAH_workplace/nhis/Restricted access data/Data/"
output <- "~/Dropbox/Mac/Desktop/Public Health Institute/Alcohol Research Group/Bash/SIMAH/SIMAH_workplace/nhis/Restricted access data/Causal mediation/Race x Lifestyle/CausMed/18-85 years old/Sensitivity analysis 2 for 4 mediators/"

source("5_Causal Mediation functions.R")

 
# Load data
nhis18_85 <- readRDS(file.path(data, "nhis18_85.rds"))
nhis18_male <- readRDS(file.path(data, "nhis18_male.rds")) 
nhis18_female <- readRDS(file.path(data, "nhis18_female.rds"))



# OBJECTIVE 2: Causal Mediation

# The causal mediation analyses involves four steps:
# 1) Fit separate multinomial logistic regressions with each mediator (M1, M2, M3, and M4) as the outcome.
# 2) Create copies of the dataset to account for all possible combinations of the exposure and mediators. 
# 3) Using the expanded dataset, calculate weights for each mediator using the predicted probabilities from Step 1. 
# 4) Fit a marginal structural model using Aalen additive hazards with the weight as weight and the id as a cluster level; this ensures thatrobust standard errors are calculated. The model with robust variance and resampling (robust=TRUE) was not used because of computation limiations.  

# For more details and theoretical justification/description see:
# Lange et al. 2014 https//doi.org/10.1093/aje/kwt270
# Lange et al. 2012 https//doi.org/10.1093/aje/kwr525
# Lange et al. 2011 https//doi.org/10.1097/EDE.0b013e31821c680c


# Data Preparation ------------------------------------------------------------------------------------------------------------------
CMed_race3_prep(nhis18_female) %>% saveRDS(paste0(output, "expandedData_fem.rds"))
CMed_race3_prep(nhis18_male)   %>% saveRDS(paste0(output, "expandedData_male.rds"))


# Run Analyses, WOMEN ----------------------------------------------------------------------------------------------------------------

# Load data
expandedData <- readRDS(file.path(output, "expandedData_fem.rds")) %>%
  filter(complete.cases(ID, bl_age, end_age, allcause_death, A.race, race_M1.alc, race_M2.smk, race_M3.bmi, race_M4.phy,
                        married2, srvy_yr, weightM))

hist(expandedData$weightM)

# Run model
CMed_f <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(A.race) * const(race_M1.alc) + 
                                                        const(A.race) * const(race_M2.smk) +
                                                        const(A.race) * const(race_M3.bmi) +
                                                        const(A.race) * const(race_M4.phy) +
                                                        const(married2) + const(srvy_yr),  # adjust for survey year as continuous variable 
                          data=expandedData, weights=expandedData$weightM, clusters=expandedData$ID, robust=0)  
                
saveRDS(CMed_f, file.path(output, "CMed_f.rds")) # Save model results      



model_coefficients <- coef(CMed_f) %>%
  row.names() %>% 
  as.data.frame() %>%
  rownames_to_column()

print("model coefficients"); print(model_coefficients)
print("Black"); print(Black)
print("Hispanic"); print(Hispanic)


# Load model and view results
CMed_model <- readRDS(file.path(output, "CMed_f.rds"))  # load model (if needed)
Black <- c(1, 3, 5, 7, 9, 13, 17, 21, 25)             # List the coefficients of interest 
Hispanic <- c(2, 4, 6, 8, 10, 16, 20, 24, 28)  
format_CMed (CMed_model, Black) %>% kable()    # print formatted results

CMed_women_black <- format_CMed (CMed_model, Black)
CMed_women_hispanic <- format_CMed (CMed_model, Hispanic)
CMed_women <- rbind(CMed_women_black, CMed_women_hispanic)



# Run Analyses, MEN ----------------------------------------------------------------------------------------------------------------

# Load data
expandedData <- readRDS(file.path(output, "expandedData_male.rds"))


# Run Model
CMed_m <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(A.race) * const(race_M1.alc) +
                                                        const(A.race) * const(race_M2.smk) +
                                                        const(A.race) * const(race_M3.bmi) +
                                                        const(A.race) * const(race_M4.phy) +
                                                        const(married2) + const(srvy_yr),  
                          data=expandedData, weights=expandedData$weightM, clusters=expandedData$ID, robust=0)

saveRDS(CMed_m, file.path(output, "CMed_m.rds"))  # Save model results     

# Load model and view results
CMed_model <- readRDS(file.path(output, "CMed_m.rds"))  # load model (if needed)
Black <- c(1, 3, 5, 7, 9, 13, 17, 21, 25)             # List the coefficients of interest 
Hispanic <- c(2, 4, 6, 8, 10, 16, 20, 24, 28)  
format_CMed (CMed_model, Black) %>% kable()    # print formatted results
format_CMed (CMed_model, Hispanic) %>% kable() 

CMed_men_black <- format_CMed (CMed_model, Black)
CMed_men_hispanic <- format_CMed (CMed_model, Hispanic)
CMed_men <- rbind(CMed_men_black, CMed_men_hispanic)


# COMBINE Results

colnames(CMed_men)   <- paste0("men_", colnames(CMed_men))
colnames(CMed_women) <- paste0("women_", colnames(CMed_women))
CMed_table <- cbind(CMed_men, CMed_women) %>% rename(term = men_label) %>% 
  mutate(race = rep(c("Black", "Hispanic"), each = 11) ) %>%
  relocate(race) %>% dplyr::select(-women_label)
  
CMed_table
write.csv(CMed_table, file=paste0(output, "Table2 Causal Mediation results.csv")) # save results
