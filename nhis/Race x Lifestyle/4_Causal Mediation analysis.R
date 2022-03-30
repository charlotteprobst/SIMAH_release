
# Race x Lifestyle Differential Vulnerability & Exposure Project
# Objective 2: Causal Mediation File 


# LOAD DATA AND SET FILE LOCATIONS

# load libraries
library(tidyverse)  # data management
library(timereg)    # additive survival models
library(VGAM)       # multinomial regression, needed for causal mediation
library(MASS)       # needed for causal mediation functions
library(boot)       # bootstrap
memory.limit(size=1e+13)

# Specify the data and output file locations
data   <- "C:/Users/klajd/Documents/2021 CAMH/SIMAH/SIMAH_workplace/nhis/Processed data/"            # Location of data
output <- "C:/Users/klajd/Documents/2021 CAMH/SIMAH/SIMAH_workplace/nhis/Race x Lifestyle/CausMed/"  # Location of model assumptions
subset <- "C:/Users/klajd/Documents/2021 CAMH/SIMAH/SIMAH_workplace/nhis/Race x Lifestyle/CausMed/Results on subset of full data/"
source("Function - AalenModel.R")
source("Function - CausalMed.R")


# Load data
nhis        <- readRDS (file.path(data, "nhis.rds"))
nhis_male   <- readRDS (file.path(data, "nhis_male.rds"))
nhis_female <- readRDS (file.path(data, "nhis_female.rds"))

# Run Causal Mediation Analyses  ----------------------------------------------------------------------------------------------------------------

# WOMEN ********************************************************************************************************

# Data preparation
CMed_prep(nhis_female, prop=0.25) %>% saveRDS(paste0(output, "expandedData_fem.rds"))

# Load data and run model
expandedData <-readRDS(file.path(output, "expandedData_fem.rds"))

hist(expandedData$weightM) # Histogram of weights 

CMed_f <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(A.race) * const(race_M1.alc) + 
                                                        const(A.race) * const(race_M2.smk) +
                                                        const(A.race) * const(race_M3.bmi) +
                                                        const(A.race) * const(race_M4.phy) +
                                                        const(married) + const(factor(edu)) + const(factor(srvy_yr)),
            data=expandedData, weights=expandedData$weightM, clusters=expandedData$ID, robust=0)  

# Save model results
saveRDS(CMed_f, file.path(output, "CMed_f.rds"))       




# MEN ********************************************************************************************************

# Data preparation
CMed_prep(nhis_male, prop=0.25)   %>% saveRDS(paste0(output, "expandedData_male.rds"))


# Load data and run model
expandedData <- readRDS(file.path(output, "expandedData_male.rds")) 

hist(expandedData$weightM)# Histogram of weights 

CMed_m <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(A.race) * const(race_M1.alc) + 
                                                        const(A.race) * const(race_M2.smk) +
                                                        const(A.race) * const(race_M3.bmi) +
                                                        const(A.race) * const(race_M4.phy) +
                                                        const(married) + const(factor(edu)) + const(factor(srvy_yr)),
              data=expandedData, weights=expandedData$weightM, clusters=expandedData$ID, robust=0)  

saveRDS(CMed_m, file.path(output, "CMed_m.rds"))  # Save model results     


# Format Causal Mediation results  ----------------------------------------------------------------------------

# Load model
CMed_model_f <-readRDS(file.path(subset, "CMed_f.rds"))  
CMed_model_m <-readRDS(file.path(subset, "CMed_m.rds"))  

# List the coefficients of interest for each race/ethinicity
Black    <- c(1,4,7,10,13,36,45,54,63)
Hispanic <- c(2,5,8,11,14,40,49,58,67)
Other    <- c(3,6,9,12,15,44,53,62,71)

# Get and format results 
black_f <- format_CMed (CMed_model_f, Black)
black_m <- format_CMed (CMed_model_m, Black)
hisp_f  <- format_CMed (CMed_model_f, Hispanic)
hisp_m  <- format_CMed (CMed_model_m, Hispanic)
other_f <- format_CMed (CMed_model_f, Other)
other_m <- format_CMed (CMed_model_m, Other)

black <- full_join(black_m, black_f, by="label") %>% add_row(.before=1, label = "RESULTS FOR BLACK (ref=White)")
hisp  <- full_join(hisp_m, hisp_f, by="label")   %>% add_row(.before=1, label = "RESULTS FOR HISPANIC (ref=White)")
other <- full_join(other_m, other_f, by="label") %>% add_row(.before=1, label = "RESULTS FOR OTHER (ref=White)")

final <- rbind (black, hisp, other) %>% view





# Bootstrap Causal Mediation ---------------------------------------------------------

single <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(alcohol5v2) + 
    const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male, robust=0)
coef(single)

  

source("Function - CausalMed.R")
aalen.boot <- boot(data=nhis_male, statistic=aalen_bootstrap, R=10, prop=0.50)
str(aalen.boot)

aalen.boot
boot.ci(aalen.boot, type="normal")

CMed.boot <- boot(data=nhis_male, statistic=CMed_bootstrap, R=5, prop=0.01)
CMed.boot



