# ----------------------------------------------------------------
# ----------------------------------------------------------------
## Project: SIMAH  
## Title: Select BRFSS data from original BRFSS for ACP analysis
## State: all US states
## Author: Carolin Kilian
## Start Date: 07/05/2023
# ----------------------------------------------------------------
# ----------------------------------------------------------------

# --------------------------------------------------------------------------------------

rm(list = ls())

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# JOIN DATA FILES
# ----------------------------------------------------------------

data <- readRDS("/Users/carolinkilian/Desktop/SIMAH_workplace/brfss/processed_data/brfss_full_selected.RDS")
gc()
data <- do.call(rbind, data)

data <- data %>% filter(State!="Guam") %>% filter(State!="Puerto Rico") %>% filter(State!="territories")

data$StateOrig <- data$State

acp_brfss_data <- data %>% filter(State != "DC") %>%
  select(YEAR, State, 
         final_sample_weight, X_STSTR, X_PSU,
         sex_recode, age_var, education_summary, race_eth, marital_status,
         drinkingstatus, alc_frequency, quantity_per_occasion, gramsperday)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# EXPORT
# ----------------------------------------------------------------

saveRDS(acp_brfss_data, "/Users/carolinkilian/Desktop/SIMAH_workplace/brfss/processed_data/ACP_brfss_full.RDS")


