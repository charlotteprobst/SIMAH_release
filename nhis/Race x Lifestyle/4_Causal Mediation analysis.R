
# Race x Lifestyle Differential Vulnerability & Exposure Project
# Causal Mediation File 


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


library(rsample)
set.seed(1230)
  
fit_CMed <- function(splits, prop=1){

  dat <- analysis(splits) # need for bootstrapping
  expandedData <- CMed_prep(dat, prop) # Causal mediation data preparation
  
  # Run model
  model <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(A.race) * const(race_M1.alc) + 
                                                              const(A.race) * const(race_M2.smk) +
                                                              const(A.race) * const(race_M3.bmi) +
                                                              const(A.race) * const(race_M4.phy) +
                                                              const(married) + const(factor(edu)) + const(factor(srvy_yr)),
                  data=expandedData, weights=expandedData$weightM, clusters=expandedData$ID, robust=0)  
  
  # List the coefficients of interest for each race/ethinicity
  Black    <- c(1,4,7,10,13,36,45,54,63)
  Hispanic <- c(2,5,8,11,14,40,49,58,67)
  Other    <- c(3,6,9,12,15,44,53,62,71)
  
  # Get and format results 
  results_black <- format_CMed_boot (model, Black)  
  results_hisp  <- format_CMed_boot (model, Hispanic)
  results_other <- format_CMed_boot (model, Other)
  
  final <- rbind (results_black, results_hisp, results_other)
  
  final
}

CMed_bt <- bootstraps(nhis_male, times = 10) %>%
  mutate (models = map(splits, ~ fit_CMed(.x, prop=0.05)))

saveRDS(CMed_bt, file.path(output, "CMed_bt.rds"))

CMed_bt

CMed_bt <- readRDS(file.path(output, "CMed_bt.rds"))

CMed_bt$models[[2]] %>%view

p_ints <- int_pctl(CMed_bt, models)
CMed_reults <- p_ints %>%
  dplyr::select(-.alpha, -.method) %>% 
  separate(term, into=c("label", "type", "race"), sep="\\__") %>% 
  arrange (race, type, label) %>%
  pivot_wider(names_from = "type", values_from = c(.lower, .estimate, .upper)) %>% 
  mutate (deaths_10000py = round(.estimate_estimate * 10000, 1),
          deaths_lower = round(.lower_estimate * 10000, 1),
          deaths_upper = round(.upper_estimate * 10000, 1),
          prop = round(.estimate_med_prop * 100, 0),
          prop_lower = round(.lower_med_prop * 100, 0),
          prop_upper = round(.upper_med_prop * 100, 0),
          deaths_10000py_ci = paste0(deaths_10000py, " (", deaths_lower, ", ", deaths_upper, ")"),
          prop_ci = paste0(prop, " (", prop_lower, ", ", prop_upper, ")")) %>%
  dplyr::select(label, race, deaths_10000py_ci, prop_ci) %>%
  add_row(.before=1, label = "RESULTS FOR BLACK (ref=White)") %>%
  add_row(.before=13, label = "RESULTS FOR HISPANIC (ref=White)") %>%
  add_row(.before=25, label = "RESULTS FOR OTHER NON-HISPANIC (ref=White)") %>%view
  



library(boot)

fit_CMed2 <- function(data, indices, prop=1){
  
  dat <- data[indices,] # need for bootstrapping
  expandedData <- CMed_prep(dat, prop) # Causal mediation data preparation
  
  # Run model
  model <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(A.race) * const(race_M1.alc) + 
      const(A.race) * const(race_M2.smk) +
      const(A.race) * const(race_M3.bmi) +
      const(A.race) * const(race_M4.phy) +
      const(married) + const(factor(edu)) + const(factor(srvy_yr)),
    data=expandedData, weights=expandedData$weightM, clusters=expandedData$ID, robust=0)  
  
  # List the coefficients of interest for each race/ethinicity
  Black    <- c(1,4,7,10,13,36,45,54,63)
  Hispanic <- c(2,5,8,11,14,40,49,58,67)
  Other    <- c(3,6,9,12,15,44,53,62,71)
  
  # Get and format results 
  results_black <- format_CMed_boot (model, Black)  
  results_hisp  <- format_CMed_boot (model, Hispanic)
  results_other <- format_CMed_boot (model, Other)
  
  final <- rbind (results_black, results_hisp, results_other) %>%
    dplyr::select(estimate)
  
  one <- slice(final, 1)
  two <- slice(final, 1)
  
  c(one, two)
  
}

CMed <- boot(nhis_male, statistic=fit_CMed2, R=3, prop=.01)
CMed[[1]]
boot.ci(CMed, type="perc")

plot(CMed)

library(foreach)
CMed_boot_manual <- function(data, reps, prop) {
  
  foreach(i = 1:reps, .combine="rbind") %do% {
    
      expandedData <- CMed_prep(data, prop) # Causal mediation data preparation
      
      # Run model
      model <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(A.race) * const(race_M1.alc) + 
          const(A.race) * const(race_M2.smk) +
          const(A.race) * const(race_M3.bmi) +
          const(A.race) * const(race_M4.phy) +
          const(married) + const(factor(edu)) + const(factor(srvy_yr)),
        data=expandedData, weights=expandedData$weightM, clusters=expandedData$ID, robust=0)  
      
      # List the coefficients of interest for each race/ethinicity
      Black    <- c(1,4,7,10,13,36,45,54,63)
      Hispanic <- c(2,5,8,11,14,40,49,58,67)
      Other    <- c(3,6,9,12,15,44,53,62,71)
      
      # Get and format results 
      results_black <- format_CMed_boot (model, Black)  
      results_hisp  <- format_CMed_boot (model, Hispanic)
      results_other <- format_CMed_boot (model, Other)
      
      final <- rbind (results_black, results_hisp, results_other) %>%
        dplyr::select(term, estimate) %>%
        t() %>%as.data.frame() 
      
  } 
}

boot2<-CMed_boot_manual(nhis_male, reps=3, prop=0.01)
boot2%>%
  rownames_to_column(var="type") %>%
  filter(grepl("estimate", type)) %>%
  dplyr::select(-type) %>%
  mutate(across(everything(), as.numeric)) %>%
  dplyr::select(V1) -> V1

str(V1)
V1 <- unlist(V1)

quantile(V1, prob=c(0.025, 0.975))



x <- round(runif(1000, 0, 100))
str(x)


view(V1)

boot%>%view



boot %>%
  group_by(term) %>%
  quantile(estimate, prob=c(0.025, 0.975)) %>%view
  

quantile(boot$estimate, prob=c(0.025, 0.975)) %>%view






