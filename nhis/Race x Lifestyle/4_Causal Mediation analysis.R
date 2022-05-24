
# Race x Lifestyle Differential Vulnerability & Exposure Project
# Causal Mediation File 


# LOAD DATA AND SET FILE LOCATIONS

# load libraries
library(tidyverse)  # data management
library(timereg)    # additive survival models
library(VGAM)       # multinomial regression, needed for causal mediation
library(MASS)       # needed for causal mediation functions
library(foreach)    # to bootstrap
library(parallel)   # for parallel processing
library(doParallel) # for parallel processing
#library(doMC)       # parallel processing in Linux

memory.limit(size=1e+13)

# Personal computer; specify locations 
data   <- "C:/Users/klajd/Documents/2021 CAMH/SIMAH/SIMAH_workplace/nhis/Processed data/"            # Location of data
model <- "C:/Users/klajd/Documents/2021 CAMH/SIMAH/SIMAH_workplace/nhis/Race x Lifestyle/CausMed/"  # Location of model output
output <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nhis/Race x Lifestyle/Output/"
source("Function - CausalMed.R")


# SCC; ; specify locations 
# setwd("/external/mgmt3/imaging/scratch/Imhpr/kpuka/nhis/")
# data    <- "Data/"
# model  <- "model/"
# source("Function - CausalMed.R")


# Load data
nhis        <- readRDS (paste0(data, "nhis18_85.rds"))
nhis_male   <- filter(nhis, female==0)
nhis_female <- filter(nhis, female==1)


# Aalen model - Sequentially adding covariates

# Function to format results
aalen_10000py <- function(model) {
  library(knitr)
  mu <- model$gamma
  var <- as.matrix(diag(model$var.gamma))
  combined <- as.data.frame(cbind(mu, var)) %>%
    rownames_to_column("variable") %>% 
    filter(!str_detect(variable, "srvy_yr")) %>% # hide coefficients related to survey year
    mutate (lower = round((estimate - (1.96 * sqrt(V2)))*10000,1), # CI * 10,000 to get result per 10,000py
            upper = round((estimate + (1.96 * sqrt(V2)))*10000,1), # CI * 10,000 to get result per 10,000py
            mu = round(estimate*10000,1),                          # mu * 10,000 to get result per 10,000py 
            mu_CI = paste0(mu, " (",lower,", ", upper, ")"),
            var = sub(".*)", "", variable)) %>%
    dplyr::select(var, mu_CI)
  return(combined)
}
  
# WOMEN
w_mod1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(ethnicity.factor), data=nhis_female, robust=0) 
w_mod2 <- update(w_mod1, . ~ . + const(married.factor) + const(factor(srvy_yr)))
w_mod3 <- update(w_mod2, . ~ . + const(edu.factor))
w_mod4 <- update(w_mod3, . ~ . + const(alcohol5v2.factor) + const(smoking4.factor) + const(bmi_cat.factor) + const(phy_act3.factor))

full_join(aalen_10000py(w_mod1), aalen_10000py(w_mod2), by="var") %>%
  full_join(aalen_10000py(w_mod3), by="var") %>%
  full_join(aalen_10000py(w_mod4), by="var") %>%
  kable()


# MEN
m_mod1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(ethnicity.factor), data=nhis_male, robust=0) 
m_mod2 <- update(w_mod1, . ~ . + const(married.factor) + const(factor(srvy_yr)))
m_mod3 <- update(w_mod2, . ~ . + const(edu.factor))
m_mod4 <- update(w_mod3, . ~ . + const(alcohol5v2.factor) + const(smoking4.factor) + const(bmi_cat.factor) + const(phy_act3.factor))

full_join(aalen_10000py(m_mod1), aalen_10000py(m_mod2), by="var") %>%
  full_join(aalen_10000py(m_mod3), by="var") %>%
  full_join(aalen_10000py(m_mod4), by="var") %>%
  kable()



# Set up parallel processing ************************************************************************************************

# foreach::getDoParWorkers()                # Identify # of cores that will be used
# registerDoMC(5)                         # Linux: Specify number of cores to use  
# cl <- makeCluster(4, outfile = "log.txt") # Windows: Specify number of cores to use  
# registerDoParallel(cl)                    # Windows: Specify number of cores to use  
# foreach::getDoParWorkers()  # Identify # of cores that will be used



# WOMEN: Bootstrap Causal Mediation *****************************************************************************************

set.seed(1235)

# Analysis
# CMed_boot_w <- bootstrap_CMed(nhis_female, reps=1000, prop=0.20)  # Run analysis using bootstrap
# saveRDS(CMed_boot_w, file.path(model, "CMed_boot_w.rds"))        # Save bootstrap results
CMed_boot_w <- readRDS(file.path(model, "CMed_boot_w.rds"))        # load bootstrap results


# Results 
CMed_women <- as.data.frame(do.call(cbind, CMed_boot_w)) %>%
    format_CMed()                                                    # Compute CI and format results 
CMed_women                                                           # print results 



# MEN: Bootstrap Causal Mediation ********************************************************************************************

set.seed(1235)

# Analysis
# CMed_boot_m <- bootstrap_CMed(nhis_male, reps=1000, prop=0.20)  # Run analysis using bootstrap
# saveRDS(CMed_boot_m, file.path(model, "CMed_boot_m.rds"))       # Save bootstrap results
CMed_boot_m <- readRDS(file.path(model, "CMed_boot_m.rds"))       # load bootstrap results

# Results 
CMed_men <- as.data.frame(do.call(cbind, CMed_boot_m)) %>%
  format_CMed()                                                    # Compute CI and format results 
CMed_men                                                           # print results 


# COMBINE Results

colnames(CMed_men)   <- paste0("men_", colnames(CMed_men))
colnames(CMed_women) <- paste0("women_", colnames(CMed_women))
CMed_table <- cbind(CMed_men, CMed_women) %>% rename(race = men_race, term = men_term)
CMed_table <- CMed_table[c(1:4, 7:8)]
CMed_table
write.csv(CMed_table, file=paste0(output, "Table2 Causal Mediation results.csv")) # save results

