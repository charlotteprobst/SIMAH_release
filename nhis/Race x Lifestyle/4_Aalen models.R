
# Race x Lifestyle Differential Vulnerability & Exposure Project
# Causal Mediation File 


# LOAD DATA AND SET FILE LOCATIONS

# load libraries
library(tidyverse)  # data management
library(timereg)    # additive survival models
library(VGAM)       # multinomial regression, needed for causal mediation
library(MASS)       # needed for causal mediation functions


# Personal computer; specify locations 
data   <- "C:/Users/klajd/Documents/2021 CAMH/SIMAH/SIMAH_workplace/nhis/Processed data/"            # Location of data
output <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nhis/Race x Lifestyle/Output/"


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
  write.csv(file=paste0(output, "Aalen Models, Women.csv"), na = "") # save results



# MEN
m_mod1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(ethnicity.factor), data=nhis_male, robust=0) 
m_mod2 <- update(w_mod1, . ~ . + const(married.factor) + const(factor(srvy_yr)))
m_mod3 <- update(w_mod2, . ~ . + const(edu.factor))
m_mod4 <- update(w_mod3, . ~ . + const(alcohol5v2.factor) + const(smoking4.factor) + const(bmi_cat.factor) + const(phy_act3.factor))

full_join(aalen_10000py(m_mod1), aalen_10000py(m_mod2), by="var") %>%
  full_join(aalen_10000py(m_mod3), by="var") %>%
  full_join(aalen_10000py(m_mod4), by="var") %>%
  write.csv(file=paste0(output, "Aalen Models, men.csv"), na = "") # save results

