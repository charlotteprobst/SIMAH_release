# ----------------------------------------------------------------
# ----------------------------------------------------------------
## Project: SIMAH  
## Title: ALCOHOL POLICY BRFSS 
## Aim: BRFSS analysis
## Author: Carolin Kilian
## Start Date: 03/01/2023
# ----------------------------------------------------------------
# ----------------------------------------------------------------

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# ----------------------------------------------------------------
# LIBARIES
# ----------------------------------------------------------------
# ----------------------------------------------------------------

library(tidyverse)
library(data.table)
library(dplyr)
library(openxlsx)
library(lme4)
library(gee)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# LOAD DATA
# ----------------------------------------------------------------

rm(list = ls())
setwd("/Users/carolinkilian/Desktop/SIMAH_workplace/")
DATE <- 20230103

datBRFSS <- data.table(readRDS("brfss/processed_data/BRFSS_upshifted_2000_2020_final.RDS"))
datBRFSS <- datBRFSS[State != "USA"]
datBRFSS[datBRFSS$State == "DC"]$State <- "District of Columbia"
  
datAV <- data.table(read.xlsx("policies/LIBERAL_control_outlet_hours.xlsx", na.strings = c("NA", "")))
datAV <- copy(datAV[year >= 2000,.(state, year, no_sun_sales, controlstate)])

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# PREPARE / CHECK DATA
# ----------------------------------------------------------------

data <- merge(datBRFSS, datAV, by.x = c("State", "YEAR"), by.y = c("state", "year"), all.x = T)
data[is.na(no_sun_sales), table(State, YEAR)]

# cap estimates >200 GPD
summary(data$gramsperday_orig)
summary(data[gramsperday_upshifted > 200]$gramsperday_orig)
data[, GPD_upshifted_capped := ifelse(gramsperday_upshifted > 200, 200, gramsperday_upshifted)]

# GPD distribution
ggplot(data = data[data$GPD_upshifted_capped > 0]) +
  geom_histogram(aes(x = GPD_upshifted_capped, color = sex_recode), binwidth = 1)

# Subgroups
data[, sex_education := paste0(sex_recode, "_", education_summary)]
data[, sex_raceth := paste0(sex_recode, "_", race_eth)]
data[, sex_education_raceth := paste0(sex_recode, "_", education_summary, "_", race_eth)]

# GPD distribution by subgroups
ggplot(data = data[data$GPD_upshifted_capped > 0]) +
  geom_histogram(aes(x = GPD_upshifted_capped, color = as.factor(YEAR)), binwidth = 1) + 
  facet_wrap(vars(sex_education_raceth), scales = "free")

# GPD distribution by subgroups and state
ggplot(data = data[data$GPD_upshifted_capped > 0 & race_eth %like% "Other"]) +
  geom_histogram(aes(x = GPD_upshifted_capped, color = as.factor(sex_education)), binwidth = 1) + 
  facet_wrap(vars(State), scales = "free")

data[GPD_upshifted_capped > 0, table(State, sex_education_raceth)]

# GPD distribution over time by subgroups
ggplot(data = data[data$GPD_upshifted_capped > 0]) +
  geom_smooth(aes(x = GPD_upshifted_capped, y = YEAR, color = as.factor(State))) + 
  facet_wrap(vars(sex_education_raceth), scales = "free")

# GPD distribution over time by state
ggplot(data = data[data$GPD_upshifted_capped > 0]) +
  geom_smooth(aes(x = GPD_upshifted_capped, y = YEAR, color = as.factor(sex_education_raceth))) + 
  facet_wrap(vars(as.factor(State)), scales = "free")

# prepare variables
summary(data$age)
data[, age_group := as.factor(ifelse(age_var < 35, 1, ifelse(age_var > 34 & age_var < 65, 2, ifelse(age_var > 64, 3, NA))))]
data[, table(age_var, age_group)]

data[, no_sun_sales := as.factor(no_sun_sales)]
data[, controlstate := as.factor(controlstate)]
data[, marital_status := as.factor(marital_status)]

data[GPD_upshifted_capped > 0, logGDP := log(GPD_upshifted_capped)]


# ----------------------------------------------------------------
# PRELIMINARY ANALYSIS - no tax data
# ----------------------------------------------------------------

ggplot(data = data[data$GPD_upshifted_capped > 0]) +
  geom_histogram(aes(x = logGDP), binwidth = 1)
  
lmer(logGDP ~ no_sun_sales + age_var + marital_status + controlstate + (1 + YEAR | State), 
      data = data[GPD_upshifted_capped > 0], weights = final_sample_weight)



