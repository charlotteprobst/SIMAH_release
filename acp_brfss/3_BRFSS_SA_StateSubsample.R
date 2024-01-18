# ----------------------------------------------------------------
# ----------------------------------------------------------------
## Project: SIMAH  
## Title: Sensitivity analysis 1 BRFSS Sunday sales ban
## State: all US states
## Author: Carolin Kilian
## Start Date: 07/05/2023
# ----------------------------------------------------------------
# ----------------------------------------------------------------

# --------------------------------------------------------------------------------------

rm(list = ls())

# ----------------------------------------------------------------
# ----------------------------------------------------------------
# LIBARIES
# ----------------------------------------------------------------
# ----------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(openxlsx)
library(lme4)
library(nlme)
library(ggeffects)
library(beepr)
library(ggpubr)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# LOAD DATA
# ----------------------------------------------------------------

setwd("/Users/carolinkilian/Desktop/SIMAH_workplace/")
DATE <- 20240112

# BRFSS 
data <- as.data.frame(readRDS("acp_brfss/20230925_brfss_clean.RDS"))

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# PREPARE DATA
# ----------------------------------------------------------------

# exclude States that had no ban during entire study period

select_states <- data %>% 
  group_by(State, YEAR, sunsalesban_di) %>% summarise() %>%
  group_by(State) %>% 
  mutate(policy.total = sum(sunsalesban_di),
         SunSalesPolicy = ifelse(policy.total == 0, "Sunday sales were never banned",
                                 ifelse(policy.total == 19, "Sunday sales were always banned", 
                                        ifelse(policy.total > 0 & policy.total < 19, "Sunday sales ban was repealed", NA)))) %>% 
  filter(SunSalesPolicy == "Sunday sales were never banned") %>% pull(State) %>% unique

# define age groups and factor variables, z-standardize unemplyoment rate
pdat <- data %>%
  
  filter(!State %in% select_states) %>% 
  
  # select random subsample (for now)
  # sample_frac(0.1) %>% 
  
  # prepare data
  mutate_at(c("race_eth", "sex_recode", "education_summary", "marital_status", 
              "drinkingstatus", "controlstate", "drinkculture", "sunsalesban", "State"), as.factor) %>%
  mutate(education_summary = factor(education_summary, levels = c("College", "SomeC", "LEHS")),
         sunsalesban_exloc = factor(ifelse(sunsalesban_exloc == 1, "ban", ifelse(sunsalesban_exloc == 0, "no ban", NA)),
                                    levels = c("no ban", "ban")),
         z.unemp.rate = (log(unemp.rate) - mean(log(unemp.rate))) / sd(log(unemp.rate))) # across the total sample

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# DRINKING ALCOHOL VS. ABSTAINING: MIXED-EFFECT MODELS
# ----------------------------------------------------------------

# MEN

drinkstatus.m <- glm(drinkingstatus ~ sunsalesban_di*education_summary + 
                         drinkculture + controlstate + z.unemp.rate +
                         race_eth + marital_status + age_gr + YEAR*State,
                       data = pdat[pdat$sex_recode == "Men",], family = binomial(link = "logit"))
#summary(drinkstatus.m)

# WOMEN 

drinkstatus.w <- glm(drinkingstatus ~ sunsalesban_di*education_summary + 
                         drinkculture + controlstate + z.unemp.rate +
                         race_eth + marital_status + age_gr + YEAR*State,
                       data = pdat[pdat$sex_recode == "Women",], family = binomial(link = "logit"))
#summary(drinkstatus.w)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# DAILY ALCOHOL CONSUMPTION: MIXED-EFFECT MODELS
# ----------------------------------------------------------------

pdat <- pdat %>% filter(gramsperday > 0) %>% mutate(gpd_log = log(gramsperday))

# MEN

gpd.m <- glm(gpd_log ~ sunsalesban_di*education_summary + 
               drinkculture + z.unemp.rate + controlstate + 
               race_eth + marital_status + age_gr + YEAR*State, 
             data = pdat[pdat$sex_recode == "Men",])
             #method = "ML") # due to convergence error set to ML
#summary(gpd.m)

# WOMEN

gpd.w <- glm(gpd_log ~ sunsalesban_di*education_summary + 
               drinkculture + z.unemp.rate + controlstate + 
               race_eth + marital_status + age_gr + YEAR*State, 
             data = pdat[pdat$sex_recode == "Women",])
#summary(gpd.w)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# CATEGORY III DRINKING PREVALENCE: MIXED-EFFECT MODELS
# ----------------------------------------------------------------

pdat <- pdat %>% filter(gramsperday > 0) %>% 
  mutate(alccat3 = ifelse(sex_recode == "Men" & gramsperday <= 60, 0,
                          ifelse(sex_recode == "Men" & gramsperday > 60, 1,
                                 ifelse(sex_recode == "Women" & gramsperday <= 40, 0,
                                        ifelse(sex_recode == "Women" & gramsperday > 40, 1, NA)))))

# MEN

alccat.m <- glm(alccat3 ~ sunsalesban_exloc*education_summary + 
                    drinkculture + controlstate + z.unemp.rate +
                    race_eth + marital_status + age_gr + YEAR,
                  data = pdat[pdat$sex_recode == "Men",], family = binomial(link = "logit"))
#summary(alccat.m)

# --------------------------------------------------------------------------------------

# WOMEN

alccat.w <- glm(alccat3 ~ sunsalesban_exloc*education_summary + 
                    drinkculture + controlstate + z.unemp.rate +
                    race_eth + marital_status + age_gr,
                  data = pdat[pdat$sex_recode == "Women",], family = binomial(link = "logit"))
#summary(alccat.w)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# EXPORT: TABLE
# ----------------------------------------------------------------

out.drink.m <- as.data.frame(coef(summary(drinkstatus.m))) %>% 
  mutate(estimate = exp(Estimate), 
         conf.low = exp(Estimate - 1.96*`Std. Error`),
         conf.high = exp(Estimate + 1.96*`Std. Error`)) %>%
  rownames_to_column(var = "term") %>% rename("p.value" = `Pr(>|z|)`) %>%
  select(c("term", "estimate", "conf.low", "conf.high", "p.value"))
out.drink.w <- as.data.frame(coef(summary(drinkstatus.w))) %>% 
  mutate(estimate = exp(Estimate), 
         conf.low = exp(Estimate - 1.96*`Std. Error`),
         conf.high = exp(Estimate + 1.96*`Std. Error`)) %>%
  rownames_to_column(var = "term") %>% rename("p.value" = `Pr(>|z|)`) %>%
  select(c("term", "estimate", "conf.low", "conf.high", "p.value"))
out.drink <- merge(out.drink.m, out.drink.w, by = "term", all = T, suffix = c(".men", ".women"))

out.gpd.m <- as.data.frame(coef(summary(gpd.m))) %>% 
  mutate(conf.low = Estimate - 1.96*`Std. Error`,
         conf.high = Estimate + 1.96*`Std. Error`) %>%
  rownames_to_column(var = "term") %>% rename("estimate" = "Estimate", "p.value" = `Pr(>|t|)`) %>%
  select(c("term", "estimate", "conf.low", "conf.high", "p.value"))
out.gpd.w <- as.data.frame(coef(summary(gpd.w))) %>% 
  mutate(conf.low = Estimate - 1.96*`Std. Error`,
         conf.high = Estimate + 1.96*`Std. Error`) %>%
  rownames_to_column(var = "term") %>% rename("estimate" = "Estimate", "p.value" = `Pr(>|t|)`) %>%
  select(c("term", "estimate", "conf.low", "conf.high", "p.value"))
out.gpd <- merge(out.gpd.m, out.gpd.w, by = "term", all = T, suffix = c(".men", ".women"))

out.gpdexp.m <- as.data.frame(exp(gpd.m$coefficients)) %>% tibble::rownames_to_column(var = "term") %>% rename("exp.estimate" = "exp(gpd.m$coefficients)")
out.gpdexp.w <- as.data.frame(exp(gpd.w$coefficients)) %>% tibble::rownames_to_column(var = "term") %>% rename("exp.estimate" = "exp(gpd.w$coefficients)")
out.gpdexp <- merge(out.gpdexp.m, out.gpdexp.w, by = "term", all = T, suffix = c(".men", ".women"))

out.alccat.m <- as.data.frame(coef(summary(alccat.m))) %>% 
  mutate(estimate = exp(Estimate), 
         conf.low = exp(Estimate - 1.96*`Std. Error`),
         conf.high = exp(Estimate + 1.96*`Std. Error`)) %>%
  rownames_to_column(var = "term") %>% rename("p.value" = `Pr(>|z|)`) %>%
  select(c("term", "estimate", "conf.low", "conf.high", "p.value"))
out.alccat.w <- as.data.frame(coef(summary(alccat.w))) %>% 
  mutate(estimate = exp(Estimate), 
         conf.low = exp(Estimate - 1.96*`Std. Error`),
         conf.high = exp(Estimate + 1.96*`Std. Error`)) %>%
  rownames_to_column(var = "term") %>% rename("p.value" = `Pr(>|z|)`) %>%
  select(c("term", "estimate", "conf.low", "conf.high", "p.value"))
out.alccat <- merge(out.alccat.m, out.alccat.w, by = "term", all = T, suffix = c(".men", ".women"))

list_out <- list("AlcUse" = out.drink, "GPD" = out.gpd, "GPDexp" = out.gpdexp, "CategoryIII" = out.alccat)
write.xlsx(list_out, file = paste0("acp_brfss/outputs/", DATE, "_SA_SubSampStates_output_glm.xlsx"), rowNames = FALSE)
