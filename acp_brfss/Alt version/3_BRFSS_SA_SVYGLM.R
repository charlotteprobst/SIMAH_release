# ----------------------------------------------------------------
# ----------------------------------------------------------------
## Project: SIMAH  
## Title: Sensitivity analysis 2 BRFSS Sunday sales ban
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
library(grid)
library(Matrix)
library(survey)
library(openxlsx)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# LOAD DATA
# ----------------------------------------------------------------

setwd("/Users/carolinkilian/Desktop/SIMAH_workplace/")
DATE <- 20240118

# BRFSS 
data <- as.data.frame(readRDS("acp_brfss/20230925_brfss_clean.RDS"))

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# PREPARE DATA
# ----------------------------------------------------------------

# define age groups and factor variables, z-standardize unemplyoment rate
pdat <- data %>%
  
  # select random subsample (for testing)
  #sample_frac(0.001) %>%
  
  # prepare data
  mutate_at(c("race_eth", "sex_recode", "education_summary", "marital_status", 
              "drinkingstatus", "controlstate", "drinkculture", "sunsalesban", "State"), as.factor) %>%
  mutate(education_summary = factor(education_summary, levels = c("College", "SomeC", "LEHS")),
         sunsalesban_di = factor(ifelse(sunsalesban_di == 1, "ban", ifelse(sunsalesban_di == 0, "no ban", NA)),
                                 levels = c("no ban", "ban")),
         z.unemp.rate = (log(unemp.rate) - mean(log(unemp.rate))) / sd(log(unemp.rate)), # across the total sample
         sampling = factor(ifelse(YEAR < 2011, 0, ifelse(YEAR >= 2011, 1, NA))), # change in sampling methodology
         gpd_log = ifelse(gramsperday > 0, log(gramsperday), ifelse(gramsperday == 0, NA, NA)),
         alccat3 = factor(ifelse(sex_recode == "Men" & gramsperday > 0 & gramsperday <= 60, 0,
                          ifelse(sex_recode == "Men" & gramsperday > 60, 1,
                                 ifelse(sex_recode == "Women" & gramsperday > 0 & gramsperday <= 40, 0,
                                        ifelse(sex_recode == "Women" & gramsperday > 40, 1, 
                                               ifelse(gramsperday == 0, NA, NA))))))) 
  
# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# SET SURVEY DESIGN
# ----------------------------------------------------------------

options(survey.lonely.psu = "adjust")
svydat <- svydesign(ids = ~X_PSU, strata = ~interaction(X_STSTR, YEAR), 
                    weights = ~final_sample_weight_adj, nest = T, data = pdat)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# DRINKING ALCOHOL VS. ABSTAINING: MIXED-EFFECT MODELS
# ----------------------------------------------------------------

# MEN

drinkstatus.m <- svyglm(drinkingstatus ~ sunsalesban_di*education_summary + 
                        drinkculture + controlstate + z.unemp.rate +
                        race_eth + marital_status + age_gr + sampling + YEAR*State,
                      design = subset(svydat, sex_recode == "Men"),
                      family = quasibinomial(link = "logit"))
#summary(drinkstatus.m)

# WOMEN 

drinkstatus.w <- svyglm(drinkingstatus ~ sunsalesban_di*education_summary + 
                          drinkculture + controlstate + z.unemp.rate +
                          race_eth + marital_status + age_gr + sampling + YEAR*State,
                        design = subset(svydat, sex_recode == "Women"),
                        family = quasibinomial(link = "logit"))
#summary(drinkstatus.w)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# DAILY ALCOHOL CONSUMPTION: MIXED-EFFECT MODELS
# ----------------------------------------------------------------

# MEN

gpd.m <- svyglm(gpd_log ~ sunsalesban_di*education_summary + 
                  drinkculture + controlstate + z.unemp.rate +
                  race_eth + marital_status + age_gr + sampling + YEAR*State,
                design = subset(svydat, sex_recode == "Men" & gramsperday > 0),
                family = gaussian(link = "identity"))
#summary(gpd.m)

# WOMEN

gpd.w <- svyglm(gpd_log ~ sunsalesban_di*education_summary + 
                  drinkculture + controlstate + z.unemp.rate +
                  race_eth + marital_status + age_gr + sampling + YEAR*State,
                design = subset(svydat, sex_recode == "Women" & gramsperday > 0),
                family = gaussian(link = "identity"))
#summary(gpd.w)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# CATEGORY III DRINKING PREVALENCE: MIXED-EFFECT MODELS
# ----------------------------------------------------------------

# MEN

alccat.m <- svyglm(alccat3 ~ sunsalesban_di*education_summary +
                     drinkculture + controlstate + z.unemp.rate +
                     race_eth + marital_status + age_gr + sampling + YEAR*State,
                   design = subset(svydat, sex_recode == "Men" & gramsperday > 0),
                   family = quasibinomial(link = "logit"))
#summary(alccat.m)

# WOMEN

alccat.w <- svyglm(alccat3 ~ sunsalesban_di*education_summary +
                     drinkculture + controlstate + z.unemp.rate +
                     race_eth + marital_status + age_gr + sampling + YEAR*State,
                   design = subset(svydat, sex_recode == "Women" & gramsperday > 0),
                   family = quasibinomial(link = "logit"))
#summary(alccat.w)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# EXPORT: TABLE
# ----------------------------------------------------------------

out.drink.m <- as.data.frame(coef(summary(drinkstatus.m))) %>% 
  mutate(estimate = exp(Estimate), 
         conf.low = exp(Estimate - 1.96*`Std. Error`),
         conf.high = exp(Estimate + 1.96*`Std. Error`)) %>%
  rownames_to_column(var = "term") %>% rename("p.value" = `Pr(>|t|)`) %>%
  select(c("term", "estimate", "conf.low", "conf.high", "p.value"))
out.drink.w <- as.data.frame(coef(summary(drinkstatus.w))) %>% 
  mutate(estimate = exp(Estimate), 
         conf.low = exp(Estimate - 1.96*`Std. Error`),
         conf.high = exp(Estimate + 1.96*`Std. Error`)) %>%
  rownames_to_column(var = "term") %>% rename("p.value" = `Pr(>|t|)`) %>%
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

out.alccat.m <- as.data.frame(coef(summary(alccat.m))) %>% 
  mutate(estimate = exp(Estimate), 
         conf.low = exp(Estimate - 1.96*`Std. Error`),
         conf.high = exp(Estimate + 1.96*`Std. Error`)) %>%
  rownames_to_column(var = "term") %>% rename("p.value" = `Pr(>|t|)`) %>%
  select(c("term", "estimate", "conf.low", "conf.high", "p.value"))
out.alccat.w <- as.data.frame(coef(summary(alccat.w))) %>% 
  mutate(estimate = exp(Estimate), 
         conf.low = exp(Estimate - 1.96*`Std. Error`),
         conf.high = exp(Estimate + 1.96*`Std. Error`)) %>%
  rownames_to_column(var = "term") %>% rename("p.value" = `Pr(>|t|)`) %>%
  select(c("term", "estimate", "conf.low", "conf.high", "p.value"))
out.alccat <- merge(out.alccat.m, out.alccat.w, by = "term", all = T, suffix = c(".men", ".women"))

list_out <- list("AlcUse" = out.drink, "GPD" = out.gpd, "CategoryIII" = out.alccat)
write.xlsx(list_out, file = paste0("acp_brfss/outputs/", DATE, "_BRFSS_SA_SVY_output_svyglm.xlsx"), rowNames = FALSE)
