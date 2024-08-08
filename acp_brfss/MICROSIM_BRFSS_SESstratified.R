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
DATE <- 20240205

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
         z.unemp.rate = (log(unemp.rate) - mean(log(unemp.rate))) / sd(log(unemp.rate)))

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# DRINKING ALCOHOL VS. ABSTAINING: FIXED-EFFECT MODELS
# ----------------------------------------------------------------

# MEN LEHS

drinkstatus.MLEHS <- glm(drinkingstatus ~ sunsalesban_di + 
                         drinkculture + controlstate + z.unemp.rate +
                         race_eth + marital_status + age_gr + State,
                       data = pdat[pdat$sex_recode == "Men" & pdat$education_summary == "LEHS",], 
                     family = binomial(link = "logit"))

# MEN SOME COLLEGE

drinkstatus.MSomeC <- glm(drinkingstatus ~ sunsalesban_di + 
                            drinkculture + controlstate + z.unemp.rate +
                            race_eth + marital_status + age_gr + State,
                          data = pdat[pdat$sex_recode == "Men" & pdat$education_summary == "SomeC",], 
                          family = binomial(link = "logit"))

# MEN COLLEGE 

drinkstatus.MCollege <- glm(drinkingstatus ~ sunsalesban_di + 
                              drinkculture + controlstate + z.unemp.rate +
                              race_eth + marital_status + age_gr + State,
                            data = pdat[pdat$sex_recode == "Men" & pdat$education_summary == "College",], 
                            family = binomial(link = "logit"))


# WOMEN LEHS

drinkstatus.WLEHS <- glm(drinkingstatus ~ sunsalesban_di + 
                           drinkculture + controlstate + z.unemp.rate +
                           race_eth + marital_status + age_gr + State,
                         data = pdat[pdat$sex_recode == "Women" & pdat$education_summary == "LEHS",], 
                         family = binomial(link = "logit"))

# WOMEN SOME COLLEGE

drinkstatus.WSomeC <- glm(drinkingstatus ~ sunsalesban_di + 
                            drinkculture + controlstate + z.unemp.rate +
                            race_eth + marital_status + age_gr + State,
                          data = pdat[pdat$sex_recode == "Women" & pdat$education_summary == "SomeC",], 
                          family = binomial(link = "logit"))

# WOMEN COLLEGE 

drinkstatus.WCollege <- glm(drinkingstatus ~ sunsalesban_di + 
                              drinkculture + controlstate + z.unemp.rate +
                              race_eth + marital_status + age_gr + State,
                            data = pdat[pdat$sex_recode == "Women" & pdat$education_summary == "College",], 
                            family = binomial(link = "logit"))

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# DAILY ALCOHOL CONSUMPTION: FIXED-EFFECT MODELS
# ----------------------------------------------------------------

pdat_NoCat3 <- pdat %>% filter(gramsperday > 0) %>% 
  mutate(alccat3 = ifelse(sex_recode == "Men" & gramsperday <= 60, 0,
                          ifelse(sex_recode == "Men" & gramsperday > 60, 1,
                                 ifelse(sex_recode == "Women" & gramsperday <= 40, 0,
                                        ifelse(sex_recode == "Women" & gramsperday > 40, 1, NA)))),
         gpd_log = log(gramsperday)) %>%
  filter(alccat3==0)

# MEN LEHS

gpd.MLEHS <- glm(gpd_log ~ sunsalesban_di + 
               drinkculture + z.unemp.rate + controlstate + 
               race_eth + marital_status + age_gr + State, 
             data = pdat_NoCat3[pdat_NoCat3$sex_recode == "Men" & pdat_NoCat3$education_summary == "LEHS",])

# MEN SomeC

gpd.MSomeC <- glm(gpd_log ~ sunsalesban_di + 
                    drinkculture + z.unemp.rate + controlstate + 
                    race_eth + marital_status + age_gr + State, 
                  data = pdat_NoCat3[pdat_NoCat3$sex_recode == "Men" & pdat_NoCat3$education_summary == "SomeC",])

# MEN College

gpd.MCollege <- glm(gpd_log ~ sunsalesban_di + 
                      drinkculture + z.unemp.rate + controlstate + 
                      race_eth + marital_status + age_gr + State, 
                    data = pdat_NoCat3[pdat_NoCat3$sex_recode == "Men" & pdat_NoCat3$education_summary == "College",])

# WOMEN LEHS

gpd.WLEHS <- glm(gpd_log ~ sunsalesban_di + 
                   drinkculture + z.unemp.rate + controlstate + 
                   race_eth + marital_status + age_gr + State, 
                 data = pdat_NoCat3[pdat_NoCat3$sex_recode == "Women" & pdat_NoCat3$education_summary == "LEHS",])

# WOMEN SomeC

gpd.WSomeC <- glm(gpd_log ~ sunsalesban_di + 
                    drinkculture + z.unemp.rate + controlstate + 
                    race_eth + marital_status + age_gr + State, 
                  data = pdat_NoCat3[pdat_NoCat3$sex_recode == "Women" & pdat_NoCat3$education_summary == "SomeC",])

# WOMEN College

gpd.WCollege <- glm(gpd_log ~ sunsalesban_di + 
                      drinkculture + z.unemp.rate + controlstate + 
                      race_eth + marital_status + age_gr + State, 
                    data = pdat_NoCat3[pdat_NoCat3$sex_recode == "Women" & pdat_NoCat3$education_summary == "College",])

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# CATEGORY III DRINKING PREVALENCE: FIXED-EFFECT MODELS
# ----------------------------------------------------------------

pdat_Cat3 <- pdat %>% filter(gramsperday > 0) %>% 
  mutate(alccat3 = ifelse(sex_recode == "Men" & gramsperday <= 60, 0,
                          ifelse(sex_recode == "Men" & gramsperday > 60, 1,
                                 ifelse(sex_recode == "Women" & gramsperday <= 40, 0,
                                        ifelse(sex_recode == "Women" & gramsperday > 40, 1, NA)))),
         gpd_log = log(gramsperday)) %>%
  filter(alccat3==1)

# MEN

alccat.m <- glm(gpd_log ~ sunsalesban_di + education_summary + 
                    drinkculture + controlstate + z.unemp.rate +
                    race_eth + marital_status + age_gr + State,
                  data = pdat_Cat3[pdat_Cat3$sex_recode == "Men",])

# WOMEN

alccat.w <- glm(gpd_log ~ sunsalesban_di + education_summary + 
                    drinkculture + controlstate + z.unemp.rate +
                    race_eth + marital_status + age_gr + State,
                  data = pdat_Cat3[pdat_Cat3$sex_recode == "Women",])

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# EXPORT: TABLE
# ----------------------------------------------------------------

out.drink.MLEHS <- as.data.frame(coef(summary(drinkstatus.MLEHS))) %>% 
  mutate(estimate = exp(Estimate), 
         se = `Std. Error`) %>%
  rownames_to_column(var = "term") %>% rename("p.value" = `Pr(>|z|)`) %>%
  select(c("term", "estimate", "se", "p.value")) %>%
  filter(term == "sunsalesban_di") %>% mutate(term = "Men_LEHS")
out.drink.MSomeC <- as.data.frame(coef(summary(drinkstatus.MSomeC))) %>% 
  mutate(estimate = exp(Estimate), 
         se = `Std. Error`) %>%
  rownames_to_column(var = "term") %>% rename("p.value" = `Pr(>|z|)`) %>%
  select(c("term", "estimate", "se", "p.value")) %>%
  filter(term == "sunsalesban_di") %>% mutate(term = "Men_SomeC")
out.drink.MCollege <- as.data.frame(coef(summary(drinkstatus.MCollege))) %>% 
  mutate(estimate = exp(Estimate), 
         se = `Std. Error`) %>%
  rownames_to_column(var = "term") %>% rename("p.value" = `Pr(>|z|)`) %>%
  select(c("term", "estimate", "se", "p.value")) %>%
  filter(term == "sunsalesban_di") %>% mutate(term = "Men_College")

out.drink.WLEHS <- as.data.frame(coef(summary(drinkstatus.WLEHS))) %>% 
  mutate(estimate = exp(Estimate), 
         se = `Std. Error`) %>%
  rownames_to_column(var = "term") %>% rename("p.value" = `Pr(>|z|)`) %>%
  select(c("term", "estimate", "se", "p.value")) %>%
  filter(term == "sunsalesban_di") %>% mutate(term = "Women_LEHS")
out.drink.WSomeC <- as.data.frame(coef(summary(drinkstatus.WSomeC))) %>% 
  mutate(estimate = exp(Estimate), 
         se = `Std. Error`) %>%
  rownames_to_column(var = "term") %>% rename("p.value" = `Pr(>|z|)`) %>%
  select(c("term", "estimate", "se", "p.value")) %>%
  filter(term == "sunsalesban_di") %>% mutate(term = "Women_SomeC")
out.drink.WCollege <- as.data.frame(coef(summary(drinkstatus.WCollege))) %>% 
  mutate(estimate = exp(Estimate), 
         se = `Std. Error`) %>%
  rownames_to_column(var = "term") %>% rename("p.value" = `Pr(>|z|)`) %>%
  select(c("term", "estimate", "se", "p.value")) %>%
  filter(term == "sunsalesban_di") %>% mutate(term = "Women_College")

out.drink <- rbind(out.drink.MLEHS, out.drink.MSomeC, out.drink.MCollege, 
                   out.drink.WLEHS, out.drink.WSomeC, out.drink.WCollege)

out.gpd.MLEHS <- as.data.frame(coef(summary(gpd.MLEHS))) %>% 
  mutate(Estimate = exp(Estimate),
         se = `Std. Error`) %>%
  rownames_to_column(var = "term") %>% rename("estimate" = "Estimate", "p.value" = `Pr(>|t|)`) %>%
  select(c("term", "estimate", "se", "p.value")) %>%
  filter(term == "sunsalesban_di") %>% mutate(term = "Men_LEHS")
out.gpd.MSomeC <- as.data.frame(coef(summary(gpd.MSomeC))) %>% 
  mutate(Estimate = exp(Estimate),
         se = `Std. Error`) %>%
  rownames_to_column(var = "term") %>% rename("estimate" = "Estimate", "p.value" = `Pr(>|t|)`) %>%
  select(c("term", "estimate", "se", "p.value")) %>%
  filter(term == "sunsalesban_di") %>% mutate(term = "Men_SomeC")
out.gpd.MCollege <- as.data.frame(coef(summary(gpd.MCollege))) %>% 
  mutate(Estimate = exp(Estimate),
         se = `Std. Error`) %>%
  rownames_to_column(var = "term") %>% rename("estimate" = "Estimate", "p.value" = `Pr(>|t|)`) %>%
  select(c("term", "estimate", "se", "p.value")) %>%
  filter(term == "sunsalesban_di") %>% mutate(term = "Men_College")

out.gpd.WLEHS <- as.data.frame(coef(summary(gpd.WLEHS))) %>% 
  mutate(Estimate = exp(Estimate),
         se = `Std. Error`) %>%
  rownames_to_column(var = "term") %>% rename("estimate" = "Estimate", "p.value" = `Pr(>|t|)`) %>%
  select(c("term", "estimate", "se", "p.value")) %>%
  filter(term == "sunsalesban_di") %>% mutate(term = "Women_LEHS")
out.gpd.WSomeC <- as.data.frame(coef(summary(gpd.WSomeC))) %>% 
  mutate(Estimate = exp(Estimate),
         se = `Std. Error`) %>%
  rownames_to_column(var = "term") %>% rename("estimate" = "Estimate", "p.value" = `Pr(>|t|)`) %>%
  select(c("term", "estimate", "se", "p.value")) %>%
  filter(term == "sunsalesban_di") %>% mutate(term = "Women_SomeC")
out.gpd.WCollege <- as.data.frame(coef(summary(gpd.WCollege))) %>% 
  mutate(Estimate = exp(Estimate),
         se = `Std. Error`) %>%
  rownames_to_column(var = "term") %>% rename("estimate" = "Estimate", "p.value" = `Pr(>|t|)`) %>%
  select(c("term", "estimate", "se", "p.value")) %>%
  filter(term == "sunsalesban_di") %>% mutate(term = "Women_College")

out.gpd <- rbind(out.gpd.MLEHS, out.gpd.MSomeC, out.gpd.MCollege,
                 out.gpd.WLEHS, out.gpd.WSomeC, out.gpd.WCollege)

out.alccat.m <- as.data.frame(coef(summary(alccat.m))) %>% 
  mutate(estimate = exp(Estimate), 
         se = `Std. Error`) %>%
  rownames_to_column(var = "term") %>% rename("p.value" = `Pr(>|z|)`) %>%
  select(c("term", "estimate", "se", "p.value")) %>%
  filter(term == "sunsalesban_di") %>% mutate(term = "Men")
out.alccat.w <- as.data.frame(coef(summary(alccat.w))) %>% 
  mutate(estimate = exp(Estimate), 
         se = `Std. Error`) %>%
  rownames_to_column(var = "term") %>% rename("p.value" = `Pr(>|z|)`) %>%
  select(c("term", "estimate", "se", "p.value")) %>%
  filter(term == "sunsalesban_di") %>% mutate(term = "Women")
out.alccat <- rbind(out.alccat.m, out.alccat.w)

list_out <- list("AlcUse" = out.drink, "GPD CAT1+2" = out.gpd, "GPD CAT3" = out.alccat)
write.xlsx(list_out, file = paste0("acp_brfss/outputs/", DATE, "_MICROSIM_GLM_stratified.xlsx"), rowNames = FALSE)
