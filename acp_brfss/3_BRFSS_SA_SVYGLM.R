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
library(survey)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# LOAD DATA
# ----------------------------------------------------------------

setwd("/Users/carolinkilian/Desktop/SIMAH_workplace/")
DATE <- 20230926

# BRFSS 
data <- as.data.frame(readRDS("acp_brfss/20230925_brfss_clean.RDS"))

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# PREPARE DATA
# ----------------------------------------------------------------

# define age groups and factor variables, z-standardize unemplyoment rate
pdat <- data %>%
  
  # select random subsample (for testing)
  # sample_frac(0.001) %>%
  
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
                        race_eth + marital_status + age_gr + sampling + State,
                      design = subset(svydat, sex_recode == "Men"),
                      family = quasibinomial(link = "logit"))
#broom.mixed::tidy(drinkstatus.m, conf.int = TRUE, exponentiate = TRUE)

# WOMEN 

drinkstatus.w <- svyglm(drinkingstatus ~ sunsalesban_di*education_summary + 
                          drinkculture + controlstate + z.unemp.rate +
                          race_eth + marital_status + age_gr + sampling + State,
                        design = subset(svydat, sex_recode == "Women"),
                        family = quasibinomial(link = "logit"))
#broom.mixed::tidy(drinkstatus.w, conf.int = TRUE, exponentiate = TRUE)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# DAILY ALCOHOL CONSUMPTION: MIXED-EFFECT MODELS
# ----------------------------------------------------------------

# MEN

gpd.m <- svyglm(gpd_log ~ sunsalesban_di*education_summary + 
                  drinkculture + controlstate + z.unemp.rate +
                  race_eth + marital_status + age_gr + sampling + State,
                design = subset(svydat, sex_recode == "Men" & gramsperday > 0),
                family = gaussian(link = "identity"))
#broom.mixed::tidy(gpd.m, conf.int = TRUE, exponentiate = FALSE)

# WOMEN

gpd.w <- svyglm(gpd_log ~ sunsalesban_di*education_summary + 
                  drinkculture + controlstate + z.unemp.rate +
                  race_eth + marital_status + age_gr + sampling + State,
                design = subset(svydat, sex_recode == "Women" & gramsperday > 0),
                family = gaussian(link = "identity"))
#broom.mixed::tidy(gpd.w, conf.int = TRUE, exponentiate = FALSE)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# CATEGORY III DRINKING PREVALENCE: MIXED-EFFECT MODELS
# ----------------------------------------------------------------

# MEN

alccat.m <- svyglm(alccat3 ~ sunsalesban_di*education_summary +
                     drinkculture + controlstate + z.unemp.rate +
                     race_eth + marital_status + age_gr + sampling + State,
                   design = subset(svydat, sex_recode == "Men" & gramsperday > 0),
                   family = quasibinomial(link = "logit"))
#broom.mixed::tidy(alccat.m, conf.int = TRUE, exponentiate = TRUE)

# WOMEN

alccat.w <- svyglm(alccat3 ~ sunsalesban_di*education_summary +
                     drinkculture + controlstate + z.unemp.rate +
                     race_eth + marital_status + age_gr + sampling + State,
                   design = subset(svydat, sex_recode == "Women" & gramsperday > 0),
                   family = quasibinomial(link = "logit"))
#broom.mixed::tidy(alccat.w, conf.int = TRUE, exponentiate = TRUE)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# EXPORT: TABLE
# ----------------------------------------------------------------

out.drink.m <- broom.mixed::tidy(drinkstatus.m, conf.int = TRUE, exponentiate = TRUE) %>% 
  select(term, estimate, conf.low, conf.high, p.value)
out.drink.w <- broom.mixed::tidy(drinkstatus.w, conf.int = TRUE, exponentiate = TRUE) %>% 
  select(term, estimate, conf.low, conf.high, p.value)
out.drink <- left_join(out.drink.m, out.drink.w, by = join_by(term), suffix = c(".men", ".women"))

out.gpd.m <- broom.mixed::tidy(gpd.m, conf.int = TRUE, exponentiate = FALSE) %>% 
  select(term, estimate, conf.low, conf.high, p.value)
out.gpd.w <- broom.mixed::tidy(gpd.w, conf.int = TRUE, exponentiate = FALSE) %>% 
  select(term, estimate, conf.low, conf.high, p.value)
out.gpd <- left_join(out.gpd.m, out.gpd.w, by = join_by(term), suffix = c(".men", ".women"))

out.gpdexp.m <- as.data.frame(exp(gpd.m$coefficients)) %>% tibble::rownames_to_column(var = "term") %>% rename("exp.estimate" = "exp(gpd.m$coefficients)")
out.gpdexp.w <- as.data.frame(exp(gpd.w$coefficients)) %>% tibble::rownames_to_column(var = "term") %>% rename("exp.estimate" = "exp(gpd.w$coefficients)")
out.gpdexp <- left_join(out.gpdexp.m, out.gpdexp.w, by = join_by(term), suffix = c(".men", ".women"))

out.alccat.m <- broom.mixed::tidy(alccat.m, conf.int = TRUE, exponentiate = TRUE) %>% 
  select(term, estimate, conf.low, conf.high, p.value)
out.alccat.w <- broom.mixed::tidy(alccat.w, conf.int = TRUE, exponentiate = TRUE) %>% 
  select(term, estimate, conf.low, conf.high, p.value)
out.alccat <- left_join(out.alccat.m, out.alccat.w, by = join_by(term), suffix = c(".men", ".women"))

list_out <- list("AlcUse" = out.drink, "GPD" = out.gpd, "GPDexp" = out.gpdexp, "CategoryIII" = out.alccat)
write.xlsx(list_out, file = paste0("acp_brfss/outputs/", DATE, "_BRFSS_SA_SVY_output_svyglm.xlsx"), rowNames = FALSE)
