# ----------------------------------------------------------------
# ----------------------------------------------------------------
## Project: SIMAH  
## Title: Sunday sales ban and BRFSS
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
library(data.table)
library(dplyr)
library(openxlsx)
library(lme4)
library(nlme)
library(beepr)
library(ggeffects)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# LOAD DATA
# ----------------------------------------------------------------

setwd("/Users/carolinkilian/Desktop/SIMAH_workplace/")
DATE <- 20230713

# BRFSS 
datBRFSS <- data.table(readRDS("brfss/processed_data/BRFSS_upshifted_2000_2020_final.RDS"))

# POLICIES
datAP <- read_csv("acp_brfss/data/20230713_ALCPOLICY_2019.csv")

# UNEMPLOYMENT RATE
datUNEMP <- read.xlsx("acp_brfss/data/20230706_state_unemployment.xlsx", sheet = 1, startRow = 7) %>%
  select(c("X2", "X3", "rate")) %>% rename("State" = "X2", "YEAR" = "X3", "unemp.rate" = "rate") %>%
  mutate(YEAR = as.numeric(YEAR))

# MERGE DATA
data <- merge(datBRFSS, datAP, by.x = c("State", "YEAR"), by.y = c("state", "year"), all.x = T)
data <- merge(data, datUNEMP, by = c("State", "YEAR"), all.x = T, all.y = F)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# PREPARE DATA
# ----------------------------------------------------------------

# define sunday sales bans and filter for states
data <- 
  data %>% mutate(sunsalesban_di = ifelse(sunsalesban >= 0.5, 1, ifelse(sunsalesban < 0.5, 0, NA)),
                  sunsalesban = factor(ifelse(sunsalesban == 0, "no ban", ifelse(sunsalesban == 0.5, "partial ban", 
                                       ifelse(sunsalesban == 1, "full ban", NA))), levels = c("no ban", "partial ban", "full ban"))) %>%
  filter(State != "USA", State != "DC") %>%
  filter(YEAR < 2020)

ggplot(data, aes(sunsalesban)) + geom_histogram(stat = "count")
ggplot(data, aes(sunsalesban_di)) + geom_histogram(stat = "count")

# --------------------------------------------------------------------------------------

# select random subsample (for now)
# pdat <- data %>% sample_frac(0.05)

# define age groups and factor variables
pdat <- data %>% mutate_at(c("race_eth", "sex_recode", "education_summary", "marital_status", 
                             "drinkingstatus", "controlstate", "sunsalesban", "sunsalesban_di", "State"), as.factor) %>%
  mutate(age_gr = as.factor(ifelse(age_var < 35, "18-34", ifelse(age_var >= 35 & age_var < 50, "35-49", 
                                   ifelse(age_var >= 50 & age_var < 65, "50-64", ifelse(age_var >= 65, "65+", NA))))),
         education_summary = factor(education_summary, levels = c("College", "SomeC", "LEHS")))

# !! NO WEIGHTS IN DATA FILE?

# ----------------------------------------------------------------
# DRINKING ALCOHOL VS. ABSTAINING: MIXED-EFFECT MODELS
# ----------------------------------------------------------------

# MEN

drinkstatus.m <- glmer(drinkingstatus ~ sunsalesban_di*education_summary + 
                         drinkculture + controlstate + unemp.rate +
                         race_eth + marital_status + age_gr + (1 | State),
                        data = pdat[pdat$sex_recode == "Male",], family = binomial(link = "logit"))
beep()
summary(drinkstatus.m)
broom.mixed::tidy(drinkstatus.m, conf.int = TRUE, exponentiate = TRUE, effects = "fixed")

# SECULAR TREND
tseries::adf.test(residuals(drinkstatus.m)) # p = .01 -> stationarity

res <- residuals(drinkstatus.m)
res <- as.data.table(cbind(pdat$YEAR, res, pdat$State))

resAGG <- res %>% group_by(V1, V3) %>% 
  summarise(alc.prev = mean(as.numeric(res)))

ggplot(data = resAGG, aes(x = V1, y = alc.prev)) + geom_smooth() + facet_wrap(vars(V3)) 
  # no unique pattern across states
  # OK => continue with one-level model without secular trend

# PREDICT INTERACTION
ggpredict(drinkstatus.m, c("education_summary", "sunsalesban_di"))

# --------------------------------------------------------------------------------------

# WOMEN 

drinkstatus.w <- glmer(drinkingstatus ~ sunsalesban_di*education_summary + 
                         drinkculture + controlstate + unemp.rate +
                         race_eth + marital_status + age_gr + (1 | State),
                       data = pdat[pdat$sex_recode == "Female",], family = binomial(link = "logit"))
beep()
summary(drinkstatus.w)
broom.mixed::tidy(drinkstatus.w, conf.int = TRUE, exponentiate = TRUE, effects = "fixed")

# SECULAR TREND
tseries::adf.test(residuals(drinkstatus.w)) # p = .01 -> stationarity

res <- residuals(drinkstatus.w)
res <- as.data.table(cbind(pdat$YEAR, res, pdat$State))

resAGG <- res %>% group_by(V1, V3) %>% 
  summarise(alc.prev = mean(as.numeric(res)))

ggplot(data = resAGG, aes(x = V1, y = alc.prev)) + geom_smooth() + facet_wrap(vars(V3)) 
# no unique pattern across states
# OK => continue with one-level model without secular trend

# DISPLAY INTERACTION

ggpredict(drinkstatus.w, c("education_summary", "sunsalesban_di")) %>% 
  plot() + theme_bw() + 
  theme(axis.text.x = element_text(color = "black"), strip.background = element_rect(fill="white")) 

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# DAILY ALCOHOL CONSUMPTION: MIXED-EFFECT MODELS
# ----------------------------------------------------------------

ggplot(pdat[pdat$gramsperday_upshifted > 0,], aes(x = gramsperday_upshifted)) + geom_histogram()
ggplot(pdat[pdat$gramsperday_upshifted > 0,], aes(x = log(gramsperday_upshifted))) + geom_histogram()

pdat <- pdat %>% filter(gramsperday_upshifted > 0) %>% mutate(gpd_log = log(gramsperday_upshifted))

# MEN

gpd.m <- lme(gpd_log ~ sunsalesban_di*education_summary + 
               drinkculture + unemp.rate + controlstate + 
               race_eth + marital_status + age_gr, random = ~1 | State, 
             data = pdat[pdat$sex_recode == "Male"])
beep()
summary(gpd.m)

# CHECK SECULAR TREND
tseries::adf.test(residuals(gpd.m)) # p = .01 -> stationarity

res <- residuals(gpd.m)
res <- as.data.table(cbind(pdat$YEAR, res, pdat$State))

resAGG <- res %>% group_by(V1, V3) %>% 
  summarise(gpd = mean(as.numeric(res)))

ggplot(data = resAGG, aes(x = V1, y = gpd)) + geom_smooth() + facet_wrap(vars(V3)) 
  # no unique pattern across states

# --------------------------------------------------------------------------------------

# WOMEN

gpd.w <- lme(gpd_log ~ sunsalesban_di*education_summary + 
               drinkculture + unemp.rate + controlstate + 
               race_eth + marital_status + age_gr, random = ~1 | State, 
             data = pdat[pdat$sex_recode == "Female"])
beep()
summary(gpd.w)

# CHECK SECULAR TREND
tseries::adf.test(residuals(gpd.w)) # p = .01 -> stationarity

res <- residuals(gpd.w)
res <- as.data.table(cbind(pdat$YEAR, res, pdat$State))

resAGG <- res %>% group_by(V1, V3) %>% 
  summarise(gpd = mean(as.numeric(res)))

ggplot(data = resAGG, aes(x = V1, y = gpd)) + geom_smooth() + facet_wrap(vars(V3)) 
  # no unique pattern across states

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# CATEGORY III DRINKING PREVALENCE: MIXED-EFFECT MODELS
# ----------------------------------------------------------------

pdat <- pdat %>% filter(gramsperday_upshifted > 0) %>% 
  mutate(alccat3 = ifelse(sex_recode == "Male" & gramsperday_upshifted <= 60, 0,
                          ifelse(sex_recode == "Male" & gramsperday_upshifted > 60, 1,
                                 ifelse(sex_recode == "Female" & gramsperday_upshifted <= 40, 0,
                                        ifelse(sex_recode == "Female" & gramsperday_upshifted > 40, 1, NA)))))

# MEN

alccat.m <- glmer(alccat3 ~ sunsalesban_di*education_summary + 
                         drinkculture + controlstate + unemp.rate +
                         race_eth + marital_status + age_gr + (1 | State),
                       data = pdat[pdat$sex_recode == "Male",], family = binomial(link = "logit"))
beep()
summary(alccat.m)
broom.mixed::tidy(alccat.m, conf.int = TRUE, exponentiate = TRUE, effects = "fixed")

# CHECK SECULAR TREND
tseries::adf.test(residuals(alccat.m)) # p = .01 -> stationarity

res <- residuals(alccat.m)
res <- as.data.table(cbind(pdat$YEAR, res, pdat$State))

resAGG <- res %>% group_by(V1, V3) %>% 
  summarise(gpd = mean(as.numeric(res)))

ggplot(data = resAGG, aes(x = V1, y = gpd)) + geom_smooth() + facet_wrap(vars(V3)) 
  # no unique pattern across states

# --------------------------------------------------------------------------------------

# WOMEN

alccat.w <- glmer(alccat3 ~ sunsalesban_di*education_summary + 
                    drinkculture + controlstate + unemp.rate +
                    race_eth + marital_status + age_gr + (1 | State),
                  data = pdat[pdat$sex_recode == "Female",], family = binomial(link = "logit"))
beep()
summary(alccat.w)
broom.mixed::tidy(alccat.w, conf.int = TRUE, exponentiate = TRUE, effects = "fixed")

# CHECK SECULAR TREND
tseries::adf.test(residuals(alccat.w)) # p = .01 -> stationarity

res <- residuals(alccat.w)
res <- as.data.table(cbind(pdat$YEAR, res, pdat$State))

resAGG <- res %>% group_by(V1, V3) %>% 
  summarise(gpd = mean(as.numeric(res)))

ggplot(data = resAGG, aes(x = V1, y = gpd)) + geom_smooth() + facet_wrap(vars(V3)) 
  # no unique pattern across states

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# EXPORT
# ----------------------------------------------------------------

out.drink.m <- broom.mixed::tidy(drinkstatus.m, conf.int = TRUE, exponentiate = TRUE, effects = "fixed") %>% 
  select(term, estimate, conf.low, conf.high, p.value)
out.drink.w <- broom.mixed::tidy(drinkstatus.w, conf.int = TRUE, exponentiate = TRUE, effects = "fixed") %>% 
  select(term, estimate, conf.low, conf.high, p.value)
out.drink <- left_join(out.drink.m, out.drink.w, by = join_by(term), suffix = c(".men", ".women"))

out.gpd.m <- as.data.frame(coef(summary(gpd.m))) %>% 
  mutate(conf.low = Value - 1.96*Std.Error,
         conf.high = Value + 1.96*Std.Error) %>%
  rownames_to_column(var = "term") %>% rename("estimate" = "Value", "p.value" = "p-value") %>%
  select(c("term", "estimate", "conf.low", "conf.high", "p.value"))
out.gpd.w <- as.data.frame(coef(summary(gpd.w))) %>% 
  mutate(conf.low = Value - 1.96*Std.Error,
         conf.high = Value + 1.96*Std.Error) %>%
  rownames_to_column(var = "term") %>% rename("estimate" = "Value", "p.value" = "p-value") %>%
  select(c("term", "estimate", "conf.low", "conf.high", "p.value"))
out.gpd <- left_join(out.gpd.m, out.gpd.w, by = join_by(term), suffix = c(".men", ".women"))

out.gpdexp.m <- as.data.frame(exp(gpd.m$coefficients$fixed)) %>% rownames_to_column(var = "term") %>% rename("exp.estimate" = "exp(gpd.m$coefficients$fixed)")
out.gpdexp.w <- as.data.frame(exp(gpd.w$coefficients$fixed)) %>% rownames_to_column(var = "term") %>% rename("exp.estimate" = "exp(gpd.w$coefficients$fixed)")
out.gpdexp <- left_join(out.gpdexp.m, out.gpdexp.w, by = join_by(term), suffix = c(".men", ".women"))

out.alccat.m <- broom.mixed::tidy(alccat.m, conf.int = TRUE, exponentiate = TRUE, effects = "fixed") %>% 
  select(term, estimate, conf.low, conf.high, p.value)
out.alccat.w <- broom.mixed::tidy(alccat.w, conf.int = TRUE, exponentiate = TRUE, effects = "fixed") %>% 
  select(term, estimate, conf.low, conf.high, p.value)
out.alccat <- left_join(out.alccat.m, out.alccat.w, by = join_by(term), suffix = c(".men", ".women"))

list_out <- list("AlcUse" = out.drink, "GPD" = out.gpd, "GPDexp" = out.gpdexp, "CategoryIII" = out.alccat)
write.xlsx(list_out, file = paste0("acp_brfss/outputs/", DATE, "_BRFSSupshifted_output_glmer.xlsx"), rowNames = FALSE)
