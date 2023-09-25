# ----------------------------------------------------------------
# ----------------------------------------------------------------
## Project: SIMAH  
## Title: Sunday sales ban and BRFSS (with SVYWEIGHT)
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
#library(nlme)
library(beepr)
library(ggpubr)
library(survey)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# LOAD DATA
# ----------------------------------------------------------------

setwd("/Users/carolinkilian/Desktop/SIMAH_workplace/")
DATE <- 20230814

# BRFSS 
datBRFSS <- data.table(readRDS("brfss/processed_data/BRFSS_upshifted_2000_2020_final.RDS"))

# POLICIES
datAP <- read_csv("acp_brfss/data/20230808_ALCPOLICY_2019.csv")

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
  data %>% mutate(sunsalesban_di = factor(ifelse(sunsalesban >= 0.5, "ban", ifelse(sunsalesban < 0.5, "no ban", NA)), levels = c("no ban", "ban")),
                  sunsalesban = factor(ifelse(sunsalesban == 0, "no ban", ifelse(sunsalesban == 0.5, "partial ban", 
                                       ifelse(sunsalesban == 1, "full ban", NA))), levels = c("no ban", "partial ban", "full ban"))) %>%
  filter(State != "USA", State != "DC") %>%
  filter(YEAR < 2020)

#ggplot(data, aes(sunsalesban)) + geom_histogram(stat = "count")
#ggplot(data, aes(sunsalesban_di)) + geom_histogram(stat = "count")

# --------------------------------------------------------------------------------------

# define age groups and factor variables, z-standardize unemplyoment rate
pdat <- data %>%
  
  # select random subsample (for now)
  #sample_frac(0.1) %>% 
  
  # prepare data
  mutate_at(c("race_eth", "sex_recode", "education_summary", "marital_status", 
                             "drinkingstatus", "controlstate", "drinkculture", "sunsalesban", "State"), as.factor) %>%
  mutate(age_gr = as.factor(ifelse(age_var < 35, "18-34", ifelse(age_var >= 35 & age_var < 50, "35-49", 
                                   ifelse(age_var >= 50 & age_var < 65, "50-64", ifelse(age_var >= 65, "65+", NA))))),
         education_summary = factor(education_summary, levels = c("College", "SomeC", "LEHS")),
         z.unemp.rate = (log(unemp.rate) - mean(log(unemp.rate))) / sd(log(unemp.rate))) 
  

# survey weights
# adjustment for multiple years? 
# https://www.cdc.gov/brfss/annual_data/2020/pdf/Complex-Smple-Weights-Prep-Module-Data-Analysis-2020-508.pdf
options(survey.lonely.psu = "adjust") 
svypdat <- svydesign(id = ~1, weights = ~llcpwt, strata = ~ststr, nest = TRUE, data = pdat)

# --------------------------------------------------------------------------------------

# prediction dataframe

predat <- crossing(sunsalesban_di = c("no ban", "ban"), education_summary = c("LEHS", "SomeC", "College"),
                   age_gr = c("18-34", "35-49", "50-64", "65+"), race_eth = c("Black", "Hispanic", "Other", "White"), 
                   marital_status = c("0", "1"), controlstate = c("0", "1"), drinkculture = c("dry", "moderate", "wet"),
                   State = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", 
                             "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", 
                             "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
                             "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
                             "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
                             "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", 
                             "West Virginia", "Wisconsin", "Wyoming"),
                   z.unemp.rate = 0)

# --------------------------------------------------------------------------------------

# set plot design

ggdesign <- theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 12), strip.text = element_text(size = 12),
        axis.text = element_text(size = 12, color = "black"), 
        axis.title.y = element_text(size = 12, color = "black"), 
        axis.title.x = element_blank(), axis.ticks = element_blank())

col1 <- c("#1D9A6C", "#0A2F51")

# ----------------------------------------------------------------
# DRINKING ALCOHOL VS. ABSTAINING: MIXED-EFFECT MODELS
# ----------------------------------------------------------------

# MEN

drinkstatus.m <- glmer(drinkingstatus ~ sunsalesban_di*education_summary + 
                         drinkculture + controlstate + z.unemp.rate +
                         race_eth + marital_status + age_gr + (1 | State),
                        data = pdat[pdat$sex_recode == "Male",], family = binomial(link = "logit"))
summary(drinkstatus.m)
broom.mixed::tidy(drinkstatus.m, conf.int = TRUE, exponentiate = TRUE, effects = "fixed")

# SECULAR TREND
tseries::adf.test(residuals(drinkstatus.m)) # p = .01 -> stationarity

res <- residuals(drinkstatus.m)
res <- as.data.table(cbind(pdat$YEAR, res, pdat$State))

resAGG <- res %>% group_by(V1, V3) %>% 
  summarise(alc.prev = mean(as.numeric(res)))

#ggplot(data = resAGG, aes(x = V1, y = alc.prev)) + geom_smooth() + facet_wrap(vars(V3)) 
  # no unique pattern across states
  # OK => continue with one-level model without secular trend

# AVERAGE MARGINAL MEANS 
p.drinkstatus.m <- cbind(predat, pred = predict(drinkstatus.m, newdata = predat, type = "response", re.form= ~(1|State))) %>%
  mutate(education_summary = factor(education_summary, levels = c("LEHS", "SomeC", "College"))) 

# INTERACTION PLOT
IA.DS.M <- 
  ggplot(p.drinkstatus.m, aes(x = education_summary, y = pred, color = sunsalesban_di, group = sunsalesban_di)) +
  stat_summary(fun.y = mean, geom = "line", linetype = "dashed") + 
  stat_summary(fun.y = mean, size = 4, geom = "point", shape = 18) + 
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0) +
  scale_color_manual(values = col1) + scale_y_continuous(labels = scales::percent_format(accuracy = 0.1L)) +
  labs(title = "\n", y = "\nAME: any alcohol use\n") + ggdesign

# --------------------------------------------------------------------------------------

# WOMEN 

drinkstatus.w <- glmer(drinkingstatus ~ sunsalesban_di*education_summary + 
                         drinkculture + controlstate + z.unemp.rate +
                         race_eth + marital_status + age_gr + (1 | State),
                       data = pdat[pdat$sex_recode == "Female",], family = binomial(link = "logit"))
summary(drinkstatus.w)
broom.mixed::tidy(drinkstatus.w, conf.int = TRUE, exponentiate = TRUE, effects = "fixed")

# SECULAR TREND
tseries::adf.test(residuals(drinkstatus.w)) # p = .01 -> stationarity

res <- residuals(drinkstatus.w)
res <- as.data.table(cbind(pdat$YEAR, res, pdat$State))

resAGG <- res %>% group_by(V1, V3) %>% 
  summarise(alc.prev = mean(as.numeric(res)))

#ggplot(data = resAGG, aes(x = V1, y = alc.prev)) + geom_smooth() + facet_wrap(vars(V3)) 
# no unique pattern across states
# OK => continue with one-level model without secular trend

# AVERAGE MARGINAL MEANS 
p.drinkstatus.w <- cbind(predat, pred = predict(drinkstatus.w, newdata = predat, type = "response", re.form= ~(1|State))) %>%
  mutate(education_summary = factor(education_summary, levels = c("LEHS", "SomeC", "College"))) 

# INTERACTION PLOT
IA.DS.W <- 
  ggplot(p.drinkstatus.w, aes(x = education_summary, y = pred, color = sunsalesban_di, group = sunsalesban_di)) +
  stat_summary(fun.y = mean, geom = "line", linetype = "dashed") + 
  stat_summary(fun.y = mean, size = 4, geom = "point", shape = 18) + 
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0) +
  scale_color_manual(values = col1) + scale_y_continuous(labels = scales::percent_format(accuracy = 0.1L)) +
  labs(title = "\n", y = "\nAME: any alcohol use\n") + ggdesign

beep()

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# DAILY ALCOHOL CONSUMPTION: MIXED-EFFECT MODELS
# ----------------------------------------------------------------

ggplot(pdat[pdat$gramsperday_raw > 0,], aes(x = gramsperday_raw)) + geom_histogram()
ggplot(pdat[pdat$gramsperday_raw > 0,], aes(x = log(gramsperday_raw))) + geom_histogram()

pdat <- pdat %>% filter(gramsperday_raw > 0) %>% mutate(gpd_log = log(gramsperday_raw))

# MEN
# svyglm for survey weights plus margins but no random intercept -> probably best model! https://www.rdocumentation.org/packages/survey/versions/4.2-1/topics/svyglm

gpd.m <- lme(gpd_log ~ sunsalesban_di*education_summary + 
               drinkculture + z.unemp.rate + controlstate + 
               race_eth + marital_status + age_gr, random = ~1|State, 
             data = pdat[pdat$sex_recode == "Male"])
summary(gpd.m)

# CHECK SECULAR TREND
tseries::adf.test(residuals(gpd.m)) # p = .01 -> stationarity

res <- residuals(gpd.m)
res <- as.data.table(cbind(pdat$YEAR, res, pdat$State))

resAGG <- res %>% group_by(V1, V3) %>% 
  summarise(gpd = mean(as.numeric(res)))

#ggplot(data = resAGG, aes(x = V1, y = gpd)) + geom_smooth() + facet_wrap(vars(V3)) 
  # no unique pattern across states

# AVERAGE MARGINAL MEANS 
p.gpd.m <- cbind(predat, pred = exp(predict(gpd.m, newdata = predat, re.form= ~(1|State)))) %>%
  mutate(education_summary = factor(education_summary, levels = c("LEHS", "SomeC", "College"))) 

# INTERACTION PLOT
IA.GPD.M <- 
  ggplot(p.gpd.m, aes(x = education_summary, y = pred, color = sunsalesban_di, group = sunsalesban_di)) +
  stat_summary(fun.y = mean, geom = "line", linetype = "dashed") + 
  stat_summary(fun.y = mean, size = 4, geom = "point", shape = 18) + 
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0) +
  scale_color_manual(values = col1) + 
  labs(title = "\n", y = "\nAME: average daily grams of pure alcohol*\n") + ggdesign

# --------------------------------------------------------------------------------------

# WOMEN

gpd.w <- lme(gpd_log ~ sunsalesban_di*education_summary + 
               drinkculture + z.unemp.rate + controlstate + 
               race_eth + marital_status + age_gr, random = ~1|State, 
             data = pdat[pdat$sex_recode == "Female"])
summary(gpd.w)

# CHECK SECULAR TREND
tseries::adf.test(residuals(gpd.w)) # p = .01 -> stationarity

res <- residuals(gpd.w)
res <- as.data.table(cbind(pdat$YEAR, res, pdat$State))

resAGG <- res %>% group_by(V1, V3) %>% 
  summarise(gpd = mean(as.numeric(res)))

#ggplot(data = resAGG, aes(x = V1, y = gpd)) + geom_smooth() + facet_wrap(vars(V3)) 
  # no unique pattern across states

# AVERAGE MARGINAL MEANS 
p.gpd.w <- cbind(predat, pred = exp(predict(gpd.w, newdata = predat, re.form= ~(1|State)))) %>%
  mutate(education_summary = factor(education_summary, levels = c("LEHS", "SomeC", "College"))) 

# INTERACTION PLOT
IA.GPD.W <- 
  ggplot(p.gpd.w, aes(x = education_summary, y = pred, color = sunsalesban_di, group = sunsalesban_di)) +
  stat_summary(fun.y = mean, geom = "line", linetype = "dashed") + 
  stat_summary(fun.y = mean, size = 4, geom = "point", shape = 18) + 
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0) +
  scale_color_manual(values = col1) + 
  labs(title = "\n", y = "\nAME: average daily grams of pure alcohol*\n") + ggdesign

beep()

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# CATEGORY III DRINKING PREVALENCE: MIXED-EFFECT MODELS
# ----------------------------------------------------------------

pdat <- pdat %>% filter(gramsperday_raw > 0) %>% 
  mutate(alccat3 = ifelse(sex_recode == "Male" & gramsperday_raw <= 60, 0,
                          ifelse(sex_recode == "Male" & gramsperday_raw > 60, 1,
                                 ifelse(sex_recode == "Female" & gramsperday_raw <= 40, 0,
                                        ifelse(sex_recode == "Female" & gramsperday_raw > 40, 1, NA)))))

# MEN

alccat.m <- glmer(alccat3 ~ sunsalesban_di*education_summary + 
                         drinkculture + controlstate + z.unemp.rate +
                         race_eth + marital_status + age_gr + (1 | State),
                       data = pdat[pdat$sex_recode == "Male",], family = binomial(link = "logit"))
summary(alccat.m)
broom.mixed::tidy(alccat.m, conf.int = TRUE, exponentiate = TRUE, effects = "fixed")

# CHECK SECULAR TREND
tseries::adf.test(residuals(alccat.m)) # p = .01 -> stationarity

res <- residuals(alccat.m)
res <- as.data.table(cbind(pdat$YEAR, res, pdat$State))

resAGG <- res %>% group_by(V1, V3) %>% 
  summarise(gpd = mean(as.numeric(res)))

#ggplot(data = resAGG, aes(x = V1, y = gpd)) + geom_smooth() + facet_wrap(vars(V3)) 
  # no unique pattern across states

# AVERAGE MARGINAL MEANS 
p.alccat.m <- cbind(predat, pred = predict(alccat.m, newdata = predat, type = "response", re.form= ~(1|State))) %>%
  mutate(education_summary = factor(education_summary, levels = c("LEHS", "SomeC", "College"))) 

# INTERACTION PLOT
IA.ALCCAT.M <- 
  ggplot(p.alccat.m, aes(x = education_summary, y = pred, color = sunsalesban_di, group = sunsalesban_di)) +
  stat_summary(fun.y = mean, geom = "line", linetype = "dashed") + 
  stat_summary(fun.y = mean, size = 4, geom = "point", shape = 18) + 
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0) +
  scale_color_manual(values = col1) + scale_y_continuous(labels = scales::percent_format(accuracy = 0.1L)) +
  labs(title = "\n", y = "\nAME: any hazardous alcohol use*\n") + ggdesign

beep()

# --------------------------------------------------------------------------------------

# WOMEN

alccat.w <- glmer(alccat3 ~ sunsalesban_di*education_summary + 
                    drinkculture + controlstate + z.unemp.rate +
                    race_eth + marital_status + age_gr + (1 | State),
                  data = pdat[pdat$sex_recode == "Female",], family = binomial(link = "logit"))
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

beep()

# AVERAGE MARGINAL MEANS 
p.alccat.w <- cbind(predat, pred = predict(alccat.w, newdata = predat, type = "response", re.form= ~(1|State))) %>%
  mutate(education_summary = factor(education_summary, levels = c("LEHS", "SomeC", "College"))) 

# INTERACTION PLOT
IA.ALCCAT.W <- 
  ggplot(p.alccat.w, aes(x = education_summary, y = pred, color = sunsalesban_di, group = sunsalesban_di)) +
  stat_summary(fun.y = mean, geom = "line", linetype = "dashed") + 
  stat_summary(fun.y = mean, size = 4, geom = "point", shape = 18) + 
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0) +
  scale_color_manual(values = col1) + scale_y_continuous(labels = scales::percent_format(accuracy = 0.1L)) +
  labs(title = "\n", y = "\nAME: any hazardous alcohol use*\n") + ggdesign

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# EXPORT: FIGURE
# ----------------------------------------------------------------

ggarrange(IA.DS.M, IA.GPD.M, IA.ALCCAT.M, IA.DS.W, IA.GPD.W, IA.ALCCAT.W,
          labels = c("1A: MEN", "2A: MEN", "3A: MEN", "1B: WOMEN", "2B: WOMEN", "3B: WOMEN"),
          ncol = 3, nrow = 2)

ggsave(paste0("acp_brfss/outputs/figures/", DATE, "_BRFSSS_INTERACT.jpg"), dpi = 300, width = 8, height = 5)

# ----------------------------------------------------------------
# EXPORT: TABLE
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
write.xlsx(list_out, file = paste0("acp_brfss/outputs/", DATE, "_BRFSSraw_output_glmer.xlsx"), rowNames = FALSE)
