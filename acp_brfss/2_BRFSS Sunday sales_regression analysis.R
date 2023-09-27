# ----------------------------------------------------------------
# ----------------------------------------------------------------
## Project: SIMAH  
## Title: Main analysis BRFSS Sunday sales ban
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
DATE <- 20230925

# BRFSS 
data <- as.data.frame(readRDS("acp_brfss/20230925_brfss_clean.RDS"))

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# PREPARE DATA
# ----------------------------------------------------------------

# define age groups and factor variables, z-standardize unemplyoment rate
pdat <- data %>%
  
  # select random subsample (for now)
  #sample_frac(0.1) %>% 
  
  # prepare data
  mutate_at(c("race_eth", "sex_recode", "education_summary", "marital_status", 
              "drinkingstatus", "controlstate", "drinkculture", "sunsalesban", "State"), as.factor) %>%
  mutate(education_summary = factor(education_summary, levels = c("College", "SomeC", "LEHS")),
         sunsalesban_di = factor(ifelse(sunsalesban_di == 1, "ban", ifelse(sunsalesban_di == 0, "no ban", NA)),
                                 levels = c("no ban", "ban")),
         z.unemp.rate = (log(unemp.rate) - mean(log(unemp.rate))) / sd(log(unemp.rate))) # across the total sample

# --------------------------------------------------------------------------------------

# set plot design

ggdesign <- theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 12), strip.text = element_text(size = 12),
        axis.text = element_text(size = 12, color = "black"), 
        axis.title.y = element_text(size = 12, color = "black"), 
        axis.title.x = element_blank(), axis.ticks = element_blank())

col1 <- c("#1D9A6C", "#0A2F51")

label_education <- c("low", "medium", "high")

# ----------------------------------------------------------------
# DRINKING ALCOHOL VS. ABSTAINING: MIXED-EFFECT MODELS
# ----------------------------------------------------------------

# MEN

drinkstatus.m <- glmer(drinkingstatus ~ sunsalesban_di*education_summary + 
                         drinkculture + controlstate + z.unemp.rate +
                         race_eth + marital_status + age_gr + (1 | State),
                        data = pdat[pdat$sex_recode == "Men",], family = binomial(link = "logit"))
summary(drinkstatus.m)
broom.mixed::tidy(drinkstatus.m, conf.int = TRUE, exponentiate = TRUE, effects = "fixed")

# SECULAR TREND
#tseries::adf.test(residuals(drinkstatus.m)) # p = .01 -> stationarity

res <- residuals(drinkstatus.m)
res <- as.data.table(cbind(pdat$YEAR, res, pdat$State))

resAGG <- res %>% group_by(V1, V3) %>% 
  summarise(alc.prev = mean(as.numeric(res)))

#ggplot(data = resAGG, aes(x = V1, y = alc.prev)) + geom_smooth() + facet_wrap(vars(V3)) 
  # no unique pattern across states
  # OK => continue with one-level model without secular trend

# MARGINAL MEANS
# https://strengejacke.github.io/ggeffects/articles/technical_differencepredictemmeans.html

margins <- ggemmeans(drinkstatus.m, terms = c("education_summary", "sunsalesban_di")) %>% 
  mutate(x = factor(x, levels = c("LEHS", "SomeC", "College")),
         group = factor(group, levels = c("ban", "no ban")))

# INTERACTION PLOT
IA.DS.M <- 
  ggplot(margins, aes(x, predicted, color = group, group = group)) +
  geom_point(size = 4, shape = 18) + geom_line(linetype = "dashed") + 
  scale_color_manual(values = col1) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1L)) +
  scale_x_discrete(label = label_education) + 
  labs(title = "\n", y = "\nEMM: any alcohol use\n") + ggdesign

# --------------------------------------------------------------------------------------

# WOMEN 

drinkstatus.w <- glmer(drinkingstatus ~ sunsalesban_di*education_summary + 
                         drinkculture + controlstate + z.unemp.rate +
                         race_eth + marital_status + age_gr + (1 | State),
                       data = pdat[pdat$sex_recode == "Women",], family = binomial(link = "logit"))
summary(drinkstatus.w)
broom.mixed::tidy(drinkstatus.w, conf.int = TRUE, exponentiate = TRUE, effects = "fixed")

# SECULAR TREND
#tseries::adf.test(residuals(drinkstatus.w)) # p = .01 -> stationarity

res <- residuals(drinkstatus.w)
res <- as.data.table(cbind(pdat$YEAR, res, pdat$State))

resAGG <- res %>% group_by(V1, V3) %>% 
  summarise(alc.prev = mean(as.numeric(res)))

#ggplot(data = resAGG, aes(x = V1, y = alc.prev)) + geom_smooth() + facet_wrap(vars(V3)) 
# no unique pattern across states
# OK => continue with one-level model without secular trend

# MARGINAL MEANS 

margins <- ggemmeans(drinkstatus.w, terms = c("education_summary", "sunsalesban_di")) %>% 
  mutate(x = factor(x, levels = c("LEHS", "SomeC", "College")),
         group = factor(group, levels = c("ban", "no ban")))

# INTERACTION PLOT
IA.DS.W <- 
  ggplot(margins, aes(x, predicted, color = group, group = group)) +
  geom_point(size = 4, shape = 18) + geom_line(linetype = "dashed") + 
  scale_color_manual(values = col1) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1L)) +
  scale_x_discrete(label = label_education) + 
  labs(title = "\n", y = "\nEMM: any alcohol use\n") + ggdesign

#beep()

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# DAILY ALCOHOL CONSUMPTION: MIXED-EFFECT MODELS
# ----------------------------------------------------------------

ggplot(pdat[pdat$gramsperday > 0,], aes(x = gramsperday)) + geom_histogram()
ggplot(pdat[pdat$gramsperday > 0,], aes(x = log(gramsperday))) + geom_histogram()

pdat <- pdat %>% filter(gramsperday > 0) %>% mutate(gpd_log = log(gramsperday))

# MEN
# svyglm for survey weights plus margins but no random intercept -> probably best model! https://www.rdocumentation.org/packages/survey/versions/4.2-1/topics/svyglm

gpd.m <- lme(gpd_log ~ sunsalesban_di*education_summary + 
               drinkculture + z.unemp.rate + controlstate + 
               race_eth + marital_status + age_gr, random = ~1|State, 
             data = pdat[pdat$sex_recode == "Men"])
summary(gpd.m)

# CHECK SECULAR TREND
#tseries::adf.test(residuals(gpd.m)) # p = .01 -> stationarity

res <- residuals(gpd.m)
res <- as.data.table(cbind(pdat$YEAR, res, pdat$State))

resAGG <- res %>% group_by(V1, V3) %>% 
  summarise(gpd = mean(as.numeric(res)))

#ggplot(data = resAGG, aes(x = V1, y = gpd)) + geom_smooth() + facet_wrap(vars(V3)) 
  # no unique pattern across states

# MARGINAL MEANS 

margins <- ggemmeans(gpd.m, terms = c("education_summary", "sunsalesban_di")) %>% 
  mutate(x = factor(x, levels = c("LEHS", "SomeC", "College")),
         group = factor(group, levels = c("ban", "no ban")))

# INTERACTION PLOT
IA.GPD.M <- 
  ggplot(margins, aes(x, exp(predicted), color = group, group = group)) +
  geom_point(size = 4, shape = 18) + geom_line(linetype = "dashed") + 
  scale_x_discrete(label = label_education) + 
  scale_color_manual(values = col1) +
  labs(title = "\n", y = "\nEMM: average daily grams of pure alcohol*\n") + ggdesign

#beep()

# --------------------------------------------------------------------------------------

# WOMEN

gpd.w <- lme(gpd_log ~ sunsalesban_di*education_summary + 
               drinkculture + z.unemp.rate + controlstate + 
               race_eth + marital_status + age_gr, random = ~1|State, 
             data = pdat[pdat$sex_recode == "Women"])
summary(gpd.w)

# CHECK SECULAR TREND
#tseries::adf.test(residuals(gpd.w)) # p = .01 -> stationarity

res <- residuals(gpd.w)
res <- as.data.table(cbind(pdat$YEAR, res, pdat$State))

resAGG <- res %>% group_by(V1, V3) %>% 
  summarise(gpd = mean(as.numeric(res)))

#ggplot(data = resAGG, aes(x = V1, y = gpd)) + geom_smooth() + facet_wrap(vars(V3)) 
  # no unique pattern across states
# MARGINAL MEANS 

margins <- ggemmeans(gpd.w, terms = c("education_summary", "sunsalesban_di")) %>% 
  mutate(x = factor(x, levels = c("LEHS", "SomeC", "College")),
         group = factor(group, levels = c("ban", "no ban")))

# INTERACTION PLOT
IA.GPD.W <- 
  ggplot(margins, aes(x, exp(predicted), color = group, group = group)) +
  geom_point(size = 4, shape = 18) + geom_line(linetype = "dashed") + 
  scale_x_discrete(label = label_education) + 
  scale_color_manual(values = col1) +
  labs(title = "\n", y = "\nEMM: average daily grams of pure alcohol*\n") + ggdesign

#beep()

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

alccat.m <- glmer(alccat3 ~ sunsalesban_di*education_summary + 
                         drinkculture + controlstate + z.unemp.rate +
                         race_eth + marital_status + age_gr + (1 | State),
                       data = pdat[pdat$sex_recode == "Men",], family = binomial(link = "logit"))
summary(alccat.m)
broom.mixed::tidy(alccat.m, conf.int = TRUE, exponentiate = TRUE, effects = "fixed")

# CHECK SECULAR TREND
#tseries::adf.test(residuals(alccat.m)) # p = .01 -> stationarity

res <- residuals(alccat.m)
res <- as.data.table(cbind(pdat$YEAR, res, pdat$State))

resAGG <- res %>% group_by(V1, V3) %>% 
  summarise(gpd = mean(as.numeric(res)))

#ggplot(data = resAGG, aes(x = V1, y = gpd)) + geom_smooth() + facet_wrap(vars(V3)) 
  # no unique pattern across states

# MARGINAL MEANS 

margins <- ggemmeans(alccat.m, terms = c("education_summary", "sunsalesban_di")) %>% 
  mutate(x = factor(x, levels = c("LEHS", "SomeC", "College")),
         group = factor(group, levels = c("ban", "no ban")))

# INTERACTION PLOT
IA.ALCCAT.M <- 
  ggplot(margins, aes(x, predicted, color = group, group = group)) +
  geom_point(size = 4, shape = 18) + geom_line(linetype = "dashed") + 
  scale_x_discrete(label = label_education) + 
  scale_color_manual(values = col1) + scale_y_continuous(labels = scales::percent_format(accuracy = 0.1L)) +
  labs(title = "\n", y = "\nEMM: any hazardous alcohol use*\n") + ggdesign

#beep()

# --------------------------------------------------------------------------------------

# WOMEN

alccat.w <- glmer(alccat3 ~ sunsalesban_di*education_summary + 
                    drinkculture + controlstate + z.unemp.rate +
                    race_eth + marital_status + age_gr + (1 | State),
                  data = pdat[pdat$sex_recode == "Women",], family = binomial(link = "logit"))
summary(alccat.w)
broom.mixed::tidy(alccat.w, conf.int = TRUE, exponentiate = TRUE, effects = "fixed")

# CHECK SECULAR TREND
#tseries::adf.test(residuals(alccat.w)) # p = .01 -> stationarity

res <- residuals(alccat.w)
res <- as.data.table(cbind(pdat$YEAR, res, pdat$State))

resAGG <- res %>% group_by(V1, V3) %>% 
  summarise(gpd = mean(as.numeric(res)))

ggplot(data = resAGG, aes(x = V1, y = gpd)) + geom_smooth() + facet_wrap(vars(V3)) 
  # no unique pattern across states

# MARGINAL MEANS 

margins <- ggemmeans(alccat.w, terms = c("education_summary", "sunsalesban_di")) %>% 
  mutate(x = factor(x, levels = c("LEHS", "SomeC", "College")),
         group = factor(group, levels = c("ban", "no ban")))

# INTERACTION PLOT
IA.ALCCAT.W <- 
  ggplot(margins, aes(x, predicted, color = group, group = group)) +
  geom_point(size = 4, shape = 18) + geom_line(linetype = "dashed") + 
  scale_x_discrete(label = label_education) + 
  scale_color_manual(values = col1) + scale_y_continuous(labels = scales::percent_format(accuracy = 0.1L)) +
  labs(title = "\n", y = "\nEMM: any hazardous alcohol use*\n") + ggdesign

#beep()

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# EXPORT: FIGURE
# ----------------------------------------------------------------

ggarrange(IA.DS.M, IA.GPD.M, IA.ALCCAT.M, IA.DS.W, IA.GPD.W, IA.ALCCAT.W,
          labels = c("1A: MEN", "2A: MEN", "3A: MEN", "1B: WOMEN", "2B: WOMEN", "3B: WOMEN"),
          ncol = 3, nrow = 2)

ggsave(paste0("acp_brfss/outputs/figures/", DATE, "_BRFSS_INTERACT.jpg"), dpi = 300, width = 12, height = 8)

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
