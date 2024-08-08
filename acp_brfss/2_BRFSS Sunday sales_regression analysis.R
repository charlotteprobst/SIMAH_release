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
DATE <- 20240129

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
         z.unemp.rate = (log(unemp.rate) - mean(log(unemp.rate))) / sd(log(unemp.rate))) 

# --------------------------------------------------------------------------------------

# set plot design

ggdesign <- theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 12), strip.text = element_text(size = 12),
        axis.text = element_text(size = 12, color = "black"), 
        axis.title.y = element_text(size = 12, color = "black"), 
        legend.title = element_text(size = 12), legend.text = element_text(size = 12), 
        axis.title.x = element_blank(), axis.ticks = element_blank())

col1 <- c("#B5C95A", "#1D9A6C", "#0A2F51")

#label_education <- c("low", "medium", "high")
label_ban <- c("No Ban", "Ban")

# SAFE MEMORY
control.compute=list(save.memory=TRUE)

# ----------------------------------------------------------------
# DRINKING ALCOHOL VS. ABSTAINING: MIXED-EFFECT MODELS
# ----------------------------------------------------------------

# MEN

drinkstatus.m <- glm(drinkingstatus ~ sunsalesban_di*education_summary + 
                         drinkculture + controlstate + z.unemp.rate +
                         race_eth + marital_status + age_gr + State,
                        data = pdat[pdat$sex_recode == "Men",], family = binomial(link = "logit"))
#summary(drinkstatus.m)


# MARGINAL MEANS
# https://strengejacke.github.io/ggeffects/articles/technical_differencepredictemmeans.html

margins <- ggeffect(drinkstatus.m, terms = c("education_summary", "sunsalesban_di")) %>% 
  as.data.frame() %>% mutate(x = factor(x, levels = c("LEHS", "SomeC", "College")),
         group = factor(group, levels = c("no ban", "ban")))

# INTERACTION PLOT
IA.DS.M <- 
  ggplot(margins, aes(group, predicted, color = x, group = x)) +
  geom_point(size = 4, shape = 18) + geom_line(linetype = "dashed") + 
  scale_color_manual(values = col1, name = "education", labels = c("low", "medium", "high")) +  
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1L)) +
  scale_x_discrete(label = label_ban) + 
  labs(title = "\n", y = "\nEMM: any alcohol use\n") + ggdesign

# --------------------------------------------------------------------------------------

# WOMEN 

drinkstatus.w <- glm(drinkingstatus ~ sunsalesban_di*education_summary + 
                         drinkculture + controlstate + z.unemp.rate +
                         race_eth + marital_status + age_gr + State,
                       data = pdat[pdat$sex_recode == "Women",], family = binomial(link = "logit"))
#summary(drinkstatus.w)

# MARGINAL MEANS 

margins <- ggeffect(drinkstatus.w, terms = c("education_summary", "sunsalesban_di")) %>% 
  as.data.frame() %>% mutate(x = factor(x, levels = c("LEHS", "SomeC", "College")),
         group = factor(group, levels = c("no ban", "ban")))

# INTERACTION PLOT
IA.DS.W <- 
  ggplot(margins, aes(group, predicted, color = x, group = x)) +
  geom_point(size = 4, shape = 18) + geom_line(linetype = "dashed") + 
  scale_color_manual(values = col1, name = "education", labels = c("low", "medium", "high")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1L)) +
  scale_x_discrete(label = label_ban) + 
  labs(title = "\n", y = "\nEMM: any alcohol use\n") + ggdesign

#beep()

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# DAILY ALCOHOL CONSUMPTION: MIXED-EFFECT MODELS
# ----------------------------------------------------------------

#ggplot(pdat[pdat$gramsperday > 0,], aes(x = gramsperday)) + geom_histogram()
#ggplot(pdat[pdat$gramsperday > 0,], aes(x = log(gramsperday))) + geom_histogram()

pdat <- pdat %>% filter(gramsperday > 0) %>% mutate(gpd_log = log(gramsperday))

# MEN

gpd.m <- glm(gpd_log ~ sunsalesban_di*education_summary + 
               drinkculture + z.unemp.rate + controlstate + 
               race_eth + marital_status + age_gr + State,
             data = pdat[pdat$sex_recode == "Men",])
#summary(gpd.m)

# MARGINAL MEANS 

margins <- ggeffect(gpd.m, terms = c("education_summary", "sunsalesban_di")) %>% 
  as.data.frame() %>% mutate(x = factor(x, levels = c("LEHS", "SomeC", "College")),
         group = factor(group, levels = c("no ban", "ban")))

# INTERACTION PLOT
IA.GPD.M <- 
  ggplot(margins, aes(group, exp(predicted), color = x, group = x)) +
  geom_point(size = 4, shape = 18) + geom_line(linetype = "dashed") + 
  scale_x_discrete(label = label_ban) + 
  scale_color_manual(values = col1, name = "education", labels = c("low", "medium", "high")) + 
  labs(title = "\n", y = "\nEMM: average daily grams of pure alcohol*\n") + ggdesign

#beep()

# --------------------------------------------------------------------------------------

# WOMEN

gpd.w <- glm(gpd_log ~ sunsalesban_di*education_summary + 
               drinkculture + z.unemp.rate + controlstate + 
               race_eth + marital_status + age_gr + State, 
             data = pdat[pdat$sex_recode == "Women",])
#summary(gpd.w)

# MARGINAL MEANS 

margins <- ggeffect(gpd.w, terms = c("education_summary", "sunsalesban_di")) %>% 
  as.data.frame() %>% mutate(x = factor(x, levels = c("LEHS", "SomeC", "College")),
         group = factor(group, levels = c("no ban", "ban")))

# INTERACTION PLOT
IA.GPD.W <- 
  ggplot(margins, aes(group, exp(predicted), color = x, group = x)) +
  geom_point(size = 4, shape = 18) + geom_line(linetype = "dashed") + 
  scale_x_discrete(label = label_ban) + 
  scale_color_manual(values = col1, name = "education", labels = c("low", "medium", "high")) + 
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

alccat.m <- glm(alccat3 ~ sunsalesban_di*education_summary + 
                         drinkculture + controlstate + z.unemp.rate +
                         race_eth + marital_status + age_gr + State,
                       data = pdat[pdat$sex_recode == "Men",], family = binomial(link = "logit"))
#summary(alccat.m)

# MARGINAL MEANS 

margins <- ggeffect(alccat.m, terms = c("education_summary", "sunsalesban_di")) %>% 
  as.data.frame() %>% mutate(x = factor(x, levels = c("LEHS", "SomeC", "College")),
         group = factor(group, levels = c("no ban", "ban")))

# INTERACTION PLOT
IA.ALCCAT.M <- 
  ggplot(margins, aes(group, predicted, color = x, group = x)) +
  geom_point(size = 4, shape = 18) + geom_line(linetype = "dashed") + 
  scale_x_discrete(label = label_ban) + 
  scale_color_manual(values = col1, name = "education", labels = c("low", "medium", "high")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1L)) +
  labs(title = "\n", y = "\nEMM: any hazardous alcohol use*\n") + ggdesign

#beep()

# --------------------------------------------------------------------------------------

# WOMEN

alccat.w <- glm(alccat3 ~ sunsalesban_di*education_summary + 
                    drinkculture + controlstate + z.unemp.rate +
                    race_eth + marital_status + age_gr + State,
                  data = pdat[pdat$sex_recode == "Women",], family = binomial(link = "logit"))
#summary(alccat.w)

# MARGINAL MEANS 

margins <- ggeffect(alccat.w, terms = c("education_summary", "sunsalesban_di")) %>% 
  as.data.frame() %>% mutate(x = factor(x, levels = c("LEHS", "SomeC", "College")),
         group = factor(group, levels = c("no ban", "ban")))

# INTERACTION PLOT
IA.ALCCAT.W <- 
  ggplot(margins, aes(group, predicted, color = x, group = x)) +
  geom_point(size = 4, shape = 18) + geom_line(linetype = "dashed") + 
  scale_x_discrete(label = label_ban) + 
  scale_color_manual(values = col1, name = "education", labels = c("low", "medium", "high")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1L)) +
  labs(title = "\n", y = "\nEMM: any hazardous alcohol use*\n") + ggdesign

#beep()

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# EXPORT: FIGURE
# ----------------------------------------------------------------

ggarrange(IA.DS.M, IA.GPD.M, IA.ALCCAT.M, IA.DS.W, IA.GPD.W, IA.ALCCAT.W,
          labels = c("1A: MEN", "2A: MEN", "3A: MEN", "1B: WOMEN", "2B: WOMEN", "3B: WOMEN"),
          ncol = 3, nrow = 2, common.legend = TRUE, legend="bottom")

ggsave(paste0("acp_brfss/outputs/figures/", DATE, "_BRFSS_INTERACT_ALT.jpg"), dpi = 300, width = 12, height = 8)

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
write.xlsx(list_out, file = paste0("acp_brfss/outputs/", DATE, "_BRFSSraw_output_glm.xlsx"), rowNames = FALSE)
