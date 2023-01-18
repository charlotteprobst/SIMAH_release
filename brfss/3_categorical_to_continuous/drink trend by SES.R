# ----------------------------------------------------------------
# ----------------------------------------------------------------
## Project: SIMAH  
## Title: Drinking trend BRFSS by SES 
## Author: Carolin Kilian
## Start Date: 09/01/2023
# ----------------------------------------------------------------
# ----------------------------------------------------------------

library(tidyverse)
library(data.table)
library(dplyr)
library(Hmisc)
library(writexl)

# --------------------------------------------------------------------------------------

rm(list = ls())
setwd("/Users/carolinkilian/Desktop/SIMAH_workplace/")
DATE <- 20230109

dat <- data.table(readRDS("brfss/processed_data/BRFSS_upshifted_2000_2020_final.RDS"))

# --------------------------------------------------------------------------------------

# weighted confidence interval: lower bound
wtd.lb <- function(x, w) {
  n    <- length(x)
  mean <- wtd.mean(x, w)
  var  <- wtd.var(x, w)
  se   <- sqrt(var)/sqrt(length(x))
  lb   <- mean - 1.96*se
  return(lb)
}

# weighted confidence interval: upper bound
wtd.ub <- function(x, w) {
  n    <- length(x)
  mean <- wtd.mean(x, w)
  var  <- wtd.var(x, w)
  se   <- sqrt(var)/sqrt(length(x))
  ub   <- mean + 1.96*se 
return(ub)
}

# --------------------------------------------------------------------------------------

# alcohol categories (simah definition)
dat <- dat %>% 
  mutate(AlcCAT.simah = factor(ifelse(drinkingstatus_detailed == "Lifetime abstainer", "LA", 
                                      ifelse(drinkingstatus_detailed == "Former drinker", "FD", 
                                             ifelse(sex_recode=="Male" & gramsperday_upshifted>0 & gramsperday_upshifted<=40, "Low risk",
                                                    ifelse(sex_recode=="Female" & gramsperday_upshifted>0 & gramsperday_upshifted<=20, "Low risk",
                                                           ifelse(sex_recode=="Male" & gramsperday_upshifted>40 & gramsperday_upshifted<=60, "Medium risk",
                                                                  ifelse(sex_recode=="Female" & gramsperday_upshifted>20 & gramsperday_upshifted<=40, "Medium risk",
                                                                         ifelse(sex_recode=="Male" & gramsperday_upshifted>60, "High risk",
                                                                                ifelse(sex_recode=="Female" & gramsperday_upshifted>40, "High risk", NA)))))))), 
                               levels = c("LA", "FD", "Low risk", "Medium risk", "High risk")))

dat[, LA := ifelse(AlcCAT.simah == "LA", 1, 0)]
dat[, FD := ifelse(AlcCAT.simah == "FD", 1, 0)]
dat[, AlcCAT.low := ifelse(AlcCAT.simah == "Low risk", 1, 0)]
dat[, AlcCAT.medium := ifelse(AlcCAT.simah == "Medium risk", 1, 0)]
dat[, AlcCAT.high := ifelse(AlcCAT.simah == "High risk", 1, 0)]

# exclude USA (?)
dat <- copy(dat[State != "USA"])

# --------------------------------------------------------------------------------------

# time trend drinking categories by sex and SES

LA <- dat %>% group_by(YEAR, sex_recode, education_summary) %>% 
                summarise(LA = wtd.mean(LA, final_sample_weight),
                          LA.lb = wtd.lb(LA, final_sample_weight),
                          LA.ub = wtd.ub(LA, final_sample_weight)) %>% as.data.table


FD <- dat %>% group_by(YEAR, sex_recode, education_summary) %>% 
  summarise(FD = wtd.mean(FD, final_sample_weight),
            FD.lb = wtd.lb(FD, final_sample_weight),
            FD.ub = wtd.ub(FD, final_sample_weight)) %>% as.data.table

LOW <- dat %>% group_by(YEAR, sex_recode, education_summary) %>% 
               summarise(LOW = wtd.mean(AlcCAT.low, final_sample_weight),
                         LOW.lb = wtd.lb(AlcCAT.low, final_sample_weight),
                         LOW.ub = wtd.ub(AlcCAT.low, final_sample_weight)) %>% as.data.table

MEDIUM <- dat %>% group_by(YEAR, sex_recode, education_summary) %>% 
                  summarise(MEDIUM = wtd.mean(AlcCAT.medium, final_sample_weight),
                            MEDIUM.lb = wtd.lb(AlcCAT.medium, final_sample_weight),
                            MEDIUM.ub = wtd.ub(AlcCAT.medium, final_sample_weight)) %>% as.data.table

HIGH <- dat %>% group_by(YEAR, sex_recode, education_summary) %>% 
                summarise(HIGH = wtd.mean(AlcCAT.high, final_sample_weight),
                          HIGH.lb = wtd.lb(AlcCAT.high, final_sample_weight),
                          HIGH.ub = wtd.ub(AlcCAT.high, final_sample_weight)) %>% as.data.table

drinkCAT <- merge(LA, FD, by = c("YEAR", "sex_recode", "education_summary"), all = T)
drinkCAT <- merge(drinkCAT, LOW, by = c("YEAR", "sex_recode", "education_summary"), all = T)
drinkCAT <- merge(drinkCAT, MEDIUM, by = c("YEAR", "sex_recode", "education_summary"), all = T)
drinkCAT <- merge(drinkCAT, HIGH, by = c("YEAR", "sex_recode", "education_summary"), all = T)

ggplot(data = drinkCAT, aes(x = YEAR, y = LA, color = as.factor(sex_recode))) +
  geom_point() + geom_line() + geom_pointrange(aes(ymin = LA.lb, ymax = LA.ub)) +
  facet_grid(rows = vars(factor(education_summary, levels = c("LEHS", "SomeC", "College"))), scale = "free") +
  scale_y_continuous(labels = scales::percent, name = "Prevalence past-year alcohol use (weighted)") +
  theme_bw() + theme(legend.position="bottom", legend.title=element_blank(), strip.background = element_rect(fill="white")) 
ggsave(paste0('brfss/outputs/figures_trend/', DATE, '_ANY.png'), dpi=300, width = 12, height = 7)

ggplot(data = drinkCAT, aes(x = YEAR, y = LOW, color = as.factor(sex_recode))) +
  geom_point() + geom_line() + geom_pointrange(aes(ymin = LOW.lb, ymax = LOW.ub)) +
  facet_grid(rows = vars(factor(education_summary, levels = c("LEHS", "SomeC", "College"))), scale = "free") +
  scale_y_continuous(labels = scales::percent, name = "Prevalence low risk drinking (weighted)") +
  theme_bw() + theme(legend.position="bottom", legend.title=element_blank(), strip.background = element_rect(fill="white")) 
ggsave(paste0('brfss/outputs/figures_trend/', DATE, '_LOW RISK.png'), dpi=300, width = 12, height = 7)

ggplot(data = drinkCAT, aes(x = YEAR, y = MEDIUM, color = as.factor(sex_recode))) +
  geom_point() + geom_line() + geom_pointrange(aes(ymin = MEDIUM.lb, ymax = MEDIUM.ub)) +
  facet_grid(rows = vars(factor(education_summary, levels = c("LEHS", "SomeC", "College"))), scale = "free") +
  scale_y_continuous(labels = scales::percent, name = "Prevalence medium risk drinking (weighted)") +
  theme_bw() + theme(legend.position="bottom", legend.title=element_blank(), strip.background = element_rect(fill="white")) 
ggsave(paste0('brfss/outputs/figures_trend/', DATE, '_MEDIUM RISK.png'), dpi=300, width = 12, height = 7)

ggplot(data = drinkCAT, aes(x = YEAR, y = HIGH, color = as.factor(sex_recode))) +
  geom_point() + geom_line() + geom_pointrange(aes(ymin = HIGH.lb, ymax = HIGH.ub)) +
  facet_grid(rows = vars(factor(education_summary, levels = c("LEHS", "SomeC", "College"))), scale = "free") +
  scale_y_continuous(labels = scales::percent, name = "Prevalence high risk drinking (weighted)") +
  theme_bw() + theme(legend.position="bottom", legend.title=element_blank(), strip.background = element_rect(fill="white")) 
ggsave(paste0('brfss/outputs/figures_trend/', DATE, '_HIGH RISK.png'), dpi=300, width = 12, height = 7)

# TABLE
write_xlsx(drinkCAT[,.(YEAR, sex_recode, education_summary,LA,FD,LOW,LOW.lb,LOW.ub,MEDIUM,MEDIUM.lb,MEDIUM.ub,HIGH,HIGH.lb,HIGH.ub)], "brfss/outputs/BRFSS_trend_AlcCAT.xlsx")

# --------------------------------------------------------------------------------------

# time trend GPD (alc-user only) by sex and SES

GPD <- dat %>% filter(gramsperday_upshifted > 0) %>% group_by(YEAR, sex_recode, education_summary) %>% 
               summarise(GPD.mean = wtd.mean(gramsperday_upshifted, final_sample_weight),
                         GPD.lb = wtd.lb(gramsperday_upshifted, final_sample_weight),
                         GPD.ub = wtd.ub(gramsperday_upshifted, final_sample_weight)) %>% as.data.table

ggplot(data = GPD, aes(x = YEAR, y = GPD.mean, color = as.factor(sex_recode))) +
  geom_point() + geom_line() + geom_pointrange(aes(ymin = GPD.lb, ymax = GPD.ub)) +
  facet_grid(rows = vars(factor(education_summary, levels = c("LEHS", "SomeC", "College"))), scale = "free") +
  labs(y = "Average grams per day, drinkers only (weighted)") +
  theme_bw() + theme(legend.position="bottom", legend.title=element_blank(), strip.background = element_rect(fill="white")) 
ggsave(paste0('brfss/outputs/figures_trend/', DATE, '_GPD drinkers.png'), dpi=300, width = 12, height = 7)

# TABLE
write_xlsx(GPD, "brfss/outputs/BRFSS_trend_GPD_current drinkers.xlsx")
