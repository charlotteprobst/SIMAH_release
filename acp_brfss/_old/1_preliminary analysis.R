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
library(Hmisc)

# --------------------------------------------------------------------------------------

# weighted confidence interval: lower bound
wtd.lb <- function(x, w) {
  n    <- length(x)
  mean <- wtd.mean(x, w)
  se   <- sqrt(wtd.var(x, w))/sqrt(length(x))
  lb   <- mean + qt(0.025,n-1)*se
  return(lb)
}

# weighted confidence interval: upper bound
wtd.ub <- function(x, w) {
  n    <- length(x)
  mean <- wtd.mean(x, w)
  se   <- sqrt(wtd.var(x, w))/sqrt(length(x))
  ub   <- mean + qt(0.975,n-1)*se 
  return(ub)
}

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# LOAD DATA
# ----------------------------------------------------------------

rm(list = ls())
setwd("/Users/carolinkilian/Desktop/SIMAH_workplace/")
DATE <- 20230207

datBRFSS <- data.table(readRDS("brfss/processed_data/BRFSS_upshifted_2000_2020_final.RDS"))
data <- datBRFSS[State %like% "Florida|California|Indiana|Pennsylvania|Texas"]
datBAN <- datBRFSS[State %like% "Colorado"]

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# CHECK DATA
# ----------------------------------------------------------------

# quarterly data
datBAN[, Q := ifelse(surveymonth %like% "Feb|Mar|Apr", 1, ifelse(surveymonth %like% "May|Jun|Jul", 2,
                                                               ifelse(surveymonth %like% "Aug|Sep|Oct", 3, ifelse(surveymonth %like% "Nov|Dec|Jan", 4, NA))))]
datBAN[, QYEAR := ifelse(surveymonth %like% "Jan", paste0(surveyyear-1, "/", Q), paste0(surveyyear, "/", Q))]

datBAN <- copy(datBAN[QYEAR != "1999/4" & QYEAR != "2021/1"]) # drop those interviewed in Jan 2000 (150) and 2021 (17)
datBAN[, QYEAR := as.factor(QYEAR)]

datBAN[, qyear := as.numeric(QYEAR)]

datBAN[, education := factor(education_summary, levels = c("LEHS", "SomeC", "College"))]

# histogram 
ggplot(data = datBAN, aes(x = qyear, fill = as.factor(sex_recode))) +
  geom_histogram(position = "dodge2", binwidth = 1) + 
  geom_hline(yintercept = 100) + 
  facet_grid(rows = vars(as.factor(State)), cols = vars(as.factor(education)), scales = "free_y") +
  theme_bw() + theme(legend.position="bottom", legend.title=element_blank(), strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/', DATE, '_TS_HISTO_obsSEX.png'), dpi=300, width = 12, height = 7)


# cap estimates >200 GPD
summary(data$gramsperday_orig)
summary(data[gramsperday_upshifted > 200]$gramsperday_orig)
data[, GPD_upshifted_capped := ifelse(gramsperday_upshifted > 200, 200, gramsperday_upshifted)]
data[, GPD_orig_capped := ifelse(gramsperday_orig > 200, 200, gramsperday_orig)]

# GPD distribution
ggplot(data = data[data$GPD_upshifted_capped > 0]) +
  geom_histogram(aes(x = GPD_upshifted_capped, color = sex_recode), binwidth = 1)

# subgroups
data[, education := factor(education_summary, levels = c("LEHS", "SomeC", "College"))]

# drinking categories
data <- data %>% 
  mutate(AlcCAT.simah = factor(ifelse(sex_recode=="Male" & GPD_upshifted_capped>0 &
                                        GPD_upshifted_capped<=40, "Low risk",
                                      ifelse(sex_recode=="Female" & GPD_upshifted_capped>0 &
                                               GPD_upshifted_capped<=20, "Low risk",
                                             ifelse(sex_recode=="Male" & GPD_upshifted_capped>40 &
                                                      GPD_upshifted_capped<=60, "Medium risk",
                                                    ifelse(sex_recode=="Female" & GPD_upshifted_capped>20 &
                                                             GPD_upshifted_capped<=40, "Medium risk",
                                                           ifelse(sex_recode=="Male" & GPD_upshifted_capped>60 &
                                                                    GPD_upshifted_capped<=100, "High risk",
                                                                  ifelse(sex_recode=="Female" & GPD_upshifted_capped>40 &
                                                                           GPD_upshifted_capped<=60, "High risk", 
                                                                         ifelse(sex_recode=="Male" & GPD_upshifted_capped>100, "Very high risk",
                                                                                ifelse(sex_recode=="Female" & GPD_upshifted_capped>60, "Very high risk", NA)))))))), 
                               levels = c("Low risk", "Medium risk", "High risk", "Very high risk")))

# quarterly data
data[, Q := ifelse(surveymonth %like% "Feb|Mar|Apr", 1, ifelse(surveymonth %like% "May|Jun|Jul", 2,
                   ifelse(surveymonth %like% "Aug|Sep|Oct", 3, ifelse(surveymonth %like% "Nov|Dec|Jan", 4, NA))))]
data[, QYEAR := ifelse(surveymonth %like% "Jan", paste0(surveyyear-1, "/", Q), paste0(surveyyear, "/", Q))]

pdat <- copy(data[QYEAR != "1999/4" & QYEAR != "2021/1"]) # drop those interviewed in Jan 2000 (150) and 2021 (17)
pdat[, QYEAR := as.factor(QYEAR)]

# intervention year
pdat[, INT_YEAR := ifelse(State %like% "California|Florida|Texas", NA, ifelse(State %like% "Pennsylvania", 2003, 
                          ifelse(State %like% "Tennessee", 2018, ifelse(State %like% "Indiana", 2018, NA))))]
pdat[, INT_QYEAR := ifelse(State %like% "California|Florida|Texas", NA, ifelse(State %like% "Pennsylvania", "2003/3", 
                          ifelse(State %like% "Tennessee", "2018/2", ifelse(State %like% "Indiana", "2018/2", NA))))]

# GPD distribution over time (quarterly, unweighted data) by state
pdat[, qyear := as.numeric(QYEAR)]
pdat[, int_qyear := ifelse(INT_QYEAR == QYEAR, qyear, NA)]

# GPD distribution (smooth) by SES - drinkers only
ggplot(data = pdat[pdat$GPD_upshifted_capped > 0 & State %like% "California|Florida|Indiana"]) +
  geom_smooth(aes(x = qyear, y = GPD_upshifted_capped, color = as.factor(State))) + 
  geom_vline(aes(xintercept = int_qyear)) +
  facet_grid(rows = vars(as.factor(education)), cols = vars(sex_recode), scales = "free") +
  theme_bw() + theme(legend.position="bottom", legend.title=element_blank(), strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/', DATE, '_TS_COMPsmooth_StateSES_GR1.png'), dpi=300, width = 12, height = 7)

ggplot(data = pdat[pdat$GPD_upshifted_capped > 0 & State %like% "Pennsylvania|Texas"]) +
  geom_smooth(aes(x = qyear, y = GPD_upshifted_capped, color = as.factor(State))) + 
  geom_vline(aes(xintercept = int_qyear)) +
  facet_grid(rows = vars(as.factor(education)), cols = vars(sex_recode), scales = "free") +
  theme_bw() + theme(legend.position="bottom", legend.title=element_blank(), strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/', DATE, '_TS_COMPsmooth_StateSES_GR2.png'), dpi=300, width = 12, height = 7)

# weighted mean consumption - drinkers only
pdatSES <- pdat[pdat$GPD_upshifted_capped > 0] %>% group_by(State, YEAR, sex_recode, education, INT_YEAR) %>% 
  summarise(GPD.mean = wtd.mean(GPD_upshifted_capped, final_sample_weight),
            GPD.lb = wtd.lb(GPD_upshifted_capped, final_sample_weight),
            GPD.ub = wtd.ub(GPD_upshifted_capped, final_sample_weight)) %>% as.data.table

# GPD distribution over time by state
ggplot(data = pdatSES[State %like% "California|Florida|Indiana"], aes(x = YEAR, y = GPD.mean, color = as.factor(State))) +
  geom_point() + geom_line() + 
  geom_pointrange(aes(ymin = GPD.lb, ymax = GPD.ub)) + 
  geom_vline(aes(xintercept = INT_YEAR)) +
  facet_grid(rows = vars(as.factor(education)), cols = vars(as.factor(sex_recode)), scales = "free") +
  theme_bw() + theme(legend.position="bottom", legend.title=element_blank(), strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/', DATE, '_annual TS_COMP state by SES.png'), dpi=300, width = 12, height = 7)

# GPD distribution over time by group
ggplot(data = pdatSES, aes(x = YEAR, y = GPD.mean, color = as.factor(State))) +
  geom_line() + geom_pointrange(aes(ymin = GPD.lb, ymax = GPD.ub)) + 
  facet_grid(rows = vars(as.factor(education)), scales = "free") +
  theme_bw() + theme(legend.position="bottom", legend.title=element_blank(), strip.background = element_rect(fill="white")) 
ggsave(paste0('acp_brfss/outputs/figures/', DATE, '_annual TS_education.png'), dpi=300, width = 12, height = 7)

# GPD distribution over time (quarterly, unweighted data) by state
pdat[, qyear := as.numeric(QYEAR)]
pdat[, int_qyear := ifelse(INT_QYEAR == QYEAR, qyear, NA)]

  # education
  ggplot(data = pdat, aes(x = qyear, y = GPD_upshifted_capped, color = education)) +
    geom_smooth() + geom_vline(aes(xintercept = int_qyear)) +
    facet_grid(cols = vars(as.factor(State)), rows = vars(sex_recode), scales = "free") +
    theme_bw() + theme(legend.position="bottom", legend.title=element_blank(), strip.background = element_rect(fill="white")) 
  ggsave(paste0('acp_brfss/outputs/figures/', DATE, '_quartly TS (unweighted) education_state.png'), dpi=300, width = 12, height = 5)
 
  # race ethnicity  
  ggplot(data = pdat, aes(x = qyear, y = GPD_upshifted_capped, color = race_eth)) +
    geom_smooth() + geom_vline(aes(xintercept = int_qyear)) +
    facet_grid(cols = vars(as.factor(State)), rows = vars(sex_recode), scales = "free") +
    theme_bw() + theme(legend.position="bottom", legend.title=element_blank(), strip.background = element_rect(fill="white")) 
  ggsave(paste0('acp_brfss/outputs/figures/', DATE, '_quartly TS (unweighted) race ethn_state.png'), dpi=300, width = 12, height = 5)

  # drinking categories
  ggplot(data = pdat[GPD_upshifted_capped > 0], aes(x = qyear, y = GPD_upshifted_capped, color = sex_recode)) +
    geom_smooth() + geom_vline(aes(xintercept = int_qyear)) +
    facet_grid(cols = vars(as.factor(State)), rows = vars(AlcCAT.simah), scales = "free") +
    theme_bw() + theme(legend.position="bottom", legend.title=element_blank(), strip.background = element_rect(fill="white")) 
  ggsave(paste0('acp_brfss/outputs/figures/', DATE, '_quartly TS (unweighted) drink cat_state.png'), dpi=300, width = 12, height = 5)
  
  
# ----------------------------------------------------------------
# PRELIMINARY ANALYSIS
# ----------------------------------------------------------------

# prepare variables
summary(data$age)
data[, age_group := as.factor(ifelse(age_var < 35, 1, ifelse(age_var > 34 & age_var < 65, 2, ifelse(age_var > 64, 3, NA))))]
data[, table(age_var, age_group)]
  
data[, no_sun_sales := as.factor(no_sun_sales)]
data[, controlstate := as.factor(controlstate)]
data[, marital_status := as.factor(marital_status)]
  
data[GPD_upshifted_capped > 0, logGDP := log(GPD_upshifted_capped)]
  
  
