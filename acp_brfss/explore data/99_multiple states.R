# ----------------------------------------------------------------
# ----------------------------------------------------------------
## Project: SIMAH  
## Title: Sunday sales ban  
## State: Indiana
## Author: Carolin Kilian
## Start Date: 09/02/2023
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

setwd("/Users/carolinkilian/Desktop/SIMAH_workplace/")
DATE <- 20230327

datBRFSS <- data.table(readRDS("brfss/processed_data/BRFSS_upshifted_2000_2020_final.RDS"))
rawBRFSS <- data.table(readRDS("brfss/raw_data/brfss_2018_2020_raw.RDS"))
#data <- datBRFSS[State %like% "Connecticut|Delaware|Massachusetts|Rhode Island|Tennessee|Indiana|Arkansas|Georgia|Illinois|Kansas|Nebraska|South Carolina"]
data <- rawBRFSS[State %like% "Connecticut|Delaware|Massachusetts|Rhode Island|Tennessee|Indiana|Arkansas|Georgia|Illinois|Kansas|Nebraska|South Carolina"]
#simah <- datBRFSS[State %like% "California|Colorado|Florida|Indiana|Kentucky|Louisiana|Massachusetts|Michigan|Minnesota|Missouri|New York|Oregon|Pennsylvania|Tennessee|Texas"]

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# PREPARE DATA
# ----------------------------------------------------------------

data[, int.state := ifelse(State %like% "Connecticut|Delaware|Massachusetts|Rhode Island|Tennessee|Indiana", 1, 0)]
data[, table(int.state, State)]

# subgroups
data[, education := factor(education_summary, levels = c("LEHS", "SomeC", "College"))]
data[, sex := ifelse(sex_recode == "Female", "Women", ifelse(sex_recode == "Male", "Men", NA))]

# quarters
data[, Q := ifelse(surveymonth %like% "Feb|Mar|Apr", 125, ifelse(surveymonth %like% "May|Jun|Jul", 375,
                   ifelse(surveymonth %like% "Aug|Sep|Oct", 625, ifelse(surveymonth %like% "Nov|Dec|Jan", 875, NA))))]
data[, QYEAR := as.numeric(ifelse(surveymonth %like% "Jan", paste0(YEAR-1, ".", Q), paste0(YEAR, ".", Q)))]

ggplot(data = data, aes(x = QYEAR)) + geom_histogram(stat = "count") + facet_grid(vars(State)) + theme(axis.text.x = element_text(angle = 90))
# drop 2000.875 (too few observations)
# drop 2020.375 onwards
# Illinois, Georgia and Arkansas have generally a lower number of participants

data <- copy(data[QYEAR > 2000.875])
data <- copy(data[QYEAR < 2020.375])

ggplot(data = data, aes(x = QYEAR)) + geom_histogram(stat = "count") + facet_grid(vars(State)) + theme(axis.text.x = element_text(angle = 90))

# intervention 
#data[, INT_YEAR := ifelse(int.state == 1, 2018, NA)]
#data[, INT_QYEAR := ifelse(int.state == 1, 2018.125, NA)]

# cap estimates >200 GPD
summary(data$gramsperday_orig)
data[gramsperday_orig > 200] # 720 observations
summary(data[gramsperday_upshifted > 200]$gramsperday_orig)
data[gramsperday_upshifted > 200] # 1918 observations
data[, GPD_upshifted_capped := ifelse(gramsperday_upshifted > 200, 200, gramsperday_upshifted)]
data[, GPD_orig_capped := ifelse(gramsperday_orig > 200, 200, gramsperday_orig)]

# drop abstainers
#data <- datIND[drinkingstatus_upshifted == 1]
data <- data[drinkingstatus_orig == 1] 

# select data
data <- data[, .(State, YEAR, QYEAR, #INT_YEAR, INT_QYEAR, 
                 final_sample_weight, sex, education, GPD_orig_capped, GPD_upshifted_capped)]

# ----------------------------------------------------------------
# DISTRIBUTION
# ----------------------------------------------------------------

ggplot(data = data, aes(x = QYEAR, fill = as.factor(sex))) + geom_histogram(stat = "count", position = "dodge2") + geom_hline(yintercept = c(50, 100)) + 
  facet_grid(cols = vars(as.factor(education)), rows = vars(as.factor(State)), scales = "free_y") +
  scale_x_continuous(breaks = seq(2000, 2020, 1), limits = c(2000, 2020)) +
  theme_bw() + theme(axis.text.x = element_text(angle = 90), strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/multiple states/', DATE, '_HIST_BY_GROUP2.png'), dpi=300, width = 12, height = 7)
# not enough observations for Tennessee 

# SEX

#smooth function
ggplot(data = data) +
  geom_smooth(aes(x = QYEAR, y = GPD_upshifted_capped, color = State)) + 
  #geom_vline(aes(xintercept = INT_QYEAR)) +
  scale_x_continuous(breaks = seq(2001, 2020, 1)) +
  facet_grid(rows = vars(as.factor(sex)), scales = "free") +
  labs(y = "GPD (unweighted)") + theme_bw() + theme(strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/multiple states/', DATE, '_smoothGPD_BY_SEX.png'), dpi=300, width = 12, height = 7)

#smooth function BUT exclude South Carolina and Nebraska
ggplot(data = data[sex == "Men"]) +
  geom_line(aes(x = QYEAR, y = GPD_orig_capped, color = State)) + 
  #geom_vline(aes(xintercept = INT_QYEAR)) +
  scale_x_continuous(breaks = seq(2001, 2020, 1)) +
  facet_grid(rows = vars(as.factor(education)), scales = "free") +
  labs(y = "GPD (unweighted)") + theme_bw() + theme(strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/multiple states/', DATE, '_smoothGPD_BY_SEX_excl SC NE.png'), dpi=300, width = 12, height = 7)

# GPD weighted (simply weights, not accounting for sampling design)
wdat.gpd.sex <- data %>% 
  group_by(State, QYEAR, sex) %>%
  summarise(GPD = wtd.mean(GPD_upshifted_capped, final_sample_weight),
            GPD.LB = wtd.lb(GPD_upshifted_capped, final_sample_weight),
            GPD.UB = wtd.ub(GPD_upshifted_capped, final_sample_weight),
            n = n(),
            INT_QYEAR = mean(INT_QYEAR)) %>% as.data.table()

# GPD distribution weighted 
ggplot(data = wdat.gpd.sex[State %like% "Kansas"]) + geom_line(aes(x = QYEAR, y = GPD, color = State)) + geom_vline(aes(xintercept = INT_QYEAR)) +
  scale_x_continuous(breaks = seq(2000, 2020, 1)) +
  facet_grid(rows = vars(sex), scales = "free") +
  labs(y = "GPD (weighted)") + theme_bw() + theme(legend.position = "right", strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/multiple states/', DATE, '_GPDweighted_BY_SEX.png'), dpi=300, width = 12, height = 7)

# SES

#smooth function
ggplot(data = data) +
  geom_smooth(aes(x = QYEAR, y = GPD_upshifted_capped, color = State)) + 
  #geom_vline(aes(xintercept = INT_QYEAR)) +
  scale_x_continuous(breaks = seq(2001, 2020, 1)) +
  facet_grid(rows = vars(as.factor(education)), scales = "free") +
  labs(y = "GPD (unweighted)") + theme_bw() + theme(strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/multiple states/', DATE, '_smoothGPD_BY_SES.png'), dpi=300, width = 12, height = 7)

# GPD weighted (simply weights, not accounting for sampling design)
wdat.gpd.ses <- data %>% 
  group_by(State, QYEAR, education) %>%
  summarise(GPD = wtd.mean(GPD_orig_capped, final_sample_weight),
            GPD.LB = wtd.lb(GPD_orig_capped, final_sample_weight),
            GPD.UB = wtd.ub(GPD_orig_capped, final_sample_weight),
            n = n(),
            #INT_QYEAR = mean(INT_QYEAR)
            ) %>% as.data.table()

# GPD distribution weighted
ggplot(data = wdat.gpd.ses) + geom_line(aes(x = QYEAR, y = GPD, color = State)) + #geom_vline(aes(xintercept = INT_QYEAR)) +
  scale_x_continuous(breaks = seq(2000, 2020, 1)) + 
  facet_grid(rows = vars(as.factor(education)), scales = "free") +
  labs(y = "GPD (weighted)") + theme_bw() + theme(legend.position = "right", strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/Indiana/', DATE, '_GPDweighted_BY_SES.png'), dpi=300, width = 12, height = 7)

# GROUP: SEX x SES

# GPD weighted (simply weights, not accounting for sampling design)
wdat.gpd <- data %>% 
  group_by(QYEAR, sex, education) %>%
  summarise(GPD = wtd.mean(GPD_orig_capped, final_sample_weight),
            GPD.LB = wtd.lb(GPD_orig_capped, final_sample_weight),
            GPD.UB = wtd.ub(GPD_orig_capped, final_sample_weight),
            n = n(),
            INT_QYEAR = mean(INT_QYEAR), w.unemp.rate = mean(w.unemp.rate), m.unemp.rate = mean(m.unemp.rate),
            gdp = mean(gdp), pop18_24 = mean(pop18_24), w.pop18_24 = mean(w.pop18_24), m.pop18_24 = mean(m.pop18_24)) %>% as.data.table()

wdat.gpd.long <- reshape(wdat.gpd, idvar = c("QYEAR", "sex", "education", "n", "INT_QYEAR"),
                         varying = c("GPD", "GPD.LB", "GPD.UB"), v.name = c("GPD"), 
                         times = c("mean", "LB", "UB"), direction = "long")

# GPD distribution weighted
ggplot(data = wdat.gpd.long) + geom_line(aes(x = QYEAR, y = GPD, color = time)) + geom_vline(aes(xintercept = INT_QYEAR)) +
  scale_x_continuous(breaks = seq(2000, 2020, 1)) + scale_colour_manual(values = c("grey", "black", "grey")) +
  facet_grid(rows = vars(as.factor(education)), cols = vars(sex), scales = "free") +
  labs(y = "GPD (weighted)") + theme_bw() + theme(legend.position = "none", strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/Indiana/', DATE, '_GPDweighted_BY_GROUP.png'), dpi=300, width = 12, height = 7)


     
