# ----------------------------------------------------------------
# ----------------------------------------------------------------
## Project: SIMAH  
## Title: Synthetic control for Sunday sales ban  
## States: ALL / intervention state: Indiana
## Author: Carolin Kilian
## Start Date: 22/03/2023
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
library(Synth)
library(corrplot)
library(plm)

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
DATE <- 20230328

# MAIN DATA
datBRFSS <- data.table(readRDS("brfss/processed_data/BRFSS_upshifted_2000_2020_final.RDS"))

# select only states where there was no change in Sunday sales ban policy between 2011 and 2020; SC excluded 
data <- datBRFSS[State %like% "Indiana|Alabama|Alaska|Arizona|Arkansas|California|Colorado|Delaware|DC|Florida|Georgia|Hawaii|Idaho|Illinois|
                               |Iowa|Kansas|Louisiana|Maine|Maryland|Massachusetts|Michigan|Mississippi|Missouri|Montana|Nebraska|Nevada|New Hampshire|New Jersey|
                               |New York|New Mexico|North Carolina|North Dakota|Ohio|Oregon|Pennsylvania|Puerto Rico|Rhode Island|South Dakota|
                               |Texas|Utah|Vermont|Virginia|Washington|West Virginia|Wisconsin|Wyoming
                               |Connecticut|Oklahoma|Tennessee"] # for explorative purposes
data <- data[surveyyear >= 2001]

# PREDICTOR DATA
rdat.inc <- read.csv("acp_brfss/data/20230322_US per capita income_20112020.csv", skip = 3, nrows = 60)
  # state-specific quarterly per capita income: https://apps.bea.gov/itable/?ReqID=70&step=1#eyJhcHBpZCI6NzAsInN0ZXBzIjpbMSwyNCwyOSwyNSwzMSwyNiwyNywzMF0sImRhdGEiOltbIlRhYmxlSWQiLCIzNiJdLFsiQ2xhc3NpZmljYXRpb24iLCJOb24tSW5kdXN0cnkiXSxbIk1ham9yX0FyZWEiLCIwIl0sWyJTdGF0ZSIsWyIwIl1dLFsiQXJlYSIsWyJYWCJdXSxbIlN0YXRpc3RpYyIsWyIzIl1dLFsiVW5pdF9vZl9tZWFzdXJlIiwiTGV2ZWxzIl0sWyJZZWFyIixbIjIwMjAiLCIyMDE5IiwiMjAxOCIsIjIwMTciLCIyMDE2IiwiMjAxNSIsIjIwMTQiLCIyMDEzIiwiMjAxMiIsIjIwMTEiXV0sWyJZZWFyQmVnaW4iLCItMSJdLFsiWWVhcl9FbmQiLCItMSJdXX0= 
rdat.pop <- read.xlsx("acp_brfss/data/20230228_US POP_2010-2020.xlsx", sheet = 1, startRow = 1)
  # state-specific annual population data based on US census: https://www.census.gov/programs-surveys/popest/technical-documentation/research/evaluation-estimates/2020-evaluation-estimates/2010s-state-detail.html 
rdat.cpi <- read.csv("acp_brfss/data/20230322_US CPI_CUSR0000SAF116_20112020.csv")
  # US monthly cpi for alcoholic beverage (US city average, urban population): https://fred.stlouisfed.org/series/CUSR0000SAF116
rdat.urban <- read.xlsx("acp_brfss/data/20230322_US Urban POP_20102020.xlsx", sheet = 1)
  # share state-specific urban population 2010 and 2020
rdat.apc <- read.xlsx("acp_brfss/data/20230327_NIAAA APC_1990-2020.xlsx", sheet = 1, startRow = 1)
  # annual state-specific APC data

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# PREPARE DATA
# ----------------------------------------------------------------

# subgroups
data[, education := factor(education_summary, levels = c("LEHS", "SomeC", "College"))]
data[, sex := ifelse(sex_recode == "Female", "Women", ifelse(sex_recode == "Male", "Men", NA))]

# quarters
data[, Q := ifelse(surveymonth %like% "Feb|Mar|Apr", 125, ifelse(surveymonth %like% "May|Jun|Jul", 375,
                   ifelse(surveymonth %like% "Aug|Sep|Oct", 625, ifelse(surveymonth %like% "Nov|Dec|Jan", 875, NA))))]
data[, QYEAR := as.numeric(ifelse(surveymonth %like% "Jan", paste0(YEAR-1, ".", Q), paste0(YEAR, ".", Q)))]

# exclude COVID years
data <- copy(data[ QYEAR < 2020.375])

# setting Indiana as intervention state
data[, int.state := ifelse(State %like% "Indiana", 1, 0)]
data[, INT_YEAR := ifelse(int.state == 1, 2018, NA)]
data[, INT_QYEAR := ifelse(int.state == 1, 2018.125, NA)]

# # setting Connecticut as intervention state
# data[, int.state := ifelse(State %like% "Connecticut", 1, 0)]
# data[, INT_YEAR := ifelse(int.state == 1, 2012, NA)]
# data[, INT_QYEAR := ifelse(int.state == 1, 2012.375, NA)]

# capping alcohol consumption >200 GPD
summary(data$gramsperday_orig)
data[gramsperday_orig > 200] # 2346 observations
summary(data[gramsperday_upshifted > 200]$gramsperday_orig)
data[gramsperday_upshifted > 200] # 6538 observations

data[, GPD_upshifted_capped := ifelse(gramsperday_upshifted > 200, 200, gramsperday_upshifted)]
data[, GPD_orig_capped := ifelse(gramsperday_orig > 200, 200, gramsperday_orig)]

# SELECT DATA
pdat <- data[, .(State, int.state, YEAR, QYEAR, INT_YEAR, INT_QYEAR, final_sample_weight, sex, education, drinkingstatus_orig, GPD_orig_capped, GPD_upshifted_capped)]

# INCLUDE CONTROL STATE VECTOR

pdat <- pdat[, CONTROL := ifelse(State %like% "Alabama|Idaho|Iowa|Maine|Michigan|Mississippi|Montana|New Hampshire|North Carolina
                                              |Ohio|Oregon|Pennsylvania|Texas|Utah|Vermont|Virginia|Washington|Wisconsin|Wyoming", 1, 0)] # for explorative purposes

# ----------------------------------------------------------------
# DISTRIBUTION
# ----------------------------------------------------------------

# check counts by sex
pdat %>% filter(drinkingstatus_orig != 0) %>% group_by(State, QYEAR, sex) %>% summarise(n = n()) %>% ungroup() %>% 
  group_by(State) %>% summarise(small.sample = sum(ifelse(n <= 50, 1, 0))) %>% filter(small.sample > 0)
  # Arkansas, DC, Louisiana, Mississippi, New Jersey, Pennsylvania
sexStates <- pdat %>% filter(drinkingstatus_orig != 0) %>% group_by(State, QYEAR, sex) %>% summarise(n = n()) %>% ungroup() %>% 
  group_by(State) %>% summarise(small.sample = sum(ifelse(n <= 50, 1, 0))) %>% filter(small.sample == 0) %>% pull("State")

pdatSEX <- pdat %>% filter(State %in% sexStates)
  
# check counts by education
pdat %>% filter(drinkingstatus_orig != 0) %>% group_by(State, QYEAR, education) %>% summarise(n = n()) %>% ungroup() %>% 
  group_by(State) %>% summarise(small.sample = sum(ifelse(n <= 50, 1, 0))) %>% filter(small.sample > 0)
  # Alaska, Arkansas, California, Delaware, Georgia, Idaho, Louisiana, Mississippi, Nevada, New Jersey, Pennsylvania
eduStates <- pdat %>% filter(drinkingstatus_orig != 0) %>% group_by(State, QYEAR, education) %>% summarise(n = n()) %>% ungroup() %>% 
  group_by(State) %>% summarise(small.sample = sum(ifelse(n <= 50, 1, 0))) %>% filter(small.sample == 0) %>% pull("State")

pdatEDU <- pdat %>% filter(State %in% eduStates)

# ----------------------------------------------------------------
# PREPARE MAIN DATA
# ----------------------------------------------------------------

# SEX

# ORIGINAL GDP: smooth function
ggplot(data = pdatSEX[drinkingstatus_orig != 0]) +
  geom_smooth(aes(x = QYEAR, y = GPD_orig_capped, color = State)) + 
  #geom_vline(aes(xintercept = INT_QYEAR)) +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) +
  facet_grid(rows = vars(as.factor(sex)), scales = "free") +
  labs(y = "GPD (original, unweighted)") + theme_bw() + theme(strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/INDIANA_SynthC/', DATE, '_GPDorig_BY_SEX.png'), dpi=300, width = 12, height = 7)

# UPSHIFTED GDP: smooth function
ggplot(data = pdatSEX[drinkingstatus_orig != 0]) +
  geom_smooth(aes(x = QYEAR, y = GPD_upshifted_capped, color = State)) + 
  #geom_vline(aes(xintercept = INT_QYEAR)) +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) +
  facet_grid(rows = vars(as.factor(sex)), scales = "free") +
  labs(y = "GPD (upshifted, unweighted)") + theme_bw() + theme(strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/INDIANA_SynthC/', DATE, '_GPDupsh_BY_SEX.png'), dpi=300, width = 12, height = 7)

# AGGREGATE GDP (upshifted, weighted)
gpdSEX <- pdatSEX %>% 
  filter(drinkingstatus_orig > 0) %>%
  group_by(State, QYEAR, sex) %>%
  summarise(GPD = wtd.mean(GPD_upshifted_capped, final_sample_weight),
            GPD.LB = wtd.lb(GPD_upshifted_capped, final_sample_weight),
            GPD.UB = wtd.ub(GPD_upshifted_capped, final_sample_weight),
            n = n(), INT_QYEAR = mean(INT_QYEAR), CONTROL = mean(CONTROL)) %>% as.data.table()
gpdSEX[, YEAR := as.integer(QYEAR)]

# EDUCATION

# ORIGINAL GDP: smooth function
ggplot(data = pdatEDU[drinkingstatus_orig != 0]) +
  geom_smooth(aes(x = QYEAR, y = GPD_orig_capped, color = State)) + 
  #geom_vline(aes(xintercept = INT_QYEAR)) +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) +
  facet_grid(rows = vars(as.factor(education)), scales = "free") +
  labs(y = "GPD (original, unweighted)") + theme_bw() + theme(strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/INDIANA_SynthC/', DATE, '_GPDorig_BY_SES.png'), dpi=300, width = 12, height = 7)

# UPSHIFTED GDP: smooth function
ggplot(data = pdatEDU[drinkingstatus_orig != 0]) +
  geom_smooth(aes(x = QYEAR, y = GPD_upshifted_capped, color = State)) + 
  #geom_vline(aes(xintercept = INT_QYEAR)) +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) +
  facet_grid(rows = vars(as.factor(education)), scales = "free") +
  labs(y = "GPD (upshifted, unweighted)") + theme_bw() + theme(strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/INDIANA_SynthC/', DATE, '_GPDupsh_BY_SES.png'), dpi=300, width = 12, height = 7)

# AGGREGATE GDP (upshifted, weighted)
gdpEDU <- data %>% 
  filter(drinkingstatus_orig > 0) %>%
  group_by(State, QYEAR, education) %>%
  summarise(GPD = wtd.mean(GPD_upshifted_capped, final_sample_weight),
            GPD.LB = wtd.lb(GPD_upshifted_capped, final_sample_weight),
            GPD.UB = wtd.ub(GPD_upshifted_capped, final_sample_weight),
            n = n(), INT_QYEAR = mean(INT_QYEAR), YEAR = round(mean(YEAR),0)) %>% as.data.table()

# ----------------------------------------------------------------
# PREPARE PREDICTOR DATA AND IDENTIFY RELEVANT PREDICTORS
# ----------------------------------------------------------------

# PER CAPITA INCOME

income <- reshape(rdat.inc, idvar = "GeoName", varying = c(3:42), v.name = "pcInc", 
                  times = c("2011.125", "2011.375", "2011.625", "2011.875", "2012.125", "2012.375", "2012.625", "2012.875", 
                            "2013.125", "2013.375", "2013.625", "2013.875", "2014.125", "2014.375", "2014.625", "2014.875", 
                            "2015.125", "2015.375", "2015.625", "2015.875", "2016.125", "2016.375", "2016.625", "2016.875", 
                            "2017.125", "2017.375", "2017.625", "2017.875", "2018.125", "2018.375", "2018.625", "2018.875", 
                            "2019.125", "2019.375", "2019.625", "2019.875", "2020.125", "2020.375", "2020.625", "2020.875"), 
                  direction = "long")
income[income$GeoName == "Alaska *",]$GeoName <- "Alaska"
income[income$GeoName == "Hawaii *",]$GeoName <- "Hawaii"
income$time <- as.numeric(income$time)

# income distribution 
ggplot(data = income) + geom_histogram(aes(x = log(as.numeric(pcInc)))) + theme_bw()

# log
income$log.pcInc <- log(income$pcInc)

# income trend 
ggplot(data = income) + geom_smooth(aes(x = as.numeric(time), y = log.pcInc, color = GeoName)) + 
  scale_x_continuous(breaks = seq(2011, 2020, 1)) + labs(y = "average personal income, per capita (log)") + 
  theme_bw() + theme(strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/INDIANA_SynthC/', DATE, '_pcIncome_trend.png'), dpi=300, width = 12, height = 7)

# PROPORTION OF YOUNG POPULATION

popAGE <- reshape(rdat.pop[rdat.pop$SEX == 0, ], idvar = c("STATE", "NAME", "AGE"), varying = c(8:19), v.name = "pop", 
                    times = c("ESTBASE2010_CIV", "POPEST2010_CIV", "POPEST2011_CIV", "POPEST2012_CIV", 
                              "POPEST2013_CIV", "POPEST2014_CIV", "POPEST2015_CIV", "POPEST2016_CIV", 
                              "POPEST2017_CIV", "POPEST2018_CIV", "POPEST2019_CIV", "POPEST2020_CIV"), 
                    direction = "long")
popAGE.young <- popAGE %>% filter(AGE >= 15 & AGE < 25 & !time %like% "ESTBASE") %>% group_by(NAME, time) %>% summarise(YPOP = sum(pop))
popAGE <- merge(popAGE.young, popAGE[popAGE$AGE == 999,], by = c("NAME", "time"))
popAGE <- popAGE %>% mutate(YPROP = YPOP / pop) %>% select(NAME, time, YPROP) %>% mutate(YEAR = as.numeric(substr(time, 7, 10))) %>% select(-time)

# age distribution 
ggplot(data = popAGE) + geom_histogram(aes(x = as.numeric(YPROP))) + theme_bw()

# age trend 
ggplot(data = popAGE) + geom_smooth(aes(x = as.numeric(YEAR), y = YPROP, color = NAME)) + 
  scale_x_continuous(breaks = seq(2011, 2020, 1)) + labs(y = "proportion of population aged 15-24 years") + 
  theme_bw() + theme(strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/INDIANA_SynthC/', DATE, '_YoungPopulation_trend.png'), dpi=300, width = 12, height = 7)

# PROPORTION OF MALE POPULATION

popSEX <- reshape(rdat.pop[rdat.pop$SEX != 2 & rdat.pop$AGE == 999, ], idvar = c("STATE", "NAME", "SEX"), 
                  varying = c(8:19), v.name = "pop", 
                  times = c("ESTBASE2010_CIV", "POPEST2010_CIV", "POPEST2011_CIV", "POPEST2012_CIV", 
                            "POPEST2013_CIV", "POPEST2014_CIV", "POPEST2015_CIV", "POPEST2016_CIV", 
                            "POPEST2017_CIV", "POPEST2018_CIV", "POPEST2019_CIV", "POPEST2020_CIV"), 
                  direction = "long")
popSEX <- reshape(popSEX[popSEX$time != "ESTBASE2010_CIV",], idvar = c("NAME", "time"), v.names = "pop", timevar = "SEX", direction = "wide")
popSEX <- popSEX %>% mutate(MPROP = pop.1 / pop.0) %>% select(NAME, time, MPROP) %>% mutate(YEAR = as.numeric(substr(time, 7, 10))) %>% select(-time)

# sex distribution 
ggplot(data = popSEX) + geom_histogram(aes(x = as.numeric(MPROP))) + theme_bw()

# sex trend 
ggplot(data = popSEX) + geom_smooth(aes(x = as.numeric(YEAR), y = MPROP, color = NAME)) + 
  scale_x_continuous(breaks = seq(2011, 2020, 1)) + labs(y = "proportion of males in total population") + 
  theme_bw() + theme(strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/INDIANA_SynthC/', DATE, '_MalePopulation_trend.png'), dpi=300, width = 12, height = 7)

# CPI ALCOHOLIC BEVERAGES

cpi <- rdat.cpi %>% separate(DATE, c("YEAR", "MONTH", "DAY"), "-") %>% 
  mutate(MONTH = as.numeric(MONTH), 
         Q = ifelse(MONTH < 4, 1, ifelse(MONTH >= 4 & MONTH < 7, 2, 
                    ifelse(MONTH >= 7 & MONTH < 10, 3, ifelse(MONTH >= 10, 4, NA))))) %>%
  group_by(YEAR, Q) %>% 
  summarise(CPIalc = mean(CUSR0000SAF116)) %>%
  mutate(QYEAR = as.numeric(ifelse(Q == 1, paste0(YEAR, ".125"), ifelse(Q == 2, paste0(YEAR, ".375"),
                                  ifelse(Q == 3, paste0(YEAR, ".625"), ifelse(Q == 4, paste0(YEAR, ".875"), NA)))))) %>%
  ungroup %>% select(QYEAR, CPIalc)

# cpi trend 
ggplot(data = cpi) + geom_smooth(aes(x = as.numeric(QYEAR), y = CPIalc)) + 
  scale_x_continuous(breaks = seq(2011, 2020, 1)) + labs(y = "CPI alcohlic beverages (US city average)") + 
  theme_bw() + theme(strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/INDIANA_SynthC/', DATE, '_CPIalc_trend.png'), dpi=300, width = 12, height = 7)

# PROPORTION OF URBAN POPULATION
popURBAN <- rdat.urban %>% select(STATE.NAME, `2020.PCT.URBAN.POP`, `2010.PCT.URBAN.POP`) %>% 
  mutate(l = (`2020.PCT.URBAN.POP` - `2010.PCT.URBAN.POP`) / 10,
         `2011` = `2010.PCT.URBAN.POP` + 1*l,
         `2012` = `2010.PCT.URBAN.POP` + 2*l,
         `2013` = `2010.PCT.URBAN.POP` + 3*l,
         `2014` = `2010.PCT.URBAN.POP` + 4*l,
         `2015` = `2010.PCT.URBAN.POP` + 5*l,
         `2016` = `2010.PCT.URBAN.POP` + 6*l,
         `2017` = `2010.PCT.URBAN.POP` + 7*l,
         `2018` = `2010.PCT.URBAN.POP` + 8*l,
         `2019` = `2010.PCT.URBAN.POP` + 9*l) %>% select(-l)
popURBAN <- reshape(popURBAN, idvar = c("STATE.NAME"), 
                    varying = c(2:12), v.name = "URBAN", 
                    times = c("2020.PCT.URBAN.POP", "2010.PCT.URBAN.POP", "2011", "2012", 
                              "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
                    direction = "long")
popURBAN <- popURBAN %>% mutate(YEAR = as.numeric(substr(time, 1, 4))) %>% select(-time)
  
# distribution of urban population 
ggplot(data = popURBAN) + geom_histogram(aes(x = as.numeric(URBAN))) + theme_bw() #non-normal

# urban population trend 
ggplot(data = popURBAN) + geom_smooth(aes(x = as.numeric(YEAR), y = URBAN, color = STATE.NAME)) + 
  scale_x_continuous(breaks = seq(2011, 2020, 1)) + labs(y = "share of urban population") + 
  theme_bw() + theme(strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/INDIANA_SynthC/', DATE, '_UrbanPopulation_trend.png'), dpi=300, width = 12, height = 7)

# APC DATA

# apc <- rdat.apc %>% filter(Beverage == "all beverages" & Year > 2010) %>% as.data.table()
  
# COMBINE SEX AND PREDICTOR DATA

gpdSEX <- merge(gpdSEX[QYEAR > 2010], income, by.x = c("State", "QYEAR"), by.y = c("GeoName", "time"), all.x = T)
gpdSEX <- merge(gpdSEX[QYEAR > 2010], popAGE, by.x = c("State", "YEAR"), by.y = c("NAME", "YEAR"), all.x = T)
gpdSEX <- merge(gpdSEX[QYEAR > 2010], popSEX, by.x = c("State", "YEAR"), by.y = c("NAME", "YEAR"), all.x = T)
gpdSEX <- merge(gpdSEX[QYEAR > 2010], cpi, by = c("QYEAR"), all.x = T)
gpdSEX <- merge(gpdSEX[QYEAR > 2010], popURBAN, by.x = c("State", "YEAR"), by.y = c("STATE.NAME", "YEAR"), all.x = T)

gpdSEX <- gpdSEX %>% mutate (State = as.factor(gpdSEX$State), INT = ifelse(State == "Connecticut", 1, 0))

# CHECK CORRELATIONS 

corrplot(cor(gpdSEX[,.(log.pcInc, YPROP, MPROP, CPIalc, URBAN)]),
         method = "number", type = "upper") # r > .5 for CPI and pcInc -> keep pcInc only

# MEN 

# check distribution

ggplot(data = gpdSEX[sex == "Men"]) + geom_histogram(aes(x = GPD)) + theme_bw() 

ggplot(data = gpdSEX[sex == "Men" & State %like% "Connecticut"]) + geom_line(aes(x = QYEAR, y = GPD)) + 
  scale_x_continuous(breaks = seq(2001.125, 2020.125, 0.25)) + theme_bw() + theme(axis.text.x = element_text(angle = 90)) 

# REMOVE STATES WITH DISTRIBUTION OUTSIDE 95% CI
# selectStates <- gpdSEX %>% filter(sex == "Men") %>% group_by(State) %>% 
#   mutate(GPD.LB.IND = gpdSEX[sex == "Men" & State == "Indiana"]$GPD.LB,
#          GPD.UB.IND = gpdSEX[sex == "Men" & State == "Indiana"]$GPD.UB,
#          q = ifelse(GPD <= GPD.UB.IND & GPD >= GPD.LB.IND, 1, 0)) %>% 
#   summarise(sumq = sum(q)) %>% filter(sumq >= 20) %>% pull("State")
# 
# gpdMEN <- gpdSEX %>% filter(sex == "Men" & State %in% selectStates)
  
#ggplot(data = gpdMEN) + geom_line(aes(x = QYEAR, y = GPD, color = as.factor(State), linetype = as.factor(INT))) + 
#  scale_x_continuous(breaks = seq(2011.125, 2020.125, 0.25)) + theme_bw() + theme(axis.text.x = element_text(angle = 90)) 
  # exclude New Hampshire (higher then other states), Utah (lower then other states), Georgia (lower then other states)

#ggplot(data = gpdSEX[sex == "Men" & !State %like% "Indiana|Utah|Hampshire|Georgia"]) + geom_line(aes(x = QYEAR, y = GPD, color = as.factor(State))) + 
#  scale_x_continuous(breaks = seq(2011.125, 2020.125, 0.25)) + theme_bw() + theme(axis.text.x = element_text(angle = 90)) 


# TEST UNIVARIATE ASSOCIATION 
# use Hausman Test for random-effects model
# https://libguides.princeton.edu/R-Panel 

fixed <- plm(log(GPD) ~ log.pcInc, data = gpdSEX[sex == "Men"], index = c("State", "QYEAR"), model = "within") 
random <- plm(log(GPD) ~ log.pcInc, data = gpdSEX[sex == "Men"], index = c("State", "QYEAR"), model = "random") 
phtest(fixed, random) # p > .05, use random effects model
summary(random) # sig

ggplot(data = gpdMEN) + geom_line(aes(x = QYEAR, y = log.pcInc, color = State, linetype = as.factor(INT))) + 
  scale_x_continuous(breaks = seq(2011.125, 2020.125, 0.25)) + theme_bw() + theme(axis.text.x = element_text(angle = 90)) 
  # exclude those States with much higher pcInc in 2011: Alaska, Massachusetts, Nebraska, Virginia

  # ggplot(data = gpdSEX[sex == "Men" & !State %like% "Utah|Hampshire|Georgia"]) + geom_line(aes(x = QYEAR, y = log.pcInc, color = State, linetype = as.factor(INT))) + 
  #   scale_x_continuous(breaks = seq(2011.125, 2020.125, 0.25)) + theme_bw() + theme(axis.text.x = element_text(angle = 90)) 
  #   #exclude North Dakota, Alaska and Wyoming due to different pattern, and Massachusetts given very high pcINC

  # ggplot(data = gpdSEX[sex == "Men" & !State %like% "Utah|Hampshire|Georgia|Wyoming|North Dakota|Alaska|Massachus"]) + 
  #   geom_line(aes(x = QYEAR, y = log.pcInc, color = State, linetype = as.factor(INT))) + 
  #   scale_x_continuous(breaks = seq(2011.125, 2020.125, 0.25)) + theme_bw() + theme(axis.text.x = element_text(angle = 90)) 
  
fixed <- plm(log(GPD) ~ YPROP, data = gpdSEX[sex == "Men"], index = c("State", "QYEAR"), model = "within") 
random <- plm(log(GPD) ~ YPROP, data = gpdSEX[sex == "Men"], index = c("State", "QYEAR"), model = "random") 
phtest(fixed, random) # p < .05, use fixed effects model
summary(fixed) # sig

  ggplot(data = gpdMEN[!State %like% "Alaska|Massachus|Nebraska" & State != "Virginia"]) + 
    geom_line(aes(x = QYEAR, y = YPROP, color = State, linetype = as.factor(INT))) + 
    scale_x_continuous(breaks = seq(2011.125, 2020.125, 0.25)) + theme_bw() + theme(axis.text.x = element_text(angle = 90)) 
  # exclude State with YPROP < 0.135 in 2011: Colorado, Florida, Oregon, West Virginia
  
  ggplot(data = gpdMEN[!State %like% "Alaska|Massachus|Nebraska|Virginia|Oregon|Colorado|Florida"]) + 
    geom_line(aes(x = QYEAR, y = YPROP, color = State, linetype = as.factor(INT))) + 
    scale_x_continuous(breaks = seq(2011.125, 2020.125, 0.25)) + theme_bw() + theme(axis.text.x = element_text(angle = 90)) 
  # exclude also Rhode Island 
  
  # ggplot(data = gpdSEX[sex == "Men" & !State %like% "Utah|Hampshire|Georgia|Wyoming|North Dakota|Alaska|Massachus"]) + 
  #   geom_line(aes(x = QYEAR, y = YPROP, color = State, linetype = as.factor(INT))) + 
  #   scale_x_continuous(breaks = seq(2011.125, 2020.125, 0.25)) + theme_bw() + theme(axis.text.x = element_text(angle = 90)) 
  # # exclude Rhode Island (very high), Hawaii (very low)

  # ggplot(data = gpdSEX[sex == "Men" & !State %like% "Rhode Island|Hawaii|Utah|Hampshire|Georgia|Wyoming|North Dakota|Alaska|Massachus"]) + 
  #   geom_line(aes(x = QYEAR, y = YPROP, color = State, linetype = as.factor(INT))) + 
  #   scale_x_continuous(breaks = seq(2011.125, 2020.125, 0.25)) + theme_bw() + theme(axis.text.x = element_text(angle = 90)) 
  # # exclude California given steep decrease over time
  
  # ggplot(data = gpdSEX[sex == "Men" & !State %like% "California|Rhode Island|Hawaii|Utah|Hampshire|Georgia|Wyoming|North Dakota|Alaska|Massachus"]) + 
  #   geom_line(aes(x = QYEAR, y = YPROP, color = State, linetype = as.factor(INT))) + 
  #   scale_x_continuous(breaks = seq(2011.125, 2020.125, 0.25)) + theme_bw() + theme(axis.text.x = element_text(angle = 90)) 

fixed <- plm(log(GPD) ~ MPROP, data = gpdSEX[sex == "Men"], index = c("State", "QYEAR"), model = "within") 
random <- plm(log(GPD) ~ MPROP, data = gpdSEX[sex == "Men"], index = c("State", "QYEAR"), model = "random") 
phtest(fixed, random) # p < .05, use fixed effects model
summary(fixed) # sig

fixed <- plm(log(GPD) ~ URBAN, data = gpdSEX[sex == "Men"], index = c("State", "QYEAR"), model = "within") 
random <- plm(log(GPD) ~ URBAN, data = gpdSEX[sex == "Men"], index = c("State", "QYEAR"), model = "random") 
phtest(fixed, random) # p < .05, use fixed effects model
summary(fixed) # n.s.

# # WOMEN 
# 
# ggplot(data = gpdSEX[sex == "Women"]) + geom_histogram(aes(x = log(GPD))) + theme_bw() 
# 
# # TEST UNIVARIATE ASSOCIATION 
# # use Hausman Test for random-effects model
# # https://libguides.princeton.edu/R-Panel 
# 
# fixed <- plm(log(GPD) ~ log.pcInc, data = gpdSEX[sex == "Women"], index = c("State", "QYEAR"), model = "within") 
# random <- plm(log(GPD) ~ log.pcInc, data = gpdSEX[sex == "Women"], index = c("State", "QYEAR"), model = "random") 
# phtest(fixed, random) # p > .05, use random effects model
# summary(random) # sig
# 
# fixed <- plm(log(GPD) ~ YPROP, data = gpdSEX[sex == "Women"], index = c("State", "QYEAR"), model = "within") 
# random <- plm(log(GPD) ~ YPROP, data = gpdSEX[sex == "Women"], index = c("State", "QYEAR"), model = "random") 
# phtest(fixed, random) # p > .05, use random effects model
# summary(random) # sig
# 
# fixed <- plm(log(GPD) ~ MPROP, data = gpdSEX[sex == "Women"], index = c("State", "QYEAR"), model = "within") 
# random <- plm(log(GPD) ~ MPROP, data = gpdSEX[sex == "Women"], index = c("State", "QYEAR"), model = "random") 
# phtest(fixed, random) # p > .05, use random effects model
# summary(random) # sig
# 
# fixed <- plm(log(GPD) ~ URBAN, data = gpdSEX[sex == "Women"], index = c("State", "QYEAR"), model = "within") 
# random <- plm(log(GPD) ~ URBAN, data = gpdSEX[sex == "Women"], index = c("State", "QYEAR"), model = "random") 
# phtest(fixed, random) # p > .05, use random effects model
# summary(random) # n.s.
# 
# # SELECT LAGGED VALUES FOR OUTCOME INDICATOR (APC DATA)
# 
# ggplot(data = apc[State %like% "Indiana"]) + geom_line(aes(x = Year, y = APC)) + scale_x_continuous(breaks = seq(2011, 2020, 1)) + theme_bw() 
#   # 2013, 2014, 2016, 2017, 2019
#   # ggsave(paste0('acp_brfss/outputs/figures/INDIANA_SynthC/', DATE, '_INDIANA_APC.png'), dpi=300, width = 12, height = 7)
# 
# ggplot(data = apc[State %like% "United States"]) + geom_line(aes(x = Year, y = APC)) + scale_x_continuous(breaks = seq(2011, 2020, 1)) + theme_bw() 
#   # 2012, 2014, 2016, 1017, 2019
#   # ggsave(paste0('acp_brfss/outputs/figures/INDIANA_SynthC/', DATE, '_US_APC.png'), dpi=300, width = 12, height = 7)
# 
# #apc.sub <- cbind(apc[Year == 2013, .(State, APC)], apc[Year == 2014, .(APC)], apc[Year == 2016, .(APC)], apc[Year == 2017, .(APC)], apc[Year == 2019, .(APC)])
# #colnames(apc.sub) <- c("State", "apc.2013", "apc.2014", "apc.2016", "apc.2017", "apc.2019")
# 
# #gpdSEX <- merge(gpdSEX, apc.sub, by = "State", all.x = T, all.y = F)

# ----------------------------------------------------------------
# SYNTHETIC CONTROL: SEX
# https://cran.r-project.org/web/packages/Synth/Synth.pdf
# ----------------------------------------------------------------

# MEN

# SELECT STATES

# gpdMEN <- gpdSEX %>% filter(sex == "Men" & !State %like% "California|Rhode Island|Hawaii|Utah|Hampshire|Georgia|Wyoming|North Dakota|Alaska|Massachus")
# gpdMEN <- gpdSEX %>% filter(sex == "Men" & State %like% "Alabama|Arizona|Idaho|Illinois|Indiana|Iowa|Kansas|Michigan|Mexico|Oregon|South Dakota|Texas")

# RESTRUCTURE

#gpdSEXlong <- reshape(gpdSEX[,.(State, QYEAR, sex, GPD)], 
#                      idvar = c("QYEAR", "sex"), v.names = c("GPD"), 
#                      timevar = "State", direction = "wide")

gpdMEN <- gpdSEX[sex == "Men" & QYEAR > 2011] %>% mutate(unit.num = as.integer(factor(State)), State = as.character(State)) %>% as.data.table()

 ggplot(data = gpdMEN[State %like% "Indiana"]) + geom_line(aes(x = QYEAR, y = GPD)) + facet_grid(vars(State)) +
   scale_x_continuous(breaks = seq(2011.125, 2020.125, 0.25)) + theme_bw() + theme(axis.text.x = element_text(angle = 90)) 

M.dat.prep <- dataprep(foo = gpdMEN, predictors = c("log.pcInc", "YPROP", "MPROP", "CONTROL"), 
                       predictors.op = "mean", 
                       dependent = "GPD", unit.variable = "unit.num",
                       time.variable = "QYEAR", special.predictors = list( list("GPD", 2012.125, "mean"), 
                                                                           list("GPD", 2014.125, "mean"), 
                                                                           list("GPD", 2015.375, "mean"), 
                                                                           list("GPD", 2016.125, "mean"), 
                                                                           list("GPD", 2017.125, "mean")),
                       treatment.identifier = 12, controls.identifier = c(1:11, 13:32),
                       time.predictors.prior = c(2011.125:2017.875), time.optimize.ssr = c(2011.125:2017.875), 
                       unit.names.variable = "State", time.plot = 2011.125:2020.125)

# BUILD SYNTHETIC CONTROL
M.synth.out <- synth(M.dat.prep)

# OUTPUT
M.synth.table <- synth.tab(dataprep.res = m.dat.prep, synth.res = M.synth.out)
print(M.synth.table)

path.plot(dataprep.res = M.dat.prep,synth.res = M.synth.out)
gaps.plot(dataprep.res = M.dat.prep,synth.res = M.synth.out)


# CHECK OPTIONS WITH Connecticut

# RESTRUCTURE

gpdMEN <- gpdSEX[sex == "Men" & QYEAR >= 2002.125 & State != "Hawaii"] %>% 
  mutate(unit.num = as.integer(factor(State)), State = as.character(State)) %>% as.data.table()

ggplot(data = gpdMEN[State %like% "Connecticut"]) + geom_line(aes(x = QYEAR, y = GPD)) + facet_grid(vars(State)) +
  scale_x_continuous(breaks = seq(2001.125, 2020.125, 0.25)) + theme_bw() + theme(axis.text.x = element_text(angle = 90)) 

M.dat.prep <- dataprep(foo = gpdMEN[!is.na(unit.num)], #predictors = c("log.pcInc", "YPROP", "MPROP"), 
                     #  predictors.op = "mean", 
                       dependent = "GPD", unit.variable = "unit.num",
                       time.variable = "QYEAR", special.predictors = list(list("GPD", 2004.125, "mean"), 
                                                                           list("GPD", 2005.125, "mean"), 
                                                                           list("GPD", 2007.375, "mean"), 
                                                                           list("GPD", 2011.875, "mean")),
                       treatment.identifier = 6, controls.identifier = c(1:5, 7:31),
                       time.predictors.prior = c(2002.125:2012.125), time.optimize.ssr = c(2002.125:2012.125), 
                       unit.names.variable = "State", time.plot = 2002.125:2020.125)

# BUILD SYNTHETIC CONTROL
M.synth.out <- synth(M.dat.prep)

# OUTPUT
M.synth.table <- synth.tab(dataprep.res = m.dat.prep, synth.res = M.synth.out)
print(M.synth.table)

path.plot(dataprep.res = M.dat.prep,synth.res = M.synth.out)
gaps.plot(dataprep.res = M.dat.prep,synth.res = M.synth.out)

 
## USE DIFFERENCING TO REMOVE SEASONAL COMPONENT? 
# diff(x, lag = 1, differences = 1)

gpdMEN <- gpdSEX[sex == "Men"] %>% mutate(unit.num = as.integer(factor(State)), State = as.character(State)) %>% as.data.table()

ggplot(data = gpdMEN[State %like% "Indiana"]) + geom_line(aes(x = QYEAR, y = GPD)) + facet_grid(vars(State)) +
  scale_x_continuous(breaks = seq(2002.125, 2020.125, 0.25)) + theme_bw() + theme(axis.text.x = element_text(angle = 90)) 

# TEST DIFFERENCING
IND <- gpdMEN %>% filter(QYEAR >= 2001.125 & QYEAR <= 2020.125) %>% filter(State %like% "Indiana") 
IND.diff <- diff(IND$GPD, lag = 4, differences = 1)
IND <- cbind(IND[QYEAR >= 2002.125], IND.diff)

ggplot(data = IND) + geom_line(aes(x = QYEAR, y = IND.diff)) + facet_grid(vars(State)) +
  scale_x_continuous(breaks = seq(2002.125, 2020.125, 0.25)) + theme_bw() + theme(axis.text.x = element_text(angle = 90)) 

  # DOESN'T LOOK MUCH BETTER...
  
# PREPARE SYNTH CONTROL
M.dat.prep <- dataprep(foo = gpdMEN[!is.na(unit.num) & QYEAR >= 2002.125 & State != "Hawaii"], 
                       predictors = c("CONTROL"),
                       #predictors = c("log.pcInc", "YPROP", "MPROP"), predictors.op = "mean", 
                       dependent = "GPD", unit.variable = "unit.num",
                       time.variable = "QYEAR", special.predictors = list(list("GPD", 2006.125, "median"), 
                                                                          list("GPD", 2010.625, "median"), 
                                                                          list("GPD", 2011.875, "median"), 
                                                                          list("GPD", 2012.375, "median"), 
                                                                          list("GPD", 2014.125, "median"), 
                                                                          list("GPD", 2016.125, "median"),
                                                                          list("GPD", 2017.375, "median")),
                       treatment.identifier = 12, controls.identifier = c(1:9, 11, 13:32),
                       time.predictors.prior = c(2002.125:2012.125), time.optimize.ssr = c(2002.125:2012.125), 
                       unit.names.variable = "State", time.plot = 2002.125:2020.125)

# BUILD SYNTHETIC CONTROL
M.synth.out <- synth(M.dat.prep)

# OUTPUT
M.synth.table <- synth.tab(dataprep.res = m.dat.prep, synth.res = M.synth.out)
print(M.synth.table)

path.plot(dataprep.res = M.dat.prep,synth.res = M.synth.out)
gaps.plot(dataprep.res = M.dat.prep,synth.res = M.synth.out)
