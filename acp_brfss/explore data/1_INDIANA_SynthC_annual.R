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

# select only states where there was no change in Sunday sales ban policy between 2000 and 2020; SC excluded 
data <- datBRFSS[State %like% "Indiana|Alabama|Alaska|Arizona|Arkansas|California|DC|Florida|Georgia|Hawaii|Idaho|Illinois|
                               |Iowa|Kansas|Louisiana|Maine|Maryland|Michigan|Mississippi|Missouri|Montana|Nebraska|Nevada|New Hampshire
                               |New Jersey|New Mexico|New York|North Carolina|North Dakota|Ohio|Oregon|South Dakota|Texas|Utah
                               |Vermont|Washington|West Virginia|Wisconsin|Wyoming"]
data <- data[surveyyear >= 2000]

# PREDICTOR DATA
rdat.inc <- read.xlsx("acp_brfss/data/20230328_US per capita disp income_ANNUAL.xlsx", sheet = 1, startRow = 6)
  # annual per capita *disposable* personal income: https:/apps.bea.gov/itable/?ReqID=70&step=1#eyJhcHBpZCI6NzAsInN0ZXBzIjpbMSwyNCwyOSwyNSwzMSwyNiwyNywzMF0sImRhdGEiOltbIlRhYmxlSWQiLCIyMyJdLFsiQ2xhc3NpZmljYXRpb24iLCJOb24tSW5kdXN0cnkiXSxbIk1ham9yX0FyZWEiLCIwIl0sWyJTdGF0ZSIsWyIwIl1dLFsiQXJlYSIsWyJYWCJdXSxbIlN0YXRpc3RpYyIsWyI1MyJdXSxbIlVuaXRfb2ZfbWVhc3VyZSIsIkxldmVscyJdLFsiWWVhciIsWyIyMDIxIiwiMjAyMCIsIjIwMTkiLCIyMDE4IiwiMjAxNyIsIjIwMTYiLCIyMDE1IiwiMjAxNCIsIjIwMTMiLCIyMDEyIiwiMjAxMSIsIjIwMTAiLCIyMDA5IiwiMjAwOCIsIjIwMDciLCIyMDA2IiwiMjAwNSIsIjIwMDQiLCIyMDAzIiwiMjAwMiIsIjIwMDEiLCIyMDAwIl1dLFsiWWVhckJlZ2luIiwiLTEiXSxbIlllYXJfRW5kIiwiLTEiXV19
rdat.pop1 <- read.xlsx("acp_brfss/data/20230228_US POP_2010-2020.xlsx", sheet = 1, startRow = 1)  %>% select(-c("SUMLEV", "REGION", "DIVISION", "STATE"))
  # state-specific annual population data based on US census 2010-2020: https:/www.census.gov/programs-surveys/popest/technical-documentation/research/evaluation-estimates/2020-evaluation-estimates/2010s-state-detail.html 
rdat.pop2 <- read.csv("acp_brfss/data/20230228_US POP_2000-2010.csv") %>% select(-c("POPESTIMATE2010", "CENSUS2010POP", "REGION", "DIVISION", "STATE"))
  # state-specific annual population data based on US census 2000-2010: https:/www2.census.gov/programs-surveys/popest/datasets/2000-2010/intercensal/state/ 
rdat.cpi <- read.xlsx("acp_brfss/data/20230322_US CPI_CUSR0000SAF116_20002022_ANNUAL.xlsx", sheet = 1, startRow = 11)
  # CPI US annual average (US city average, urban population): https:/fred.stlouisfed.org/series/CUSR0000SAF116#0
rdat.apc <- read.xlsx("acp_brfss/data/20230327_NIAAA APC_1990-2020.xlsx", sheet = 1, startRow = 1)
  # annual state-specific APC data

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# PREPARE DATA
# ----------------------------------------------------------------

# subgroups
data[, education := factor(education_summary, levels = c("LEHS", "SomeC", "College"))]
data[, sex := ifelse(sex_recode == "Female", "Women", ifelse(sex_recode == "Male", "Men", NA))]

# exclude COVID years
data <- copy(data[YEAR >= 2000 & YEAR < 2020])

# setting Indiana as intervention state
data[, int.state := ifelse(State %like% "Indiana", 1, 0)]
data[, INT_YEAR := ifelse(int.state == 1, 2018, NA)]

# capping alcohol consumption >200 GPD
summary(data$gramsperday_orig)
data[gramsperday_orig > 200] # 2434 observations
summary(data[gramsperday_upshifted > 200]$gramsperday_orig)
data[gramsperday_upshifted > 200] # 6856 observations

data[, GPD_upshifted_capped := ifelse(gramsperday_upshifted > 200, 200, gramsperday_upshifted)]
data[, GPD_orig_capped := ifelse(gramsperday_orig > 200, 200, gramsperday_orig)]

# select data
pdat <- data[, .(State, YEAR, INT_YEAR, final_sample_weight, sex, education, drinkingstatus_orig, GPD_orig_capped, GPD_upshifted_capped)]

# ----------------------------------------------------------------
# DISTRIBUTION
# ----------------------------------------------------------------

# check counts by sex
pdat %>% filter(drinkingstatus_orig != 0) %>% group_by(State, YEAR, sex) %>% summarise(n = n()) %>% ungroup() %>% 
  group_by(State) %>% summarise(small.sample = sum(ifelse(n <= 50, 1, 0))) %>% filter(small.sample > 0)
  # Alaska, Hawaii, Illinois, Louisiana, New Mexiko, Utah, Wiscnonsin
sexStates <- pdat %>% filter(drinkingstatus_orig != 0) %>% group_by(State, YEAR, sex) %>% summarise(n = n()) %>% ungroup() %>% 
  group_by(State) %>% summarise(small.sample = sum(ifelse(n <= 50, 1, 0))) %>% filter(small.sample == 0) %>% pull("State")

pdatSEX <- pdat %>% filter(State %in% sexStates)
  
# check counts by education
pdat %>% filter(drinkingstatus_orig != 0) %>% group_by(State, YEAR, education) %>% summarise(n = n()) %>% ungroup() %>% 
  group_by(State) %>% summarise(small.sample = sum(ifelse(n <= 50, 1, 0))) %>% filter(small.sample > 0)
  # Alaska, Arkansas, California, Delaware, Georgia, Idaho, Louisiana, Mississippi, Nevada, New Jersey, Pennsylvania
eduStates <- pdat %>% filter(drinkingstatus_orig != 0) %>% group_by(State, YEAR, education) %>% summarise(n = n()) %>% ungroup() %>% 
  group_by(State) %>% summarise(small.sample = sum(ifelse(n <= 50, 1, 0))) %>% filter(small.sample == 0) %>% pull("State")

pdatEDU <- pdat %>% filter(State %in% eduStates)

# ----------------------------------------------------------------
# PREPARE MAIN DATA
# ----------------------------------------------------------------

# SEX

# ORIGINAL GDP: smooth function
ggplot(data = pdatSEX[drinkingstatus_orig != 0]) +
  geom_smooth(aes(x = YEAR, y = GPD_orig_capped, color = State)) + 
  #geom_vline(aes(xintercept = INT_YEAR)) +
  scale_x_continuous(breaks = seq(2000, 2020, 1)) +
  facet_grid(rows = vars(as.factor(sex)), scales = "free") +
  labs(y = "GPD (original, unweighted)") + theme_bw() + theme(strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/INDIANA_SynthC/annual data/', DATE, '_GPDorig_BY_SEX.png'), dpi=300, width = 12, height = 7)

# UPSHIFTED GDP: smooth function
ggplot(data = pdatSEX[drinkingstatus_orig != 0]) +
  geom_smooth(aes(x = YEAR, y = GPD_upshifted_capped, color = State)) + 
  #geom_vline(aes(xintercept = INT_YEAR)) +
  scale_x_continuous(breaks = seq(2000, 2020, 1)) +
  facet_grid(rows = vars(as.factor(sex)), scales = "free") +
  labs(y = "GPD (upshifted, unweighted)") + theme_bw() + theme(strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/INDIANA_SynthC/annual data/', DATE, '_GPDupsh_BY_SEX.png'), dpi=300, width = 12, height = 7)

# AGGREGATE GDP (upshifted, weighted)
gpdSEX <- pdatSEX %>% 
  filter(drinkingstatus_orig > 0) %>%
  group_by(State, YEAR, sex) %>%
  summarise(GPD = wtd.mean(GPD_upshifted_capped, final_sample_weight),
            GPD.LB = wtd.lb(GPD_upshifted_capped, final_sample_weight),
            GPD.UB = wtd.ub(GPD_upshifted_capped, final_sample_weight),
            n = n(), INT_YEAR = mean(INT_YEAR)) %>% as.data.table()

# EDUCATION

# ORIGINAL GDP: smooth function
ggplot(data = pdatEDU[drinkingstatus_orig != 0]) +
  geom_smooth(aes(x = YEAR, y = GPD_orig_capped, color = State)) + 
  #geom_vline(aes(xintercept = INT_YEAR)) +
  scale_x_continuous(breaks = seq(2000, 2020, 1)) +
  facet_grid(rows = vars(as.factor(education)), scales = "free") +
  labs(y = "GPD (original, unweighted)") + theme_bw() + theme(strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/INDIANA_SynthC/annual data/', DATE, '_GPDorig_BY_SES.png'), dpi=300, width = 12, height = 7)

# UPSHIFTED GDP: smooth function
ggplot(data = pdatEDU[drinkingstatus_orig != 0]) +
  geom_smooth(aes(x = YEAR, y = GPD_upshifted_capped, color = State)) + 
  #geom_vline(aes(xintercept = INT_YEAR)) +
  scale_x_continuous(breaks = seq(2000, 2020, 1)) +
  facet_grid(rows = vars(as.factor(education)), scales = "free") +
  labs(y = "GPD (upshifted, unweighted)") + theme_bw() + theme(strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/INDIANA_SynthC/annual data/', DATE, '_GPDupsh_BY_SES.png'), dpi=300, width = 12, height = 7)

# AGGREGATE GDP (upshifted, weighted)
gdpEDU <- data %>% 
  filter(drinkingstatus_orig > 0) %>%
  group_by(State, YEAR, education) %>%
  summarise(GPD = wtd.mean(GPD_upshifted_capped, final_sample_weight),
            GPD.LB = wtd.lb(GPD_upshifted_capped, final_sample_weight),
            GPD.UB = wtd.ub(GPD_upshifted_capped, final_sample_weight),
            n = n(), INT_YEAR = mean(INT_YEAR)) %>% as.data.table()

# ----------------------------------------------------------------
# PREPARE PREDICTOR DATA AND IDENTIFY RELEVANT PREDICTORS
# ----------------------------------------------------------------

# PER CAPITA INCOME

income <- reshape(rdat.inc, idvar = "GeoName", varying = c(3:24), v.name = "pcInc", 
                  times = c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010",
                            "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"), 
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
  scale_x_continuous(breaks = seq(2000, 2020, 1)) + labs(y = "average personal income, per capita (log)") + 
  theme_bw() + theme(strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/INDIANA_SynthC/annual data/', DATE, '_pcIncome_trend.png'), dpi=300, width = 12, height = 7)

# PROPORTION OF YOUNG POPULATION

rdat.pop <- merge(rdat.pop1, rdat.pop2, by = c("NAME", "SEX", "AGE"))
popAGE <- reshape(rdat.pop[rdat.pop$SEX == 0, ], idvar = c("NAME", "AGE"), varying = c(4:26), v.name = "pop", 
                    times = c("ESTBASE2010_CIV", "POPEST2010_CIV", "POPEST2011_CIV", "POPEST2012_CIV", 
                              "POPEST2013_CIV", "POPEST2014_CIV", "POPEST2015_CIV", "POPEST2016_CIV", 
                              "POPEST2017_CIV", "POPEST2018_CIV", "POPEST2019_CIV", "POPEST2020_CIV", 
                              "ESTIMATESBASE2000", "POPESTIMATE2000", "POPESTIMATE2001", "POPESTIMATE2002", "POPESTIMATE2003",
                              "POPESTIMATE2004", "POPESTIMATE2005", "POPESTIMATE2006", "POPESTIMATE2007", "POPESTIMATE2008", 
                              "POPESTIMATE2009"), 
                    direction = "long")
popAGE.young <- popAGE %>% filter(AGE >= 15 & AGE < 25 & !time %like% "ESTBASE|ESTIMATESB") %>% group_by(NAME, time) %>% summarise(YPOP = sum(pop))
popAGE <- merge(popAGE.young, popAGE[popAGE$AGE == 999,], by = c("NAME", "time"))
popAGE <- popAGE %>% mutate(YPROP = YPOP / pop) %>% select(NAME, time, YPROP) %>%
  mutate(YEAR = ifelse(time %like% "POPEST2", as.numeric(substr(time, 7, 10)), 
                       ifelse(time %like% "POPESTIMATE", as.numeric(substr(time, 12, 15)), NA))) %>% select(-time)

# age distribution 
ggplot(data = popAGE) + geom_histogram(aes(x = as.numeric(YPROP))) + theme_bw()

# age trend 
ggplot(data = popAGE) + geom_smooth(aes(x = as.numeric(YEAR), y = YPROP, color = NAME)) + 
  scale_x_continuous(breaks = seq(2000, 2020, 1)) + labs(y = "proportion of population aged 15-24 years") + 
  theme_bw() + theme(strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/INDIANA_SynthC/annual data/', DATE, '_YoungPopulation_trend.png'), dpi=300, width = 12, height = 7)

# PROPORTION OF MALE POPULATION

popSEX <- reshape(rdat.pop[rdat.pop$SEX != 2 & rdat.pop$AGE == 999, ], idvar = c("NAME", "SEX"), 
                  varying = c(4:26), v.name = "pop", 
                  times = c("ESTBASE2010_CIV", "POPEST2010_CIV", "POPEST2011_CIV", "POPEST2012_CIV", 
                            "POPEST2013_CIV", "POPEST2014_CIV", "POPEST2015_CIV", "POPEST2016_CIV", 
                            "POPEST2017_CIV", "POPEST2018_CIV", "POPEST2019_CIV", "POPEST2020_CIV", 
                            "ESTIMATESBASE2000", "POPESTIMATE2000", "POPESTIMATE2001", "POPESTIMATE2002", "POPESTIMATE2003",
                            "POPESTIMATE2004", "POPESTIMATE2005", "POPESTIMATE2006", "POPESTIMATE2007", "POPESTIMATE2008", 
                            "POPESTIMATE2009"), 
                  direction = "long")
popSEX <- reshape(popSEX[!time %like% "ESTBASE|ESTIMATESB"], idvar = c("NAME", "time"), v.names = "pop", timevar = "SEX", direction = "wide")
popSEX <- popSEX %>% mutate(MPROP = pop.1 / pop.0) %>% select(NAME, time, MPROP) %>% 
  mutate(YEAR = ifelse(time %like% "POPEST2", as.numeric(substr(time, 7, 10)), 
                       ifelse(time %like% "POPESTIMATE", as.numeric(substr(time, 12, 15)), NA))) %>% select(-time)

# sex distribution 
ggplot(data = popSEX) + geom_histogram(aes(x = as.numeric(MPROP))) + theme_bw()

# sex trend 
ggplot(data = popSEX) + geom_smooth(aes(x = as.numeric(YEAR), y = MPROP, color = NAME)) + 
  scale_x_continuous(breaks = seq(2000, 2020, 1)) + labs(y = "proportion of males in total population") + 
  theme_bw() + theme(strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/INDIANA_SynthC/annual data/', DATE, '_MalePopulation_trend.png'), dpi=300, width = 12, height = 7)

# CPI ALCOHOLIC BEVERAGES

cpi <- copy(rdat.cpi) 
colnames(cpi) <- c("YEAR", "CPIalc")
  
# cpi trend 
ggplot(data = cpi) + geom_smooth(aes(x = as.numeric(YEAR), y = CPIalc)) + 
  scale_x_continuous(breaks = seq(2000, 2020, 1)) + labs(y = "CPI alcohlic beverages (US city average)") + 
  theme_bw() + theme(strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/INDIANA_SynthC/annual data/', DATE, '_CPIalc_trend.png'), dpi=300, width = 12, height = 7)

# APC DATA

apc <- rdat.apc %>% filter(Beverage == "all beverages" & Year >= 2000) %>% as.data.table()
  
# COMBINE SEX AND PREDICTOR DATA

gpdSEX <- merge(gpdSEX, income, by.x = c("State", "YEAR"), by.y = c("GeoName", "time"), all.x = T)
gpdSEX <- merge(gpdSEX, popAGE, by.x = c("State", "YEAR"), by.y = c("NAME", "YEAR"), all.x = T)
gpdSEX <- merge(gpdSEX, popSEX, by.x = c("State", "YEAR"), by.y = c("NAME", "YEAR"), all.x = T)
gpdSEX <- merge(gpdSEX, cpi, by = c("YEAR"), all.x = T)

gpdSEX <- gpdSEX %>% mutate (State = as.factor(gpdSEX$State), INT = ifelse(State == "Indiana", 1, 0))

# CHECK CORRELATIONS 

corrplot(cor(gpdSEX[!is.na(log.pcInc),.(log.pcInc, YPROP, MPROP, CPIalc)]),
         method = "number", type = "upper") # r > .5 for CPI and pcInc -> keep pcInc only

# MEN 

# check distribution

ggplot(data = gpdSEX[sex == "Men"]) + geom_histogram(aes(x = log(GPD))) + theme_bw() 

ggplot(data = gpdSEX[sex == "Men" & State %like% "Indiana"]) + geom_line(aes(x = YEAR, y = GPD)) + 
  scale_x_continuous(breaks = seq(2000, 2020, 1)) + theme_bw() + theme(axis.text.x = element_text(angle = 90)) 

# selectStates <- gpdSEX %>% filter(sex == "Men") %>% group_by(State) %>% 
#   mutate(GPD.LB.IND = gpdSEX[sex == "Men" & State == "Indiana"]$GPD.LB,
#          GPD.UB.IND = gpdSEX[sex == "Men" & State == "Indiana"]$GPD.UB,
#          q = ifelse(GPD <= GPD.UB.IND & GPD >= GPD.LB.IND, 1, 0)) %>% 
#   summarise(sumq = sum(q)) %>% filter(sumq >= 20) %>% pull("State")
# 
# gpdMEN <- gpdSEX %>% filter(sex == "Men" & State %in% selectStates)
gpdMEN <- gpdSEX %>% filter(sex == "Men")


# TEST UNIVARIATE ASSOCIATION 
# use Hausman Test for random-effects model
# https:/libguides.princeton.edu/R-Panel 

fixed <- plm(log(GPD) ~ log.pcInc, data = gpdMEN, index = c("State", "YEAR"), model = "within") 
random <- plm(log(GPD) ~ log.pcInc, data = gpdMEN, index = c("State", "YEAR"), model = "random") 
phtest(fixed, random) # p < .05, use fixed effects model
summary(fixed) # sig
  # no data for DC

ggplot(data = gpdMEN) + geom_line(aes(x = YEAR, y = log.pcInc, color = State, linetype = as.factor(INT))) + 
  scale_x_continuous(breaks = seq(2000, 2020, 1)) + theme_bw() + theme(axis.text.x = element_text(angle = 90)) 

fixed <- plm(log(GPD) ~ YPROP, data = gpdMEN, index = c("State", "YEAR"), model = "within") 
random <- plm(log(GPD) ~ YPROP, data = gpdMEN, index = c("State", "YEAR"), model = "random") 
phtest(fixed, random) # p < .05, use fixed effects model
summary(fixed) # sig

fixed <- plm(log(GPD) ~ MPROP, data = gpdMEN, index = c("State", "YEAR"), model = "within") 
random <- plm(log(GPD) ~ MPROP, data = gpdMEN, index = c("State", "YEAR"), model = "random") 
phtest(fixed, random) # p > .05, use random effects model
summary(random) # sig


# WOMEN

# ...

# ----------------------------------------------------------------
# SYNTHETIC CONTROL: SEX
# https:/cran.r-project.org/web/packages/Synth/Synth.pdf
# ----------------------------------------------------------------

# MEN

# RESTRUCTURE

gpdMEN <- gpdMEN %>% filter(YEAR > 2000 & !State %like% "Hawaii|Jersey" & State != "DC") %>% group_by(YEAR) %>% 
  mutate(unit.num = 1:n(), logGPD = log(GPD), State = as.character(State)) %>% as.data.table()

 # check distribution to identify special.predictors
 ggplot(data = gpdMEN[State %like% "Indiana"]) + geom_line(aes(x = YEAR, y = GPD)) + 
   scale_x_continuous(breaks = seq(2000, 2020, 1)) + theme_bw() + theme(axis.text.x = element_text(angle = 90)) 

M.dat.prep <- dataprep(foo = gpdMEN, predictors = c("log.pcInc", "YPROP", "MPROP"), 
                       predictors.op = "mean", 
                       dependent = "GPD", unit.variable = "unit.num",
                       time.variable = "YEAR", special.predictors = list(list("GPD", 2005, "mean"), 
                                                                         list("GPD", 2007, "mean"), 
                                                                         list("GPD", 2011, "mean"), 
                                                                         list("GPD", 2012, "mean"), 
                                                                         list("GPD", 2016, "mean")),
                       treatment.identifier = 10, controls.identifier = c(1:9, 11:34),
                       time.predictors.prior = c(2001:2017), time.optimize.ssr = c(2001:2017), 
                       unit.names.variable = "State", time.plot = 2001:2019)

# BUILD SYNTHETIC CONTROL
M.synth.out <- synth(M.dat.prep)

# OUTPUT
M.synth.table <- synth.tab(dataprep.res = m.dat.prep, synth.res = M.synth.out)
print(M.synth.table)

path.plot(dataprep.res = M.dat.prep,synth.res = M.synth.out)
gaps.plot(dataprep.res = M.dat.prep,synth.res = M.synth.out)





