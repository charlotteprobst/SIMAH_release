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
library(forecast)
library(xts)
library(lmtest)

#library(base)
#library(precrec)
#library(urca)


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
DATE <- 20230228

datBRFSS <- data.table(readRDS("brfss/processed_data/BRFSS_upshifted_2000_2020_final.RDS"))
datIND <- datBRFSS[State %like% "Indiana"]

datUNEMP <- data.table(read.csv("acp_brfss/data/20230228_INDIANAunempPREP.csv"))
datGDP <- read.xlsx("acp_brfss/data/20230228_INDIANA_GDP.xlsx", sheet = 1, startRow = 1) %>% select("year","gdp") %>% data.table()
datPOP <- data.table(read.csv("acp_brfss/data/20230228_INDIANApopPREP.csv"))
 
# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# PREPARE DATA
# ----------------------------------------------------------------

# subgroups
datIND[, education := factor(education_summary, levels = c("LEHS", "SomeC", "College"))]

# quarters
datIND[, Q := ifelse(surveymonth %like% "Feb|Mar|Apr", 125, ifelse(surveymonth %like% "May|Jun|Jul", 375,
                                                               ifelse(surveymonth %like% "Aug|Sep|Oct", 625, ifelse(surveymonth %like% "Nov|Dec|Jan", 875, NA))))]
datIND[, QYEAR := as.numeric(ifelse(surveymonth %like% "Jan", paste0(YEAR-1, ".", Q), paste0(YEAR, ".", Q)))]

  ggplot(data = datIND, aes(x = QYEAR)) + geom_histogram(stat = "count") + theme(axis.text.x = element_text(angle = 90))
  # drop 2000.875 (too few observations)
  # drop 2020.375 onwards

datIND <- copy(datIND[QYEAR != 2000.875])
datIND <- copy(datIND[QYEAR < 2020.375])

  ggplot(data = datIND, aes(x = QYEAR)) + geom_histogram(stat = "count") + theme(axis.text.x = element_text(angle = 90))

# intervention 
datIND[, INT_YEAR := 2018]
datIND[, INT_QYEAR := 2018.125]

# group
datIND[, sex := ifelse(sex_recode == "Female", "Women", ifelse(sex_recode == "Male", "Men", NA))]
datIND[, group := paste0(sex, "_", education)]

# cap estimates >200 GPD
summary(datIND$gramsperday_orig)
datIND[gramsperday_orig > 200] # 31 observations
summary(datIND[gramsperday_upshifted > 200]$gramsperday_orig)
datIND[gramsperday_upshifted > 200] # 276 observations
datIND[, GPD_upshifted_capped := ifelse(gramsperday_upshifted > 200, 200, gramsperday_upshifted)]
datIND[, GPD_orig_capped := ifelse(gramsperday_orig > 200, 200, gramsperday_orig)]

# drop abstainers
#data <- datIND[drinkingstatus_upshifted == 1]
data <- datIND[drinkingstatus_orig == 1]

# add covariates
data <- merge(data[, QYEAR := as.numeric(QYEAR)], datUNEMP[, QYEAR := as.numeric(QYEAR)], by = "QYEAR")
data <- merge(data[, YEAR := as.numeric(YEAR)], datGDP[, YEAR := as.numeric(year)], by = "YEAR")
data <- merge(data[, YEAR := as.numeric(YEAR)], datPOP[, YEAR := as.numeric(year)], by = "YEAR")

# select data
data <- data[, .(YEAR, QYEAR, INT_YEAR, INT_QYEAR, final_sample_weight, State, sex, education, group, GPD_orig_capped, unemp.rate, w.unemp.rate, m.unemp.rate, gdp, pop18_24, w.pop18_24, m.pop18_24)]
  
# ----------------------------------------------------------------
# EXPLORE DATA
# ----------------------------------------------------------------

# participants by groups
ggplot(data = data, aes(x = QYEAR)) + geom_histogram(stat = "count") + geom_hline(yintercept = c(50, 100)) + 
  facet_grid(rows = vars(as.factor(education)), cols = vars(as.factor(sex)), scales = "free_y") +
  scale_x_continuous(breaks = seq(2000, 2020, 1), limits = c(2000, 2020)) +
  theme_bw() + theme(axis.text.x = element_text(angle = 90), strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/Indiana/', DATE, '_HIST_BY_GROUP.png'), dpi=300, width = 12, height = 7)

data <- data[QYEAR > 2002]

# ----------------------------------------------------------------
# OUTCOME 1: GPD
# ----------------------------------------------------------------

# GPD distrubtion unweighted
ggplot(data = data) +
  geom_smooth(aes(x = QYEAR, y = GPD_orig_capped)) + 
  geom_vline(aes(xintercept = INT_QYEAR)) +
  scale_x_continuous(breaks = seq(2000, 2020, 1)) +
  facet_grid(rows = vars(as.factor(education)), cols = vars(sex), scales = "free") +
  labs(y = "GPD (unweighted)") + theme_bw() + theme(strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/Indiana/', DATE, '_GPDsmooth_BY_GROUP.png'), dpi=300, width = 12, height = 7)

# SEX

# GPD weighted (simply weights, not accounting for sampling design)
wdat.gpd.sex <- data %>% 
  group_by(QYEAR, sex) %>%
  summarise(GPD = wtd.mean(GPD_orig_capped, final_sample_weight),
            GPD.LB = wtd.lb(GPD_orig_capped, final_sample_weight),
            GPD.UB = wtd.ub(GPD_orig_capped, final_sample_weight),
            n = n(),
            INT_QYEAR = mean(INT_QYEAR), w.unemp.rate = mean(w.unemp.rate), m.unemp.rate = mean(m.unemp.rate),
            gdp = mean(gdp), pop18_24 = mean(pop18_24), w.pop18_24 = mean(w.pop18_24), m.pop18_24 = mean(m.pop18_24)) %>% as.data.table()

wdat.gpd.sex.long <- reshape(wdat.gpd.sex, idvar = c("QYEAR", "sex", "n", "INT_QYEAR"),
                     varying = c("GPD", "GPD.LB", "GPD.UB"), v.name = c("GPD"), 
                     times = c("mean", "LB", "UB"), direction = "long")

# GPD distribution weighted
ggplot(data = wdat.gpd.sex.long) + geom_line(aes(x = QYEAR, y = GPD, color = time)) + geom_vline(aes(xintercept = INT_QYEAR)) +
  scale_x_continuous(breaks = seq(2000, 2020, 1)) + scale_colour_manual(values = c("grey", "black", "grey")) +
  facet_grid(cols = vars(sex), scales = "free") +
  labs(y = "GPD (weighted)") + theme_bw() + theme(legend.position = "none", strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/Indiana/', DATE, '_GPDweighted_BY_SEX.png'), dpi=300, width = 12, height = 7)

# SES

# GPD weighted (simply weights, not accounting for sampling design)
wdat.gpd.ses <- data %>% 
  group_by(QYEAR, education) %>%
  summarise(GPD = wtd.mean(GPD_orig_capped, final_sample_weight),
            GPD.LB = wtd.lb(GPD_orig_capped, final_sample_weight),
            GPD.UB = wtd.ub(GPD_orig_capped, final_sample_weight),
            n = n(),
            INT_QYEAR = mean(INT_QYEAR), unemp.rate = mean(unemp.rate),
            gdp = mean(gdp), pop18_24 = mean(pop18_24), w.pop18_24 = mean(w.pop18_24), m.pop18_24 = mean(m.pop18_24)) %>% as.data.table()

wdat.gpd.ses.long <- reshape(wdat.gpd.ses, idvar = c("QYEAR", "education", "n", "INT_QYEAR"),
                     varying = c("GPD", "GPD.LB", "GPD.UB"), v.name = c("GPD"), 
                     times = c("mean", "LB", "UB"), direction = "long")

# GPD distribution weighted
ggplot(data = wdat.gpd.ses.long) + geom_line(aes(x = QYEAR, y = GPD, color = time)) + geom_vline(aes(xintercept = INT_QYEAR)) +
  scale_x_continuous(breaks = seq(2000, 2020, 1)) + scale_colour_manual(values = c("grey", "black", "grey")) +
  facet_grid(rows = vars(as.factor(education)), scales = "free") +
  labs(y = "GPD (weighted)") + theme_bw() + theme(legend.position = "none", strip.background = element_rect(fill="white")) 
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

# ----------------------------------------------------------------
# OUTCOME 2: GPD among category II/III drinkers
# ----------------------------------------------------------------

# subset
sub <- data[sex == "Women" & GPD_orig_capped >= 14 | sex == "Men" & GPD_orig_capped >= 28,] 
  # 20 / 20: n = 8127 (original)
  # 14 / 28: n = 8605 (original)

# available participants by group -> by SEX and SES not possible due to low n
ggplot(data = sub, aes(x = QYEAR)) + geom_histogram(stat = "count") + geom_hline(yintercept = c(50, 100)) + 
  facet_grid(rows = vars(as.factor(education)), cols = vars(as.factor(sex)), scales = "free_y") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90), strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/Indiana/', DATE, '_HIST_BY_GROUP (SUBSAMPLE).png'), dpi=300, width = 12, height = 7)

# available participants by group -> by SES only 
ggplot(data = sub, aes(x = QYEAR)) + geom_histogram(stat = "count") + geom_hline(yintercept = c(50, 100)) + 
  facet_grid(rows = vars(as.factor(education)), scales = "free_y") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90), strip.background = element_rect(fill="white")) 

# reduce SES groups to two ? different level of consumption across groups
wtd.mean(sub[education == "LEHS"]$GPD_orig_capped) # 47g
wtd.mean(sub[education == "SomeC"]$GPD_orig_capped) # 38g
wtd.mean(sub[education == "College"]$GPD_orig_capped) # 33g
  
# combine LEHS and SomeC for subanalysis
sub[, education := ifelse(education %like% "LEHS|SomeC", "LEHS/SomeC", ifelse(education %like% "College", "College", NA))]

ggplot(data = sub, aes(x = QYEAR)) + geom_histogram(stat = "count") + geom_hline(yintercept = c(50, 100)) + 
  facet_grid(rows = vars(as.factor(education)), scales = "free_y") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90), strip.background = element_rect(fill="white")) 

# SEX

# GPD weighted (simply weights, not accounting for sampling design)
sub.sex <- sub %>% 
  group_by(QYEAR, sex) %>%
  summarise(GPD = wtd.mean(GPD_orig_capped, final_sample_weight),
            GPD.LB = wtd.lb(GPD_orig_capped, final_sample_weight),
            GPD.UB = wtd.ub(GPD_orig_capped, final_sample_weight),
            n = n(),
            INT_QYEAR = mean(INT_QYEAR), w.unemp.rate = mean(w.unemp.rate), m.unemp.rate = mean(m.unemp.rate),
            gdp = mean(gdp), pop18_24 = mean(pop18_24), w.pop18_24 = mean(w.pop18_24), m.pop18_24 = mean(m.pop18_24)) %>% as.data.table()

sub.sex.long <- reshape(sub.sex, idvar = c("QYEAR", "sex", "n", "INT_QYEAR"),
                             varying = c("GPD", "GPD.LB", "GPD.UB"), v.name = c("GPD"), 
                             times = c("mean", "LB", "UB"), direction = "long")

# GPD distribution weighted
ggplot(data = sub.sex.long) + geom_line(aes(x = QYEAR, y = GPD, color = time)) + geom_vline(aes(xintercept = INT_QYEAR)) +
  scale_x_continuous(breaks = seq(2000, 2020, 1)) + scale_colour_manual(values = c("grey", "black", "grey")) +
  facet_grid(cols = vars(sex), scales = "free") +
  labs(y = "GPD (weighted)") + theme_bw() + theme(legend.position = "none", strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/Indiana/', DATE, '_GPDweighted_BY_SEX (SUBSAMPLE).png'), dpi=300, width = 12, height = 7)

# SES

# GPD weighted (simply weights, not accounting for sampling design)
sub.ses <- sub %>% 
  group_by(QYEAR, education) %>%
  summarise(GPD = wtd.mean(GPD_orig_capped, final_sample_weight),
            GPD.LB = wtd.lb(GPD_orig_capped, final_sample_weight),
            GPD.UB = wtd.ub(GPD_orig_capped, final_sample_weight),
            n = n(),
            INT_QYEAR = mean(INT_QYEAR), unemp.rate = mean(unemp.rate),
            gdp = mean(gdp), pop18_24 = mean(pop18_24), w.pop18_24 = mean(w.pop18_24), m.pop18_24 = mean(m.pop18_24)) %>% as.data.table()

sub.ses.long <- reshape(sub.ses, idvar = c("QYEAR", "education", "n", "INT_QYEAR"),
                             varying = c("GPD", "GPD.LB", "GPD.UB"), v.name = c("GPD"), 
                             times = c("mean", "LB", "UB"), direction = "long")

# GPD distribution weighted
ggplot(data = sub.ses.long) + geom_line(aes(x = QYEAR, y = GPD, color = time)) + geom_vline(aes(xintercept = INT_QYEAR)) +
  scale_x_continuous(breaks = seq(2000, 2020, 1)) + scale_colour_manual(values = c("grey", "black", "grey")) +
  facet_grid(rows = vars(as.factor(education)), scales = "free") +
  labs(y = "GPD (weighted)") + theme_bw() + theme(legend.position = "none", strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/Indiana/', DATE, '_GPDweighted_BY_SES (SUBSAMPLE).png'), dpi=300, width = 12, height = 7)

# CHECK COVARIATES

# gdp x unemployment
cor.test(wdat.gpd.ses[education == "LEHS"]$gdp, log(wdat.gpd.ses[education == "LEHS"]$unemp.rate), method = "pearson") # sig
cor.test(wdat.gpd.ses[education == "LEHS"]$gdp, log(wdat.gpd.ses[education == "LEHS"]$unemp.rate), method = "spearman") # sig

# gdp x population
cor.test(wdat.gpd.ses[education == "LEHS"]$gdp, log(wdat.gpd.ses[education == "LEHS"]$pop18_24), method = "pearson") # sig
cor.test(wdat.gpd.ses[education == "LEHS"]$gdp, log(wdat.gpd.ses[education == "LEHS"]$pop18_24), method = "spearman") # sig

# population x unemployment
cor.test(wdat.gpd.ses[education == "LEHS"]$pop18_24, log(wdat.gpd.ses[education == "LEHS"]$unemp.rate), method = "pearson") # sig
cor.test(wdat.gpd.ses[education == "LEHS"]$pop18_24, log(wdat.gpd.ses[education == "LEHS"]$unemp.rate), method = "spearman") # sig


# ----------------------------------------------------------------
# OUTCOME: GPD
# ----------------------------------------------------------------

# ----------------------------------------------------------------
# OUTCOME: GPD
# ITS - WOMEN
# https://nicolarighetti.github.io/Time-Series-Analysis-With-R/intervention-analysis.html
# ----------------------------------------------------------------

# COVARIATES

# unemployment
ggplot(wdat.gpd.sex[sex == "Women"], aes(x = log(w.unemp.rate))) + geom_histogram()
cor.test(wdat.gpd.sex[sex == "Women"]$GPD, log(wdat.gpd.sex[sex == "Women"]$w.unemp.rate), method = "pearson") # p = 0.015
cor.test(wdat.gpd.sex[sex == "Women"]$GPD, log(wdat.gpd.sex[sex == "Women"]$w.unemp.rate), method = "spearman") # p = 0.049
unemp <- log(wdat.gpd.sex[sex == "Women"]$w.unemp.rate)

# % population 18-24
ggplot(wdat.gpd.sex[sex == "Women"], aes(x = w.pop18_24)) + geom_histogram()
cor.test(wdat.gpd.sex[sex == "Women"]$GPD, wdat.gpd.sex[sex == "Women"]$w.pop18_24, method = "pearson") # p = .02
cor.test(wdat.gpd.sex[sex == "Women"]$GPD, wdat.gpd.sex[sex == "Women"]$w.pop18_24, method = "spearman") # n.s.

# BASELINE MODEL

TS.GPD.W <- ts(wdat.gpd.sex[sex == "Women", .(GPD)], frequency = 4, start = c(2002,1))
plot.ts(TS.GPD.W) + abline(v = 2018, lty = "dashed")

# NORMALITY
shapiro.test(TS.GPD.W) # p = .052
car::qqPlot(TS.GPD.W)

# add intervention

# step increase
step <- as.numeric(as.yearqtr(time(TS.GPD.W)) >= "2018 Q1")
step 

# slope increase
slope <- c(rep(0,64), seq(1,9,1)) 
slope

# identify ARIMA model
model1 <- auto.arima(TS.GPD.W, xreg = cbind(step, slope, unemp), stepwise = FALSE)
checkresiduals(model1)  
model1 #ARIMA(0,1,2)

# COUNTREFACTUAL MODEL

# use arima model without post-intervention period
model2 <- arima(window(TS.GPD.W, xreg = cbind(unemp), end = c(2017,4)), order = c(0, 1, 2))
checkresiduals(model2) 

# forecast based on the counterfactual model
fc <- forecast(model2, h = 9)

# make forecast a timeseries
TS.fc <- ts(as.numeric(fc$mean), start = c(2018,1), frequency = 4)

# COMBINE AND TEST INTERVENTION EFFECT

# combine both timeseries
TS.GPD.W.comb <- ts.union(TS.GPD.W, TS.fc)
TS.GPD.W.comb

# plot combined data
png(file = paste0('acp_brfss/outputs/figures/Indiana/', DATE, '_TS_GPD_WOMEN.png'), width = 800, height = 450)
plot.ts(TS.GPD.W.comb, plot.type = "single", 
        col = c("blue", "red"), xlab = "QYEAR", ylab = "Grams pure alcohol per day (WOMEN)", lty = c("solid", "dashed")) +
  abline(v = 2018, lty = "dashed")
dev.off()

# quantify intervention effect
coeftest(model1) #n.s.
confint(model1)

# observations
min(wdat.gpd.sex[sex == "Women"]$n)
max(wdat.gpd.sex[sex == "Women"]$n)

# ----------------------------------------------------------------
# OUTCOME: GPD
# ITS - MEN
# ----------------------------------------------------------------

# COVARIATES

# unemployment
ggplot(wdat.gpd.sex[sex == "Men"], aes(x = log(m.unemp.rate))) + geom_histogram()
cor.test(wdat.gpd.sex[sex == "Men"]$GPD, log(wdat.gpd.sex[sex == "Men"]$m.unemp.rate), method = "pearson") # p < 0.001
cor.test(wdat.gpd.sex[sex == "Men"]$GPD, log(wdat.gpd.sex[sex == "Men"]$m.unemp.rate), method = "spearman") # p < 0.001
unemp <- log(wdat.gpd.sex[sex == "Men"]$m.unemp.rate)

# % population 18-24
ggplot(wdat.gpd.sex[sex == "Men"], aes(x = m.pop18_24)) + geom_histogram()
cor.test(wdat.gpd.sex[sex == "Men"]$GPD, wdat.gpd.sex[sex == "Men"]$m.pop18_24, method = "pearson") # n.s.
cor.test(wdat.gpd.sex[sex == "Men"]$GPD, wdat.gpd.sex[sex == "Men"]$m.pop18_24, method = "spearman") # n.s.

# BASELINE MODEL

TS.GPD.M <- ts(wdat.gpd.sex[sex == "Men", .(GPD)], frequency = 4, start = c(2002,1))
plot.ts(TS.GPD.M) + abline(v = 2018, lty = "dashed")

# NORMALITY
shapiro.test(TS.GPD.M) # p = .404
car::qqPlot(TS.GPD.M)

# identify ARIMA model
model1 <- auto.arima(TS.GPD.M, xreg = cbind(step, slope, unemp), stepwise = FALSE)
checkresiduals(model1)  
model1 #ARIMA(0,0,0)(1,0,0)[4] some autocorrelation left

# COUNTREFACTUAL MODEL

# use arima model without post-intervention period
model2 <- arima(window(TS.GPD.M, xreg = cbind(unemp), end = c(2017,4)), order = c(0, 0, 0), seasonal = list(order = c(1, 0, 0), period = 4))
checkresiduals(model2) # some autocorrelation left

# forecast based on the counterfactual model
fc <- forecast(model2, h = 9)

# make forecast a timeseries
TS.fc <- ts(as.numeric(fc$mean), start = c(2018,1), frequency = 4)

# COMBINE AND TEST INTERVENTION EFFECT

# combine both timeseries
TS.GPD.M.comb <- ts.union(TS.GPD.M, TS.fc)
TS.GPD.M.comb

# plot combined data
png(file = paste0('acp_brfss/outputs/figures/Indiana/', DATE, '_TS_GPD_MEN.png'), width = 800, height = 450)
plot.ts(TS.GPD.M.comb, plot.type = "single", 
        col = c("blue", "red"), xlab = "QYEAR", ylab = "Grams pure alcohol per day (MEN)", lty = c("solid", "dashed")) +
  abline(v = 2018, lty = "dashed")
dev.off()

# quantify intervention effect
coeftest(model1) # step increase, p = .041
confint(model1)

# observations
min(wdat.gpd.sex[sex == "Men"]$n)
max(wdat.gpd.sex[sex == "Men"]$n)

# ----------------------------------------------------------------
# OUTCOME: GPD
# ITS - LEHS
# ----------------------------------------------------------------

# COVARIATES

# unemployment
ggplot(wdat.gpd.ses[education == "LEHS"], aes(x = log(unemp.rate))) + geom_histogram()
cor.test(wdat.gpd.ses[education == "LEHS"]$GPD, log(wdat.gpd.ses[education == "LEHS"]$unemp.rate), method = "pearson") # n.s.
cor.test(wdat.gpd.ses[education == "LEHS"]$GPD, log(wdat.gpd.ses[education == "LEHS"]$unemp.rate), method = "spearman") # n.s.

# % population 18-24
ggplot(wdat.gpd.ses[education == "LEHS"], aes(x = pop18_24)) + geom_histogram()
cor.test(wdat.gpd.ses[education == "LEHS"]$GPD, wdat.gpd.ses[education == "LEHS"]$pop18_24, method = "pearson") # n.s.
cor.test(wdat.gpd.ses[education == "LEHS"]$GPD, wdat.gpd.ses[education == "LEHS"]$pop18_24, method = "spearman") # n.s.

# BASELINE MODEL

TS.GPD.LEHS <- ts(wdat.gpd.ses[education == "LEHS", .(GPD)], frequency = 4, start = c(2002,1))
plot.ts(TS.GPD.LEHS) + abline(v = 2018, lty = "dashed")

# NORMALITY
shapiro.test(TS.GPD.LEHS) # p = .125
car::qqPlot(TS.GPD.LEHS)

# identify ARIMA model
model1 <- auto.arima(TS.GPD.LEHS, xreg = cbind(step, slope), stepwise = FALSE)
checkresiduals(model1)  
model1 #ARIMA(0,0,1)

# COUNTREFACTUAL MODEL

# use arima model without post-intervention period
model2 <- arima(window(TS.GPD.LEHS, end = c(2017,4)), order = c(0, 0, 1))
checkresiduals(model2)  

# forecast based on the counterfactual model
fc <- forecast(model2, h = 9)

# make forecast a timeseries
TS.fc <- ts(as.numeric(fc$mean), start = c(2018,1), frequency = 4)

# COMBINE AND TEST INTERVENTION EFFECT

# combine both timeseries
TS.GPD.LEHS.comb <- ts.union(TS.GPD.LEHS, TS.fc)
TS.GPD.LEHS.comb

# plot combined data
png(file = paste0('acp_brfss/outputs/figures/Indiana/', DATE, '_TS_GPD_LEHS.png'), width = 800, height = 450)
plot.ts(TS.GPD.LEHS.comb, plot.type = "single", 
        col = c("blue", "red"), xlab = "QYEAR", ylab = "Grams pure alcohol per day (LEHS)", lty = c("solid", "dashed")) +
  abline(v = 2018, lty = "dashed")
dev.off()

# quantify intervention effect
coeftest(model1) #n.s.
confint(model1)

# observations
min(wdat.gpd.ses[education == "LEHS"]$n)
max(wdat.gpd.ses[education == "LEHS"]$n)

# ----------------------------------------------------------------
# OUTCOME: GPD
# ITS - SomeC
# ----------------------------------------------------------------

# COVARIATES

# unemployment
ggplot(wdat.gpd.ses[education == "SomeC"], aes(x = log(unemp.rate))) + geom_histogram()
cor.test(wdat.gpd.ses[education == "SomeC"]$GPD, log(wdat.gpd.ses[education == "SomeC"]$unemp.rate), method = "pearson") # 0.02
cor.test(wdat.gpd.ses[education == "SomeC"]$GPD, log(wdat.gpd.ses[education == "SomeC"]$unemp.rate), method = "spearman") # 0.03
unemp <- log(wdat.gpd.ses[education == "SomeC"]$unemp.rate)
  
# % population 18-24
ggplot(wdat.gpd.ses[education == "SomeC"], aes(x = pop18_24)) + geom_histogram()
cor.test(wdat.gpd.ses[education == "SomeC"]$GPD, wdat.gpd.ses[education == "SomeC"]$pop18_24, method = "pearson") # 0.05
cor.test(wdat.gpd.ses[education == "SomeC"]$GPD, wdat.gpd.ses[education == "SomeC"]$pop18_24, method = "spearman") # n.s.

# BASELINE MODEL

TS.GPD.SomeC <- ts(wdat.gpd.ses[education == "SomeC", .(GPD)], frequency = 4, start = c(2002,1))
plot.ts(TS.GPD.SomeC) + abline(v = 2018, lty = "dashed")

# NORMALITY
shapiro.test(TS.GPD.SomeC) # p = .712
car::qqPlot(TS.GPD.SomeC)

# identify ARIMA model
model1 <- auto.arima(TS.GPD.SomeC, xreg = cbind(step, slope, unemp), stepwise = FALSE)
checkresiduals(model1)  
model1 #ARIMA(0,0,0)(2,0,0)[4]

# COUNTREFACTUAL MODEL

# use arima model without post-intervention period
model2 <- arima(window(TS.GPD.SomeC, xreg = cbind(unemp), end = c(2017,4)), order = c(0, 0, 0), seasonal = list(order = c(2, 0, 0), period = 4))
checkresiduals(model2)  

# forecast based on the counterfactual model
fc <- forecast(model2, h = 9)

# make forecast a timeseries
TS.fc <- ts(as.numeric(fc$mean), start = c(2018,1), frequency = 4)

# COMBINE AND TEST INTERVENTION EFFECT

# combine both timeseries
TS.GPD.SomeC.comb <- ts.union(TS.GPD.SomeC, TS.fc)
TS.GPD.SomeC.comb

# plot combined data
png(file = paste0('acp_brfss/outputs/figures/Indiana/', DATE, '_TS_GPD_SomeC.png'), width = 800, height = 450)
plot.ts(TS.GPD.SomeC.comb, plot.type = "single", 
        col = c("blue", "red"), xlab = "QYEAR", ylab = "Grams pure alcohol per day (SomeC)", lty = c("solid", "dashed")) +
  abline(v = 2018, lty = "dashed")
dev.off()

# quantify intervention effect
coeftest(model1) # n.s.
confint(model1)

# observations
min(wdat.gpd.ses[education == "SomeC"]$n)
max(wdat.gpd.ses[education == "SomeC"]$n)

# ----------------------------------------------------------------
# OUTCOME: GPD
# ITS - College
# ----------------------------------------------------------------

# COVARIATES

# unemployment
ggplot(wdat.gpd.ses[education == "College"], aes(x = log(unemp.rate))) + geom_histogram()
cor.test(wdat.gpd.ses[education == "College"]$GPD, log(wdat.gpd.ses[education == "College"]$unemp.rate), method = "pearson") # 0.008
cor.test(wdat.gpd.ses[education == "College"]$GPD, log(wdat.gpd.ses[education == "College"]$unemp.rate), method = "spearman") # 0.006
unemp <- log(wdat.gpd.ses[education == "College"]$unemp.rate)

# % population 18-24
ggplot(wdat.gpd.ses[education == "College"], aes(x = pop18_24)) + geom_histogram()
cor.test(wdat.gpd.ses[education == "College"]$GPD, wdat.gpd.ses[education == "College"]$pop18_24, method = "pearson") # n.s.
cor.test(wdat.gpd.ses[education == "College"]$GPD, wdat.gpd.ses[education == "College"]$pop18_24, method = "spearman") # n.s.

# BASELINE MODEL

TS.GPD.College <- ts(wdat.gpd.ses[education == "College", .(GPD)], frequency = 4, start = c(2002,1))
plot.ts(TS.GPD.College) + abline(v = 2018, lty = "dashed")

# NORMALITY
shapiro.test(TS.GPD.College) # p = .583
car::qqPlot(TS.GPD.College)

# identify ARIMA model
model1 <- auto.arima(TS.GPD.College, xreg = cbind(step, slope, unemp), stepwise = FALSE)
checkresiduals(model1) #some autocorrelation left  
model1 #ARIMA(0,1,1)(1,0,0)[4] 

# COUNTREFACTUAL MODEL

# use arima model without post-intervention period
model2 <- arima(window(TS.GPD.College, xreg = cbind(unemp), end = c(2017,4)), order = c(0, 1, 1), seasonal = list(order = c(1, 0, 0), period = 4))
checkresiduals(model2)  

# forecast based on the counterfactual model
fc <- forecast(model2, h = 9)

# make forecast a timeseries
TS.fc <- ts(as.numeric(fc$mean), start = c(2018,1), frequency = 4)

# COMBINE AND TEST INTERVENTION EFFECT

# combine both timeseries
TS.GPD.College.comb <- ts.union(TS.GPD.College, TS.fc)
TS.GPD.College.comb

# plot combined data
png(file = paste0('acp_brfss/outputs/figures/Indiana/', DATE, '_TS_GPD_College.png'), width = 800, height = 450)
plot.ts(TS.GPD.College.comb, plot.type = "single", 
        col = c("blue", "red"), xlab = "QYEAR", ylab = "Grams pure alcohol per day (College)", lty = c("solid", "dashed")) +
  abline(v = 2018, lty = "dashed")
dev.off()

# quantify intervention effect
coeftest(model1) # n.s.
confint(model1)

# observations
min(wdat.gpd.ses[education == "College"]$n)
max(wdat.gpd.ses[education == "College"]$n)

# ----------------------------------------------------------------
# OUTCOME: GPD
# ITS - M - LEHS
# ----------------------------------------------------------------

# COVARIATES

# unemployment
ggplot(wdat.gpd[sex == "Men" & education == "LEHS"], aes(x = log(m.unemp.rate))) + geom_histogram()
cor.test(wdat.gpd[sex == "Men" & education == "LEHS"]$GPD, log(wdat.gpd[sex == "Men" & education == "LEHS"]$m.unemp.rate), method = "pearson") # n.s.
cor.test(wdat.gpd[sex == "Men" & education == "LEHS"]$GPD, log(wdat.gpd[sex == "Men" & education == "LEHS"]$m.unemp.rate), method = "spearman") # n.s.

# % population 18-24
ggplot(wdat.gpd[sex == "Men" & education == "LEHS"], aes(x = m.pop18_24)) + geom_histogram()
cor.test(wdat.gpd[sex == "Men" & education == "LEHS"]$GPD, wdat.gpd[sex == "Men" & education == "LEHS"]$m.pop18_24, method = "pearson") # n.s.
cor.test(wdat.gpd[sex == "Men" & education == "LEHS"]$GPD, wdat.gpd[sex == "Men" & education == "LEHS"]$m.pop18_24, method = "spearman") # n.s.

# BASELINE MODEL

TS.GPD.MLEHS <- ts(wdat.gpd[sex == "Men" & education == "LEHS", .(GPD)], frequency = 4, start = c(2002,1))
plot.ts(TS.GPD.MLEHS) + abline(v = 2018, lty = "dashed")

# NORMALITY
shapiro.test(TS.GPD.MLEHS) # p = .144
car::qqPlot(TS.GPD.MLEHS)

# identify ARIMA model
model1 <- auto.arima(TS.GPD.MLEHS, xreg = cbind(step, slope), stepwise = FALSE)
checkresiduals(model1)  
model1 #ARIMA(0,0,1)

# COUNTREFACTUAL MODEL

# use arima model without post-intervention period
model2 <- arima(window(TS.GPD.MLEHS, end = c(2017,4)), order = c(0, 0, 1))
checkresiduals(model2)  

# forecast based on the counterfactual model
fc <- forecast(model2, h = 9)

# make forecast a timeseries
TS.fc <- ts(as.numeric(fc$mean), start = c(2018,1), frequency = 4)

# COMBINE AND TEST INTERVENTION EFFECT

# combine both timeseries
TS.GPD.MLEHS.comb <- ts.union(TS.GPD.MLEHS, TS.fc)
TS.GPD.MLEHS.comb

# plot combined data
png(file = paste0('acp_brfss/outputs/figures/Indiana/', DATE, '_TS_GPD_MLEHS.png'), width = 800, height = 450)
plot.ts(TS.GPD.MLEHS.comb, plot.type = "single", 
        col = c("blue", "red"), xlab = "QYEAR", ylab = "Grams pure alcohol per day (Men - LEHS)", lty = c("solid", "dashed")) +
  abline(v = 2018, lty = "dashed")
dev.off()

# quantify intervention effect
coeftest(model1) # significant step increase p = 0.05
confint(model1)

# observations
min(wdat.gpd[sex == "Men" & education == "LEHS"]$n)
max(wdat.gpd[sex == "Men" & education == "LEHS"]$n)

# ----------------------------------------------------------------
# OUTCOME: GPD
# ITS - M - SomeC
# ----------------------------------------------------------------

# COVARIATES

# unemployment
ggplot(wdat.gpd[sex == "Men" & education == "SomeC"], aes(x = log(m.unemp.rate))) + geom_histogram()
cor.test(wdat.gpd[sex == "Men" & education == "SomeC"]$GPD, log(wdat.gpd[sex == "Men" & education == "SomeC"]$m.unemp.rate), method = "pearson") # n.s.
cor.test(wdat.gpd[sex == "Men" & education == "SomeC"]$GPD, log(wdat.gpd[sex == "Men" & education == "SomeC"]$m.unemp.rate), method = "spearman") # n.s.

# % population 18-24
ggplot(wdat.gpd[sex == "Men" & education == "SomeC"], aes(x = m.pop18_24)) + geom_histogram()
cor.test(wdat.gpd[sex == "Men" & education == "SomeC"]$GPD, wdat.gpd[sex == "Men" & education == "SomeC"]$m.pop18_24, method = "pearson") # n.s.
cor.test(wdat.gpd[sex == "Men" & education == "SomeC"]$GPD, wdat.gpd[sex == "Men" & education == "SomeC"]$m.pop18_24, method = "spearman") # n.s.

# BASELINE MODEL

TS.GPD.MSomeC <- ts(wdat.gpd[sex == "Men" & education == "SomeC", .(GPD)], frequency = 4, start = c(2002,1))
plot.ts(TS.GPD.MSomeC) + abline(v = 2018, lty = "dashed")

# NORMALITY
shapiro.test(TS.GPD.MSomeC) # p = .005
car::qqPlot(TS.GPD.MSomeC)

# use log
shapiro.test(log(TS.GPD.MSomeC)) # p = .599
car::qqPlot(log(TS.GPD.MSomeC))

TS.GPD.MSomeC <- log(TS.GPD.MSomeC)

# identify ARIMA model
model1 <- auto.arima(TS.GPD.MSomeC, xreg = cbind(step, slope), stepwise = FALSE)
checkresiduals(model1)  
model1 #ARIMA(3,1,0)(2,0,0)[4]

# COUNTREFACTUAL MODEL

# use arima model without post-intervention period
model2 <- arima(window(TS.GPD.MSomeC, end = c(2017,4)), order = c(3, 1, 0), seasonal = list(order = c(2, 0, 0), period = 4))
checkresiduals(model2)  

# forecast based on the counterfactual model
fc <- forecast(model2, h = 9)

# make forecast a timeseries
TS.fc <- ts(as.numeric(fc$mean), start = c(2018,1), frequency = 4)

# COMBINE AND TEST INTERVENTION EFFECT

# combine both timeseries
TS.GPD.MSomeC.comb <- ts.union(TS.GPD.MSomeC, TS.fc)
TS.GPD.MSomeC.comb

# plot combined data
png(file = paste0('acp_brfss/outputs/figures/Indiana/', DATE, '_TS_GPD_MSomeC.png'), width = 800, height = 450)
plot.ts(TS.GPD.MSomeC.comb, plot.type = "single", 
        col = c("blue", "red"), xlab = "QYEAR", ylab = "Grams pure alcohol per day (log, Men - SomeC)", lty = c("solid", "dashed")) +
  abline(v = 2018, lty = "dashed")
dev.off()

# quantify intervention effect
coeftest(model1) # significant step increase p = 0.05
confint(model1)

# observations
min(wdat.gpd[sex == "Men" & education == "SomeC"]$n)
max(wdat.gpd[sex == "Men" & education == "SomeC"]$n)

# ----------------------------------------------------------------
# OUTCOME: GPD
# ITS - M - College
# ----------------------------------------------------------------

# COVARIATES

# unemployment
ggplot(wdat.gpd[sex == "Men" & education == "College"], aes(x = log(m.unemp.rate))) + geom_histogram()
cor.test(wdat.gpd[sex == "Men" & education == "College"]$GPD, log(wdat.gpd[sex == "Men" & education == "College"]$m.unemp.rate), method = "pearson") # .002
cor.test(wdat.gpd[sex == "Men" & education == "College"]$GPD, log(wdat.gpd[sex == "Men" & education == "College"]$m.unemp.rate), method = "spearman") # .001
unemp <- log(wdat.gpd[sex == "Men" & education == "College"]$m.unemp.rate)

# % population 18-24
ggplot(wdat.gpd[sex == "Men" & education == "College"], aes(x = m.pop18_24)) + geom_histogram()
cor.test(wdat.gpd[sex == "Men" & education == "College"]$GPD, wdat.gpd[sex == "Men" & education == "College"]$m.pop18_24, method = "pearson") # n.s.
cor.test(wdat.gpd[sex == "Men" & education == "College"]$GPD, wdat.gpd[sex == "Men" & education == "College"]$m.pop18_24, method = "spearman") # n.s.

# BASELINE MODEL

TS.GPD.MCollege <- ts(wdat.gpd[sex == "Men" & education == "College", .(GPD)], frequency = 4, start = c(2002,1))
plot.ts(TS.GPD.MCollege) + abline(v = 2018, lty = "dashed")

# NORMALITY
shapiro.test(TS.GPD.MCollege) # p = .817
car::qqPlot(TS.GPD.MCollege)

# identify ARIMA model
model1 <- auto.arima(TS.GPD.MCollege, xreg = cbind(step, slope, unemp), stepwise = FALSE)
checkresiduals(model1)  
model1 #ARIMA(0,1,1)(1,0,0)[4]

# COUNTREFACTUAL MODEL

# use arima model without post-intervention period
model2 <- arima(window(TS.GPD.MCollege, xreg = c(unemp), end = c(2017,4)), order = c(0, 1, 1), seasonal = list(order = c(1, 0, 0), period = 4))
checkresiduals(model2)  

# forecast based on the counterfactual model
fc <- forecast(model2, h = 9)

# make forecast a timeseries
TS.fc <- ts(as.numeric(fc$mean), start = c(2018,1), frequency = 4)

# COMBINE AND TEST INTERVENTION EFFECT

# combine both timeseries
TS.GPD.MCollege.comb <- ts.union(TS.GPD.MCollege, TS.fc)
TS.GPD.MCollege.comb

# plot combined data
png(file = paste0('acp_brfss/outputs/figures/Indiana/', DATE, '_TS_GPD_MCollege.png'), width = 800, height = 450)
plot.ts(TS.GPD.MCollege.comb, plot.type = "single", 
        col = c("blue", "red"), xlab = "QYEAR", ylab = "Grams pure alcohol per day (Men - College)", lty = c("solid", "dashed")) +
  abline(v = 2018, lty = "dashed")
dev.off()

# quantify intervention effect
coeftest(model1) # significant slope p = .04
confint(model1)

# observations
min(wdat.gpd[sex == "Men" & education == "College"]$n)
max(wdat.gpd[sex == "Men" & education == "College"]$n)

# ----------------------------------------------------------------
# OUTCOME: GPD SUBSAMPLE
# ----------------------------------------------------------------

# ----------------------------------------------------------------
# OUTCOME: GPD SUBSAMPLE 
# ITS - WOMEN
# ----------------------------------------------------------------

# COVARIATES

# unemployment
ggplot(sub.sex[sex == "Women"], aes(x = log(w.unemp.rate))) + geom_histogram()
cor.test(sub.sex[sex == "Women"]$GPD, log(sub.sex[sex == "Women"]$w.unemp.rate), method = "pearson") # n.s.
cor.test(sub.sex[sex == "Women"]$GPD, log(sub.sex[sex == "Women"]$w.unemp.rate), method = "spearman") # n.s.

# % population 18-24
ggplot(sub.sex[sex == "Women"], aes(x = w.pop18_24)) + geom_histogram()
cor.test(sub.sex[sex == "Women"]$GPD, sub.sex[sex == "Women"]$w.pop18_24, method = "pearson") # n.s.
cor.test(sub.sex[sex == "Women"]$GPD, sub.sex[sex == "Women"]$w.pop18_24, method = "spearman") # n.s.

# BASELINE MODEL

TS.SUB.W <- ts(sub.sex[sex == "Women", .(GPD)], frequency = 4, start = c(2002,1))
plot.ts(TS.SUB.W) + abline(v = 2018, lty = "dashed")

Decomp <- decompose(TS.SUB.W)
plot(Decomp)

# NORMALITY
shapiro.test(TS.SUB.W) # p < .001
car::qqPlot(TS.SUB.W)

shapiro.test(log(TS.SUB.W)) # p = 0.474
car::qqPlot(log(TS.SUB.W))

TS.SUB.W <- log(TS.SUB.W)

# identify ARIMA model
model1 <- auto.arima(TS.SUB.W, xreg = cbind(step, slope), stepwise = FALSE)
checkresiduals(model1)  
model1 #ARIMA(0,0,0)

# COUNTREFACTUAL MODEL

# use arima model without post-intervention period
model2 <- arima(window(TS.SUB.W, end = c(2017,4)), order = c(0, 0, 0))
checkresiduals(model2)  

# forecast based on the counterfactual model
fc <- forecast(model2, h = 9)

# make forecast a timeseries
TS.fc <- ts(as.numeric(fc$mean), start = c(2018,1), frequency = 4)

# COMBINE AND TEST INTERVENTION EFFECT

# combine both timeseries
TS.SUB.W.comb <- ts.union(TS.SUB.W, TS.fc)
TS.SUB.W.comb

# plot combined data
png(file = paste0('acp_brfss/outputs/figures/Indiana/', DATE, '_TS_GPD_WOMEN (SUBSAMPLE).png'), width = 800, height = 450)
plot.ts(TS.SUB.W.comb, plot.type = "single", 
        col = c("blue", "red"), xlab = "QYEAR", ylab = "Grams pure alcohol per day (log, SUBSAMPLE - WOMEN)", lty = c("solid", "dashed")) +
  abline(v = 2018, lty = "dashed")
dev.off()

# quantify intervention effect
coeftest(model1) # n.s.
confint(model1)

# observations
min(sub.sex[sex == "Women"]$n)
max(sub.sex[sex == "Women"]$n)
sub.sex[sex == "Women" & n < 50] %>% count()

# ----------------------------------------------------------------
# OUTCOME: GPD SUBSAMPLE 
# ITS - MEN
# ----------------------------------------------------------------

# COVARIATES

# unemployment
ggplot(sub.sex[sex == "Men"], aes(x = log(m.unemp.rate))) + geom_histogram()
cor.test(sub.sex[sex == "Men"]$GPD, log(sub.sex[sex == "Men"]$m.unemp.rate), method = "pearson") # 0.07
cor.test(sub.sex[sex == "Men"]$GPD, log(sub.sex[sex == "Men"]$m.unemp.rate), method = "spearman") # 0.05
unemp <- log(sub.sex[sex == "Men"]$m.unemp.rate)

# % population 18-24
ggplot(sub.sex[sex == "Men"], aes(x = m.pop18_24)) + geom_histogram()
cor.test(sub.sex[sex == "Men"]$GPD, sub.sex[sex == "Men"]$m.pop18_24, method = "pearson") # n.s.
cor.test(sub.sex[sex == "Men"]$GPD, sub.sex[sex == "Men"]$m.pop18_24, method = "spearman") # n.s.

# BASELINE MODEL

TS.SUB.M <- ts(sub.sex[sex == "Men", .(GPD)], frequency = 4, start = c(2002,1))
plot.ts(TS.SUB.M) + abline(v = 2018, lty = "dashed")

# NORMALITY
shapiro.test(TS.SUB.M) # p = .02
car::qqPlot(TS.SUB.M)

shapiro.test(log(TS.SUB.M)) # p = 37
car::qqPlot(log(TS.SUB.M))

TS.SUB.M <- log(TS.SUB.M)

# identify ARIMA model
model1 <- auto.arima(TS.SUB.M, xreg = cbind(step, slope, unemp), stepwise = FALSE)
checkresiduals(model1)  
model1 #ARIMA(0,0,1)

# COUNTREFACTUAL MODEL

# use arima model without post-intervention period
model2 <- arima(window(TS.SUB.M, xreg = unemp, end = c(2017,4)), order = c(0, 0, 1))
checkresiduals(model2)  

# forecast based on the counterfactual model
fc <- forecast(model2, h = 9)

# make forecast a timeseries
TS.fc <- ts(as.numeric(fc$mean), start = c(2018,1), frequency = 4)

# COMBINE AND TEST INTERVENTION EFFECT

# combine both timeseries
TS.SUB.M.comb <- ts.union(TS.SUB.M, TS.fc)
TS.SUB.M.comb

# plot combined data
png(file = paste0('acp_brfss/outputs/figures/Indiana/', DATE, '_TS_GPD_MEN_adj (SUBSAMPLE.png'), width = 800, height = 450)
plot.ts(TS.SUB.M.comb, plot.type = "single", 
        col = c("blue", "red"), xlab = "QYEAR", ylab = "Grams pure alcohol per day (log, SUBSAMPLE - MEN)", lty = c("solid", "dashed")) +
  abline(v = 2018, lty = "dashed")
dev.off()

# quantify intervention effect
coeftest(model1) # n.s.
confint(model1)

# observations
min(sub.sex[sex == "Men"]$n)
max(sub.sex[sex == "Men"]$n)
sub.sex[sex == "Men" & n < 50] %>% count()

# ----------------------------------------------------------------
# OUTCOME: GPD SUBSAMPLE 
# ITS - LEHS/SomeC
# ----------------------------------------------------------------

# COVARIATES

# unemployment
ggplot(sub.ses[education == "LEHS/SomeC"], aes(x = log(unemp.rate))) + geom_histogram()
cor.test(sub.ses[education == "LEHS/SomeC"]$GPD, log(sub.ses[education == "LEHS/SomeC"]$unemp.rate), method = "pearson") # n.s.
cor.test(sub.ses[education == "LEHS/SomeC"]$GPD, log(sub.ses[education == "LEHS/SomeC"]$unemp.rate), method = "spearman") # n.s.

# % population 18-24
ggplot(sub.ses[education == "LEHS/SomeC"], aes(x = pop18_24)) + geom_histogram()
cor.test(sub.ses[education == "LEHS/SomeC"]$GPD, sub.ses[education == "LEHS/SomeC"]$pop18_24, method = "pearson") # n.s.
cor.test(sub.ses[education == "LEHS/SomeC"]$GPD, sub.ses[education == "LEHS/SomeC"]$pop18_24, method = "spearman") # n.s.

# BASELINE MODEL

TS.SUB.LSC <- ts(sub.ses[education == "LEHS/SomeC", .(GPD)], frequency = 4, start = c(2002,1))
plot.ts(TS.SUB.LSC) + abline(v = 2018, lty = "dashed")

# NORMALITY
shapiro.test(TS.SUB.LSC) # p = .296
car::qqPlot(TS.SUB.LSC)

# identify ARIMA model
model1 <- auto.arima(TS.SUB.LSC, xreg = cbind(step, slope), stepwise = FALSE)
checkresiduals(model1) #some autocorrelation left
model1 #ARIMA(0,0,1)

# COUNTREFACTUAL MODEL

# use arima model without post-intervention period
model2 <- arima(window(TS.SUB.LSC, end = c(2017,4)), order = c(0, 0, 1))
checkresiduals(model2)  

# forecast based on the counterfactual model
fc <- forecast(model2, h = 9)

# make forecast a timeseries
TS.fc <- ts(as.numeric(fc$mean), start = c(2018,1), frequency = 4)

# COMBINE AND TEST INTERVENTION EFFECT

# combine both timeseries
TS.SUB.LSC.comb <- ts.union(TS.SUB.LSC, TS.fc)
TS.SUB.LSC.comb

# plot combined data
png(file = paste0('acp_brfss/outputs/figures/Indiana/', DATE, '_TS_GPD_LEHS/SomeC (SUBSAMPLE) adj.png'), width = 800, height = 450)
plot.ts(TS.SUB.LSC.comb, plot.type = "single", 
        col = c("blue", "red"), xlab = "QYEAR", ylab = "Grams pure alcohol per day (SUBSAMPLE - LEHS/SomeC)", lty = c("solid", "dashed")) +
  abline(v = 2018, lty = "dashed")
dev.off()

# quantify intervention effect
coeftest(model1) # n.s.
confint(model1)

# observations
min(sub.ses[education == "LEHS/SomeC"]$n)
max(sub.ses[education == "LEHS/SomeC"]$n)
sub.ses[education == "LEHS/SomeC" & n < 50] %>% count()

# ----------------------------------------------------------------
# OUTCOME: GPD SUBSAMPLE 
# ITS - College
# ----------------------------------------------------------------

# COVARIATES

# unemployment
ggplot(sub.ses[education == "College"], aes(x = log(unemp.rate))) + geom_histogram()
cor.test(sub.ses[education == "College"]$GPD, log(sub.ses[education == "College"]$unemp.rate), method = "pearson") # n.s.
cor.test(sub.ses[education == "College"]$GPD, log(sub.ses[education == "College"]$unemp.rate), method = "spearman") # n.s.

# % population 18-24
ggplot(sub.ses[education == "College"], aes(x = pop18_24)) + geom_histogram()
cor.test(sub.ses[education == "College"]$GPD, sub.ses[education == "College"]$pop18_24, method = "pearson") # n.s.
cor.test(sub.ses[education == "College"]$GPD, sub.ses[education == "College"]$pop18_24, method = "spearman") # n.s.

# BASELINE MODEL

TS.SUB.College <- ts(sub.ses[education == "College", .(GPD)], frequency = 4, start = c(2002,1))
plot.ts(TS.SUB.College) + abline(v = 2018, lty = "dashed")

# NORMALITY
shapiro.test(TS.SUB.College) # p = .04
car::qqPlot(TS.SUB.College)

# log
shapiro.test(log(TS.SUB.College)) # p = .082
car::qqPlot(log(TS.SUB.College))

TS.SUB.College <- log(TS.SUB.College)

# identify ARIMA model
model1 <- auto.arima(TS.SUB.College, xreg = cbind(step, slope), stepwise = FALSE)
checkresiduals(model1) #some residual autocorrelation  
model1 #ARIMA(0,0,0)(1,0,0)[4]

# COUNTREFACTUAL MODEL

# use arima model without post-intervention period
model2 <- arima(window(TS.SUB.College, end = c(2017,4)), order = c(0, 0, 0), seasonal = list(order = c(1, 0, 0), period = 4))
checkresiduals(model2)  

# forecast based on the counterfactual model
fc <- forecast(model2, h = 9)

# make forecast a timeseries
TS.fc <- ts(as.numeric(fc$mean), start = c(2018,1), frequency = 4)

# COMBINE AND TEST INTERVENTION EFFECT

# combine both timeseries
TS.SUB.College.comb <- ts.union(TS.SUB.College, TS.fc)
TS.SUB.College.comb

# plot combined data
png(file = paste0('acp_brfss/outputs/figures/Indiana/', DATE, '_TS_GPD_College (SUBSAMPLE.png'), width = 800, height = 450)
plot.ts(TS.SUB.College.comb, plot.type = "single", 
        col = c("blue", "red"), xlab = "QYEAR", ylab = "Grams pure alcohol per day (log, SUBSAMPLE - College)", lty = c("solid", "dashed")) +
  abline(v = 2018, lty = "dashed")
dev.off()

# quantify intervention effect
coeftest(model1) # n.s.
confint(model1)

# observations
min(sub.ses[education == "College"]$n)
max(sub.ses[education == "College"]$n)
sub.ses[education == "College" & n < 50] %>% count()




## OLD ###



# ----------------------------------------------------------------
# OUTCOME: HED
# ITS - WOMEN
# ----------------------------------------------------------------

# COVARIATES

# unemployment
cor.test(wdat.hed.sex[sex == "Women"]$HED, wdat.hed.sex[sex == "Women"]$unemp.rate, method = "pearson") # n.s.

# BASELINE MODEL

TS.HED.W <- ts(wdat.hed.sex[sex == "Women", .(HED)], frequency = 4, start = c(2001,1))
plot.ts(TS.HED.W) + abline(v = 2018, lty = "dashed")

# NORMALITY
shapiro.test(TS.HED.W) # p = .076
car::qqPlot(TS.HED.W)

# identify ARIMA model
model1 <- auto.arima(TS.HED.W, xreg = cbind(step, slope), stepwise = FALSE)
checkresiduals(model1)  
model1 #ARIMA(0,1,1)(0,0,1)[4]

# COUNTREFACTUAL MODEL

# use arima model without post-intervention period
model2 <- arima(window(TS.HED.W, end = c(2017,4)), order = c(0, 1, 1), seasonal = list(order = c(0, 0, 1), period = 4))
checkresiduals(model2)  

# forecast based on the counterfactual model
fc <- forecast(model2, h = 9)

# make forecast a timeseries
TS.fc <- ts(as.numeric(fc$mean), start = c(2018,1), frequency = 4)

# COMBINE AND TEST INTERVENTION EFFECT

# combine both timeseries
TS.HED.W.comb <- ts.union(TS.HED.W, TS.fc)
TS.HED.W.comb

# plot combined data
png(file = paste0('acp_brfss/outputs/figures/Indiana/', DATE, '_TS_HED_WOMEN.png'), width = 800, height = 450)
plot.ts(TS.HED.W.comb, plot.type = "single", 
        col = c("blue", "red"), xlab = "QYEAR", ylab = "HED (WOMEN)", lty = c("solid", "dashed")) +
  abline(v = 2018, lty = "dashed")
dev.off()

# quantify intervention effect
coeftest(model1) # n.s.
confint(model1)


# ----------------------------------------------------------------
# OUTCOME: HED
# ITS - MEN
# ----------------------------------------------------------------

# COVARIATES

# unemployment
cor.test(wdat.hed.sex[sex == "Men"]$HED, wdat.hed.sex[sex == "Men"]$unemp.rate, method = "pearson") # n.s.

# BASELINE MODEL

TS.HED.M <- ts(wdat.hed.sex[sex == "Men", .(HED)], frequency = 4, start = c(2001,1))
plot.ts(TS.HED.M) + abline(v = 2018, lty = "dashed")

# NORMALITY
shapiro.test(TS.HED.M) # p = .218
car::qqPlot(TS.HED.M)

# identify ARIMA model
model1 <- auto.arima(TS.HED.M, xreg = cbind(step, slope), stepwise = FALSE)
checkresiduals(model1)  
model1 #ARIMA(0,0,2)

# COUNTREFACTUAL MODEL

# use arima model without post-intervention period
model2 <- arima(window(TS.HED.M, end = c(2017,4)), order = c(0, 0, 2))
checkresiduals(model2)  

# forecast based on the counterfactual model
fc <- forecast(model2, h = 9)

# make forecast a timeseries
TS.fc <- ts(as.numeric(fc$mean), start = c(2018,1), frequency = 4)

# COMBINE AND TEST INTERVENTION EFFECT

# combine both timeseries
TS.HED.M.comb <- ts.union(TS.HED.M, TS.fc)
TS.HED.M.comb

# plot combined data
png(file = paste0('acp_brfss/outputs/figures/Indiana/', DATE, '_TS_HED_MEN.png'), width = 800, height = 450)
plot.ts(TS.HED.M.comb, plot.type = "single", 
        col = c("blue", "red"), xlab = "QYEAR", ylab = "HED (MEN)", lty = c("solid", "dashed")) +
  abline(v = 2018, lty = "dashed")
dev.off()

# quantify intervention effect
coeftest(model1) # n.s.
confint(model1)


# ----------------------------------------------------------------
# OUTCOME: HED
# ITS - LEHS
# ----------------------------------------------------------------

# COVARIATES

# unemployment
cor <- glm(HED ~ unemp.rate, data = wdat.hed.ses[education == "LEHS"], family = quasibinomial('logit'))
summary(cor) # n.s.

# BASELINE MODEL

TS.HED.LEHS <- ts(wdat.hed.ses[education == "LEHS", .(HED)], frequency = 4, start = c(2001,1))
plot.ts(TS.HED.LEHS) + abline(v = 2018, lty = "dashed")

# NORMALITY
shapiro.test(TS.HED.LEHS) # p = .334
car::qqPlot(TS.HED.LEHS)

# identify ARIMA model
model1 <- auto.arima(TS.HED.LEHS, xreg = cbind(step, slope), stepwise = FALSE)
checkresiduals(model1)  
model1 #ARIMA(1,0,0)

# COUNTREFACTUAL MODEL

# use arima model without post-intervention period
model2 <- arima(window(TS.HED.LEHS, end = c(2017,4)), order = c(1, 0, 0))
checkresiduals(model2)  

# forecast based on the counterfactual model
fc <- forecast(model2, h = 9)

# make forecast a timeseries
TS.fc <- ts(as.numeric(fc$mean), start = c(2018,1), frequency = 4)

# COMBINE AND TEST INTERVENTION EFFECT

# combine both timeseries
TS.HED.LEHS.comb <- ts.union(TS.HED.LEHS, TS.fc)
TS.HED.LEHS.comb

# plot combined data
png(file = paste0('acp_brfss/outputs/figures/Indiana/', DATE, '_TS_HED_LEHS.png'), width = 800, height = 450)
plot.ts(TS.HED.LEHS.comb, plot.type = "single", 
        col = c("blue", "red"), xlab = "QYEAR", ylab = "HED (LEHS)", lty = c("solid", "dashed")) +
  abline(v = 2018, lty = "dashed")
dev.off()

# quantify intervention effect
coeftest(model1) # n.s.
confint(model1)


# ----------------------------------------------------------------
# OUTCOME: HED
# ITS - SomeC
# ----------------------------------------------------------------

# COVARIATES

# unemployment
cor <- glm(HED ~ unemp.rate, data = wdat.hed.ses[education == "SomeC"], family = quasibinomial('logit'))
summary(cor) # n.s.

# BASELINE MODEL

TS.HED.SomeC <- ts(wdat.hed.ses[education == "SomeC", .(HED)], frequency = 4, start = c(2001,1))
plot.ts(TS.HED.SomeC) + abline(v = 2018, lty = "dashed")

# NORMALITY
shapiro.test(TS.HED.SomeC) # p = .403
car::qqPlot(TS.HED.SomeC)

# identify ARIMA model
model1 <- auto.arima(TS.HED.SomeC, xreg = cbind(step, slope), stepwise = FALSE)
checkresiduals(model1)  
model1 #ARIMA(0,0,0)(1,0,1)[4]

# COUNTREFACTUAL MODEL

# use arima model without post-intervention period
model2 <- arima(window(TS.HED.SomeC, end = c(2017,4)), order = c(0, 0, 0), seasonal = list(order = c(1, 0, 1), period = 4))
checkresiduals(model2)  

# forecast based on the counterfactual model
fc <- forecast(model2, h = 9)

# make forecast a timeseries
TS.fc <- ts(as.numeric(fc$mean), start = c(2018,1), frequency = 4)

# COMBINE AND TEST INTERVENTION EFFECT

# combine both timeseries
TS.HED.SomeC.comb <- ts.union(TS.HED.SomeC, TS.fc)
TS.HED.SomeC.comb

# plot combined data
png(file = paste0('acp_brfss/outputs/figures/Indiana/', DATE, '_TS_HED_SomeC.png'), width = 800, height = 450)
plot.ts(TS.HED.SomeC.comb, plot.type = "single", 
        col = c("blue", "red"), xlab = "QYEAR", ylab = "HED (SomeC)", lty = c("solid", "dashed")) +
  abline(v = 2018, lty = "dashed")
dev.off()

# quantify intervention effect
coeftest(model1) # n.s.
confint(model1)


# ----------------------------------------------------------------
# OUTCOME: HED
# ITS - College
# ----------------------------------------------------------------

# COVARIATES

# unemployment
cor <- glm(HED ~ unemp.rate, data = wdat.hed.ses[education == "College"], family = quasibinomial('logit'))
summary(cor)# SIG! p = 0.02

# BASELINE MODEL

TS.HED.College <- ts(wdat.hed.ses[education == "College", .(HED)], frequency = 4, start = c(2001,1))
plot.ts(TS.HED.College) + abline(v = 2018, lty = "dashed")

# NORMALITY
shapiro.test(TS.HED.College) # p = .017
car::qqPlot(TS.HED.College)
ggplot(wdat.hed.ses[education == "College", ], aes(x = HED)) + geom_histogram()

# log 
shapiro.test(log(TS.HED.College)) # p = 0.009
car::qqPlot(log(TS.HED.College))
ggplot(wdat.hed.ses[education == "College", ], aes(x = log(HED))) + geom_histogram()

# use log for now but data still not normally distributed!
TS.HED.College <- log(TS.HED.College)

# identify ARIMA model
model1 <- auto.arima(TS.HED.College, xreg = cbind(step, slope, unemp), stepwise = FALSE)
checkresiduals(model1)  
model1 #ARIMA(1,1,1)

# COUNTREFACTUAL MODEL

# use arima model without post-intervention period
model2 <- arima(window(TS.HED.College, xreg = unemp, end = c(2017,4)), order = c(1, 1, 1))
checkresiduals(model2)  

# forecast based on the counterfactual model
fc <- forecast(model2, h = 9)

# make forecast a timeseries
TS.fc <- ts(as.numeric(fc$mean), start = c(2018,1), frequency = 4)

# COMBINE AND TEST INTERVENTION EFFECT

# combine both timeseries
TS.HED.College.comb <- ts.union(TS.HED.College, TS.fc)
TS.HED.College.comb

# plot combined data
png(file = paste0('acp_brfss/outputs/figures/Indiana/', DATE, '_TS_HED_College adj.png'), width = 800, height = 450)
plot.ts(TS.HED.College.comb, plot.type = "single", 
        col = c("blue", "red"), xlab = "QYEAR", ylab = "HED (College)", lty = c("solid", "dashed")) +
  abline(v = 2018, lty = "dashed")
dev.off()

# quantify intervention effect
coeftest(model1) # n.s.
confint(model1)


# ----------------------------------------------------------------
# ----------------------------------------------------------------
# ----------------------------------------------------------------
# ----------------------------------------------------------------
# ----------------------------------------------------------------
# ----------------------------------------------------------------

## ADDITIONAL EXPLORATIONS

# different approach  
  auto.arima(TS.WCOL, stationary = F, seasonal = T) # ARIMA(3,1,0)(2,0,0)[4]

# autocorrelation 
acf(TS.WCOL, lag.max = 30)
pacf(TS.WCOL, lag.max = 30)

Box.test(TS.WCOL, type ="Ljung-Box") # p = .007 autocorrelation present

# seasonality / decomposing data (https://ademos.people.uic.edu/Chapter23.html#6_decomposing_data)
TSdecomp.WCOL <- decompose(ts(TS.WCOL, frequency = 4))
plot(TSdecomp.WCOL)

  # remove seasonal pattern
  TSdecomp.WCOL.SeasonalAdj <- seasadj(TSdecomp.WCOL)
  plot(TSdecomp.WCOL.SeasonalAdj)

# stationarity (based on https://otexts.com/fpp2/stationarity.html)
ur.kpss(TSdecomp.WCOL.SeasonalAdj) %>% summary() # non-stationary

  # seasonal differencing (d = 1)
  TSdecomp.WCOL.SeasonalAdj.diff <- diff(TSdecomp.WCOL.SeasonalAdj, lag = 4)
  ur.kpss(TSdecomp.WCOL.SeasonalAdj.diff) %>% summary() # stationary
  plot(TSdecomp.WCOL.SeasonalAdj.diff, type = "l") # increasing variance -> log
  
  Box.test(TSdecomp.WCOL.SeasonalAdj.diff, type ="Ljung-Box") # p = 659 no autocorrelation
  
# best ARIMA model
fit <- auto.arima(TSdecomp.WCOL.SeasonalAdj.diff, stationary = T, seasonal = F) # p = 2, d = 0, q = 2  
plot(forecast(fit, 8))

# fit ARIMA model
fit <- arima(TS.WCOL, order = c(3,1,0), seasonal = list(order = c(2,0,0), period = 4))
plot(forecast(fit, 8))



library('tseries')
test <- ts(wdat[sex == "Women" & education == "LEHS", .(GPD)], frequency=4)
Decomp <- decompose(test)
plot(Decomp)




 ## OLD ## 

# ----------------------------------------------------------------
# OUTCOME 3: HED
# ----------------------------------------------------------------

# Missings
count(data[is.na(data$hed)==T]) # n = 678
count(data[is.na(data$hed)==T]) / count(data[is.na(data$hed)==F]) # 0.76% 

# HED, available participants by group
ggplot(data = data[is.na(data$hed) == F], aes(x = QYEAR)) + geom_histogram(stat = "count") + geom_hline(yintercept = c(50, 100)) + 
  facet_grid(rows = vars(as.factor(education)), cols = vars(as.factor(sex)), scales = "free_y") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90), strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/Indiana/', DATE, '_HIST_BY_GROUP (HED).png'), dpi=300, width = 12, height = 7)

# SEX

# HED prevalence weighted (simply weights, not accounting for sampling design)
wdat.hed.sex <- data[is.na(data$hed)==F] %>% 
  mutate(hed.di = ifelse(hed == 0, 0, ifelse(hed > 0, 1, NA))) %>%
  group_by(QYEAR, sex) %>%
  summarise(HED = wtd.mean(hed.di, final_sample_weight),
            HED.LB = wtd.lb(hed.di, final_sample_weight),
            HED.UB = wtd.ub(hed.di, final_sample_weight),
            n = n(),
            INT_QYEAR = mean(INT_QYEAR), unemp.rate = mean(unemp.rate)) %>% as.data.table()

wdat.hed.sex.long <- reshape(wdat.hed.sex, idvar = c("QYEAR", "sex", "n", "INT_QYEAR"),
                             varying = c("HED", "HED.LB", "HED.UB"), v.name = c("HED"), 
                             times = c("mean", "LB", "UB"), direction = "long")

# GPD distribution weighted
ggplot(data = wdat.hed.sex.long) + geom_line(aes(x = QYEAR, y = HED, color = time)) + geom_vline(aes(xintercept = INT_QYEAR)) +
  scale_x_continuous(breaks = seq(2000, 2020, 1)) + scale_y_continuous(labels = scales::percent) + scale_colour_manual(values = c("grey", "black", "grey")) +
  facet_grid(cols = vars(sex), scales = "free") +
  labs(y = "HED prevalence (weighted)") + theme_bw() + theme(legend.position = "none", strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/Indiana/', DATE, '_HEDweighted_BY_SEX.png'), dpi=300, width = 12, height = 7)

# SES

# HED prevalence weighted (simply weights, not accounting for sampling design)
wdat.hed.ses <- data[is.na(data$hed)==F] %>% 
  mutate(hed.di = ifelse(hed == 0, 0, ifelse(hed > 0, 1, NA))) %>%
  group_by(QYEAR, education) %>%
  summarise(HED = wtd.mean(hed.di, final_sample_weight),
            HED.LB = wtd.lb(hed.di, final_sample_weight),
            HED.UB = wtd.ub(hed.di, final_sample_weight),
            n = n(),
            INT_QYEAR = mean(INT_QYEAR), unemp.rate = mean(unemp.rate)) %>% as.data.table()

wdat.hed.ses.long <- reshape(wdat.hed.ses, idvar = c("QYEAR", "education", "n", "INT_QYEAR"),
                             varying = c("HED", "HED.LB", "HED.UB"), v.name = c("HED"), 
                             times = c("mean", "LB", "UB"), direction = "long")

# GPD distribution weighted
ggplot(data = wdat.hed.ses.long) + geom_line(aes(x = QYEAR, y = HED, color = time)) + geom_vline(aes(xintercept = INT_QYEAR)) +
  scale_x_continuous(breaks = seq(2000, 2020, 1)) + scale_y_continuous(labels = scales::percent) + scale_colour_manual(values = c("grey", "black", "grey")) +
  facet_grid(rows = vars(as.factor(education)), scales = "free") +
  labs(y = "HED prevalence (weighted)") + theme_bw() + theme(legend.position = "none", strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/Indiana/', DATE, '_HEDweighted_BY_SES.png'), dpi=300, width = 12, height = 7)

# GROUP: SEX x SES

# HED prevalence weighted (simply weights, not accounting for sampling design)
wdat.hed <- data[is.na(data$hed)==F] %>% 
  mutate(hed.di = ifelse(hed == 0, 0, ifelse(hed > 0, 1, NA))) %>%
  group_by(QYEAR, sex, education) %>%
  summarise(HED = wtd.mean(hed.di, final_sample_weight),
            HED.LB = wtd.lb(hed.di, final_sample_weight),
            HED.UB = wtd.ub(hed.di, final_sample_weight),
            n = n(),
            INT_QYEAR = mean(INT_QYEAR), unemp.rate = mean(unemp.rate)) %>% as.data.table()

wdat.hed.long <- reshape(wdat.hed, idvar = c("QYEAR", "sex", "education", "n", "INT_QYEAR"),
                         varying = c("HED", "HED.LB", "HED.UB"), v.name = c("HED"), 
                         times = c("mean", "LB", "UB"), direction = "long")

# GPD distribution weighted
ggplot(data = wdat.hed.long) + geom_line(aes(x = QYEAR, y = HED, color = time)) + geom_vline(aes(xintercept = INT_QYEAR)) +
  scale_x_continuous(breaks = seq(2000, 2020, 1)) + scale_y_continuous(labels = scales::percent) + scale_colour_manual(values = c("grey", "black", "grey")) +
  facet_grid(rows = vars(as.factor(education)), cols = vars(sex), scales = "free") +
  labs(y = "HED prevalence (weighted)") + theme_bw() + theme(legend.position = "none", strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/Indiana/', DATE, '_HEDweighted_BY_GROUP.png'), dpi=300, width = 12, height = 7)
