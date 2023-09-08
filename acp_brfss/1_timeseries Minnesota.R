# ----------------------------------------------------------------
# ----------------------------------------------------------------
## Project: SIMAH  
## Title: (partial) Sunday sales ban  
## State: Minnesota
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
library(astsa)

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
datMIN <- datBRFSS[State %like% "Minnesota"]

datUNEMP <- data.table(read.csv("acp_brfss/data/20230228_MINNESOTAunempPREP.csv"))
datGDP <- read.xlsx("acp_brfss/data/20230228_MINNESOTA_GDP.xlsx", sheet = 1, startRow = 1) %>% select("year","gdp") %>% data.table()
datPOP <- data.table(read.csv("acp_brfss/data/20230228_MINNESOTApopPREP.csv"))

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# PREPARE DATA
# ----------------------------------------------------------------

# subgroups
datMIN[, education := factor(education_summary, levels = c("LEHS", "SomeC", "College"))]

# quarters
datMIN[, Q := ifelse(surveymonth %like% "Feb|Mar|Apr", 125, ifelse(surveymonth %like% "May|Jun|Jul", 375,
                                                               ifelse(surveymonth %like% "Aug|Sep|Oct", 625, ifelse(surveymonth %like% "Nov|Dec|Jan", 875, NA))))]
datMIN[, QYEAR := as.numeric(ifelse(surveymonth %like% "Jan", paste0(YEAR-1, ".", Q), paste0(YEAR, ".", Q)))]

  ggplot(data = datMIN, aes(x = QYEAR)) + geom_histogram(stat = "count") + theme(axis.text.x = element_text(angle = 90))
  # drop data < 2000 (too few observations)
  # drop 2020.375 onwards

datMIN <- copy(datMIN[QYEAR > 2000.000])
datMIN <- copy(datMIN[QYEAR < 2020.375])

  ggplot(data = datMIN, aes(x = QYEAR)) + geom_histogram(stat = "count") + theme(axis.text.x = element_text(angle = 90))

# intervention: 07/01/2017
datMIN[, INT_YEAR := 2017]
datMIN[, INT_QYEAR := 2017.625]

# group
datMIN[, sex := ifelse(sex_recode == "Female", "Women", ifelse(sex_recode == "Male", "Men", NA))]
datMIN[, group := paste0(sex, "_", education)]

# cap estimates >200 GPD
summary(datMIN$gramsperday_orig)
datMIN[gramsperday_orig > 200] # 142 observations
summary(datMIN[gramsperday_upshifted > 200]$gramsperday_orig)
datMIN[gramsperday_upshifted > 200] # 399 observations
datMIN[, GPD_upshifted_capped := ifelse(gramsperday_upshifted > 200, 200, gramsperday_upshifted)]
datMIN[, GPD_orig_capped := ifelse(gramsperday_orig > 200, 200, gramsperday_orig)]

# drop abstainers
#data <- datMIN[drinkingstatus_upshifted == 1]
data <- datMIN[drinkingstatus_orig == 1]

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
  theme_bw() + theme(axis.text.x = element_text(angle = 90), strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/Minnesota/', DATE, '_HIST_BY_GROUP.png'), dpi=300, width = 12, height = 7)

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
#ggsave(paste0('acp_brfss/outputs/figures/Minnesota/', DATE, '_GPDsmooth_BY_GROUP.png'), dpi=300, width = 12, height = 7)

# SEX

# GPD weighted (simply weights, not accounting for sampling design)
wdat.gpd.sex <- data %>% 
  group_by(QYEAR, sex) %>%
  summarise(GPD = wtd.mean(GPD_orig_capped, final_sample_weight),
            GPD.LB = wtd.lb(GPD_orig_capped, final_sample_weight),
            GPD.UB = wtd.ub(GPD_orig_capped, final_sample_weight),
            n = n(),
            INT_QYEAR = mean(INT_QYEAR), unemp.rate = mean(unemp.rate), w.unemp.rate = mean(w.unemp.rate), m.unemp.rate = mean(m.unemp.rate),
            gdp = mean(gdp), pop18_24 = mean(pop18_24), w.pop18_24 = mean(w.pop18_24), m.pop18_24 = mean(m.pop18_24)) %>% as.data.table()

wdat.gpd.sex.long <- reshape(wdat.gpd.sex, idvar = c("QYEAR", "sex", "n", "INT_QYEAR"),
                     varying = c("GPD", "GPD.LB", "GPD.UB"), v.name = c("GPD"), 
                     times = c("mean", "LB", "UB"), direction = "long")

# GPD distribution weighted
ggplot(data = wdat.gpd.sex.long) + geom_line(aes(x = QYEAR, y = GPD, color = time)) + geom_vline(aes(xintercept = INT_QYEAR)) +
  scale_x_continuous(breaks = seq(2000, 2020, 1)) + scale_colour_manual(values = c("grey", "black", "grey")) +
  facet_grid(cols = vars(sex), scales = "free") +
  labs(y = "GPD (weighted)") + theme_bw() + theme(legend.position = "none", strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/Minnesota/', DATE, '_GPDweighted_BY_SEX.png'), dpi=300, width = 12, height = 7)

# SES

# GPD weighted (simply weights, not accounting for sampling design)
wdat.gpd.ses <- data %>% 
  group_by(QYEAR, education) %>%
  summarise(GPD = wtd.mean(GPD_orig_capped, final_sample_weight),
            GPD.LB = wtd.lb(GPD_orig_capped, final_sample_weight),
            GPD.UB = wtd.ub(GPD_orig_capped, final_sample_weight),
            n = n(),
            INT_QYEAR = mean(INT_QYEAR), unemp.rate = mean(unemp.rate), w.unemp.rate = mean(w.unemp.rate), m.unemp.rate = mean(m.unemp.rate),
            gdp = mean(gdp), pop18_24 = mean(pop18_24), w.pop18_24 = mean(w.pop18_24), m.pop18_24 = mean(m.pop18_24)) %>% as.data.table()

wdat.gpd.ses.long <- reshape(wdat.gpd.ses, idvar = c("QYEAR", "education", "n", "INT_QYEAR"),
                     varying = c("GPD", "GPD.LB", "GPD.UB"), v.name = c("GPD"), 
                     times = c("mean", "LB", "UB"), direction = "long")

# GPD distribution weighted
ggplot(data = wdat.gpd.ses.long) + geom_line(aes(x = QYEAR, y = GPD, color = time)) + geom_vline(aes(xintercept = INT_QYEAR)) +
  scale_x_continuous(breaks = seq(2000, 2020, 1)) + scale_colour_manual(values = c("grey", "black", "grey")) +
  facet_grid(rows = vars(as.factor(education)), scales = "free") +
  labs(y = "GPD (weighted)") + theme_bw() + theme(legend.position = "none", strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/Minnesota/', DATE, '_GPDweighted_BY_SES.png'), dpi=300, width = 12, height = 7)

# GROUP: SEX x SES

# GPD weighted (simply weights, not accounting for sampling design)
wdat.gpd <- data %>% 
  group_by(QYEAR, sex, education) %>%
  summarise(GPD = wtd.mean(GPD_orig_capped, final_sample_weight),
            GPD.LB = wtd.lb(GPD_orig_capped, final_sample_weight),
            GPD.UB = wtd.ub(GPD_orig_capped, final_sample_weight),
            n = n(),
            INT_QYEAR = mean(INT_QYEAR), unemp.rate = mean(unemp.rate), w.unemp.rate = mean(w.unemp.rate), m.unemp.rate = mean(m.unemp.rate),
            gdp = mean(gdp), pop18_24 = mean(pop18_24), w.pop18_24 = mean(w.pop18_24), m.pop18_24 = mean(m.pop18_24)) %>% as.data.table()

wdat.gpd.long <- reshape(wdat.gpd, idvar = c("QYEAR", "sex", "education", "n", "INT_QYEAR"),
                     varying = c("GPD", "GPD.LB", "GPD.UB"), v.name = c("GPD"), 
                     times = c("mean", "LB", "UB"), direction = "long")

# GPD distribution weighted
ggplot(data = wdat.gpd.long) + geom_line(aes(x = QYEAR, y = GPD, color = time)) + geom_vline(aes(xintercept = INT_QYEAR)) +
  scale_x_continuous(breaks = seq(2000, 2020, 1)) + scale_colour_manual(values = c("grey", "black", "grey")) +
  facet_grid(rows = vars(as.factor(education)), cols = vars(sex), scales = "free") +
  labs(y = "GPD (weighted)") + theme_bw() + theme(legend.position = "none", strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/Minnesota/', DATE, '_GPDweighted_BY_GROUP.png'), dpi=300, width = 12, height = 7)

# ----------------------------------------------------------------
# OUTCOME 2: GPD among category II/III drinkers
# ----------------------------------------------------------------

# subset
sub <- data[sex == "Women" & GPD_orig_capped > 20 | sex == "Men" & GPD_orig_capped > 20,] 
  # 20 / 20: n = 15157
  # 14 / 28: n = 

# available participants by group -> by SEX and SES not possible due to low n
ggplot(data = sub, aes(x = QYEAR)) + geom_histogram(stat = "count") + geom_hline(yintercept = c(50, 100)) + 
  facet_grid(rows = vars(as.factor(education)), cols = vars(as.factor(sex)), scales = "free_y") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90), strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/Minnesota/', DATE, '_HIST_BY_GROUP (SUBSAMPLE).png'), dpi=300, width = 12, height = 7)

# available participants by group -> by SES only 
ggplot(data = sub, aes(x = QYEAR)) + geom_histogram(stat = "count") + geom_hline(yintercept = c(50, 100)) + 
  facet_grid(rows = vars(as.factor(education)), scales = "free_y") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90), strip.background = element_rect(fill="white")) 

# reduce SES groups to two ? different level of consumption across groups
wtd.mean(sub[education == "LEHS"]$GPD_orig_capped) # 45
wtd.mean(sub[education == "SomeC"]$GPD_orig_capped) # 40g
wtd.mean(sub[education == "College"]$GPD_orig_capped) # 35g

sub[, education := ifelse(education %like% "LEHS|SomeC", "LEHS/SomeC", ifelse(education %like% "College", "College", NA))]

# SEX

# GPD weighted (simply weights, not accounting for sampling design)
sub.sex <- sub %>% 
  group_by(QYEAR, sex) %>%
  summarise(GPD = wtd.mean(GPD_orig_capped, final_sample_weight),
            GPD.LB = wtd.lb(GPD_orig_capped, final_sample_weight),
            GPD.UB = wtd.ub(GPD_orig_capped, final_sample_weight),
            n = n(),
            INT_QYEAR = mean(INT_QYEAR), unemp.rate = mean(unemp.rate), w.unemp.rate = mean(w.unemp.rate), m.unemp.rate = mean(m.unemp.rate),
            gdp = mean(gdp), pop18_24 = mean(pop18_24), w.pop18_24 = mean(w.pop18_24), m.pop18_24 = mean(m.pop18_24)) %>% as.data.table()

sub.sex.long <- reshape(sub.sex, idvar = c("QYEAR", "sex", "n", "INT_QYEAR"),
                             varying = c("GPD", "GPD.LB", "GPD.UB"), v.name = c("GPD"), 
                             times = c("mean", "LB", "UB"), direction = "long")

# GPD distribution weighted
ggplot(data = sub.sex.long) + geom_line(aes(x = QYEAR, y = GPD, color = time)) + geom_vline(aes(xintercept = INT_QYEAR)) +
  scale_x_continuous(breaks = seq(2000, 2020, 1)) + scale_colour_manual(values = c("grey", "black", "grey")) +
  facet_grid(cols = vars(sex), scales = "free") +
  labs(y = "GPD (weighted)") + theme_bw() + theme(legend.position = "none", strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/Minnesota/', DATE, '_GPDweighted_BY_SEX (SUBSAMPLE).png'), dpi=300, width = 12, height = 7)

# SES

# GPD weighted (simply weights, not accounting for sampling design)
sub.ses <- sub %>% 
  group_by(QYEAR, education) %>%
  summarise(GPD = wtd.mean(GPD_orig_capped, final_sample_weight),
            GPD.LB = wtd.lb(GPD_orig_capped, final_sample_weight),
            GPD.UB = wtd.ub(GPD_orig_capped, final_sample_weight),
            n = n(),
            INT_QYEAR = mean(INT_QYEAR), unemp.rate = mean(unemp.rate), w.unemp.rate = mean(w.unemp.rate), m.unemp.rate = mean(m.unemp.rate),
            gdp = mean(gdp), pop18_24 = mean(pop18_24), w.pop18_24 = mean(w.pop18_24), m.pop18_24 = mean(m.pop18_24)) %>% as.data.table()

sub.ses.long <- reshape(sub.ses, idvar = c("QYEAR", "education", "n", "INT_QYEAR"),
                             varying = c("GPD", "GPD.LB", "GPD.UB"), v.name = c("GPD"), 
                             times = c("mean", "LB", "UB"), direction = "long")

# GPD distribution weighted
ggplot(data = sub.ses.long) + geom_line(aes(x = QYEAR, y = GPD, color = time)) + geom_vline(aes(xintercept = INT_QYEAR)) +
  scale_x_continuous(breaks = seq(2000, 2020, 1)) + scale_colour_manual(values = c("grey", "black", "grey")) +
  facet_grid(rows = vars(as.factor(education)), scales = "free") +
  labs(y = "GPD (weighted)") + theme_bw() + theme(legend.position = "none", strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/Minnesota/', DATE, '_GPDweighted_BY_SES (SUBSAMPLE).png'), dpi=300, width = 12, height = 7)

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
cor.test(wdat.gpd.sex[sex == "Women"]$GPD, log(wdat.gpd.sex[sex == "Women"]$w.unemp.rate), method = "pearson") # p = 0.1
cor.test(wdat.gpd.sex[sex == "Women"]$GPD, log(wdat.gpd.sex[sex == "Women"]$w.unemp.rate), method = "spearman") # p = 0.04
unemp <- log(wdat.gpd.sex[sex == "Women"]$unemp)

# % population 18-24
ggplot(wdat.gpd.sex[sex == "Women"], aes(x = w.pop18_24)) + geom_histogram()
cor.test(wdat.gpd.sex[sex == "Women"]$GPD, wdat.gpd.sex[sex == "Women"]$w.pop18_24, method = "pearson") # p < .001
cor.test(wdat.gpd.sex[sex == "Women"]$GPD, wdat.gpd.sex[sex == "Women"]$w.pop18_24, method = "spearman") # p < .001
pop18_24 <- wdat.gpd.sex[sex == "Women"]$w.pop18_24

# BASELINE MODEL

TS.GPD.W <- ts(wdat.gpd.sex[sex == "Women", .(GPD)], frequency = 4, start = c(2000,1))
plot.ts(TS.GPD.W) + abline(v = 2017.5, lty = "dashed")

# NORMALITY
shapiro.test(TS.GPD.W) # p = .021
car::qqPlot(TS.GPD.W)

shapiro.test(log(TS.GPD.W)) # p = .024
car::qqPlot(log(TS.GPD.W))

hist(TS.GPD.W) # keep non-log
hist(log(TS.GPD.W))

# add intervention

# step increase
step <- as.numeric(as.yearqtr(time(TS.GPD.W)) >= "2017 Q3")
step 

# slope increase
slope <- c(rep(0,70), seq(1,11,1)) 
cbind(step, slope)

# tax policy change
tax_step <- as.numeric(as.yearqtr(time(TS.GPD.W)) >= "2006 Q1")

# identify ARIMA model
# acf2(TS.GPD.W)
model1 <- auto.arima(TS.GPD.W, xreg = cbind(step, slope, tax_step, unemp), stepwise = FALSE)
checkresiduals(model1) # some autocorrelation left
model1 #ARIMA(0,1,2)

# COUNTREFACTUAL MODEL

# use arima model without post-intervention period
model2 <- arima(window(TS.GPD.W, xreg = c(tax_step, unemp), end = c(2017,2)), order = c(0, 1, 2))
checkresiduals(model2)  

# forecast based on the counterfactual model
fc <- forecast(model2, h = 11)

# make forecast a timeseries
TS.fc <- ts(as.numeric(fc$mean), start = c(2017,3), frequency = 4)

# COMBINE AND TEST INTERVENTION EFFECT

# combine both timeseries
TS.GPD.W.comb <- ts.union(TS.GPD.W, TS.fc)
TS.GPD.W.comb

# plot combined data
png(file = paste0('acp_brfss/outputs/figures/Minnesota/', DATE, '_TS_GPD_WOMEN.png'), width = 800, height = 450)
plot.ts(TS.GPD.W.comb, plot.type = "single", 
        col = c("blue", "red"), xlab = "QYEAR", ylab = "Grams pure alcohol per day (WOMEN)", lty = c("solid", "dashed")) +
  abline(v = 2017.5, lty = "dashed")
dev.off()

# quantify intervention effect
coeftest(model1) #n.s.
confint(model1)

# counts
min(data[sex == "Women"] %>% count(QYEAR))
max(data[sex == "Women"] %>% count(QYEAR))

# ----------------------------------------------------------------
# OUTCOME: GPD
# ITS - MEN
# https://nicolarighetti.github.io/Time-Series-Analysis-With-R/intervention-analysis.html
# ----------------------------------------------------------------

# COVARIATES

# unemployment
ggplot(wdat.gpd.sex[sex == "Men"], aes(x = log(m.unemp.rate))) + geom_histogram()
cor.test(wdat.gpd.sex[sex == "Men"]$GPD, log(wdat.gpd.sex[sex == "Men"]$m.unemp.rate), method = "pearson") # n.s.
cor.test(wdat.gpd.sex[sex == "Men"]$GPD, log(wdat.gpd.sex[sex == "Men"]$m.unemp.rate), method = "spearman") # n.s.

# % population 18-24
ggplot(wdat.gpd.sex[sex == "Men"], aes(x = m.pop18_24)) + geom_histogram()
cor.test(wdat.gpd.sex[sex == "Men"]$GPD, wdat.gpd.sex[sex == "Men"]$m.pop18_24, method = "pearson") # p < .001
cor.test(wdat.gpd.sex[sex == "Men"]$GPD, wdat.gpd.sex[sex == "Men"]$m.pop18_24, method = "spearman") # p < .001
pop18_24 <- wdat.gpd.sex[sex == "Men" & QYEAR > 2002]$m.pop18_24

# BASELINE MODEL

TS.GPD.M <- ts(wdat.gpd.sex[sex == "Men", .(GPD)], frequency = 4, start = c(2000,1))
plot.ts(TS.GPD.M) + abline(v = 2017.5, lty = "dashed")

# much lower consumption in 2000, exclude data before 2001

TS.GPD.M <- ts(wdat.gpd.sex[sex == "Men" & QYEAR > 2002, .(GPD)], frequency = 4, start = c(2002,1))
plot.ts(TS.GPD.M) + abline(v = 2017.5, lty = "dashed")

# NORMALITY
shapiro.test(TS.GPD.M) # p = .066
car::qqPlot(TS.GPD.M)

# add intervention for shorter period

# step increase
step2 <- as.numeric(as.yearqtr(time(TS.GPD.M)) >= "2017 Q3")
step2

# slope increase
slope2 <- c(rep(0,62), seq(1,11,1)) 
cbind(step2, slope2)

# tax policy change
tax_step2 <- as.numeric(as.yearqtr(time(TS.GPD.M)) >= "2006 Q1")

# identify ARIMA model
# acf2(TS.GPD.M)
model1 <- auto.arima(TS.GPD.M, xreg = cbind(step2, slope2, tax_step2, pop18_24), stepwise = FALSE)
checkresiduals(model1)  
model1 #ARIMA(0,0,0)(2,0,0)[4]
car::qqPlot(model1$residuals) # critical

# COUNTREFACTUAL MODEL

# use arima model without post-intervention period
model2 <- arima(window(TS.GPD.M, xreg = tax_step2, end = c(2017,2)), order = c(0, 0, 0), seasonal = list(order = c(2, 0, 0), period = 4))
checkresiduals(model2)  
car::qqPlot(model2$residuals) # ok

# forecast based on the counterfactual model
fc <- forecast(model2, h = 11)

# make forecast a timeseries
TS.fc <- ts(as.numeric(fc$mean), start = c(2017,3), frequency = 4)

# COMBINE AND TEST INTERVENTION EFFECT

# combine both timeseries
TS.GPD.M.comb <- ts.union(TS.GPD.M, TS.fc)
TS.GPD.M.comb

# plot combined data
png(file = paste0('acp_brfss/outputs/figures/Minnesota/', DATE, '_TS_GPD_MEN.png'), width = 800, height = 450)
plot.ts(TS.GPD.M.comb, plot.type = "single", 
        col = c("blue", "red"), xlab = "QYEAR", ylab = "Grams pure alcohol per day (MEN)", lty = c("solid", "dashed")) +
  abline(v = 2017.5, lty = "dashed")
dev.off()

# quantify intervention effect
coeftest(model1) #n.s.
confint(model1)

# counts
min(data[sex == "Men"] %>% count(QYEAR))
max(data[sex == "Men"] %>% count(QYEAR))

# ----------------------------------------------------------------
# OUTCOME: GPD
# ITS - LEHS
# ----------------------------------------------------------------

# COVARIATES

# unemployment
cor.test(wdat.gpd.ses[education == "LEHS"]$GPD, wdat.gpd.ses[education == "LEHS"]$unemp.rate, method = "pearson") # p = 0.03
unemp <- wdat.gpd.ses[education == "LEHS"]$unemp.rate

# BASELINE MODEL

TS.GPD.LEHS <- ts(wdat.gpd.ses[education == "LEHS", .(GPD)], frequency = 4, start = c(2000,1))
plot.ts(TS.GPD.LEHS) + abline(v = 2017.5, lty = "dashed")

# NORMALITY
shapiro.test(TS.GPD.LEHS) # p = .754
car::qqPlot(TS.GPD.LEHS)

# identify ARIMA model
model1 <- auto.arima(TS.GPD.LEHS, xreg = cbind(step, slope, tax_step, unemp), stepwise = FALSE)
checkresiduals(model1)  

#search manually since ACF is not look ok
acf2(TS.GPD.LEHS)
model1a <-arima(TS.GPD.LEHS, xreg = cbind(step, slope, tax_step, unemp), order = c(0, 0, 10))
checkresiduals(model1a) # slightly improved AIC = 355.66 (vs. AIC = 359)
car::qqPlot(model1a$residuals) # critical

# COUNTREFACTUAL MODEL

# use arima model without post-intervention period
model2 <- arima(window(TS.GPD.LEHS, xreg = cbind(tax_step, unemp), end = c(2017,2)), order = c(0, 0, 10))
checkresiduals(model2)  
car::qqPlot(model2$residuals) # ok

# forecast based on the counterfactual model
fc <- forecast(model2, h = 11)

# make forecast a timeseries
TS.fc <- ts(as.numeric(fc$mean), start = c(2017,3), frequency = 4)

# COMBINE AND TEST INTERVENTION EFFECT

# combine both timeseries
TS.GPD.LEHS.comb <- ts.union(TS.GPD.LEHS, TS.fc)
TS.GPD.LEHS.comb

# plot combined data
png(file = paste0('acp_brfss/outputs/figures/Minnesota/', DATE, '_TS_GPD_LEHS.png'), width = 800, height = 450)
plot.ts(TS.GPD.LEHS.comb, plot.type = "single", 
        col = c("blue", "red"), xlab = "QYEAR", ylab = "Grams pure alcohol per day (LEHS)", lty = c("solid", "dashed")) +
  abline(v = 2017.5, lty = "dashed")
dev.off()

# quantify intervention effect
coeftest(model1a) #n.s.
confint(model1a)

# counts
min(data[education == "LEHS"] %>% count(QYEAR))
max(data[education == "LEHS"] %>% count(QYEAR))

# ----------------------------------------------------------------
# OUTCOME: GPD
# ITS - SomeC
# ----------------------------------------------------------------

# COVARIATES

# unemployment
cor.test(wdat.gpd.ses[education == "SomeC"]$GPD, wdat.gpd.ses[education == "SomeC"]$unemp.rate, method = "pearson") # n.s.

# BASELINE MODEL

TS.GPD.SomeC <- ts(wdat.gpd.ses[education == "SomeC", .(GPD)], frequency = 4, start = c(2000,1))
plot.ts(TS.GPD.SomeC) + abline(v = 2017.5, lty = "dashed")

# NORMALITY
shapiro.test(TS.GPD.SomeC) # p = .980
car::qqPlot(TS.GPD.SomeC)

# identify ARIMA model
acf2(TS.GPD.SomeC)
model1 <- auto.arima(TS.GPD.SomeC, xreg = cbind(step, slope, tax_step), stepwise = FALSE)
checkresiduals(model1)  
model1 #ARIMA(0,0,0)
car::qqPlot(model1$residuals) # ok

# COUNTREFACTUAL MODEL

# use arima model without post-intervention period
model2 <- arima(window(TS.GPD.SomeC, xreg = tax_step, end = c(2017,2)), order = c(0, 0, 0))
checkresiduals(model2)  
car::qqPlot(model2$residuals) # ok

# forecast based on the counterfactual model
fc <- forecast(model2, h = 11)

# make forecast a timeseries
TS.fc <- ts(as.numeric(fc$mean), start = c(2017,3), frequency = 4)

# COMBINE AND TEST INTERVENTION EFFECT

# combine both timeseries
TS.GPD.SomeC.comb <- ts.union(TS.GPD.SomeC, TS.fc)
TS.GPD.SomeC.comb

# plot combined data
png(file = paste0('acp_brfss/outputs/figures/Minnesota/', DATE, '_TS_GPD_SomeC.png'), width = 800, height = 450)
plot.ts(TS.GPD.SomeC.comb, plot.type = "single", 
        col = c("blue", "red"), xlab = "QYEAR", ylab = "Grams pure alcohol per day (SomeC)", lty = c("solid", "dashed")) +
  abline(v = 2017.5, lty = "dashed")
dev.off()

# quantify intervention effect
coeftest(model1) # n.s.
confint(model1)

# counts
min(data[education == "SomeC"] %>% count(QYEAR))
max(data[education == "SomeC"] %>% count(QYEAR))

# ----------------------------------------------------------------
# OUTCOME: GPD
# ITS - College
# ----------------------------------------------------------------

# COVARIATES

# unemployment
cor.test(wdat.gpd.ses[education == "College"]$GPD, wdat.gpd.ses[education == "College"]$unemp.rate, method = "pearson") # n.s.

# BASELINE MODEL

TS.GPD.College <- ts(wdat.gpd.ses[education == "College", .(GPD)], frequency = 4, start = c(2000,1))
plot.ts(TS.GPD.College) + abline(v = 2017.5, lty = "dashed")

# much lower consumption in 2000, exclude data before 2001

TS.GPD.College <- ts(wdat.gpd.ses[education == "College" & QYEAR > 2001, .(GPD)], frequency = 4, start = c(2001,1))
plot.ts(TS.GPD.College) + abline(v = 2017.5, lty = "dashed")

# NORMALITY
shapiro.test(TS.GPD.College) # p = .581
car::qqPlot(TS.GPD.College)

# identify ARIMA model
acf2(TS.GPD.College)
model1 <- auto.arima(TS.GPD.College, xreg = cbind(step2, slope2, tax_step2), stepwise = FALSE)
checkresiduals(model1) 

#search manually since ACF is not look ok
model1a <-arima(TS.GPD.College, xreg = cbind(step2, slope2, tax_step2), order = c(0, 0, 4))
checkresiduals(model1a) # slightly improved AIC = 355.66 (vs. AIC = 359)
car::qqPlot(model1a$residuals) # critical
  #ARIMA(0,0,4)

# COUNTREFACTUAL MODEL

# use arima model without post-intervention period
model2 <- arima(window(TS.GPD.College, xreg = tax_step2, end = c(2017,2)), order = c(0, 0, 4))
checkresiduals(model2)  

# forecast based on the counterfactual model
fc <- forecast(model2, h = 11)

# make forecast a timeseries
TS.fc <- ts(as.numeric(fc$mean), start = c(2017,3), frequency = 4)

# COMBINE AND TEST INTERVENTION EFFECT

# combine both timeseries
TS.GPD.College.comb <- ts.union(TS.GPD.College, TS.fc)
TS.GPD.College.comb

# plot combined data
png(file = paste0('acp_brfss/outputs/figures/Minnesota/', DATE, '_TS_GPD_College.png'), width = 800, height = 450)
plot.ts(TS.GPD.College.comb, plot.type = "single", 
        col = c("blue", "red"), xlab = "QYEAR", ylab = "Grams pure alcohol per day (College)", lty = c("solid", "dashed")) +
  abline(v = 2017.5, lty = "dashed")
dev.off()

# quantify intervention effect
coeftest(model1) # n.s.
confint(model1)

# counts
min(data[education == "College"] %>% count(QYEAR))
max(data[education == "College"] %>% count(QYEAR))

# ----------------------------------------------------------------
# OUTCOME: GPD SUBSAMPLE
# ----------------------------------------------------------------

----------------------------------------------------------------
  # OUTCOME: GPD SUBSAMPLE
  # ITS - WOMEN
  # https://nicolarighetti.github.io/Time-Series-Analysis-With-R/intervention-analysis.html
  # ----------------------------------------------------------------

# COVARIATES

# unemployment
cor.test(sub.sex[sex == "Women"]$GPD, sub.sex[sex == "Women"]$unemp.rate, method = "pearson") # n.s.

# BASELINE MODEL

TS.GPD.SUB.W <- ts(sub.sex[sex == "Women", .(GPD)], frequency = 4, start = c(2000,1))
plot.ts(TS.GPD.SUB.W) + abline(v = 2017.5, lty = "dashed")

# NORMALITY
shapiro.test(TS.GPD.SUB.W) # p = .017
car::qqPlot(TS.GPD.SUB.W)

# use log
shapiro.test(log(TS.GPD.SUB.W)) # p = .06

TS.GPD.SUB.W <- log(TS.GPD.SUB.W)

# add intervention

# identify ARIMA model
acf2(TS.GPD.SUB.W)
model1 <- auto.arima(TS.GPD.SUB.W, xreg = cbind(step, slope, tax_step), stepwise = FALSE)
checkresiduals(model1)  
model1 #ARIMA(0,0,0)
car::qqPlot(model1$residuals) # ok

# COUNTREFACTUAL MODEL

# use arima model without post-intervention period
model2 <- arima(window(TS.GPD.SUB.W, xreg = tax_step, end = c(2017,2)), order = c(0,0,0))
checkresiduals(model2)  
car::qqPlot(model1$residuals) # ok

# forecast based on the counterfactual model
fc <- forecast(model2, h = 11)

# make forecast a timeseries
TS.fc <- ts(as.numeric(fc$mean), start = c(2017,3), frequency = 4)

# COMBINE AND TEST INTERVENTION EFFECT

# combine both timeseries
TS.GPD.SUB.W.comb <- ts.union(TS.GPD.SUB.W, TS.fc)
TS.GPD.SUB.W.comb

# plot combined data
png(file = paste0('acp_brfss/outputs/figures/Minnesota/', DATE, '_TS_GPD_WOMEN (SUBSAMPLE).png'), width = 800, height = 450)
plot.ts(TS.GPD.SUB.W.comb, plot.type = "single", 
        col = c("blue", "red"), xlab = "QYEAR", ylab = "Grams pure alcohol per day (log, WOMEN SUBSAMPLE)", lty = c("solid", "dashed")) +
  abline(v = 2017.5, lty = "dashed")
dev.off()

# quantify intervention effect
coeftest(model1) #n.s.
confint(model1)

# counts
min(sub[sex == "Women"] %>% count(QYEAR))
max(sub[sex == "Women"] %>% count(QYEAR))
sub[sex == "Women"] %>% count(QYEAR) %>% count(n < 50)

# ----------------------------------------------------------------
# OUTCOME: GPD SUBSAMPLE
# ITS - MEN
# https://nicolarighetti.github.io/Time-Series-Analysis-With-R/intervention-analysis.html
# ----------------------------------------------------------------

# COVARIATES

# unemployment
cor.test(sub.sex[sex == "Men"]$GPD, sub.sex[sex == "Men"]$unemp.rate, method = "pearson") # n.s.

# BASELINE MODEL

TS.GPD.SUB.M <- ts(sub.sex[sex == "Men", .(GPD)], frequency = 4, start = c(2000,1))
plot.ts(TS.GPD.SUB.M) + abline(v = 2017.5, lty = "dashed")

# NORMALITY
shapiro.test(TS.GPD.SUB.M) # p = .829
car::qqPlot(TS.GPD.SUB.M)

# identify ARIMA model
acf2(TS.GPD.SUB.M)
model1 <- auto.arima(TS.GPD.SUB.M, xreg = cbind(step, slope, tax_step), stepwise = FALSE)
checkresiduals(model1)  
model1 #ARIMA(0,0,0)
car::qqPlot(model1$residuals) # ok

# COUNTREFACTUAL MODEL

# use arima model without post-intervention period
model2 <- arima(window(TS.GPD.SUB.M, xreg = tax_step, end = c(2017,2)), order = c(0, 0, 0))
checkresiduals(model2)  
car::qqPlot(model2$residuals) # ok

# forecast based on the counterfactual model
fc <- forecast(model2, h = 11)

# make forecast a timeseries
TS.fc <- ts(as.numeric(fc$mean), start = c(2017,3), frequency = 4)

# COMBINE AND TEST INTERVENTION EFFECT

# combine both timeseries
TS.GPD.SUB.M.comb <- ts.union(TS.GPD.SUB.M, TS.fc)
TS.GPD.SUB.M.comb

# plot combined data
png(file = paste0('acp_brfss/outputs/figures/Minnesota/', DATE, '_TS_GPD_MEN (SUBSAMPLE).png'), width = 800, height = 450)
plot.ts(TS.GPD.SUB.M.comb, plot.type = "single", 
        col = c("blue", "red"), xlab = "QYEAR", ylab = "Grams pure alcohol per day (MEN SUBSAMPLE)", lty = c("solid", "dashed")) +
  abline(v = 2017.5, lty = "dashed")
dev.off()

# quantify intervention effect
coeftest(model1) #n.s.
confint(model1)

# counts
min(sub[sex == "Men"] %>% count(QYEAR))
max(sub[sex == "Men"] %>% count(QYEAR))
sub[sex == "Men"] %>% count(QYEAR) %>% count(n < 50)


# ----------------------------------------------------------------
# OUTCOME: GPD SUBSAMPLE
# ITS - LEHS
# https://nicolarighetti.github.io/Time-Series-Analysis-With-R/intervention-analysis.html
# ----------------------------------------------------------------

# COVARIATES

# unemployment
cor.test(sub.ses[education == "LEHS"]$GPD, sub.ses[education == "LEHS"]$unemp.rate, method = "pearson") # n.s.

# BASELINE MODEL

TS.GPD.SUB.LEHS <- ts(sub.ses[education == "LEHS", .(GPD)], frequency = 4, start = c(2000,1))
plot.ts(TS.GPD.SUB.LEHS) + abline(v = 2017.5, lty = "dashed")

# NORMALITY
shapiro.test(TS.GPD.SUB.LEHS) # p = .829
car::qqPlot(TS.GPD.SUB.LEHS)

# identify ARIMA model
acf2(TS.GPD.SUB.LEHS)
model1 <- auto.arima(TS.GPD.SUB.LEHS, xreg = cbind(step, slope, tax_step), stepwise = FALSE)
checkresiduals(model1)  

#search manually since ACF is not look ok
model1a <-arima(TS.GPD.SUB.LEHS, xreg = cbind(step, slope, tax_step), order = c(0, 0, 14))
checkresiduals(model1a) # slightly improved AIC = 552 (vs. AIC = 564) BUT error variance decrease over time!
car::qqPlot(model1a$residuals) # ok

# COUNTREFACTUAL MODEL

# use arima model without post-intervention period
model2 <- arima(window(TS.GPD.SUB.LEHS, xreg = tax_step, end = c(2017,2)), order = c(0, 0, 14))
checkresiduals(model2)  
car::qqPlot(model2$residuals) # ok

# forecast based on the counterfactual model
fc <- forecast(model2, h = 11)

# make forecast a timeseries
TS.fc <- ts(as.numeric(fc$mean), start = c(2017,3), frequency = 4)

# COMBINE AND TEST INTERVENTION EFFECT

# combine both timeseries
TS.GPD.SUB.LEHS.comb <- ts.union(TS.GPD.SUB.LEHS, TS.fc)
TS.GPD.SUB.LEHS.comb

# plot combined data
png(file = paste0('acp_brfss/outputs/figures/Minnesota/', DATE, '_TS_GPD_LEHS (SUBSAMPLE).png'), width = 800, height = 450)
plot.ts(TS.GPD.SUB.LEHS.comb, plot.type = "single", 
        col = c("blue", "red"), xlab = "QYEAR", ylab = "Grams pure alcohol per day (LEHS SUBSAMPLE)", lty = c("solid", "dashed")) +
  abline(v = 2017.5, lty = "dashed")
dev.off()

# quantify intervention effect
coeftest(model1a) #n.s.
confint(model1a)

# counts
min(sub[education == "LEHS"] %>% count(QYEAR))
max(sub[education == "LEHS"] %>% count(QYEAR))
sub[education == "LEHS"] %>% count(QYEAR) %>% count(n < 50)



## OLD ##

# ----------------------------------------------------------------
# OUTCOME 3: HED
# ----------------------------------------------------------------

# Missings
count(data[is.na(data$hed)==T]) # n = 1158
count(data[is.na(data$hed)==T]) / count(data[is.na(data$hed)==F]) # 0.86% 

# HED, available participants by group
ggplot(data = data[is.na(data$hed) == F], aes(x = QYEAR)) + geom_histogram(stat = "count") + geom_hline(yintercept = c(50, 100)) + 
  facet_grid(rows = vars(as.factor(education)), cols = vars(as.factor(sex)), scales = "free_y") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90), strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/Minnesota/', DATE, '_HIST_BY_GROUP (HED).png'), dpi=300, width = 12, height = 7)

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
  labs(y = "HED prevalence (weighted)") + theme_bw() + theme(legxreg = tax_step, end.position = "none", strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/Minnesota/', DATE, '_HEDweighted_BY_SEX.png'), dpi=300, width = 12, height = 7)

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
  labs(y = "HED prevalence (weighted)") + theme_bw() + theme(legxreg = tax_step, end.position = "none", strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/Minnesota/', DATE, '_HEDweighted_BY_SES.png'), dpi=300, width = 12, height = 7)

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
  labs(y = "HED prevalence (weighted)") + theme_bw() + theme(legxreg = tax_step, end.position = "none", strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/Minnesota/', DATE, '_HEDweighted_BY_GROUP.png'), dpi=300, width = 12, height = 7)

