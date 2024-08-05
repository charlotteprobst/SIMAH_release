# ----------------------------------------------------------------
# ----------------------------------------------------------------
## Project: SIMAH  
## Title: Sunday sales ban  
## State: Indiana
## Author: Carolin Kilian
## Start Date: 27/06/2023
# ----------------------------------------------------------------
# ----------------------------------------------------------------

# Furhter reading: https://onlinelibrary.wiley.com/doi/10.1111/add.14643 
# ITS general introduction: https://nicolarighetti.github.io/Time-Series-Analysis-With-R/intervention-analysis.html
# Stationarity: https://www.machinelearningplus.com/time-series/kpss-test-for-stationarity/ 
# Autocorrelation: https://towardsdatascience.com/interpreting-acf-and-pacf-plots-for-time-series-forecasting-af0d6db4061c

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
library(forecast)
library(mgcv)

# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# LOAD DATA
# ----------------------------------------------------------------

setwd("/Users/carolinkilian/Desktop/SIMAH_workplace/")
DATE <- 20230704

# prepared data file 
data <- read.csv("acp_brfss/data/20230704_AGEST_MORTALITY_SEX_INDIANA.csv")

# covariates
datUNEMP <- read.csv("acp_brfss/data/20230628_UNEMP_INDIANA.csv")

# combine data
pdat <- left_join(data, datUNEMP)

# split datasets
datMEN <- pdat %>% filter(sex == 1 & QYEAR <= 2020.125)
datWOMEN <- pdat %>% filter(sex == 2 & QYEAR <= 2020.125)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# COVARIATES
# ----------------------------------------------------------------

# distributions
ggplot(pdat, aes(x = unemp.rate)) + geom_histogram()
ggplot(pdat, aes(x = log(unemp.rate))) + geom_histogram()

# include as covariate 
UNEMP <- log(pdat[pdat$QYEAR > 2000 & pdat$QYEAR < 2020.2,]$unemp.rate)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# OUTCOME: ACUTE CONDITIONS: MEN
# ----------------------------------------------------------------

pdatMEN <- datMEN %>% filter(year >= 2000) %>% mutate(qnum = 1:nrow(.))

# CORRELATION WITH COVARIATE
cor.test(pdatMEN$ACUTrate, log(pdatMEN$unemp.rate), method = "pearson") # p > .05
cor.test(pdatMEN$ACUTrate, log(pdatMEN$unemp.rate), method = "spearman") # p > .05

# BASELINE MODEL

# DEFINE EFFECT
pdatMEN <- pdatMEN %>% mutate(eff.level = ifelse(QYEAR < 2018.125, F, T), 
                                eff.slope = ifelse(QYEAR <= 2018.125, 0, qnum - 73))

# DEFINE TIMESERIES
TS.ACUT.MEN <- ts(pdatMEN$ACUTrate, frequency = 4, start = c(2000,1))
plot.ts(TS.ACUT.MEN) + abline(v = 2018, lty = "dashed")

# DECOMPOSITION
TS.ACUT.MEN.decomp <- decompose(TS.ACUT.MEN)
plot(TS.ACUT.MEN.decomp) #seasonal pattern

# SET BASELINE MODEL
TS.ACUT.MEN.mod0 <- gam(ACUTrate ~ qnum + s(Q, bs="cc", k=4), data = pdatMEN[pdatMEN$eff.level == F,], method = "ML")
summary(TS.ACUT.MEN.mod0) # significant smooth term -> seasonality

# STATIONARITY
tseries::adf.test(TS.ACUT.MEN.mod0$residuals) # p = .02 -> time series is stationary
tseries::kpss.test(TS.ACUT.MEN.mod0$residuals, null="Trend") # p = .10 -> time series is *trend* stationary

# CHECK RESIDUALS
auto.arima(resid(TS.ACUT.MEN.mod0, type = "pearson"), stationary = T, seasonal = F) # ARIMA(0,0,1) -> MA term?

# AUTOCORRELATION
acf(TS.ACUT.MEN.mod0$residuals, main = "ACF") # no AR
pacf(TS.ACUT.MEN.mod0$residuals, main = "PACF") # no MA
Box.test(TS.ACUT.MEN.mod0$residuals, type = "Ljung-Box") # no autocorrelation p = .07

plot(x = 1:72, y = TS.ACUT.MEN.mod0$y)
lines(TS.ACUT.MEN.mod0$fitted.values)

# MAIN MODEL: INCLUDE INTERVENTION EFFECT

# LEVEL CHANGE
TS.ACUT.MEN.mod1 <- gam(ACUTrate ~ qnum + s(Q, bs="cc", k=4) + eff.level, 
                         data = pdatMEN, method = "ML")
summary(TS.ACUT.MEN.mod1) # no sign. effect

# SLOPE CHANGE
TS.ACUT.MEN.mod2 <- gam(ACUTrate ~ qnum + s(Q, bs="cc", k=4) + eff.slope, 
                         data = pdatMEN, method = "ML")
summary(TS.ACUT.MEN.mod2) # no sign. effect

# LEVEL + SLOPE CHANGE
TS.ACUT.MEN.mod3 <- gam(ACUTrate ~ qnum + s(Q, bs="cc", k=4) + eff.level + eff.slope, 
                         data = pdatMEN, method = "ML")
summary(TS.ACUT.MEN.mod3) # no sign. effect

# MODEL COMPARISON
anova(TS.ACUT.MEN.mod1, TS.ACUT.MEN.mod2, test = "Chisq") # no difference
anova(TS.ACUT.MEN.mod1, TS.ACUT.MEN.mod3, test = "Chisq") # p < .001
anova(TS.ACUT.MEN.mod2, TS.ACUT.MEN.mod3, test = "Chisq") # no difference

# PLOT MAIN MODEL
plot(x = 1:81, y = TS.ACUT.MEN.mod2$y)
lines(TS.ACUT.MEN.mod0$fitted.values)

# LIST 
rm(TS.ACUT.MEN.mod0, TS.ACUT.MEN.mod1, TS.ACUT.MEN.mod2, TS.ACUT.MEN.mod3)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# OUTCOME: CHRONIC CONDITIONS (no K70-K70.9): MEN
# ----------------------------------------------------------------

# FOR NOW: limit TS for CRONIC CONDITIONS to 2007 to 2020.125
pdatMEN <- datMEN %>% filter(year >= 2007) %>% mutate(qnum = 1:nrow(.))

# CORRELATION WITH COVARIATE
cor.test(pdatMEN$CHRONrate, log(pdatMEN$unemp.rate), method = "pearson") # p < .001
cor.test(pdatMEN$CHRONrate, log(pdatMEN$unemp.rate), method = "spearman") # p < .001

# BASELINE MODEL

# DEFINE EFFECT
pdatMEN <- pdatMEN %>% mutate(eff.level = ifelse(QYEAR < 2018.125, F, T), 
                              eff.slope = ifelse(QYEAR <= 2018.125, 0, qnum - 45))

# DEFINE TIMESERIES
TS.CHRON.MEN <- ts(pdatMEN$CHRONrate, frequency = 4, start = c(2007,1))
plot.ts(TS.CHRON.MEN) + abline(v = 2018, lty = "dashed")

# DECOMPOSITION
TS.CHRON.MEN.decomp <- decompose(TS.CHRON.MEN)
plot(TS.CHRON.MEN.decomp) #seasonal pattern

# SET BASELINE MODEL
TS.CHRON.MEN.mod0 <- gam(CHRONrate ~ qnum + log(unemp.rate) + s(Q, bs="cc", k=4), data = pdatMEN[pdatMEN$eff.level == F,], method = "ML")
summary(TS.CHRON.MEN.mod0) # smooth term not significant -> no seasonality

# exclude unemp.rate
TS.CHRON.MEN.mod0 <- gam(CHRONrate ~ qnum + s(Q, bs="cc", k=4), data = pdatMEN[pdatMEN$eff.level == F,], method = "ML")
summary(TS.CHRON.MEN.mod0) # smooth term not significant -> no seasonality

# exclude seasonal term
TS.CHRON.MEN.mod0 <- lm(CHRONrate ~ qnum, data = pdatMEN[pdatMEN$eff.level == F,])
summary(TS.CHRON.MEN.mod0) # smooth term not significant -> no seasonality

# STATIONARITY
tseries::adf.test(TS.CHRON.MEN.mod0$residuals) # p = .06 -> time series is stationary
tseries::kpss.test(TS.CHRON.MEN.mod0$residuals, null="Trend") # p = .10 -> time series is *trend* stationary

# CHECK RESIDUALS
auto.arima(resid(TS.CHRON.MEN.mod0, type = "pearson"), stationary = T, seasonal = F) # ARIMA(0,0,0)

# AUTOCORRELATION
acf(TS.CHRON.MEN.mod0$residuals, main = "ACF") # no AR
pacf(TS.CHRON.MEN.mod0$residuals, main = "PACF") # no MA
Box.test(TS.CHRON.MEN.mod0$residuals, type = "Ljung-Box") # no autocorrelation p = .766

plot(x = 1:44, y = pdatMEN$CHRONrate[1:44])
lines(TS.CHRON.MEN.mod0$fitted.values)

# MAIN MODEL: INCLUDE INTERVENTION EFFECT

# LEVEL CHANGE
TS.CHRON.MEN.mod1 <- lm(CHRONrate ~ qnum + eff.level, data = pdatMEN)
summary(TS.CHRON.MEN.mod1) # no sign. effect

# SLOPE CHANGE
TS.CHRON.MEN.mod2 <- lm(CHRONrate ~ qnum + eff.slope, data = pdatMEN)
summary(TS.CHRON.MEN.mod2) # no sign. effect

# LEVEL + SLOPE CHANGE
TS.CHRON.MEN.mod3 <- lm(CHRONrate ~ qnum + eff.level + eff.slope, data = pdatMEN)
summary(TS.CHRON.MEN.mod3) # no sign. effect

# MODEL COMPARISON
anova(TS.CHRON.MEN.mod1, TS.CHRON.MEN.mod2, test = "Chisq") # no difference
anova(TS.CHRON.MEN.mod1, TS.CHRON.MEN.mod3, test = "Chisq") # no difference
anova(TS.CHRON.MEN.mod2, TS.CHRON.MEN.mod3, test = "Chisq") # no difference

# PLOT MAIN MODEL
plot(x = 1:53, y = pdatMEN$CHRONrate)
lines(TS.CHRON.MEN.mod0$fitted.values)

# LIST 
rm(TS.CHRON.MEN.mod0, TS.CHRON.MEN.mod1, TS.CHRON.MEN.mod2, TS.CHRON.MEN.mod3)

# ----------------------------------------------------------------
# OUTCOME: CHRONIC CONDITIONS (no K70-K70.9): WOMEN
# ----------------------------------------------------------------

# FOR NOW: limit TS for CRONIC CONDITIONS to 2007 to 2019.875
pdatWOMEN <- datWOMEN %>% filter(year >= 2007 & year < 2020) %>% mutate(qnum = 1:nrow(.))

# CORRELATION WITH COVARIATE
cor.test(pdatWOMEN$CHRONrate, log(pdatWOMEN$unemp.rate), method = "pearson") # p = .04
cor.test(pdatWOMEN$CHRONrate, log(pdatWOMEN$unemp.rate), method = "spearman") # p = .015

# BASELINE MODEL

# DEFINE EFFECT
pdatWOMEN <- pdatWOMEN %>% mutate(eff.level = ifelse(QYEAR < 2018.125, F, T), 
                              eff.slope = ifelse(QYEAR <= 2018.125, 0, qnum - 45))

# DEFINE TIMESERIES
TS.CHRON.WOMEN <- ts(pdatWOMEN$CHRONrate, frequency = 4, start = c(2007,1))
plot.ts(TS.CHRON.WOMEN) + abline(v = 2018, lty = "dashed")

# DECOMPOSITION
TS.CHRON.WOMEN.decomp <- decompose(TS.CHRON.WOMEN)
plot(TS.CHRON.WOMEN.decomp) #seasonal pattern

# SET BASELINE MODEL
TS.CHRON.WOMEN.mod0 <- gam(CHRONrate ~ qnum + log(unemp.rate) + s(Q, bs="cc", k=4), data = pdatWOMEN[pdatWOMEN$eff.level == F,], method = "ML")
summary(TS.CHRON.WOMEN.mod0) # smooth term significant -> seasonality

# exclude unemp.rate
TS.CHRON.WOMEN.mod0 <- gam(CHRONrate ~ qnum + s(Q, bs="cc", k=4), data = pdatWOMEN[pdatWOMEN$eff.level == F,], method = "ML")
summary(TS.CHRON.WOMEN.mod0) # smooth term significant -> seasonality

# STATIONARITY
tseries::adf.test(TS.CHRON.WOMEN.mod0$residuals) # p = .59 -> time series is not stationary
tseries::kpss.test(TS.CHRON.WOMEN.mod0$residuals, null="Trend") # p = .10 -> time series is *trend* stationary

# CHECK RESIDUALS
auto.arima(resid(TS.CHRON.WOMEN.mod0, type = "pearson"), stationary = T, seasonal = F) # ARIMA(0,0,0)

# AUTOCORRELATION
acf(TS.CHRON.WOMEN.mod0$residuals, main = "ACF") # term AR, lag = 3
pacf(TS.CHRON.WOMEN.mod0$residuals, main = "PACF") # no MA
Box.test(TS.CHRON.WOMEN.mod0$residuals, type = "Ljung-Box") # no autocorrelation p = .963

# INCLUDE AR IN BASELINE MODEL
LAG3.CHRONrate <- stats::lag(TS.CHRON.WOMEN, -3)
pdatWOMEN$LAG3.CHRONrate <- c(NA, NA, NA, LAG3.CHRONrate[1:49])

TS.CHRON.WOMEN.mod0 <- gam(CHRONrate ~ qnum + LAG3.CHRONrate + s(Q, bs="cc", k=4), data = pdatWOMEN[pdatWOMEN$eff.level == F,], method = "ML")
summary(TS.CHRON.WOMEN.mod0) # smooth term significant -> seasonality

# linear trend no longer significant, exclude
TS.CHRON.WOMEN.mod0 <- gam(CHRONrate ~ LAG3.CHRONrate + s(Q, bs="cc", k=4), data = pdatWOMEN[pdatWOMEN$eff.level == F,], method = "ML")
summary(TS.CHRON.WOMEN.mod0) # smooth term significant -> seasonality

# CHECK AUTOCORRELATION AGAIN
acf(TS.CHRON.WOMEN.mod0$residuals, main = "ACF") # ok
pacf(TS.CHRON.WOMEN.mod0$residuals, main = "ACF") # ok
Box.test(TS.CHRON.WOMEN.mod0$residuals, type = "Ljung-Box") # no autocorrelation p = .851

plot(x = 1:44, y = pdatWOMEN$CHRONrate[1:44])
lines(TS.CHRON.WOMEN.mod0$fitted.values)

# MAIN MODEL: INCLUDE INTERVENTION EFFECT

# LEVEL CHANGE
TS.CHRON.WOMEN.mod1 <- gam(CHRONrate ~ LAG3.CHRONrate + s(Q, bs="cc", k=4) + eff.level, 
                           data = pdatWOMEN, method = "ML")
summary(TS.CHRON.WOMEN.mod1) # no sign. effect

# SLOPE CHANGE
TS.CHRON.WOMEN.mod2 <- gam(CHRONrate ~ LAG3.CHRONrate + s(Q, bs="cc", k=4) + eff.slope, 
                           data = pdatWOMEN, method = "ML")
summary(TS.CHRON.WOMEN.mod2) # p = .010 / n.s. if limited to 2007-2019

# LEVEL + SLOPE CHANGE
TS.CHRON.WOMEN.mod3 <- gam(CHRONrate ~ LAG3.CHRONrate + s(Q, bs="cc", k=4) + eff.level + eff.slope, 
                                       data = pdatWOMEN, method = "ML")
summary(TS.CHRON.WOMEN.mod3) # no sign. effect

# MODEL COMPARISON
anova(TS.CHRON.WOMEN.mod1, TS.CHRON.WOMEN.mod2, test = "Chisq") # ?
anova(TS.CHRON.WOMEN.mod1, TS.CHRON.WOMEN.mod3, test = "Chisq") # p = .046
anova(TS.CHRON.WOMEN.mod2, TS.CHRON.WOMEN.mod3, test = "Chisq") # p = .606

# PLOT MAIN MODEL
plot(x = 1:52, y = pdatWOMEN$CHRONrate)
lines(TS.CHRON.WOMEN.mod3$fitted.values)
Box.test(resid(TS.CHRON.WOMEN.mod3), type = "Ljung-Box") # p = .844 no autocorrleation
tseries::adf.test(resid(TS.CHRON.WOMEN.mod3)) # p = .153 not stationary
tseries::kpss.test(resid(TS.CHRON.WOMEN.mod3)) # p = .1 *trend* stationary

# LIST 
rm(TS.CHRON.WOMEN.mod0, TS.CHRON.WOMEN.mod1, TS.CHRON.WOMEN.mod2, TS.CHRON.WOMEN.mod3)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# OUTCOME: ALCOHOLIC LIVER (K70-K70.9): MEN
# ----------------------------------------------------------------

# FOR NOW: limit TS for CRONIC CONDITIONS to 2000 to 2020.125
pdatMEN <- datMEN %>% filter(year >= 2007 & year < 2020) %>% mutate(qnum = 1:nrow(.))

# CORRELATION WITH COVARIATE
cor.test(pdatMEN$ALCLIVrate, log(pdatMEN$unemp.rate), method = "pearson") # p < .001
cor.test(pdatMEN$ALCLIVrate, log(pdatMEN$unemp.rate), method = "spearman") # p < .001

# BASELINE MODEL

# DEFINE EFFECT
pdatMEN <- pdatMEN %>% mutate(eff.level = ifelse(QYEAR < 2018.125, F, T), 
                              eff.slope = ifelse(QYEAR <= 2018.125, 0, qnum - 45))

# DEFINE TIMESERIES
TS.ALCLIV.MEN <- ts(pdatMEN$ALCLIVrate, frequency = 4, start = c(2007,1))
plot.ts(TS.ALCLIV.MEN) + abline(v = 2018, lty = "dashed")

# DECOMPOSITION
TS.ALCLIV.MEN.decomp <- decompose(TS.ALCLIV.MEN)
plot(TS.ALCLIV.MEN.decomp) #seasonal pattern

# SET BASELINE MODEL
TS.ALCLIV.MEN.mod0 <- gam(ALCLIVrate ~ qnum + log(unemp.rate) + s(Q, bs="cc", k=4), data = pdatMEN[pdatMEN$eff.level == F,], method = "ML")
summary(TS.ALCLIV.MEN.mod0) # smooth term not significant -> no seasonality

# exclude unemp.rate
TS.ALCLIV.MEN.mod0 <- gam(ALCLIVrate ~ qnum + s(Q, bs="cc", k=4), data = pdatMEN[pdatMEN$eff.level == F,], method = "ML")
summary(TS.ALCLIV.MEN.mod0) # smooth term not significant -> no seasonality

# exclude seasonal smooth term
TS.ALCLIV.MEN.mod0 <- lm(ALCLIVrate ~ qnum, data = pdatMEN[pdatMEN$eff.level == F,])
summary(TS.ALCLIV.MEN.mod0)

# STATIONARITY
tseries::adf.test(TS.ALCLIV.MEN.mod0$residuals) # p = .423 -> time series not stationary
tseries::kpss.test(TS.ALCLIV.MEN.mod0$residuals, null="Trend") # p = .1 -> time series is *trend* stationary

# CHECK RESIDUALS
auto.arima(resid(TS.ALCLIV.MEN.mod0, type = "pearson"), stationary = T, seasonal = F) # ARIMA(0,0,0)

# AUTOCORRELATION
acf(TS.ALCLIV.MEN.mod0$residuals, main = "ACF") # no AR
pacf(TS.ALCLIV.MEN.mod0$residuals, main = "PACF") # no MA
Box.test(TS.ALCLIV.MEN.mod0$residuals, type = "Ljung-Box") # no autocorrelation p = .651

plot(x = 1:44, y = pdatMEN$ALCLIVrate[1:44])
lines(TS.ALCLIV.MEN.mod0$fitted.values)

# MAIN MODEL: INCLUDE INTERVENTION EFFECT

# LEVEL CHANGE
TS.ALCLIV.MEN.mod1 <- lm(ALCLIVrate ~ qnum + eff.level, data = pdatMEN)
summary(TS.ALCLIV.MEN.mod1) # no sign. effect

# SLOPE CHANGE
TS.ALCLIV.MEN.mod2 <- lm(ALCLIVrate ~ qnum + eff.slope, data = pdatMEN)
summary(TS.ALCLIV.MEN.mod2) # no sign. effect

# LEVEL + SLOPE CHANGE
TS.ALCLIV.MEN.mod3 <- lm(ALCLIVrate ~ qnum + eff.level + eff.slope, data = pdatMEN)
summary(TS.ALCLIV.MEN.mod3) # no sign. effect

# MODEL COMPARISON
anova(TS.ALCLIV.MEN.mod1, TS.ALCLIV.MEN.mod2, test = "Chisq") # ?
anova(TS.ALCLIV.MEN.mod1, TS.ALCLIV.MEN.mod3, test = "Chisq") # no difference
anova(TS.ALCLIV.MEN.mod2, TS.ALCLIV.MEN.mod3, test = "Chisq") # no difference

# PLOT MAIN MODEL
plot(x = 1:52, y = pdatMEN$ALCLIVrate)
lines(TS.ALCLIV.MEN.mod0$fitted.values)

# LIST 
rm(TS.ALCLIV.MEN.mod0, TS.ALCLIV.MEN.mod1, TS.ALCLIV.MEN.mod2, TS.ALCLIV.MEN.mod3)

# ----------------------------------------------------------------
# OUTCOME: ALCOHOLIC LIVER (K70-K70.9): WOMEN
# ----------------------------------------------------------------

# FOR NOW: limit TS for CRONIC CONDITIONS to 2007 to 2019.875
pdatWOMEN <- datWOMEN %>% filter(year >= 2007 & year < 2020) %>% mutate(qnum = 1:nrow(.))

# CORRELATION WITH COVARIATE
cor.test(pdatWOMEN$ALCLIVrate, log(pdatWOMEN$unemp.rate), method = "pearson") # p < .001
cor.test(pdatWOMEN$ALCLIVrate, log(pdatWOMEN$unemp.rate), method = "spearman") # p < .001

# BASELINE MODEL

# DEFINE EFFECT
pdatWOMEN <- pdatWOMEN %>% mutate(eff.level = ifelse(QYEAR < 2018.125, F, T), 
                                  eff.slope = ifelse(QYEAR <= 2018.125, 0, qnum - 45))

# DEFINE TIMESERIES
TS.ALCLIV.WOMEN <- ts(pdatWOMEN$ALCLIVrate, frequency = 4, start = c(2007,1))
plot.ts(TS.ALCLIV.WOMEN) + abline(v = 2018, lty = "dashed")

# DECOMPOSITION
TS.ALCLIV.WOMEN.decomp <- decompose(TS.ALCLIV.WOMEN)
plot(TS.ALCLIV.WOMEN.decomp) #seasonal pattern

# SET BASELINE MODEL
TS.ALCLIV.WOMEN.mod0 <- gam(ALCLIVrate ~ qnum + log(unemp.rate) + s(Q, bs="cc", k=4), data = pdatWOMEN[pdatWOMEN$eff.level == F,], method = "ML")
summary(TS.ALCLIV.WOMEN.mod0) # smooth term significant -> seasonality

# exclude unemp.rate
TS.ALCLIV.WOMEN.mod0 <- gam(ALCLIVrate ~ qnum + s(Q, bs="cc", k=4), data = pdatWOMEN[pdatWOMEN$eff.level == F,], method = "ML")
summary(TS.ALCLIV.WOMEN.mod0) # smooth term significant -> seasonality

# exclude smooth term
TS.ALCLIV.WOMEN.mod0 <- lm(ALCLIVrate ~ qnum, data = pdatWOMEN[pdatWOMEN$eff.level == F,])
summary(TS.ALCLIV.WOMEN.mod0)

# STATIONARITY
tseries::adf.test(TS.ALCLIV.WOMEN.mod0$residuals) # p = .02 -> time series is stationary
tseries::kpss.test(TS.ALCLIV.WOMEN.mod0$residuals, null="Trend") # p = .10 -> time series is *trend* stationary

# CHECK RESIDUALS
auto.arima(resid(TS.ALCLIV.WOMEN.mod0, type = "pearson"), stationary = T, seasonal = F) # ARIMA(0,0,0)

# AUTOCORRELATION
acf(TS.ALCLIV.WOMEN.mod0$residuals, main = "ACF") # no AR
pacf(TS.ALCLIV.WOMEN.mod0$residuals, main = "PACF") # no MA
Box.test(TS.ALCLIV.WOMEN.mod0$residuals, type = "Ljung-Box") # no autocorrelation p = .111

plot(x = 1:44, y = pdatWOMEN$ALCLIVrate[1:44])
lines(TS.ALCLIV.WOMEN.mod0$fitted.values)

# MAIN MODEL: INCLUDE INTERVENTION EFFECT

# LEVEL CHANGE
TS.ALCLIV.WOMEN.mod1 <- lm(ALCLIVrate ~ qnum + eff.level, data = pdatWOMEN)
summary(TS.ALCLIV.WOMEN.mod1) # no sign. effect

# SLOPE CHANGE
TS.ALCLIV.WOMEN.mod2 <- lm(ALCLIVrate ~ qnum + eff.slope, data = pdatWOMEN)
summary(TS.ALCLIV.WOMEN.mod2) # no sign. effect

# LEVEL + SLOPE CHANGE
TS.ALCLIV.WOMEN.mod3 <- lm(ALCLIVrate ~ qnum + eff.level + eff.slope, data = pdatWOMEN)
summary(TS.ALCLIV.WOMEN.mod3) # no sign. effect

# MODEL COMPARISON
anova(TS.ALCLIV.WOMEN.mod1, TS.ALCLIV.WOMEN.mod2, test = "Chisq") # ?
anova(TS.ALCLIV.WOMEN.mod1, TS.ALCLIV.WOMEN.mod3, test = "Chisq") # p = .080
anova(TS.ALCLIV.WOMEN.mod2, TS.ALCLIV.WOMEN.mod3, test = "Chisq") # p = .200

# PLOT MAIN MODEL
plot(x = 1:52, y = pdatWOMEN$ALCLIVrate)
lines(TS.ALCLIV.WOMEN.mod0$fitted.values)

# LIST 
rm(TS.ALCLIV.WOMEN.mod0, TS.ALCLIV.WOMEN.mod1, TS.ALCLIV.WOMEN.mod2, TS.ALCLIV.WOMEN.mod3)

