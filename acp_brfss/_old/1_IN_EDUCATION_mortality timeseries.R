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
library(EnvStats)
library(rcompanion)

# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# LOAD DATA
# ----------------------------------------------------------------

setwd("/Users/carolinkilian/Desktop/SIMAH_workplace/")
DATE <- 20230711

# prepared data file 
data <- read.csv("acp_brfss/data/20230711_AGEST_MORTALITY_INDIANA.csv")

# covariates
datUNEMP <- read.csv("acp_brfss/data/20230628_UNEMP_INDIANA.csv")

# combine data
pdat <- left_join(data, datUNEMP)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# COVARIATES
# ----------------------------------------------------------------

# distributions
ggplot(pdat, aes(x = unemp.rate)) + geom_histogram() 
qqPlot(pdat$unemp.rate) # non-normal

ggplot(pdat, aes(x = log(unemp.rate))) + geom_histogram() 
qqPlot(log(pdat$unemp.rate)) # non-normal

ggplot(pdat, aes(x = QYEAR, y = unemp.rate)) + geom_line() + geom_point()
pdat <- pdat %>% mutate(unemp.level = factor(ifelse(unemp.rate >=7, 1, ifelse(unemp.rate < 7, 0, NA))))

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# SELECT DATA
# ----------------------------------------------------------------

datMLEHS <- pdat %>% filter(edclass == "LEHS" & sex == 1) %>% mutate(qnum = 1:nrow(.))
datMSomeC <- pdat %>% filter(edclass == "SomeC" & sex == 1) %>% mutate(qnum = 1:nrow(.))
datMCollege <- pdat %>% filter(edclass == "College" & sex == 1) %>% mutate(qnum = 1:nrow(.))
datWLEHS <- pdat %>% filter(edclass == "LEHS" & sex == 2) %>% mutate(qnum = 1:nrow(.))
datWSomeC <- pdat %>% filter(edclass == "SomeC" & sex == 2) %>% mutate(qnum = 1:nrow(.))
datWCollege <- pdat %>% filter(edclass == "College" & sex == 2) %>% mutate(qnum = 1:nrow(.))

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# OUTCOME TIMESERIES ANALYSIS: 100% AAM CONDITIONS BY GROUP
# ----------------------------------------------------------------

# GROUP 1: MEN / LEHRS

# BASELINE MODEL

# define effect
datMLEHS <- datMLEHS %>% mutate(eff.level = ifelse(QYEAR < 2018.125, F, T), 
                                eff.slope = ifelse(QYEAR <= 2018.125, 0, qnum - 45))

# DEFINE TIMESERIES
TS.AAM.MLEHS <- ts(datMLEHS$AAMrate, frequency = 4, start = c(2007,1))
plot.ts(TS.AAM.MLEHS) + abline(v = 2018, lty = "dashed")

# DECOMPOSITION
TS.AAM.MLEHS.decomp <- decompose(TS.AAM.MLEHS)
plot(TS.AAM.MLEHS.decomp) #seasonal pattern + linear trend

# SET BASELINE MODEL
TS.AAM.MLEHS.mod0 <- gam(AAMrate ~ qnum + unemp.level + s(Q, bs="cc", k=4), data = datMLEHS[datMLEHS$eff.level == F,], method = "ML")
summary(TS.AAM.MLEHS.mod0) # significant smooth term -> seasonality

# drop unemp.rate
TS.AAM.MLEHS.mod0 <- gam(AAMrate ~ qnum + s(Q, bs="cc", k=4), data = datMLEHS[datMLEHS$eff.level == F,], method = "ML")
summary(TS.AAM.MLEHS.mod0) # smooth term significant with p = .054 -> keep for now 

# STATIONARITY
tseries::adf.test(TS.AAM.MLEHS.mod0$residuals) # p = .04 -> time series is stationary
tseries::kpss.test(TS.AAM.MLEHS.mod0$residuals, null="Trend") # p = .10 -> time series is *trend* stationary

# CHECK RESIDUALS
auto.arima(resid(TS.AAM.MLEHS.mod0, type = "pearson"), stationary = T, seasonal = F) # ARIMA(0,0,0) -> ok

# AUTOCORRELATION
acf(TS.AAM.MLEHS.mod0$residuals, main = "ACF") # no AR
pacf(TS.AAM.MLEHS.mod0$residuals, main = "PACF") # no MA
Box.test(TS.AAM.MLEHS.mod0$residuals, type = "Ljung-Box") # no autocorrelation p = .1099

plot(x = 1:44, y = TS.AAM.MLEHS.mod0$y)
lines(TS.AAM.MLEHS.mod0$fitted.values)

# MAIN MODEL: INCLUDE INTERVENTION EFFECT

# LEVEL CHANGE
TS.AAM.MLEHS.mod1 <- gam(AAMrate ~ qnum + s(Q, bs="cc", k=4) + eff.level, 
                         data = datMLEHS, method = "ML")
summary(TS.AAM.MLEHS.mod1) # p = .484

# SLOPE CHANGE
TS.AAM.MLEHS.mod2 <- gam(AAMrate ~ qnum + s(Q, bs="cc", k=4) + eff.slope, 
                         data = datMLEHS, method = "ML")
summary(TS.AAM.MLEHS.mod2) # p = .459

# LEVEL + SLOPE CHANGE
TS.AAM.MLEHS.mod3 <- gam(AAMrate ~ qnum + s(Q, bs="cc", k=4) + eff.level + eff.slope, 
                         data = datMLEHS, method = "ML")
summary(TS.AAM.MLEHS.mod3) # p = .814 / p = .737

# PLOT MAIN MODEL
plot(x = 1:52, y = datMLEHS$AAMrate)
lines(TS.AAM.MLEHS.mod0$fitted.values)

# LIST 
rm(TS.AAM.MLEHS.mod0, TS.AAM.MLEHS.mod1, TS.AAM.MLEHS.mod2, TS.AAM.MLEHS.mod3)

# ----------------------------------------------------------------

# GROUP 2: MEN — SomeC

# BASELINE MODEL

# define effect
datMSomeC <- datMSomeC %>% mutate(eff.level = ifelse(QYEAR < 2018.125, F, T),
                                  eff.slope = ifelse(QYEAR <= 2018.125, 0, qnum - 45))

# DEFINE TIMESERIES
TS.AAM.MSomeC <- ts(datMSomeC$AAMrate, frequency = 4, start = c(2007,1))
plot.ts(TS.AAM.MSomeC) + abline(v = 2018, lty = "dashed")

# DECOMPOSITION
TS.AAM.MSomeC.decomp <- decompose(TS.AAM.MSomeC)
plot(TS.AAM.MSomeC.decomp) #seasonal pattern + non-linear trend

# SET BASELINE MODEL
TS.AAM.MSomeC.mod0 <- gam(AAMrate ~ s(qnum, bs="cs", k = -1) + unemp.level + s(Q, bs="cc", k=4), 
                          data = datMSomeC[datMSomeC$eff.level == F,], method = "ML")
summary(TS.AAM.MSomeC.mod0) # unemployment not significant

# drop unemp.rate
TS.AAM.MSomeC.mod0 <- gam(AAMrate ~ s(qnum, bs="cs", k = -1) + s(Q, bs="cc", k=4), data = datMSomeC[datMSomeC$eff.level == F,], method = "ML")
summary(TS.AAM.MSomeC.mod0) # smooth term not significant

# include spline smooth function
TS.AAM.MSomeC.mod0 <- gam(AAMrate ~ s(qnum, bs="cs", k = -1), data = datMSomeC[datMSomeC$eff.level == F,], method = "ML")
summary(TS.AAM.MSomeC.mod0) 
k.check(TS.AAM.MSomeC.mod0) # k = 9

# STATIONARITY
tseries::adf.test(TS.AAM.MSomeC.mod0$residuals) # p = .490 -> non-stationary 
tseries::kpss.test(TS.AAM.MSomeC.mod0$residuals, null="Trend") # p = .1 -> *trend* stationary 

# CHECK RESIDUALS
auto.arima(resid(TS.AAM.MSomeC.mod0, type = "pearson"), stationary = T, seasonal = F) # ARIMA(0,0,0) -> ok

# AUTOCORRELATION
acf(TS.AAM.MSomeC.mod0$residuals, main = "ACF") # no AR
pacf(TS.AAM.MSomeC.mod0$residuals, main = "PACF") # no MA
Box.test(TS.AAM.MSomeC.mod0$residuals, type = "Ljung-Box") # no autocorrelation p = .358

plot(x = 1:44, y = TS.AAM.MSomeC.mod0$y)
lines(TS.AAM.MSomeC.mod0$fitted.values)

# MAIN MODEL: INCLUDE INTERVENTION EFFECT

# LEVEL CHANGE
TS.AAM.MSomeC.mod1 <- gam(AAMrate ~ s(qnum, bs="cs", k = 9) + eff.level, 
                          data = datMSomeC, method = "ML")
summary(TS.AAM.MSomeC.mod1) # p = .41

# SLOPE CHANGE
TS.AAM.MSomeC.mod2 <- gam(AAMrate ~ s(qnum, bs="cs", k = 9)  + eff.slope, 
                          data = datMSomeC, method = "ML")
summary(TS.AAM.MSomeC.mod2) # p = .336

# LEVEL + SLOPE CHANGE
TS.AAM.MSomeC.mod3 <- gam(AAMrate ~ s(qnum, bs="cs", k = 9)  + eff.level + eff.slope, 
                          data = datMSomeC, method = "ML")
summary(TS.AAM.MSomeC.mod3) # p = .666 / p = .523

# PLOT MAIN MODEL
plot(x = 1:52, y = datMSomeC$AAMrate)
lines(TS.AAM.MSomeC.mod0$fitted.values)

# LIST 
rm(TS.AAM.MSomeC.mod0, TS.AAM.MSomeC.mod1, TS.AAM.MSomeC.mod2, TS.AAM.MSomeC.mod3)

# ----------------------------------------------------------------

# GROUP 3: MEN — COLLEGE

# BASELINE MODEL

# define effect
datMCollege <- datMCollege %>% mutate(eff.level = ifelse(QYEAR < 2018.125, F, T),
                                      eff.slope = ifelse(QYEAR <= 2018.125, 0, qnum - 45))

# DEFINE TIMESERIES
TS.AAM.MCollege <- ts(datMCollege$AAMrate, frequency = 4, start = c(2007,1))
plot.ts(TS.AAM.MCollege) + abline(v = 2018, lty = "dashed")

# DECOMPOSITION
TS.AAM.MCollege.decomp <- decompose(TS.AAM.MCollege)
plot(TS.AAM.MCollege.decomp) #seasonal pattern + linear trend

# SET BASELINE MODEL
TS.AAM.MCollege.mod0 <- gam(AAMrate ~ qnum + unemp.level + s(Q, bs="cc", k=4), 
                            data = datMCollege[datMCollege$eff.level == F,], method = "ML")
summary(TS.AAM.MCollege.mod0) # unemployment not significant

# drop unemp.rate
TS.AAM.MCollege.mod0 <- gam(AAMrate ~ qnum + s(Q, bs="cc", k=4), data = datMCollege[datMCollege$eff.level == F,], method = "ML")
summary(TS.AAM.MCollege.mod0) # smooth term not significant

# include spline smooth function
TS.AAM.MCollege.mod0 <- lm(AAMrate ~ qnum, data = datMCollege[datMCollege$eff.level == F,])
summary(TS.AAM.MCollege.mod0) 

# STATIONARITY
tseries::adf.test(TS.AAM.MCollege.mod0$residuals) # p = .07 -> non-stationary 
tseries::kpss.test(TS.AAM.MCollege.mod0$residuals, null="Trend") # p = .089 -> *trend* stationary 

# CHECK RESIDUALS
auto.arima(resid(TS.AAM.MCollege.mod0, type = "pearson"), stationary = T, seasonal = F) # ARIMA(0,0,0) -> ok

# AUTOCORRELATION
acf(TS.AAM.MCollege.mod0$residuals, main = "ACF") # AR = 2
pacf(TS.AAM.MCollege.mod0$residuals, main = "PACF") # no MA

# add AR term (AR = 2)
LAG2 <- stats::lag(TS.AAM.MCollege, -2)
datMCollege$LAG2 <- c(NA, NA, LAG2[1:50])

TS.AAM.MCollege.mod0 <- lm(AAMrate ~ qnum + LAG2, data = datMCollege[datMCollege$eff.level == F,])
summary(TS.AAM.MCollege.mod0) 

# CHECK AUTOCORRELATION AGAIN
acf(TS.AAM.MCollege.mod0$residuals, main = "ACF") # ok
pacf(TS.AAM.MCollege.mod0$residuals, main = "ACF") # ok
Box.test(TS.AAM.MCollege.mod0$residuals, type = "Ljung-Box") # no autocorrelation p = .703

plot(x = 1:44, y = datMCollege$AAMrate[1:44])
lines(TS.AAM.MCollege.mod0$fitted.values)

# MAIN MODEL: INCLUDE INTERVENTION EFFECT

# LEVEL CHANGE
TS.AAM.MCollege.mod1 <- lm(AAMrate ~ qnum + LAG2 + eff.level, data = datMCollege)
summary(TS.AAM.MCollege.mod1) # p = .546

# SLOPE CHANGE
TS.AAM.MCollege.mod2 <- lm(AAMrate ~ qnum + LAG2 + eff.slope, data = datMCollege)
summary(TS.AAM.MCollege.mod2) # p = .045 -> R2 adjusted = 51.6%

# LEVEL + SLOPE CHANGE
TS.AAM.MCollege.mod3 <- lm(AAMrate ~ qnum + LAG2 + eff.level + eff.slope, data = datMCollege)
summary(TS.AAM.MCollege.mod3) # p = .175 / p = .020 -> R2 adjusted = 52.5%

# MODEL COMPARISON
compareLM(TS.AAM.MCollege.mod1, TS.AAM.MCollege.mod2, TS.AAM.MCollege.mod3)

# PLOT MAIN MODEL: use model 3 as larger R2
plot(x = 1:52, y = datMCollege$AAMrate)
lines(TS.AAM.MCollege.mod3$fitted.values)

# LIST 
rm(TS.AAM.MCollege.mod0, TS.AAM.MCollege.mod1, TS.AAM.MCollege.mod2, TS.AAM.MCollege.mod3)

# ----------------------------------------------------------------
# ----------------------------------------------------------------

# GROUP 4: WOMEN / LEHS

# BASELINE MODEL

# define effect
datWLEHS <- datWLEHS %>% mutate(eff.level = ifelse(QYEAR < 2018.125, F, T), 
                                eff.slope = ifelse(QYEAR <= 2018.125, 0, qnum - 45))

# DEFINE TIMESERIES
TS.AAM.WLEHS <- ts(datWLEHS$AAMrate, frequency = 4, start = c(2007,1))
plot.ts(TS.AAM.WLEHS) + abline(v = 2018, lty = "dashed")

# DECOMPOSITION
TS.AAM.WLEHS.decomp <- decompose(TS.AAM.WLEHS)
plot(TS.AAM.WLEHS.decomp) #seasonal pattern + linear trend

# SET BASELINE MODEL
TS.AAM.WLEHS.mod0 <- gam(AAMrate ~ qnum + unemp.level + s(Q, bs="cc", k=4), data = datWLEHS[datWLEHS$eff.level == F,], method = "ML")
summary(TS.AAM.WLEHS.mod0) # unemployment not significant

# drop unemp.rate
TS.AAM.WLEHS.mod0 <- gam(AAMrate ~ qnum + s(Q, bs="cc", k=4), data = datWLEHS[datWLEHS$eff.level == F,], method = "ML")
summary(TS.AAM.WLEHS.mod0) # smooth term significant

# STATIONARITY
tseries::adf.test(TS.AAM.WLEHS.mod0$residuals) # p = .04 -> time series is stationary
tseries::kpss.test(TS.AAM.WLEHS.mod0$residuals, null="Trend") # p = .10 -> time series is *trend* stationary

# CHECK RESIDUALS
auto.arima(resid(TS.AAM.WLEHS.mod0, type = "pearson"), stationary = T, seasonal = F) # ARIMA(0,0,0) -> ok

# AUTOCORRELATION
acf(TS.AAM.WLEHS.mod0$residuals, main = "ACF") # no AR
pacf(TS.AAM.WLEHS.mod0$residuals, main = "PACF") # no MA
Box.test(TS.AAM.WLEHS.mod0$residuals, type = "Ljung-Box") # no autocorrelation p = .1351

plot(x = 1:44, y = TS.AAM.WLEHS.mod0$y)
lines(TS.AAM.WLEHS.mod0$fitted.values)

# MAIN MODEL: INCLUDE INTERVENTION EFFECT

# LEVEL CHANGE
TS.AAM.WLEHS.mod1 <- gam(AAMrate ~ qnum + s(Q, bs="cc", k=4) + eff.level, 
                         data = datWLEHS, method = "ML")
summary(TS.AAM.WLEHS.mod1) # p = .009 -> R2 adjusted = 73.3%

# SLOPE CHANGE
TS.AAM.WLEHS.mod2 <- gam(AAMrate ~ qnum + s(Q, bs="cc", k=4) + eff.slope, 
                         data = datWLEHS, method = "ML")
summary(TS.AAM.WLEHS.mod2) # p = .019 -> R2 adjusted = 72.5%

# LEVEL + SLOPE CHANGE
TS.AAM.WLEHS.mod3 <- gam(AAMrate ~ qnum + s(Q, bs="cc", k=4) + eff.level + eff.slope, 
                         data = datWLEHS, method = "ML")
summary(TS.AAM.WLEHS.mod3) # p = .185 / p = .479 -> R2 adjusted = 73.6% 

# MODEL COMPARISON
AIC(TS.AAM.WLEHS.mod1, TS.AAM.WLEHS.mod2, TS.AAM.WLEHS.mod3)
BIC(TS.AAM.WLEHS.mod1, TS.AAM.WLEHS.mod2, TS.AAM.WLEHS.mod3)

# PLOT MAIN MODEL: best fitting model 3 based on R2
plot(x = 1:52, y = TS.AAM.WLEHS.mod1$y)
lines(TS.AAM.WLEHS.mod1$fitted.values)

# LIST 
rm(TS.AAM.WLEHS.mod0, TS.AAM.WLEHS.mod1, TS.AAM.WLEHS.mod2, TS.AAM.WLEHS.mod3)

# ----------------------------------------------------------------

# GROUP 5: WOMEN — SomeC

# BASELINE MODEL

# define effect
datWSomeC <- datWSomeC %>% mutate(eff.level = ifelse(QYEAR < 2018.125, F, T),
                                  eff.slope = ifelse(QYEAR <= 2018.125, 0, qnum - 45))

# DEFINE TIMESERIES
TS.AAM.WSomeC <- ts(datWSomeC$AAMrate, frequency = 4, start = c(2007,1))
plot.ts(TS.AAM.WSomeC) + abline(v = 2018, lty = "dashed")

# DECOMPOSITION
TS.AAM.WSomeC.decomp <- decompose(TS.AAM.WSomeC)
plot(TS.AAM.WSomeC.decomp) #seasonal pattern + non-linear trend

# SET BASELINE MODEL
TS.AAM.WSomeC.mod0 <- gam(AAMrate ~ qnum + unemp.level + s(Q, bs="cc", k=4), 
                          data = datWSomeC[datWSomeC$eff.level == F,], method = "ML")
summary(TS.AAM.WSomeC.mod0) # unemployment not significant

# drop unemp.rate
TS.AAM.WSomeC.mod0 <- gam(AAMrate ~ qnum + s(Q, bs="cc", k=4), data = datWSomeC[datWSomeC$eff.level == F,], method = "ML")
summary(TS.AAM.WSomeC.mod0) # smooth term not significant

# include spline smooth function
TS.AAM.WSomeC.mod0 <- lm(AAMrate ~ qnum, data = datWSomeC[datWSomeC$eff.level == F,])
summary(TS.AAM.WSomeC.mod0) 

# STATIONARITY
tseries::adf.test(TS.AAM.WSomeC.mod0$residuals) # p = .171 -> non-stationary 
tseries::kpss.test(TS.AAM.WSomeC.mod0$residuals, null="Trend") # p = .1 -> *trend* stationary 

# CHECK RESIDUALS
auto.arima(resid(TS.AAM.WSomeC.mod0, type = "pearson"), stationary = T, seasonal = F) # ARIMA(2,0,0) -> AR = 2

# AUTOCORRELATION
acf(TS.AAM.WSomeC.mod0$residuals, main = "ACF") # AR = 2
pacf(TS.AAM.WSomeC.mod0$residuals, main = "PACF") # no MA

# add AR term (AR = 2)
LAG2 <- stats::lag(TS.AAM.WSomeC, -2)
datWSomeC$LAG2 <- c(NA, NA, LAG2[1:50])

TS.AAM.WSomeC.mod0 <- lm(AAMrate ~ qnum + LAG2, data = datWSomeC[datWSomeC$eff.level == F,])
summary(TS.AAM.WSomeC.mod0) 

# CHECK AUTOCORRELATION AGAIN
acf(TS.AAM.WSomeC.mod0$residuals, main = "ACF") # ok
pacf(TS.AAM.WSomeC.mod0$residuals, main = "ACF") # ok
Box.test(TS.AAM.WSomeC.mod0$residuals, type = "Ljung-Box") # no autocorrelation p = .257
 
plot(x = 1:44, y = datWSomeC$AAMrate[1:44])
lines(TS.AAM.WSomeC.mod0$fitted.values)

# MAIN MODEL: INCLUDE INTERVENTION EFFECT

# LEVEL CHANGE
TS.AAM.WSomeC.mod1 <- lm(AAMrate ~ qnum + LAG2 + eff.level, data = datWSomeC)
summary(TS.AAM.WSomeC.mod1) # p = .002; R2 adjusted = 70.5%

# SLOPE CHANGE
TS.AAM.WSomeC.mod2 <- lm(AAMrate ~ qnum + LAG2 + eff.slope, data = datWSomeC)
summary(TS.AAM.WSomeC.mod2) # p = .157

# LEVEL + SLOPE CHANGE
TS.AAM.WSomeC.mod3 <- lm(AAMrate ~ qnum + LAG2 + eff.level + eff.slope, data = datWSomeC)
summary(TS.AAM.WSomeC.mod3) # p = .004 / p = .301; R2 adjusted = 70.6%

# MODEL COMPARISON
compareLM(TS.AAM.WSomeC.mod1, TS.AAM.WSomeC.mod2, TS.AAM.WSomeC.mod3)

# PLOT MAIN MODEL
plot(x = 1:52, y = datWSomeC$AAMrate)
lines(TS.AAM.WSomeC.mod1$fitted.values)

# LIST 
rm(TS.AAM.WSomeC.mod0, TS.AAM.WSomeC.mod1, TS.AAM.WSomeC.mod2, TS.AAM.WSomeC.mod3)

# ----------------------------------------------------------------

# GROUP 6: WOMEN — COLLEGE

# BASELINE MODEL

# define effect
datWCollege <- datWCollege %>% mutate(eff.level = ifelse(QYEAR < 2018.125, F, T),
                                      eff.slope = ifelse(QYEAR <= 2018.125, 0, qnum - 45))

# DEFINE TIMESERIES
TS.AAM.WCollege <- ts(datWCollege$AAMrate, frequency = 4, start = c(2007,1))
plot.ts(TS.AAM.WCollege) + abline(v = 2018, lty = "dashed")

# DECOMPOSITION
TS.AAM.WCollege.decomp <- decompose(TS.AAM.WCollege)
plot(TS.AAM.WCollege.decomp) #seasonal pattern + linear trend

# SET BASELINE MODEL
TS.AAM.WCollege.mod0 <- gam(AAMrate ~ qnum + unemp.level + s(Q, bs="cc", k=4), 
                            data = datWCollege[datWCollege$eff.level == F,], method = "ML")
summary(TS.AAM.WCollege.mod0) # unemployment not significant

# drop unemp.rate
TS.AAM.WCollege.mod0 <- gam(AAMrate ~ qnum + s(Q, bs="cc", k=4), data = datWCollege[datWCollege$eff.level == F,], method = "ML")
summary(TS.AAM.WCollege.mod0) # smooth term not significant

# remove seasonal term
TS.AAM.WCollege.mod0 <- lm(AAMrate ~ qnum, data = datWCollege[datWCollege$eff.level == F,])
summary(TS.AAM.WCollege.mod0) 

# STATIONARITY
tseries::adf.test(TS.AAM.WCollege.mod0$residuals) # p = .13 -> non-stationary 
tseries::kpss.test(TS.AAM.WCollege.mod0$residuals, null="Trend") # p = .089 -> *trend* stationary 

# CHECK RESIDUALS
auto.arima(resid(TS.AAM.WCollege.mod0, type = "pearson"), stationary = T, seasonal = F) # ARIMA(0,0,0) -> ok

# AUTOCORRELATION
acf(TS.AAM.WCollege.mod0$residuals, main = "ACF") # no AR
pacf(TS.AAM.WCollege.mod0$residuals, main = "PACF") # no MA
Box.test(TS.AAM.WCollege.mod0$residuals, type = "Ljung-Box") # no autocorrelation p = .615

plot(x = 1:44, y = datWCollege$AAMrate[1:44])
lines(TS.AAM.WCollege.mod0$fitted.values)

# MAIN MODEL: INCLUDE INTERVENTION EFFECT

# LEVEL CHANGE
TS.AAM.WCollege.mod1 <- lm(AAMrate ~ qnum + eff.level, data = datWCollege)
summary(TS.AAM.WCollege.mod1) # p = .039 -> R2 adjusted = 40.6%

# SLOPE CHANGE
TS.AAM.WCollege.mod2 <- lm(AAMrate ~ qnum + eff.slope, data = datWCollege)
summary(TS.AAM.WCollege.mod2) # p = .737 

# LEVEL + SLOPE CHANGE
TS.AAM.WCollege.mod3 <- lm(AAMrate ~ qnum + eff.level + eff.slope, data = datWCollege)
summary(TS.AAM.WCollege.mod3) # p = .008 / p = .085 -> R2 adjusted = 43.1%

# MODEL COMPARISON
compareLM(TS.AAM.WCollege.mod1, TS.AAM.WCollege.mod2, TS.AAM.WCollege.mod3)

# PLOT MAIN MODEL: use model 3 as larger R2
plot(x = 1:52, y = datWCollege$AAMrate)
lines(TS.AAM.WCollege.mod3$fitted.values)

# LIST 
rm(TS.AAM.WCollege.mod0, TS.AAM.WCollege.mod1, TS.AAM.WCollege.mod2, TS.AAM.WCollege.mod3)


# --------------------------------------------------------------------------------------

# ------------------------------------------------------------------
# OUTCOME TIMESERIES ANALYSIS: 100% AAM CONDITIONS College vs. LEHS
# ------------------------------------------------------------------

# GENERATE NEW TIMESERIES: MEN — COLLEGE VS. LEHS
datMCL <- pdat %>% filter(edclass %like% "LEHS|College" & sex == 1) %>% 
  select(-c(EDCLASS)) %>%
  pivot_wider(names_from = edclass, values_from = AAMrate) %>%
  mutate(qnum = 1:nrow(.),
         AAMratio = College / LEHS) %>%
  filter(year < 2020)

# BASELINE MODEL

# define effect
datMCL <- datMCL %>% mutate(eff.level = ifelse(QYEAR < 2018.125, F, T), 
                            eff.slope = ifelse(QYEAR <= 2018.125, 0, qnum - 73))

# DEFINE TIMESERIES
TS.RATIO.CL <- ts(datMCL$AAMratio, frequency = 4, start = c(2000,1))
plot.ts(TS.RATIO.CL) + abline(v = 2018, lty = "dashed")

# DECOMPOSITION
TS.RATIO.CL.decomp <- decompose(TS.RATIO.CL)
plot(TS.RATIO.CL.decomp) #seasonal pattern + linear trend

# SET BASELINE MODEL
TS.RATIO.CL.mod0 <- gam(AAMratio ~ qnum + unemp.level + s(Q, bs="cc", k=4), data = datMCL[datMCL$eff.level == F,], method = "ML")
summary(TS.RATIO.CL.mod0) # significant smooth term -> seasonality

# drop unemp.rate
TS.RATIO.CL.mod0 <- gam(AAMratio ~ qnum + s(Q, bs="cc", k=4), data = datMCL[datMCL$eff.level == F,], method = "ML")
summary(TS.RATIO.CL.mod0) # smooth term significant with p = .054 -> keep for now 

# drop unemp.rate
TS.RATIO.CL.mod0 <- lm(AAMratio ~ qnum, data = datMCL[datMCL$eff.level == F,])
summary(TS.RATIO.CL.mod0) # smooth term significant with p = .054 -> keep for now 

# STATIONARITY
tseries::adf.test(datMCL$AAMratio) # p = .01 -> time series is stationary
tseries::kpss.test(datMCL$AAMratio, null="Trend") # p = .10 -> time series is *trend* stationary

# CHECK RESIDUALS
auto.arima(datMCL$AAMratio, stationary = T, seasonal = F) # ARIMA(0,0,0) -> ok

# AUTOCORRELATION
acf(datMCL$AAMratio, main = "ACF") # no AR
pacf(datMCL$AAMratio, main = "PACF") # no MA
Box.test(datMCL$AAMratio, type = "Ljung-Box") # no autocorrelation p = .561

plot(x = 1:72, y = datMCL$AAMratio[1:72])

# MAIN MODEL: INCLUDE INTERVENTION EFFECT

# LEVEL CHANGE
TS.RATIO.CL.mod1 <- lm(AAMratio ~ eff.level, data = datMCL)
summary(TS.RATIO.CL.mod1) # p > .5

# SLOPE CHANGE
TS.RATIO.CL.mod2 <- lm(AAMratio ~ eff.slope, data = datMCL)
summary(TS.RATIO.CL.mod2) # p > .5

# LEVEL + SLOPE CHANGE
TS.RATIO.CL.mod3 <- lm(AAMratio ~ eff.level + eff.slope, data = datMCL)
summary(TS.RATIO.CL.mod3) # p > .5

# PLOT MAIN MODEL
plot(x = 1:80, y = datMCL$AAMratio)

# LIST 
rm(TS.RATIO.CL.mod0, TS.RATIO.CL.mod1, TS.RATIO.CL.mod2, TS.RATIO.CL.mod3)

# ----------------------------------------------------------------

# GENERATE NEW TIMESERIES: WOMEN — COLLEGE VS. LEHS
datWCL <- pdat %>% filter(edclass %like% "LEHS|College" & sex == 2) %>% 
  select(-c(EDCLASS)) %>%
  pivot_wider(names_from = edclass, values_from = AAMrate) %>%
  mutate(qnum = 1:nrow(.),
         AAMratio = College / LEHS) %>%
  filter(year < 2020)

# BASELINE MODEL

# define effect
datWCL <- datWCL %>% mutate(eff.level = ifelse(QYEAR < 2018.125, F, T), 
                            eff.slope = ifelse(QYEAR <= 2018.125, 0, qnum - 73))

# DEFINE TIMESERIES
TS.RATIO.CL <- ts(datWCL$AAMratio, frequency = 4, start = c(2000,1))
plot.ts(TS.RATIO.CL) + abline(v = 2018, lty = "dashed")

# DECOMPOSITION
TS.RATIO.CL.decomp <- decompose(TS.RATIO.CL)
plot(TS.RATIO.CL.decomp) #seasonal pattern + linear trend

# SET BASELINE MODEL
TS.RATIO.CL.mod0 <- gam(AAMratio ~ qnum + unemp.level + s(Q, bs="cc", k=4), data = datWCL[datWCL$eff.level == F,], method = "ML")
summary(TS.RATIO.CL.mod0) # significant smooth term -> seasonality

# drop unemp.rate
TS.RATIO.CL.mod0 <- gam(AAMratio ~ qnum + s(Q, bs="cc", k=4), data = datWCL[datWCL$eff.level == F,], method = "ML")
summary(TS.RATIO.CL.mod0) # smooth term significant with p = .054 -> keep for now 

# drop unemp.rate
TS.RATIO.CL.mod0 <- lm(AAMratio ~ qnum, data = datWCL[datWCL$eff.level == F,])
summary(TS.RATIO.CL.mod0) # smooth term significant with p = .054 -> keep for now 

# STATIONARITY
tseries::adf.test(TS.RATIO.CL.mod0$residuals) # p = .01 -> time series is stationary
tseries::kpss.test(TS.RATIO.CL.mod0$residuals, null="Trend") # p = .10 -> time series is *trend* stationary

# CHECK RESIDUALS
auto.arima(datWCL$AAMratio, stationary = T, seasonal = F) # ARIMA(0,0,0) -> AR = 1

# AUTOCORRELATION
acf(datWCL$AAMratio, main = "ACF") # no AR
pacf(datWCL$AAMratio, main = "PACF") # no MA
Box.test(datWCL$AAMratio, type = "Ljung-Box") # no autocorrelation p = .073

plot(x = 1:72, y = datWCL$AAMratio[1:72])
lines(TS.RATIO.CL.mod0$fitted.values)

# MAIN MODEL: INCLUDE INTERVENTION EFFECT

# LEVEL CHANGE
TS.RATIO.CL.mod1 <- lm(AAMratio ~ qnum + eff.level, data = datWCL)
summary(TS.RATIO.CL.mod1) # p > .5

# SLOPE CHANGE
TS.RATIO.CL.mod2 <- lm(AAMratio ~ qnum + eff.slope, data = datWCL)
summary(TS.RATIO.CL.mod2) # p > .5

# LEVEL + SLOPE CHANGE
TS.RATIO.CL.mod3 <- lm(AAMratio ~ qnum + eff.level + eff.slope, data = datWCL)
summary(TS.RATIO.CL.mod3) # p > .5

# PLOT MAIN MODEL
plot(x = 1:80, y = datWCL$AAMratio)
lines(TS.RATIO.CL.mod1$fitted.values)

# LIST 
rm(TS.RATIO.CL.mod0, TS.RATIO.CL.mod1, TS.RATIO.CL.mod2, TS.RATIO.CL.mod3)

# ----------------------------------------------------------------

