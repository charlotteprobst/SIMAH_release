# ----------------------------------------------------------------
# ----------------------------------------------------------------
## Project: SIMAH  
## Title: Sunday sales ban  
## State: Minnesota
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
library(smooth)
library(DescTools)

# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# LOAD DATA
# ----------------------------------------------------------------

setwd("/Users/carolinkilian/Desktop/SIMAH_workplace/")
DATE <- 20230810

# prepared data file 
data <- read.csv("acp_brfss/data/20230712_AGEST_MORTALITY_MINNESOTA.csv")

# covariates
datUNEMP <- read.csv("acp_brfss/data/20230712_UNEMP_MINNESOTA.csv")

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
qqPlot(log(pdat$unemp.rate)) # ok

pdat <- pdat %>% mutate(z.unemp.rate = (log(unemp.rate) - mean(log(unemp.rate))) / sd(log(unemp.rate)))

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# SELECT DATA
# ----------------------------------------------------------------

datMLEHS <- pdat %>% filter(edclass == "LEHS" & sex == 1 & year < 2020) %>% mutate(qnum = 1:nrow(.))
datMSomeC <- pdat %>% filter(edclass == "SomeC" & sex == 1 & year < 2020) %>% mutate(qnum = 1:nrow(.))
datMCollege <- pdat %>% filter(edclass == "College" & sex == 1 & year < 2020) %>% mutate(qnum = 1:nrow(.))
datWLEHS <- pdat %>% filter(edclass == "LEHS" & sex == 2 & year < 2020) %>% mutate(qnum = 1:nrow(.))
datWSomeC <- pdat %>% filter(edclass == "SomeC" & sex == 2 & year < 2020) %>% mutate(qnum = 1:nrow(.))
datWCollege <- pdat %>% filter(edclass == "College" & sex == 2 & year < 2020) %>% mutate(qnum = 1:nrow(.))

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# OUTCOME TIMESERIES ANALYSIS: 100% AAM CONDITIONS BY GROUP
# ----------------------------------------------------------------

# GROUP 1: MEN / LEHRS

# BASELINE MODEL

# define effect
datMLEHS <- datMLEHS %>% mutate(eff.level = ifelse(QYEAR < 2017.625, F, T), 
                                eff.slope = ifelse(QYEAR <= 2017.625, 0, qnum - 71))

# DEFINE TIMESERIES
TS.AAM.MLEHS <- ts(datMLEHS$AAMrate, frequency = 4, start = c(2000,1))
plot.ts(TS.AAM.MLEHS) + abline(v = 2017, lty = "dashed")

# DECOMPOSITION
TS.AAM.MLEHS.decomp <- decompose(TS.AAM.MLEHS)
plot(TS.AAM.MLEHS.decomp) #seasonal pattern + linear trend

# SET BASELINE MODEL
TS.AAM.MLEHS.mod0 <- gam(AAMrate ~ qnum + z.unemp.rate + s(Q, bs="cc", k=4), data = datMLEHS[datMLEHS$eff.level == F,], method = "ML")
summary(TS.AAM.MLEHS.mod0) # smooth term not significant 

# drop smooth term
TS.AAM.MLEHS.mod0 <- lm(AAMrate ~ qnum + z.unemp.rate, data = datMLEHS[datMLEHS$eff.level == F,])
summary(TS.AAM.MLEHS.mod0) 

# STATIONARITY
tseries::adf.test(TS.AAM.MLEHS.mod0$residuals) # p = .38 -> time series is non-stationary
tseries::kpss.test(TS.AAM.MLEHS.mod0$residuals, null="Trend") # p = .06 -> time series is *trend* stationary

# include smooth term for annual trend
TS.AAM.MLEHS.mod0 <- gam(AAMrate ~ s(qnum, bs = "cs", k = -1) + z.unemp.rate, data = datMLEHS[datMLEHS$eff.level == F,], method = "ML")
summary(TS.AAM.MLEHS.mod0) # unemployment rate not significant

# drop unemployment rate
TS.AAM.MLEHS.mod0 <- gam(AAMrate ~ s(qnum, bs = "cs", k = -1), data = datMLEHS[datMLEHS$eff.level == F,], method = "ML")
summary(TS.AAM.MLEHS.mod0) 
k.check(TS.AAM.MLEHS.mod0) # k = 9

# STATIONARITY
tseries::adf.test(TS.AAM.MLEHS.mod0$residuals) # p = .03 -> time series is stationary
tseries::kpss.test(TS.AAM.MLEHS.mod0$residuals, null="Trend") # p = .1 -> time series is *trend* stationary

# CHECK RESIDUALS
auto.arima(resid(TS.AAM.MLEHS.mod0, type = "pearson"), stationary = T, seasonal = F) # ARIMA(0,0,0) -> ok

# AUTOCORRELATION
acf(TS.AAM.MLEHS.mod0$residuals, main = "ACF") # no AR
pacf(TS.AAM.MLEHS.mod0$residuals, main = "PACF") # no MA
Box.test(TS.AAM.MLEHS.mod0$residuals, type = "Ljung-Box") # no autocorrelation p = .1093

plot(x = 1:72, y = datMLEHS$AAMrate[1:72])
lines(TS.AAM.MLEHS.mod0$fitted.values)

# MAIN MODEL: INCLUDE INTERVENTION EFFECT

# LEVEL CHANGE
TS.AAM.MLEHS.mod1 <- gam(AAMrate ~ s(qnum, bs="cs", k=9) + eff.level, 
                         data = datMLEHS, method = "ML")
summary(TS.AAM.MLEHS.mod1) # p = .764

# SLOPE CHANGE
TS.AAM.MLEHS.mod2 <- gam(AAMrate ~ s(qnum, bs="cs", k=9) + eff.slope, 
                         data = datMLEHS, method = "ML")
summary(TS.AAM.MLEHS.mod2) # p = .716

# LEVEL + SLOPE CHANGE
TS.AAM.MLEHS.mod3 <- gam(AAMrate ~ s(qnum, bs="cs", k=9) + eff.level + eff.slope, 
                         data = datMLEHS, method = "ML")
summary(TS.AAM.MLEHS.mod3) # p = .894 / p = .807

# MODEL COMPARISON 
AIC(TS.AAM.MLEHS.mod1, TS.AAM.MLEHS.mod2, TS.AAM.MLEHS.mod3)
BIC(TS.AAM.MLEHS.mod1, TS.AAM.MLEHS.mod2, TS.AAM.MLEHS.mod3)

# FINAL MODEL: Model 2
out <- summary(TS.AAM.MLEHS.mod2)
out <- data.frame(out$p.table)
within(out, {uci <- Estimate + qnorm(0.975) * Std..Error
             lci <- Estimate - qnorm(0.975) * Std..Error})

plot(x = 1:80, y = datMLEHS$AAMrate)
lines(TS.AAM.MLEHS.mod1$fitted.values)

# INTERVENTION EFFECT
fit <- mean(TS.AAM.MLEHS.mod2$fitted.values[71:74])
pred.dat <- datMLEHS %>% mutate(eff.level = ifelse(eff.level == T, F, eff.level),
                                eff.slope = ifelse(eff.slope > 0, 0, eff.slope))
alt <- mean(predict.gam(TS.AAM.MLEHS.mod2, newdata = pred.dat)[71:74])
(fit - alt) / alt # -0.006%

# LIST 
rm(TS.AAM.MLEHS.mod0, TS.AAM.MLEHS.mod1, TS.AAM.MLEHS.mod2, TS.AAM.MLEHS.mod3)

# ----------------------------------------------------------------

# GROUP 2: MEN — SomeC

# BASELINE MODEL

# define effect
datMSomeC <- datMSomeC %>% mutate(eff.level = ifelse(QYEAR < 2017.625, F, T),
                                  eff.slope = ifelse(QYEAR <= 2017.625, 0, qnum - 71))

# DEFINE TIMESERIES
TS.AAM.MSomeC <- ts(datMSomeC$AAMrate, frequency = 4, start = c(2000,1))
plot.ts(TS.AAM.MSomeC) + abline(v = 2017, lty = "dashed")

# DECOMPOSITION
TS.AAM.MSomeC.decomp <- decompose(TS.AAM.MSomeC)
plot(TS.AAM.MSomeC.decomp) #seasonal pattern + non-linear trend

# SET BASELINE MODEL
TS.AAM.MSomeC.mod0 <- gam(AAMrate ~ qnum + z.unemp.rate + s(Q, bs="cc", k=4), 
                          data = datMSomeC[datMSomeC$eff.level == F,], method = "ML")
summary(TS.AAM.MSomeC.mod0) # smooth term not significant

# drop smooth term
TS.AAM.MSomeC.mod0 <- lm(AAMrate ~ qnum + z.unemp.rate, data = datMSomeC[datMSomeC$eff.level == F,])
summary(TS.AAM.MSomeC.mod0) 

# STATIONARITY
tseries::adf.test(TS.AAM.MSomeC.mod0$residuals) # p = .067 -> non-stationary 
tseries::kpss.test(TS.AAM.MSomeC.mod0$residuals, null="Trend") # p = .1 -> *trend* stationary 

# CHECK RESIDUALS
auto.arima(resid(TS.AAM.MSomeC.mod0, type = "pearson"), stationary = T, seasonal = F) # ARIMA(0,0,0) -> ok

# AUTOCORRELATION
acf(TS.AAM.MSomeC.mod0$residuals, main = "ACF") # no AR
pacf(TS.AAM.MSomeC.mod0$residuals, main = "PACF") # no MA
Box.test(TS.AAM.MSomeC.mod0$residuals, type = "Ljung-Box") # no autocorrelation p = .336

plot(x = 1:72, y = datMSomeC$AAMrate[1:72])
lines(TS.AAM.MSomeC.mod0$fitted.values)

# MAIN MODEL: INCLUDE INTERVENTION EFFECT

# LEVEL CHANGE
TS.AAM.MSomeC.mod1 <- lm(AAMrate ~ qnum + z.unemp.rate + eff.level, data = datMSomeC)
summary(TS.AAM.MSomeC.mod1) # p = .037

# SLOPE CHANGE
TS.AAM.MSomeC.mod2 <- lm(AAMrate ~ qnum + z.unemp.rate + eff.slope, data = datMSomeC)
summary(TS.AAM.MSomeC.mod2) # p = .059

# LEVEL + SLOPE CHANGE
TS.AAM.MSomeC.mod3 <- lm(AAMrate ~ qnum + z.unemp.rate + eff.level + eff.slope, data = datMSomeC)
summary(TS.AAM.MSomeC.mod3) # p = .289 / p = .551

# MODEL COMPARISON
compareLM(TS.AAM.MSomeC.mod1, TS.AAM.MSomeC.mod2, TS.AAM.MSomeC.mod3)

# FINAL MODEL: best fitting model 1 based on AIC, BIC, R2
out <- summary(TS.AAM.MSomeC.mod1)
out$coefficients[,1]
out$coefficients[,1] - qnorm(0.975) * out$coefficients[,2]
out$coefficients[,1] + qnorm(0.975) * out$coefficients[,2]

# PLOT FINAL MODEL
plot(x = 1:80, y = datMSomeC$AAMrate)
lines(TS.AAM.MSomeC.mod1$fitted.values)

# CHECK STATIONARITY AND ADDITIONAL AUTOCORRELATION
tseries::adf.test(TS.AAM.MSomeC.mod1$residuals) # p = .01 -> stationary 
tseries::kpss.test(TS.AAM.MSomeC.mod1$residuals, null="Trend") # p = .1 -> *trend* stationary 
Box.test(TS.AAM.MSomeC.mod1$residuals, type = "Ljung-Box") # no autocorrelation p = .5591

# INTERVENTION EFFECT
fit <- mean(TS.AAM.MSomeC.mod1$fitted.values[71:74])
pred.dat <- datMSomeC %>% mutate(eff.level = ifelse(eff.level == T, F, eff.level),
                                 eff.slope = ifelse(eff.slope > 0, 0, eff.slope))
alt <- mean(predict(TS.AAM.MSomeC.mod1, newdata = pred.dat)[71:74])
(fit - alt) / alt # +19.0%

# LIST 
rm(TS.AAM.MSomeC.mod0, TS.AAM.MSomeC.mod1, TS.AAM.MSomeC.mod2, TS.AAM.MSomeC.mod3)

# ----------------------------------------------------------------

# GROUP 3: MEN — COLLEGE

# BASELINE MODEL

# define effect
datMCollege <- datMCollege %>% mutate(eff.level = ifelse(QYEAR < 2017.625, F, T),
                                      eff.slope = ifelse(QYEAR <= 2017.625, 0, qnum - 71))

# DEFINE TIMESERIES
TS.AAM.MCollege <- ts(datMCollege$AAMrate, frequency = 4, start = c(2000,1))
plot.ts(TS.AAM.MCollege) + abline(v = 2017, lty = "dashed")

# DECOMPOSITION
TS.AAM.MCollege.decomp <- decompose(TS.AAM.MCollege)
plot(TS.AAM.MCollege.decomp) #seasonal pattern + possible non-linear trend

# SET BASELINE MODEL
TS.AAM.MCollege.mod0 <- gam(AAMrate ~ qnum + z.unemp.rate + s(Q, bs="cc", k=4), 
                            data = datMCollege[datMCollege$eff.level == F,], method = "ML")
summary(TS.AAM.MCollege.mod0) # unemployment not significant

# drop unemp.rate
TS.AAM.MCollege.mod0 <- gam(AAMrate ~ qnum + s(Q, bs="cc", k=4), data = datMCollege[datMCollege$eff.level == F,], method = "ML")
summary(TS.AAM.MCollege.mod0) # no significant time trend

# drop secular trend
TS.AAM.MCollege.mod0 <- gam(AAMrate ~ s(Q, bs="cc", k=4), data = datMCollege[datMCollege$eff.level == F,], method = "ML")
summary(TS.AAM.MCollege.mod0) # no annual trend

# STATIONARITY
tseries::adf.test(TS.AAM.MCollege.mod0$residuals) # p = .01 -> stationary 
tseries::kpss.test(TS.AAM.MCollege.mod0$residuals, null="Trend") # p = .1 -> *trend* stationary 

# CHECK RESIDUALS
auto.arima(resid(TS.AAM.MCollege.mod0, type = "pearson"), stationary = T, seasonal = F) # ARIMA(0,0,0) -> ok

# AUTOCORRELATION
acf(TS.AAM.MCollege.mod0$residuals, main = "ACF") # no AR
pacf(TS.AAM.MCollege.mod0$residuals, main = "PACF") # no MA
Box.test(TS.AAM.MCollege.mod0$residuals, type = "Ljung-Box") # no autocorrelation p = .329

plot(x = 1:72, y = datMCollege$AAMrate[1:72])
lines(TS.AAM.MCollege.mod0$fitted.values)

# MAIN MODEL: INCLUDE INTERVENTION EFFECT

# LEVEL CHANGE
TS.AAM.MCollege.mod1 <- gam(AAMrate ~ s(Q, bs="cc", k=4) + eff.level, data = datMCollege, method = "ML")
summary(TS.AAM.MCollege.mod1) # p < .001 -> R2 adjusted = 44.3%

# SLOPE CHANGE
TS.AAM.MCollege.mod2 <- gam(AAMrate ~ s(Q, bs="cc", k=4) + eff.slope, data = datMCollege, method = "ML")
summary(TS.AAM.MCollege.mod2) # p < .001 -> R2 adjusted = 40.3%

# LEVEL + SLOPE CHANGE
TS.AAM.MCollege.mod3 <- gam(AAMrate ~ s(Q, bs="cc", k=4) + eff.level + eff.slope, data = datMCollege, method = "ML")
summary(TS.AAM.MCollege.mod3) # p = .005 / p = .087 -> R2 adjusted = 45.8%

# MODEL COMPARISON
AIC(TS.AAM.MCollege.mod1, TS.AAM.MCollege.mod2, TS.AAM.MCollege.mod3)
BIC(TS.AAM.MCollege.mod1, TS.AAM.MCollege.mod2, TS.AAM.MCollege.mod3)

# FINAL MODEL: best fitting model 3 based AIC, R2
out <- summary(TS.AAM.MCollege.mod3)
out <- data.frame(out$p.table)
within(out, {uci <- Estimate + qnorm(0.975) * Std..Error
             lci <- Estimate - qnorm(0.975) * Std..Error})

# PLOT FINAL MODEL
plot(x = 1:80, y = datMCollege$AAMrate)
lines(TS.AAM.MCollege.mod3$fitted.values)

# CHECK STATIONARITY AND ADDITIONAL AUTOCORRELATION
tseries::adf.test(TS.AAM.MCollege.mod3$residuals) # p = .01 -> stationary 
tseries::kpss.test(TS.AAM.MCollege.mod3$residuals, null="Trend") # p = .1 -> *trend* stationary 
Box.test(TS.AAM.MCollege.mod3$residuals, type = "Ljung-Box") # no autocorrelation p = .3347

# INTERVENTION EFFECT
fit <- mean(TS.AAM.MCollege.mod3$fitted.values[71:74])
pred.dat <- datMCollege %>% mutate(eff.level = ifelse(eff.level == T, F, eff.level),
                                   eff.slope = ifelse(eff.slope > 0, 0, eff.slope))
alt <- mean(predict.gam(TS.AAM.MCollege.mod3, newdata = pred.dat)[71:74])
(fit - alt) / alt # +51.8%

# LIST 
rm(TS.AAM.MCollege.mod0, TS.AAM.MCollege.mod1, TS.AAM.MCollege.mod2, TS.AAM.MCollege.mod3)

# ----------------------------------------------------------------
# ----------------------------------------------------------------

# GROUP 4: WOMEN / LEHS

# BASELINE MODEL

# define effect
datWLEHS <- datWLEHS %>% mutate(eff.level = ifelse(QYEAR < 2017.625, F, T), 
                                eff.slope = ifelse(QYEAR <= 2017.625, 0, qnum - 71))

# DEFINE TIMESERIES
TS.AAM.WLEHS <- ts(datWLEHS$AAMrate, frequency = 4, start = c(2000,1))
plot.ts(TS.AAM.WLEHS) + abline(v = 2017, lty = "dashed")

# DECOMPOSITION
TS.AAM.WLEHS.decomp <- decompose(TS.AAM.WLEHS)
plot(TS.AAM.WLEHS.decomp) #seasonal pattern + linear trend

# SET BASELINE MODEL
TS.AAM.WLEHS.mod0 <- gam(AAMrate ~ qnum + z.unemp.rate + s(Q, bs="cc", k=4), data = datWLEHS[datWLEHS$eff.level == F,], method = "ML")
summary(TS.AAM.WLEHS.mod0) # unemployment not significant

# drop unemp.rate
TS.AAM.WLEHS.mod0 <- gam(AAMrate ~ qnum + s(Q, bs="cc", k=4), data = datWLEHS[datWLEHS$eff.level == F,], method = "ML")
summary(TS.AAM.WLEHS.mod0) # smooth term not significant

# drop smooth term
TS.AAM.WLEHS.mod0 <- lm(AAMrate ~ qnum, data = datWLEHS[datWLEHS$eff.level == F,])
summary(TS.AAM.WLEHS.mod0) # smooth term not significant

# STATIONARITY
tseries::adf.test(TS.AAM.WLEHS.mod0$residuals) # p = .03 -> time series is stationary
tseries::kpss.test(TS.AAM.WLEHS.mod0$residuals, null="Trend") # p = .10 -> time series is *trend* stationary

# CHECK RESIDUALS
auto.arima(resid(TS.AAM.WLEHS.mod0, type = "pearson"), stationary = T, seasonal = F) # ARIMA(0,0,2) -> MA = 2?

# AUTOCORRELATION
acf(TS.AAM.WLEHS.mod0$residuals, main = "ACF") # ok 
pacf(TS.AAM.WLEHS.mod0$residuals, main = "PACF") # ok
Box.test(TS.AAM.WLEHS.mod0$residuals, type = "Ljung-Box") # no autocorrelation p = .189 -> no autocorrelation adjustment

plot(x = 1:72, y = datWLEHS$AAMrate[1:72])
lines(TS.AAM.WLEHS.mod0$fitted.values)

# MAIN MODEL: INCLUDE INTERVENTION EFFECT

# LEVEL CHANGE
TS.AAM.WLEHS.mod1 <- lm(AAMrate ~ qnum + + eff.level, data = datWLEHS)
summary(TS.AAM.WLEHS.mod1) # p = .005 -> R2 adjusted = 69.2%

# SLOPE CHANGE
TS.AAM.WLEHS.mod2 <- lm(AAMrate ~ qnum + + eff.slope, data = datWLEHS)
summary(TS.AAM.WLEHS.mod2) # p < .001 -> R2 adjusted = 74.8%

# LEVEL + SLOPE CHANGE
TS.AAM.WLEHS.mod3 <- lm(AAMrate ~ qnum + + eff.level + eff.slope, data = datWLEHS)
summary(TS.AAM.WLEHS.mod3) # p = .222 / p < .001 -> R2 adjusted = 74.9% 

# MODEL COMPARISON
compareLM(TS.AAM.WLEHS.mod1, TS.AAM.WLEHS.mod2, TS.AAM.WLEHS.mod3)

# FINAL MODEL: : best fitting model 2 based on AIC, BIC
out <- summary(TS.AAM.WLEHS.mod2)
out$coefficients[,1]
out$coefficients[,1] - qnorm(0.975) * out$coefficients[,2]
out$coefficients[,1] + qnorm(0.975) * out$coefficients[,2]

# PLOT FINAL MODEL
plot(x = 1:80, y = datWLEHS$AAMrate)
lines(TS.AAM.WLEHS.mod2$fitted.values)

# CHECK STATIONARITY AND ADDITIONAL AUTOCORRELATION
tseries::adf.test(TS.AAM.WLEHS.mod2$residuals) # p = .01 -> stationary 
tseries::kpss.test(TS.AAM.WLEHS.mod2$residuals, null="Trend") # p = .1 -> *trend* stationary 
Box.test(TS.AAM.WLEHS.mod2$residuals, type = "Ljung-Box") # no autocorrelation p = .3109

# INTERVENTION EFFECT 
fit <- mean(TS.AAM.WLEHS.mod2$fitted.values[71:74])
pred.dat <- datWLEHS %>% mutate(eff.level = ifelse(eff.level == T, F, eff.level),
                                eff.slope = ifelse(eff.slope > 0, 0, eff.slope))
alt <- mean(predict(TS.AAM.WLEHS.mod2, newdata = pred.dat)[71:74])
(fit - alt) / alt # +9.5%

# LIST 
rm(TS.AAM.WLEHS.mod0, TS.AAM.WLEHS.mod1, TS.AAM.WLEHS.mod2, TS.AAM.WLEHS.mod3)

# ----------------------------------------------------------------

# GROUP 5: WOMEN — SomeC

# BASELINE MODEL

# define effect
datWSomeC <- datWSomeC %>% mutate(eff.level = ifelse(QYEAR < 2017.625, F, T),
                                  eff.slope = ifelse(QYEAR <= 2017.625, 0, qnum - 71))

# DEFINE TIMESERIES
TS.AAM.WSomeC <- ts(datWSomeC$AAMrate, frequency = 4, start = c(2000,1))
plot.ts(TS.AAM.WSomeC) + abline(v = 2017, lty = "dashed")

# DECOMPOSITION
TS.AAM.WSomeC.decomp <- decompose(TS.AAM.WSomeC)
plot(TS.AAM.WSomeC.decomp) #seasonal pattern + linear trend

# SET BASELINE MODEL
TS.AAM.WSomeC.mod0 <- gam(AAMrate ~ qnum + z.unemp.rate + s(Q, bs="cc", k=4), 
                          data = datWSomeC[datWSomeC$eff.level == F,], method = "ML")
summary(TS.AAM.WSomeC.mod0) # unemployment not significant

# drop unemp.rate
TS.AAM.WSomeC.mod0 <- gam(AAMrate ~ qnum + s(Q, bs="cc", k=4), data = datWSomeC[datWSomeC$eff.level == F,], method = "ML")
summary(TS.AAM.WSomeC.mod0) # smooth term not significant

# include spline smooth function
TS.AAM.WSomeC.mod0 <- lm(AAMrate ~ qnum, data = datWSomeC[datWSomeC$eff.level == F,])
summary(TS.AAM.WSomeC.mod0) 

# STATIONARITY
tseries::adf.test(TS.AAM.WSomeC.mod0$residuals) # p = .01 -> stationary 
tseries::kpss.test(TS.AAM.WSomeC.mod0$residuals, null="Trend") # p = .1 -> *trend* stationary 

# CHECK RESIDUALS
auto.arima(resid(TS.AAM.WSomeC.mod0, type = "pearson"), stationary = T, seasonal = F) # ARIMA(0,0,0)

# AUTOCORRELATION
acf(TS.AAM.WSomeC.mod0$residuals, main = "ACF") # ok, though there is a periodical pattern in early years
pacf(TS.AAM.WSomeC.mod0$residuals, main = "PACF") # ok
Box.test(TS.AAM.WSomeC.mod0$residuals, type = "Ljung-Box") # no autocorrelation p = .271
 
plot(x = 1:72, y = datWSomeC$AAMrate[1:72])
lines(TS.AAM.WSomeC.mod0$fitted.values)

# MAIN MODEL: INCLUDE INTERVENTION EFFECT

# LEVEL CHANGE
TS.AAM.WSomeC.mod1 <- lm(AAMrate ~ qnum + eff.level, data = datWSomeC)
summary(TS.AAM.WSomeC.mod1) # p = .001; R2 adjusted = 50.5%

# SLOPE CHANGE
TS.AAM.WSomeC.mod2 <- lm(AAMrate ~ qnum + eff.slope, data = datWSomeC)
summary(TS.AAM.WSomeC.mod2) # p < .001; R2 adjusted = 55.2%

# LEVEL + SLOPE CHANGE
TS.AAM.WSomeC.mod3 <- lm(AAMrate ~ qnum + eff.level + eff.slope, data = datWSomeC)
summary(TS.AAM.WSomeC.mod3) # p = .893 / p = .005; R2 adjusted = 50.6%

# MODEL COMPARISON
compareLM(TS.AAM.WSomeC.mod1, TS.AAM.WSomeC.mod2, TS.AAM.WSomeC.mod3)

# FINAL MODELL: best fitting model 2 based on AIC, BIC and R2
out <- summary(TS.AAM.WSomeC.mod2)
out$coefficients[,1]
out$coefficients[,1] - qnorm(0.975) * out$coefficients[,2]
out$coefficients[,1] + qnorm(0.975) * out$coefficients[,2]

# PLOT MAIN MODE
plot(x = 1:80, y = datWSomeC$AAMrate)
lines(TS.AAM.WSomeC.mod2$fitted.values)

# CHECK STATIONARITY AND ADDITIONAL AUTOCORRELATION
tseries::adf.test(TS.AAM.WSomeC.mod2$residuals) # p = .01 -> stationary 
tseries::kpss.test(TS.AAM.WSomeC.mod2$residuals, null="Trend") # p = .1 -> *trend* stationary 
Box.test(TS.AAM.WSomeC.mod2$residuals, type = "Ljung-Box") # no autocorrelation p = .4347

# INTERVENTION EFFECT
fit <- mean(TS.AAM.WSomeC.mod2$fitted.values[71:74])
pred.dat <- datWSomeC %>% mutate(eff.level = ifelse(eff.level == T, F, eff.level),
                                 eff.slope = ifelse(eff.slope > 0, 0, eff.slope))
alt <- mean(predict(TS.AAM.WSomeC.mod2, newdata = pred.dat)[71:74])
(fit - alt) / alt # +11.4%

# LIST 
rm(TS.AAM.WSomeC.mod0, TS.AAM.WSomeC.mod1, TS.AAM.WSomeC.mod2, TS.AAM.WSomeC.mod3)

# ----------------------------------------------------------------

# GROUP 6: WOMEN — COLLEGE

# BASELINE MODEL

# define effect
datWCollege <- datWCollege %>% mutate(eff.level = ifelse(QYEAR < 2017.625, F, T),
                                      eff.slope = ifelse(QYEAR <= 2017.625, 0, qnum - 71))

# DEFINE TIMESERIES
TS.AAM.WCollege <- ts(datWCollege$AAMrate, frequency = 4, start = c(2000,1))
plot.ts(TS.AAM.WCollege) + abline(v = 2017, lty = "dashed")

# DECOMPOSITION
TS.AAM.WCollege.decomp <- decompose(TS.AAM.WCollege)
plot(TS.AAM.WCollege.decomp) #seasonal pattern + linear trend

# SET BASELINE MODEL
TS.AAM.WCollege.mod0 <- gam(AAMrate ~ qnum + z.unemp.rate + s(Q, bs="cc", k=4), 
                            data = datWCollege[datWCollege$eff.level == F,], method = "ML")
summary(TS.AAM.WCollege.mod0) # smooth term not significant

# drop smooth term
TS.AAM.WCollege.mod0 <- lmm(AAMrate ~ qnum + z.unemp.rate, data = datWCollege[datWCollege$eff.level == F,])
summary(TS.AAM.WCollege.mod0) # unemployment rate not significant

# drop unemployment rate
TS.AAM.WCollege.mod0 <- lm(AAMrate ~ qnum, data = datWCollege[datWCollege$eff.level == F,])
summary(TS.AAM.WCollege.mod0) # unemployment rate not significant

# STATIONARITY
tseries::adf.test(TS.AAM.WCollege.mod0$residuals) # p = .026 -> non-stationary 
tseries::kpss.test(TS.AAM.WCollege.mod0$residuals, null="Trend") # p = .069 -> *trend* stationary 

# CHECK RESIDUALS
auto.arima(resid(TS.AAM.WCollege.mod0, type = "pearson"), stationary = T, seasonal = F) # ARIMA(0,0,0) -> ok

# AUTOCORRELATION
acf(TS.AAM.WCollege.mod0$residuals, main = "ACF") # no AR
pacf(TS.AAM.WCollege.mod0$residuals, main = "PACF") # no MA
Box.test(TS.AAM.WCollege.mod0$residuals, type = "Ljung-Box") # no autocorrelation p = .6232

plot(x = 1:72, y = datWCollege$AAMrate[1:72])
lines(TS.AAM.WCollege.mod0$fitted.values)

# MAIN MODEL: INCLUDE INTERVENTION EFFECT

# LEVEL CHANGE
TS.AAM.WCollege.mod1 <- lm(AAMrate ~ qnum + eff.level, data = datWCollege)
summary(TS.AAM.WCollege.mod1) # p = .007 -> R2 adjusted = 23.3%

# SLOPE CHANGE
TS.AAM.WCollege.mod2 <- lm(AAMrate ~ qnum + eff.slope, data = datWCollege)
summary(TS.AAM.WCollege.mod2) # p = .011 -> R2 adjusted = 22.6% 

# LEVEL + SLOPE CHANGE
TS.AAM.WCollege.mod3 <- lm(AAMrate ~ qnum + eff.level + eff.slope, data = datWCollege)
summary(TS.AAM.WCollege.mod3) # p = .246 / p = .422 -> R2 adjusted = 22.9%

# MODEL COMPARISON
compareLM(TS.AAM.WCollege.mod1, TS.AAM.WCollege.mod2, TS.AAM.WCollege.mod3)

# FINAL MODELL: use model 1 based on AIC / BIC / R2
out <- summary(TS.AAM.WCollege.mod1)
out$coefficients[,1]
out$coefficients[,1] - qnorm(0.975) * out$coefficients[,2]
out$coefficients[,1] + qnorm(0.975) * out$coefficients[,2]

# PLOT MAIN MODEL
plot(x = 1:80, y = datWCollege$AAMrate)
lines(TS.AAM.WCollege.mod1$fitted.values)

# CHECK STATIONARITY AND ADDITIONAL AUTOCORRELATION
tseries::adf.test(TS.AAM.WCollege.mod1$residuals) # p = .01 -> stationary 
tseries::kpss.test(TS.AAM.WCollege.mod1$residuals, null="Trend") # p = .1 -> *trend* stationary 
Box.test(TS.AAM.WCollege.mod1$residuals, type = "Ljung-Box") # no autocorrelation p = .4347

# INTERVENTION EFFECT
fit <- mean(TS.AAM.WCollege.mod1$fitted.values[71:74])
pred.dat <- datWCollege %>% mutate(eff.level = ifelse(eff.level == T, F, eff.level),
                                   eff.slope = ifelse(eff.slope > 0, 0, eff.slope))
alt <- mean(predict(TS.AAM.WCollege.mod1, newdata = pred.dat)[71:74])
(fit - alt) / alt # +49.38%

# LIST 
rm(TS.AAM.WCollege.mod0, TS.AAM.WCollege.mod1, TS.AAM.WCollege.mod2, TS.AAM.WCollege.mod3)


# --------------------------------------------------------------------------------------

# ------------------------------------------------------------------
# OUTCOME TIMESERIES ANALYSIS: RATIO HIGH VERSUS LOW SES
# ------------------------------------------------------------------

# GENERATE NEW TIMESERIES: MEN — COLLEGE VS. LEHS
datMCL <- pdat %>% filter(edclass %like% "LEHS|College" & sex == 1) %>% 
  select(-c(EDCLASS)) %>%
  pivot_wider(names_from = edclass, values_from = AAMrate) %>%
  mutate(qnum = 1:nrow(.),
         AAMratio = LEHS / College) %>%
  filter(year < 2020)

# BASELINE MODEL

# define effect
datMCL <- datMCL %>% mutate(eff.level = ifelse(QYEAR < 2017.625, F, T), 
                            eff.slope = ifelse(QYEAR <= 2017.625, 0, qnum - 71))

# DEFINE TIMESERIES
TS.RATIO.CL <- ts(datMCL$AAMratio, frequency = 4, start = c(2000,1))
plot.ts(TS.RATIO.CL) + abline(v = 2017, lty = "dashed")

# DECOMPOSITION
TS.RATIO.CL.decomp <- decompose(TS.RATIO.CL)
plot(TS.RATIO.CL.decomp) #seasonal pattern + linear trend?

# SET BASELINE MODEL
TS.RATIO.CL.mod0 <- gam(AAMratio ~ qnum + z.unemp.rate + s(Q, bs="cc", k=4), data = datMCL[datMCL$eff.level == F,], method = "ML")
summary(TS.RATIO.CL.mod0) # significant smooth term -> seasonality

# drop unemp.rate
TS.RATIO.CL.mod0 <- gam(AAMratio ~ qnum + s(Q, bs="cc", k=4), data = datMCL[datMCL$eff.level == F,], method = "ML")
summary(TS.RATIO.CL.mod0) # smooth term not significant

# drop smooth term
TS.RATIO.CL.mod0 <- lm(AAMratio ~ qnum, data = datMCL[datMCL$eff.level == F,])
summary(TS.RATIO.CL.mod0) 

# STATIONARITY
tseries::adf.test(TS.RATIO.CL.mod0$residuals) # p = .01 -> time series is stationary
tseries::kpss.test(TS.RATIO.CL.mod0$residuals, null="Trend") # p = .10 -> time series is *trend* stationary

# CHECK RESIDUALS
auto.arima(TS.RATIO.CL.mod0$residuals, stationary = T, seasonal = F) # ARIMA(0,0,0) -> ok

# AUTOCORRELATION
acf(TS.RATIO.CL.mod0$residuals, main = "ACF") # ok
pacf(TS.RATIO.CL.mod0$residuals, main = "PACF") # ok
Box.test(TS.RATIO.CL.mod0$residuals, type = "Ljung-Box") # no autocorrelation p = .810

plot(x = 1:72, y = datMCL$AAMratio[1:72])
lines(TS.RATIO.CL.mod0$fitted.values)

# MAIN MODEL: INCLUDE INTERVENTION EFFECT

# LEVEL CHANGE
TS.RATIO.CL.mod1 <- lm(AAMratio ~ qnum + eff.level, data = datMCL)
summary(TS.RATIO.CL.mod1) # p = .011; R2 adjusted = 6.6%

# SLOPE CHANGE
TS.RATIO.CL.mod2 <- lm(AAMratio ~ qnum + eff.slope, data = datMCL)
summary(TS.RATIO.CL.mod2) # p = .030; R2 adjusted = 4.4%

# LEVEL + SLOPE CHANGE
TS.RATIO.CL.mod3 <- lm(AAMratio ~ qnum + eff.level + eff.slope, data = datMCL)
summary(TS.RATIO.CL.mod3) # p = .171 / p = .594; R2 adjusted = 5.6%

# MODEL COMPARISON
compareLM(TS.RATIO.CL.mod1, TS.RATIO.CL.mod2, TS.RATIO.CL.mod3)

# PLOT MAIN MODEL: best-fitting model 1 based on AIC, BIC and R
plot(x = 1:80, y = datMCL$AAMratio)
lines(TS.RATIO.CL.mod1$fitted.values)

# CHECK STATIONARITY AND ADDITIONAL AUTOCORRELATION
tseries::adf.test(TS.RATIO.CL.mod1$residuals) # p = .01 -> stationary 
tseries::kpss.test(TS.RATIO.CL.mod1$residuals, null="Trend") # p = .1 -> *trend* stationary 
Box.test(TS.RATIO.CL.mod1$residuals, type = "Ljung-Box") # no autocorrelation p = .8209

# PREDICT AAM WITHOUT INTERVENTION (pre: 67-70, post: 71-74)
fit <- mean(TS.RATIO.CL.mod1$fitted.values[71:74])
pred.dat <- datMCL %>% mutate(eff.level = ifelse(eff.level == T, F, eff.level),
                              eff.slope = ifelse(eff.slope > 0, 0, eff.slope))
alt <- mean(predict(TS.RATIO.CL.mod1, newdata = pred.dat)[71:74])
(fit - alt) / alt # -32.3%

# LIST 
rm(TS.RATIO.CL.mod0, TS.RATIO.CL.mod1, TS.RATIO.CL.mod2, TS.RATIO.CL.mod3)

# ----------------------------------------------------------------

# GENERATE NEW TIMESERIES: MEN — COLLEGE VS. SomeC
datMCSC <- pdat %>% filter(edclass %like% "SomeC|College" & sex == 1) %>% 
  select(-c(EDCLASS)) %>%
  pivot_wider(names_from = edclass, values_from = AAMrate) %>%
  mutate(qnum = 1:nrow(.),
         AAMratio = SomeC / College) %>%
  filter(year < 2020)

# BASELINE MODEL

# define effect
datMCSC <- datMCSC %>% mutate(eff.level = ifelse(QYEAR < 2017.625, F, T), 
                            eff.slope = ifelse(QYEAR <= 2017.625, 0, qnum - 71))

# DEFINE TIMESERIES
TS.RATIO.CL <- ts(datMCSC$AAMratio, frequency = 4, start = c(2000,1))
plot.ts(TS.RATIO.CL) + abline(v = 2017, lty = "dashed")

# DECOMPOSITION
TS.RATIO.CL.decomp <- decompose(TS.RATIO.CL)
plot(TS.RATIO.CL.decomp) #seasonal pattern + linear trend?

# SET BASELINE MODEL
TS.RATIO.CL.mod0 <- gam(AAMratio ~ qnum + z.unemp.rate + s(Q, bs="cc", k=4), data = datMCSC[datMCSC$eff.level == F,], method = "ML")
summary(TS.RATIO.CL.mod0) # significant smooth term -> seasonality

# drop unemp.rate
TS.RATIO.CL.mod0 <- gam(AAMratio ~ qnum + s(Q, bs="cc", k=4), data = datMCSC[datMCSC$eff.level == F,], method = "ML")
summary(TS.RATIO.CL.mod0) # smooth term not significant

# drop smooth term
TS.RATIO.CL.mod0 <- lm(AAMratio ~ qnum, data = datMCSC[datMCSC$eff.level == F,])
summary(TS.RATIO.CL.mod0) # p = .06

# STATIONARITY
tseries::adf.test(TS.RATIO.CL.mod0$residuals) # p = .017 -> time series is stationary
tseries::kpss.test(TS.RATIO.CL.mod0$residuals, null="Trend") # p = .10 -> time series is *trend* stationary

# CHECK RESIDUALS
auto.arima(TS.RATIO.CL.mod0$residuals, stationary = T, seasonal = F) # ARIMA(0,0,0) -> ok

# AUTOCORRELATION
acf(TS.RATIO.CL.mod0$residuals, main = "ACF") # ok
pacf(TS.RATIO.CL.mod0$residuals, main = "PACF") # ok
Box.test(TS.RATIO.CL.mod0$residuals, type = "Ljung-Box") # no autocorrelation p = 354

plot(x = 1:72, y = datMCSC$AAMratio[1:72])

# MAIN MODEL: INCLUDE INTERVENTION EFFECT

# LEVEL CHANGE
TS.RATIO.CL.mod1 <- lm(AAMratio ~ eff.level, data = datMCSC)
summary(TS.RATIO.CL.mod1) # p = .763

# SLOPE CHANGE
TS.RATIO.CL.mod2 <- lm(AAMratio ~ eff.slope, data = datMCSC)
summary(TS.RATIO.CL.mod2) # p = .744

# LEVEL + SLOPE CHANGE
TS.RATIO.CL.mod3 <- lm(AAMratio ~ eff.level + eff.slope, data = datMCSC)
summary(TS.RATIO.CL.mod3) # p = .955 / p = .892

# LIST 
rm(TS.RATIO.CL.mod0, TS.RATIO.CL.mod1, TS.RATIO.CL.mod2, TS.RATIO.CL.mod3)

# ----------------------------------------------------------------

# GENERATE NEW TIMESERIES: WOMEN — COLLEGE VS. LEHS
datWCL <- pdat %>% filter(edclass %like% "LEHS|College" & sex == 2) %>% 
  select(-c(EDCLASS)) %>%
  pivot_wider(names_from = edclass, values_from = AAMrate) %>%
  mutate(qnum = 1:nrow(.),
         AAMratio = LEHS / College) %>%
  filter(year < 2020)

# BASELINE MODEL

# define effect
datWCL <- datWCL %>% mutate(eff.level = ifelse(QYEAR < 2017.625, F, T), 
                            eff.slope = ifelse(QYEAR <= 2017.625, 0, qnum - 71))

# DEFINE TIMESERIES
TS.RATIO.CL <- ts(datWCL$AAMratio, frequency = 4, start = c(2000,1))
plot.ts(TS.RATIO.CL) + abline(v = 2017, lty = "dashed")

# DECOMPOSITION
TS.RATIO.CL.decomp <- decompose(TS.RATIO.CL)
plot(TS.RATIO.CL.decomp) #seasonal pattern + maybe not linear trend

# SET BASELINE MODEL
TS.RATIO.CL.mod0 <- gam(AAMratio ~ qnum + z.unemp.rate + s(Q, bs="cc", k=4), data = datWCL[datWCL$eff.level == F,], method = "ML")
summary(TS.RATIO.CL.mod0) # significant smooth term -> seasonality

# drop unemp.rate
TS.RATIO.CL.mod0 <- gam(AAMratio ~ qnum + s(Q, bs="cc", k=4), data = datWCL[datWCL$eff.level == F,], method = "ML")
summary(TS.RATIO.CL.mod0) # smooth term not significant 

# drop smooth term
TS.RATIO.CL.mod0 <- lm(AAMratio ~ qnum, data = datWCL[datWCL$eff.level == F,])
summary(TS.RATIO.CL.mod0) 

# STATIONARITY
tseries::adf.test(TS.RATIO.CL.mod0$residuals) # p = .06 -> time series is not stationary
tseries::kpss.test(TS.RATIO.CL.mod0$residuals, null="Trend") # p = .10 -> time series is *trend* stationary

TS.RATIO.CL.mod0 <- gam(AAMratio ~ s(qnum, bs = "cs", k = -1), data = datWCL[datWCL$eff.level == F,], method = "ML")
summary(TS.RATIO.CL.mod0) 
k.check(TS.RATIO.CL.mod0)

# STATIONARITY
tseries::adf.test(TS.RATIO.CL.mod0$residuals) # p = .049 -> stationary
tseries::kpss.test(TS.RATIO.CL.mod0$residuals, null="Trend") # p = .10 -> time series is *trend* stationary

# CHECK RESIDUALS
auto.arima(TS.RATIO.CL.mod0$residuals, stationary = T, seasonal = F) # ARIMA(0,0,0) 

# AUTOCORRELATION
acf(TS.RATIO.CL.mod0$residuals, main = "ACF") # ok
pacf(TS.RATIO.CL.mod0$residuals, main = "PACF") # ok
Box.test(TS.RATIO.CL.mod0$residuals, type = "Ljung-Box") # no autocorrelation p = .296

plot(x = 1:72, y = datWCL$AAMratio[1:72])
lines(TS.RATIO.CL.mod0$fitted.values)

# MAIN MODEL: INCLUDE INTERVENTION EFFECT

# LEVEL CHANGE
TS.RATIO.CL.mod1 <- gam(AAMratio ~ s(qnum, bs = "cs", k = -1) + eff.level, data = datWCL, method = "ML")
summary(TS.RATIO.CL.mod1) # p > .05

# SLOPE CHANGE
TS.RATIO.CL.mod2 <- gam(AAMratio ~ s(qnum, bs = "cs", k = -1) + eff.slope, data = datWCL, method = "ML")
summary(TS.RATIO.CL.mod2) # p > .05

# LEVEL + SLOPE CHANGE
TS.RATIO.CL.mod3 <- gam(AAMratio ~ s(qnum, bs = "cs", k = -1) + eff.level + eff.slope, data = datWCL, method = "ML")
summary(TS.RATIO.CL.mod3) # p > .05

# PLOT MAIN MODEL
TS.RATIO.CL.mod0 <- gam(AAMratio ~ s(qnum, bs = "cs", k = -1), data = datWCL, method = "ML")
summary(TS.RATIO.CL.mod0) 

plot(x = 1:80, y = datWCL$AAMratio)
lines(TS.RATIO.CL.mod0$fitted.values)

# CHECK STATIONARITY AND ADDITIONAL AUTOCORRELATION
tseries::adf.test(TS.RATIO.CL.mod0$residuals) # p = .02 -> stationary 
tseries::kpss.test(TS.RATIO.CL.mod0$residuals, null="Trend") # p = .1 -> *trend* stationary 
Box.test(TS.RATIO.CL.mod0$residuals, type = "Ljung-Box") # no autocorrelation p = 311

# LIST 
rm(TS.RATIO.CL.mod0, TS.RATIO.CL.mod1, TS.RATIO.CL.mod2, TS.RATIO.CL.mod3)

# ----------------------------------------------------------------

# GENERATE NEW TIMESERIES: WOMEN — COLLEGE VS. SomeC
datWCSC <- pdat %>% filter(edclass %like% "SomeC|College" & sex == 2) %>% 
  select(-c(EDCLASS)) %>%
  pivot_wider(names_from = edclass, values_from = AAMrate) %>%
  mutate(qnum = 1:nrow(.),
         AAMratio = SomeC / College) %>%
  filter(year < 2020)

# BASELINE MODEL

# define effect
datWCSC <- datWCSC %>% mutate(eff.level = ifelse(QYEAR < 2017.625, F, T), 
                            eff.slope = ifelse(QYEAR <= 2017.625, 0, qnum - 71))

# DEFINE TIMESERIES
TS.RATIO.CSC <- ts(datWCSC$AAMratio, frequency = 4, start = c(2000,1))
plot.ts(TS.RATIO.CSC) + abline(v = 2017, lty = "dashed")

# DECOMPOSITION
TS.RATIO.CSC.decomp <- decompose(TS.RATIO.CSC)
plot(TS.RATIO.CSC.decomp) #seasonal pattern + maybe not linear trend

# SET BASELINE MODEL
TS.RATIO.CSC.mod0 <- gam(AAMratio ~ qnum + z.unemp.rate + s(Q, bs="cc", k=4), data = datWCSC[datWCSC$eff.level == F,], method = "ML")
summary(TS.RATIO.CSC.mod0) # drop time trend

# drop unemp.rate
TS.RATIO.CSC.mod0 <- gam(AAMratio ~ z.unemp.rate + s(Q, bs="cc", k=4), data = datWCSC[datWCSC$eff.level == F,], method = "ML")
summary(TS.RATIO.CSC.mod0) # smooth term not significant 

# drop smooth term
TS.RATIO.CSC.mod0 <- lm(AAMratio ~ z.unemp.rate, data = datWCSC[datWCSC$eff.level == F,])
summary(TS.RATIO.CSC.mod0) 

# STATIONARITY
tseries::adf.test(datWCSC$AAMratio) # p = .01 -> time series is stationary
tseries::kpss.test(datWCSC$AAMratio, null="Trend") # p = .10 -> time series is *trend* stationary

# CHECK RESIDUALS
auto.arima(datWCSC$AAMratio, stationary = T, seasonal = F) # ARIMA(0,0,0) 

# AUTOCORRELATION
acf(datWCSC$AAMratio, main = "ACF") # no AR
pacf(datWCSC$AAMratio, main = "PACF") # no MA
Box.test(datWCSC$AAMratio, type = "Ljung-Box") # no autocorrelation p = 6432

plot(x = 1:72, y = datWCSC$AAMratio[1:72])

# MAIN MODEL: INCLUDE INTERVENTION EFFECT

# LEVEL CHANGE
TS.RATIO.CSC.mod1 <- lm(AAMratio ~ eff.level, data = datWCSC)
summary(TS.RATIO.CSC.mod1) # p > .05

# SLOPE CHANGE
TS.RATIO.CSC.mod2 <- lm(AAMratio ~ eff.slope, data = datWCSC)
summary(TS.RATIO.CSC.mod2) # p > .05

# LEVEL + SLOPE CHANGE
TS.RATIO.CSC.mod3 <- lm(AAMratio ~ eff.level + eff.slope, data = datWCSC)
summary(TS.RATIO.CSC.mod3) # p > .05

# PLOT MAIN MODEL
plot(x = 1:80, y = datWCSC$AAMratio)

# CHECK STATIONARITY AND ADDITIONAL AUTOCORRELATION
tseries::adf.test(datWCSC$AAMratio) # p = .01 -> stationary 
tseries::kpss.test(datWCSC$AAMratio, null="Trend") # p = .1 -> *trend* stationary 
Box.test(datWCSC$AAMratio, type = "Ljung-Box") # no autocorrelation p = .6432

# LIST 
rm(TS.RATIO.CSC.mod0, TS.RATIO.CSC.mod1, TS.RATIO.CSC.mod2, TS.RATIO.CSC.mod3)

# ----------------------------------------------------------------

