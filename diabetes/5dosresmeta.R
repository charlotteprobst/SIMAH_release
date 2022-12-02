library(dosresmeta)
library(tidyverse)
library(ggplot2)

library(readxl)
dataset <- read_excel("CAMH/DIABETES/analysis/SIMAH_workplace/5dosresmeta.xlsx", 
                      col_types = c("numeric","numeric", "numeric", "text", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric"))

####MALE

#LINEAR DOSE-RESPONSE

male <- dataset %>%
  filter(sex ==1)

linear_male <- dosresmeta(formula = logrr ~ dose, id = id, type = "ci", lb = rr.low, ub = rr.hi,
                          intercept = F, cases = cases, n = n, method = "reml", data = male)
summary(linear_male)
predict(linear_male, delta = 12, exp = TRUE)

dosex_bin <- data.frame(dose=seq(0, 100, 1))
with(predict(linear_male, dosex_bin, order=TRUE, exp=TRUE), 
     {plot(dose, pred, type="l", col="blue", ylim=c(0.5, 2), ylab= "Relative risk", xlab="Alcohol intake, grams/day - Male")
       lines(dose, ci.lb, lty=2)
       lines(dose, ci.ub, lty=2)})

#QUADRATIC DOSE-RESPONSE

quad_male <- dosresmeta(formula = logrr ~ dose + I(dose^2), id = id, type = "ci", lb = rr.low, ub = rr.hi,
                          intercept = F, cases = cases, n = n, method = "reml", data = male)
summary(quad_male)

predict(quad_male, expo = TRUE)

dosex_bin <- data.frame(dose=seq(0, 100, 1))
with(predict(quad_male, dosex_bin, order=TRUE, exp=TRUE), 
     {plot(dose, pred, type="l", col="blue", ylim=c(0.5, 2), ylab= "Relative risk", xlab="Alcohol intake, grams/day - Male")
       lines(dose, ci.lb, lty=2)
       lines(dose, ci.ub, lty=2)})

#RESTRICTIVE CUBIC SPLINES
library("rms")
knotsm <- quantile(male$dose, c(.05, .35, .65, .95))
splmale <- dosresmeta(formula = logrr ~ rcs(dose, knotsm), id = id, type = "ci", 
                      cases = cases, n = n,
                      data = male, se = se, proc = "1stage")
summary(splmale)

dosex_bins <- data.frame(dose=seq(0, 100, 1))
xref <- 0
with(predict(splmale, dosex_bins, xref, exp = TRUE),
     {plot(get("rcs(dose, knotsm)dose"), pred, type= "l", ylim= c(0,3), ylab= "Relative risk", 
           xlab= "Alcohol consumption, grams/day - Male")
       matlines(get("rcs(dose, knotsm)dose"), cbind(ci.lb, ci.ub), col = 1, lty = "dashed")})

#test non-linearity
waldtest(b=coef(splmale), Sigma=vcov(splmale), Terms=c(2,3))

#goodness of fit
models <- list(linear_male, quad_male, splmale)
data.frame(do.call("rbind", lapply(models, function(m) 
  unlist(gof(m)[c("deviance","R2", "R2adj")]))))

#not working
par(mfrow = c(1, 3))
lapply(models, function(m)
  with(gof(m)$tdata,{
    plot(male$dose[male$se != 0], residuals, ylim = -c(-5, 5), xlab = "Alcohol consumption, gr/day")
  lines(lowess(male$dose[male$se != 0], residuals), lwd = 4)
  })
)


#CUBIC SPLINE MODEL - graph not working

polymale <- dosresmeta(formula = logrr ~ poly(dose, degree=2, raw=TRUE), id = id, type = "ci", 
                      cases = cases, n = n,
                      data = male, se = se, proc = "1stage")
summary(polymale)

dosex_binp <- data.frame(dose=seq(0, 100, 1))
xref <- 0
with(predict(polymale, dosex_binp, xref, exp = TRUE),
     {plot(dose, pred, type= "l", ylim= c(0,3), ylab= "Relative risk", 
           xlab= "Alcohol consumption, grams/day - Male")
       matlines(dose, cbind(ci.lb, ci.ub), col = 1, lty = "dashed")})

#FEMALE

female <- dataset %>%
  filter(sex ==0)

lin_female <- dosresmeta(formula = logrr ~ dose, id = id, type = "ci", se = se, intercept = F,
                  cases = cases, n = n, data = female)
summary(lin_female)
predict(lin_female, delta = 12, exp = TRUE)

dosex_bin <- data.frame(dose=seq(0, 150, 1))
with(predict(lin_female, dosex_bin, order=TRUE, exp=TRUE), 
     {plot(dose, pred, type="l", col="blue", ylim=c(0, 2), ylab= "Relative risk", xlab="Alcohol intake, grams/day - Female")
       lines(dose, ci.lb, lty=2)
       lines(dose, ci.ub, lty=2)})

#RESTRICTIVE CUBIC SPLINES

knotsf <- quantile(female$dose, c(.05, .35, .65, .95))
splfemale <- dosresmeta(formula = logrr ~ rcs(dose, knotsf), id = id, type = "ci", 
                      cases = cases, n = n,
                      data = female, se = se, proc = "1stage")
summary(splfemale)

dosex_bins <- data.frame(dose=seq(0, 150, 1))
xref <- 0
with(predict(splfemale, dosex_bins, xref, exp = TRUE),
     {plot(get("rcs(dose, knotsf)dose"), pred, type= "l", ylim= c(0,3), ylab= "Relative risk", 
           xlab= "Alcohol consumption, grams/day - Female")
       matlines(get("rcs(dose, knotsf)dose"), cbind(ci.lb, ci.ub), col = 1, lty = "dashed")})

waldtest(b=coef(splfemale), Sigma=vcov(splfemale), Terms=2:3)
