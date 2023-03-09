library(tidyverse)
library(ggplot2)
library(mvmeta)

library(dosresmeta)
library(meta)
library(metafor)

#for restrictive cubic spline
library(rms)

library(readxl)
dataset <- read_excel("CAMH/DIABETES/analysis/SIMAH_workplace/6dataset.xlsx", 
                      col_types = c("numeric", "numeric", "numeric", "numeric", "numeric", "text", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

#ONLY OBJECTIVE ASCERTAIMENT

####MALE MODELS

male <- dataset %>%
  filter(analysis_id==0 & dose != 0.00 & sex ==1 & outcome.ascertaiment ==0)

#erase extreme value - Burke 2007
male <- male[-c(10),]

dim(table(male$results_id))

##LINEAR REGRESSION

linear_male <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+0, data=male,
                      random = ~ 1 | cohort_id/line_id, method = "REML")
summary(linear_male)

#comparing model
linear_male_2 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+0, data=male,
                        random = ~ 1 | cohort_id/line_id, method = "REML", sigma2 =  c(0, NA))
summary(linear_male_2)

anova(linear_male, linear_male_2)

#graph
ms <- seq(0,150,length=150)
pred_lin_male <- predict(linear_male, cbind(ms))
regplot(linear_male, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), pch=NA_integer_,
        ylim = c(0, 2), pred = pred_lin_male, xvals = ms)

weights(linear_male)

predict(linear_male, 20, transf=exp)

#test for linearity
waldtest(b = coef(linear_male), Sigma = vcov(linear_male), Terms = 1:nrow(vcov(linear_male)))

##QUADRATIC REGRESSION

quad_male <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=male, 
                    random = ~ 1 | cohort_id/line_id, method = "REML")
summary(quad_male)

pred_quad_male <- predict(quad_male, newmods=cbind(ms,ms^2))
regplot(quad_male, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), pch=NA_integer_,
        ylim = c(0, 2), pred = pred_quad_male, xvals = ms, main="Male - Quadratic Regression")

waldtest(b = coef(quad_male), Sigma = vcov(quad_male), Terms = 1:nrow(vcov(quad_male)))

##RESTRICTED CUBIC SPLINE

knotsm <- quantile(male$dose, c(.05, .35, .65, .95))

rcs_male <- rma.mv(yi= lnor ~ rcs(dose, knotsm)+0, V=se^2, data=male, 
                   random = ~ 1 | cohort_id/line_id, method = "REML")
summary(rcs_male)

pred_rcs_male <- predict(rcs_male, newmods=rcspline.eval(ms, knotsm, inclx=TRUE))
regplot(rcs_male, mod="rcs(dose, knotsm)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), pch=NA_integer_,
        ylim = c(0, 2), pred = pred_rcs_male, xvals = ms, main="Male - RCS Regression")
abline(v=knotsm, lty="dotted")

waldtest(b = coef(rcs_male), Sigma = vcov(rcs_male), Terms = 1:nrow(vcov(rcs_male)))

##MODEL COMPARISON 
fitstats(linear_male, quad_male, rcs_male)

####FEMALE MODELS

female <- dataset %>%
  filter(analysis_id==0 & dose != 0.00 & sex ==0 & outcome.ascertaiment ==0)

#erase extreme value - Burke 2007
female <- female[-c(8),]
dim(table(female$results_id))

##LINEAR REGRESSION

linear_female <- rma.mv(yi=lnor, V=se^2, mods = ~ dose-1, data=female,
                        random = ~ 1 | cohort_id/line_id, method = "REML")
summary(linear_female)

fs <- seq(0,150,length=150)
pred_lin_female <- predict(linear_female, cbind(fs))
regplot(linear_female, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), pch=NA_integer_,
        ylim = c(0, 2), pred = pred_lin_female, xvals = fs, main="Female - Linear Regression")

waldtest(b = coef(linear_female), Sigma = vcov(linear_female), Terms = 1:nrow(vcov(linear_female)))

##QUADRATIC REGRESSION

quad_female <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=female, 
                      random = ~ 1 | cohort_id/line_id, method = "ML")
summary(quad_female)

pred_quad_female <- predict(quad_female, newmods=cbind(fs,fs^2))
regplot(quad_female, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100),pch=NA_integer_, 
        ylim = c(0, 2), pred = pred_quad_female, xvals = fs, main="Female - Quadratic Regression")

##RESTRICTED CUBIC SPLINE

knotsf <- quantile(female$dose, c(.05, .35, .65, .95))

rcs_female <- rma.mv(yi= lnor ~ rcs(dose, knotsf)+0, V=se^2, data=female, 
                     random = ~ 1 | cohort_id/line_id, method = "REML")
summary(rcs_female)

pred_rcs_female <- predict(rcs_female, newmods=rcspline.eval(fs, knotsf, inclx=TRUE))
regplot(rcs_female, mod="rcs(dose, knotsf)dose", xlab="Alcohol intake, grams/day", ylab="Relative risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), pch=NA_integer_,
        ylim = c(0, 2), pred = pred_rcs_female, xvals = fs)
abline(v=knotsf, lty="dotted")

weights(rcs_female)

fitstats(linear_female, quad_female, rcs_female)

predict(rcs_female, newmods= rcspline.eval(67, knotsf, inclx=TRUE), transf=exp)

###sensitivity 2: studies with new criteria dx 1998

####MALE MODELS

male2 <- dataset %>%
  filter(analysis_id==0 & dose != 0.00 & sex ==1 & new_dx %in% c(1))

#erase extreme value - Burke 2007
male2 <- male2[-c(10),]

dim(table(male2$results_id))

##LINEAR REGRESSION

linear_male2 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+0, data=male2,
                      random = ~ 1 | cohort_id/line_id, method = "REML")
summary(linear_male2)

#graph
ms <- seq(0,150,length=150)
pred_lin_male2 <- predict(linear_male2, cbind(ms))
regplot(linear_male2, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), pch=NA_integer_,
        ylim = c(0, 2), pred = pred_lin_male2, xvals = ms)

predict(linear_male2, 100, transf=exp)

#test for linearity
waldtest(b = coef(linear_male2), Sigma = vcov(linear_male2), Terms = 1:nrow(vcov(linear_male2)))

##QUADRATIC REGRESSION

quad_male2 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=male2, 
                    random = ~ 1 | cohort_id/line_id, method = "REML")
summary(quad_male2)

pred_quad_male2 <- predict(quad_male2, newmods=cbind(ms,ms^2))
regplot(quad_male2, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), pch=NA_integer_,
        ylim = c(0, 2), pred = pred_quad_male2, xvals = ms, main="Male - Quadratic Regression")

waldtest(b = coef(quad_male2), Sigma = vcov(quad_male2), Terms = 1:nrow(vcov(quad_male2)))

##RESTRICTED CUBIC SPLINE

knotsm2 <- quantile(male2$dose, c(.05, .35, .65, .95))

rcs_male2 <- rma.mv(yi= lnor ~ rcs(dose, knotsm2)+0, V=se^2, data=male2, 
                   random = ~ 1 | cohort_id/line_id, method = "REML")
summary(rcs_male2)

pred_rcs_male2 <- predict(rcs_male2, newmods=rcspline.eval(ms, knotsm2, inclx=TRUE))
regplot(rcs_male2, mod="rcs(dose, knotsm2)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), pch=NA_integer_,
        ylim = c(0, 2), pred = pred_rcs_male2, xvals = ms, main="Male - RCS Regression")
abline(v=knotsm2, lty="dotted")

waldtest(b = coef(rcs_male2), Sigma = vcov(rcs_male2), Terms = 1:nrow(vcov(rcs_male2)))

##MODEL COMPARISON 
fitstats(linear_male2, quad_male2, rcs_male2)

####FEMALE MODELS

female2 <- dataset %>%
  filter(analysis_id==0 & dose != 0.00 & sex ==0 & new_dx %in% c(1))

#erase extreme value - Burke 2007
female2 <- female2[-c(12),]
dim(table(female2$results_id))

##LINEAR REGRESSION

linear_female2 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose-1, data=female2,
                        random = ~ 1 | cohort_id/line_id, method = "REML")
summary(linear_female2)

fs <- seq(0,150,length=150)
pred_lin_female2 <- predict(linear_female2, cbind(fs))
regplot(linear_female2, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), pch=NA_integer_,
        ylim = c(0, 2), pred = pred_lin_female2, xvals = fs, main="Female - Linear Regression")

waldtest(b = coef(linear_female2), Sigma = vcov(linear_female2), Terms = 1:nrow(vcov(linear_female2)))

##QUADRATIC REGRESSION

quad_female2 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=female2, 
                      random = ~ 1 | cohort_id/line_id, method = "ML")
summary(quad_female2)

pred_quad_female2 <- predict(quad_female2, newmods=cbind(fs,fs^2))
regplot(quad_female2, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100),pch=NA_integer_, 
        ylim = c(0, 2), pred = pred_quad_female2, xvals = fs, main="Female - Quadratic Regression")

##RESTRICTED CUBIC SPLINE

knotsf2 <- quantile(female2$dose, c(.05, .35, .65, .95))

rcs_female2 <- rma.mv(yi= lnor ~ rcs(dose, knotsf2)+0, V=se^2, data=female2, 
                     random = ~ 1 | cohort_id/line_id, method = "REML")
summary(rcs_female2)

pred_rcs_female2 <- predict(rcs_female2, newmods=rcspline.eval(fs, knotsf2, inclx=TRUE))
regplot(rcs_female2, mod="rcs(dose, knotsf2)dose", xlab="Alcohol intake, grams/day", ylab="Relative risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), pch=NA_integer_,
        ylim = c(0, 2), pred = pred_rcs_female2, xvals = fs)
abline(v=knotsf2, lty="dotted")

fitstats(linear_female2, quad_female2, rcs_female2)

predict(rcs_female2, newmods= rcspline.eval(100, knotsf2, inclx=TRUE), transf=exp)

