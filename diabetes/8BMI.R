library(tidyverse)
library(ggplot2)
library(mvmeta)

library(dosresmeta)
library(meta)
library(metafor)
#for restrictive cubic spline
library(rms)

setwd("C:/Users/laura/Documents/CAMH/")

library(readxl)
dataset <- read_excel("DIABETES/analysis/SIMAH_workplace/8BMI.xlsx",
                      col_types = c("numeric", "numeric", "numeric", "numeric", "text", "numeric",
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric", "numeric"))

dataset <- dataset %>%
  filter(reference != 3)

#& first_author != "Waki" & first_author !="Tsumura"

###MALE

male <- dataset %>%
  filter(dose != 0.00 & sex ==1)

#NORMAL WEIGHT

normalm <- male %>%
  filter(BMI ==0)
dim(table(normalm$results_id))

##LINEAR REGRESSION

linear_normalm <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+0, data=normalm,
                      random = ~ 1 | cohort_id/line_id, method = "REML")
summary(linear_normalm)

ms <- seq(0,150,length=150)
pred_lin_normalm <- predict(linear_normalm, cbind(ms))
regplot(linear_normalm, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,70),
        ylim = c(0, 2), pred = pred_lin_normalm, xvals = ms, main="Male - Linear Regression")
abline(h=1)
predict(linear_normalm, 0.57, transf=exp)

#pch=NA_integer_,

##QUADRATIC REGRESSION

quad_normalm <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=normalm,
                    random = ~ 1 | cohort_id/line_id, method = "REML")
summary(quad_normalm)

pred_quad_normalm <- predict(quad_normalm, newmods=cbind(ms,ms^2))
regplot(quad_normalm, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,70),pch=NA_integer_,
        ylim = c(0, 2), pred = pred_quad_normalm, xvals = ms, main="Male - Quadratic Regression")

predict(quad_normalm, c(15.5,15.5^2), transf=exp)

#for figure3
tiff("F3menhw.tiff", width = 6, height = 5, units = 'in',res = 1000)
regplot(quad_normalm, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative risk", lwd = c(3.5,1.5), lcol= "blue4",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,45),pch=NA_integer_, shade =FALSE, 
        ylim = c(0.4, 2), pred = pred_quad_normalm, xvals = ms)
abline(h=1)
title("Healthy weight range - Men", adj = 0, line = 2)
dev.off()

##RESTRICTED CUBIC SPLINE
knotsnm <- quantile(normalm$dose, c(.05, .35, .65, .95))

rcs_normalm <- rma.mv(yi= lnor ~ rcs(dose, knotsnm)+0, V=se^2, data=normalm,
                   random = ~ 1 | cohort_id/line_id, method = "REML")
summary(rcs_normalm)

pred_rcs_normalm <- predict(rcs_normalm, newmods=rcspline.eval(ms, knotsnm, inclx=TRUE))
regplot(rcs_normalm, mod="rcs(dose, knotsnm)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,80),
        ylim = c(0, 2), pred = pred_rcs_normalm, xvals = ms, main="Male - RCS Regression")

waldtest(b = coef(rcs_normalm), Sigma = vcov(rcs_normalm), Terms = 1:nrow(vcov(rcs_normalm)))

fitstats(linear_normalm, quad_normalm, rcs_normalm)

#OVERWEIGHT

ovm <- male %>%
  filter(BMI ==1)
dim(table(ovm$results_id))

##LINEAR REGRESSION

linear_ovm <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+0, data=ovm,
                         random = ~ 1 | cohort_id/line_id, method = "REML")
summary(linear_ovm)

ms <- seq(0,150,length=150)
pred_lin_ovm <- predict(linear_ovm, cbind(ms))
regplot(linear_ovm, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,80),
        ylim = c(0, 2), pred = pred_lin_ovm, xvals = ms, main="Male - Linear Regression")

predict(linear_ovm, 0.57, transf=exp)

#for figure 3
tiff("F3menov.tiff", width = 6, height = 5, units = 'in',res = 1000)
regplot(linear_ovm, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk", 
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,45),pch=NA_integer_, shade =FALSE,
        ylim = c(0.4, 2), lcol= "blue4", lwd = c(3.5,1.5),
        pred = pred_lin_ovm, xvals = ms)
abline(h=1)
title("Overweight range - Men", adj = 0, line = 2)
dev.off()

##QUADRATIC REGRESSION

quad_ovm <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=ovm,
                       random = ~ 1 | cohort_id/line_id, method = "REML")
summary(quad_ovm)

pred_quad_ovm <- predict(quad_ovm, newmods=cbind(ms,ms^2))
regplot(quad_ovm, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,80),
        ylim = c(0, 2), pred = pred_quad_ovm, xvals = ms, main="Male - Quadratic Regression")
abline(h=1)

##RESTRICTED CUBIC SPLINE
knotsovm <- quantile(ovm$dose, c(.05, .35, .65, .95))

rcs_ovm <- rma.mv(yi= lnor ~ rcs(dose, knotsovm)+0, V=se^2, data=ovm,
                      random = ~ 1 | cohort_id/line_id, method = "REML")
summary(rcs_ovm)

pred_rcs_ovm <- predict(rcs_ovm, newmods=rcspline.eval(ms, knotsovm, inclx=TRUE))
regplot(rcs_ovm, mod="rcs(dose, knotsovm)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,80),
        ylim = c(0, 2), pred = pred_rcs_ovm, xvals = ms, main="Male - RCS Regression")
abline(h=1)
waldtest(b = coef(rcs_ovm), Sigma = vcov(rcs_ovm), Terms = 1:nrow(vcov(rcs_ovm)))

fitstats(linear_ovm, quad_ovm, rcs_ovm)

#pch=NA_integer_,

#OBESE

obm <- male %>%
  filter(BMI ==2)
dim(table(obm$results_id))

##LINEAR REGRESSION

linear_obm <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+0, data=obm,
                     random = ~ 1 | results_id/line_id, method = "REML")
summary(linear_obm)

ms <- seq(0,150,length=150)
pred_lin_obm <- predict(linear_obm, cbind(ms))
regplot(linear_obm, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,80),
        ylim = c(0, 2), pred = pred_lin_obm, xvals = ms, main="Male - Linear Regression")

weights(linear_obm)

predict(linear_obm, 0.57, transf=exp)

#for figure 3
tiff("F3menob.tiff", width = 6, height = 5, units = 'in',res = 1000)
regplot(linear_obm, mod="dose", ylab="Relative Risk", 
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,45),pch=NA_integer_, shade =FALSE,
        ylim = c(0.4, 2), lcol= "blue4", lwd = c(3.5,1.5), xlab="Alcohol intake, grams/day",
        pred = pred_lin_obm, xvals = ms)
abline(h=1)
title("Obese range - Men", adj = 0, line = 2)
dev.off()

##QUADRATIC REGRESSION

quad_obm <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=obm,
                   random = ~ 1 | cohort_id/line_id, method = "REML")
summary(quad_obm)

pred_quad_obm <- predict(quad_obm, newmods=cbind(ms,ms^2))
regplot(quad_obm, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,80),
        ylim = c(0, 2), pred = pred_quad_obm, xvals = ms, main="Male - Quadratic Regression")

##RESTRICTED CUBIC SPLINE
knotsobm <- quantile(obm$dose, c(.05, .35, .65, .95))

rcs_obm <- rma.mv(yi= lnor ~ rcs(dose, knotsobm)+0, V=se^2, data=obm,
                  random = ~ 1 | cohort_id/line_id, method = "REML")
summary(rcs_obm)

pred_rcs_obm <- predict(rcs_obm, newmods=rcspline.eval(ms, knotsobm, inclx=TRUE))
regplot(rcs_obm, mod="rcs(dose, knotsobm)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,80),
        ylim = c(0, 2), pred = pred_rcs_obm, xvals = ms, main="Male - RCS Regression")

waldtest(b = coef(rcs_obm), Sigma = vcov(rcs_obm), Terms = 1:nrow(vcov(rcs_obm)))

fitstats(linear_obm, quad_obm, rcs_obm)


###FEMALE

female <- dataset %>%
  filter(dose != 0.00 & sex ==0)

#NORMAL WEIGHT

normalf <- female %>%
  filter(BMI ==0)
dim(table(normalf$results_id))

##LINEAR REGRESSION

linear_normalf <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+0, data=normalf,
                         random = ~ 1 | cohort_id/line_id, method = "REML")
summary(linear_normalf)

fs <- seq(0,150,length=150)
pred_lin_normalf <- predict(linear_normalf, cbind(fs))
regplot(linear_normalf, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,50),
        ylim = c(0, 2), pred = pred_lin_normalf, xvals = fs, main="Female - Linear Regression")

#for figure 3
tiff("F3womenhw.tiff", width = 6, height = 5, units = 'in',res = 1000)
regplot(linear_normalf, xlab="Alcohol intake, grams/day", mod="dose", ylab="Relative risk", 
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,45),pch=NA_integer_, shade =FALSE,
        ylim = c(0.4, 2), lcol= "firebrick2", lwd = c(3.5,1.5),
        pred = pred_lin_normalf, xvals = fs)
abline(h=1)
title("Healthy weight range - Women", adj = 0, line = 2)
dev.off()

##QUADRATIC REGRESSION

quad_normalf <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=normalf,
                       random = ~ 1 | cohort_id/line_id, method = "REML")
summary(quad_normalf)

pred_quad_normalf <- predict(quad_normalf, newmods=cbind(fs,fs^2))
regplot(quad_normalf, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,50),
        ylim = c(0, 2), pred = pred_quad_normalf, xvals = fs, main="Female - Quadratic Regression")

##RESTRICTED CUBIC SPLINE
knotsnf <- quantile(normalf$dose, c(.05, .35, .65, .95))

rcs_normalf <- rma.mv(yi= lnor ~ rcs(dose, knotsnf)+0, V=se^2, data=normalf,
                      random = ~ 1 | cohort_id/line_id, method = "REML")
summary(rcs_normalf)

pred_rcs_normalf <- predict(rcs_normalf, newmods=rcspline.eval(fs, knotsnf, inclx=TRUE))
regplot(rcs_normalf, mod="rcs(dose, knotsnf)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,50),
        ylim = c(0, 2), pred = pred_rcs_normalf, xvals = fs, main="Female - RCS Regression")

waldtest(b = coef(rcs_normalf), Sigma = vcov(rcs_normalf), Terms = 1:nrow(vcov(rcs_normalf)))

fitstats(linear_normalf, quad_normalf, rcs_normalf)

#OVERWEIGHT

ovf <- female %>%
  filter(BMI ==1)
dim(table(ovf$results_id))

##LINEAR REGRESSION

linear_ovf <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+0, data=ovf,
                     random = ~ 1 | cohort_id/line_id, method = "REML")
summary(linear_ovf)

pred_lin_ovf <- predict(linear_ovf, cbind(fs))
regplot(linear_ovf, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,50),
        ylim = c(0, 2), pred = pred_lin_ovf, xvals = fs, main="Female - Linear Regression")

##QUADRATIC REGRESSION

quad_ovf <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=ovf,
                   random = ~ 1 | cohort_id/line_id, method = "REML")
summary(quad_ovf)

pred_quad_ovf <- predict(quad_ovf, newmods=cbind(fs,fs^2))
regplot(quad_ovf, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,50),
        ylim = c(0, 2), pred = pred_quad_ovf, xvals = fs, main="Female - Quadratic Regression")

#for figure 3
tiff("F3womenov.tiff", width = 6, height = 5, units = 'in',res = 1000)
regplot(quad_ovf, xlab="Alcohol intake, grams/day", mod="dose", ylab="Relative risk", 
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,45),pch=NA_integer_, shade =FALSE,
        ylim = c(0.4, 2), lcol= "firebrick2", lwd = c(3.5,1.5),
        pred = pred_quad_ovf, xvals = fs)
abline(h=1)
title("Overweight range - Women", adj = 0, line = 2)
dev.off()

predict(quad_ovf, c(33,33^2), transf=exp)

##RESTRICTED CUBIC SPLINE
knotsovf <- quantile(ovf$dose, c(.05, .35, .65, .95))

rcs_ovf <- rma.mv(yi= lnor ~ rcs(dose, knotsovf)+0, V=se^2, data=ovf,
                  random = ~ 1 | cohort_id/line_id, method = "REML")
summary(rcs_ovf)

pred_rcs_ovf <- predict(rcs_ovf, newmods=rcspline.eval(fs, knotsovf, inclx=TRUE))
regplot(rcs_ovf, mod="rcs(dose, knotsovf)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,50),
        ylim = c(0, 2), pred = pred_rcs_ovf, xvals = fs, main="Female - RCS Regression")

waldtest(b = coef(rcs_ovf), Sigma = vcov(rcs_ovf), Terms = 1:nrow(vcov(rcs_ovf)))

fitstats(linear_ovf, quad_ovf, rcs_ovf)

#OBESE

obf <- female %>%
  filter(BMI ==2)
dim(table(obf$results_id))

##LINEAR REGRESSION

linear_obf <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+0, data=obf,
                     random = ~ 1 | cohort_id/line_id, method = "REML")
summary(linear_obf)

ms <- seq(0,150,length=150)
pred_lin_obf <- predict(linear_obf, cbind(fs))
regplot(linear_obf, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,50),
        ylim = c(0, 2), pred = pred_lin_obf, xvals = fs, main="Female - Linear Regression")

weights(linear_obm)

##QUADRATIC REGRESSION

quad_obf <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=obf,
                   random = ~ 1 | cohort_id/line_id, method = "REML")
summary(quad_obf)

pred_quad_obf <- predict(quad_obf, newmods=cbind(fs,fs^2))
regplot(quad_obf, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,50),
        ylim = c(0, 2), pred = pred_quad_obf, xvals = fs, main="Female - Quadratic Regression")

#for figure 3
tiff("F3womenob.tiff", width = 6, height = 5, units = 'in',res = 1000)
regplot(quad_obf, xlab="Alcohol intake, grams/day", mod="dose", ylab="Relative risk", 
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,45),pch=NA_integer_, shade =FALSE,
        ylim = c(0.4, 2), lcol= "firebrick2", lwd = c(3.5,1.5),
        pred = pred_quad_obf, xvals = fs)
abline(h=1)
title("Obese range - Women", adj = 0, line = 2)
dev.off()

predict(quad_obf, c(18,18^2), transf=exp)

##RESTRICTED CUBIC SPLINE
knotsobf <- quantile(obf$dose, c(.05, .35, .65, .95))

rcs_obf <- rma.mv(yi= lnor ~ rcs(dose, knotsobf)+0, V=se^2, data=obf,
                  random = ~ 1 | cohort_id/line_id, method = "REML")
summary(rcs_obf)

pred_rcs_obf <- predict(rcs_obf, newmods=rcspline.eval(fs, knotsobf, inclx=TRUE))
regplot(rcs_obf, mod="rcs(dose, knotsobf)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,50),
        ylim = c(0, 2), pred = pred_rcs_obf, xvals = fs, main="Female - RCS Regression")

waldtest(b = coef(rcs_obf), Sigma = vcov(rcs_obf), Terms = 1:nrow(vcov(rcs_obf)))

fitstats(linear_obf, quad_obf, rcs_obf)

###BOTH COMBINED

both <- dataset %>%
  filter(dose != 0.00)

#NORMAL WEIGHT

normal <- both %>%
  filter(BMI ==0)
dim(table(normal$results_id))

##LINEAR REGRESSION

linear_normal <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+0, data=normal,
                         random = ~ 1 | cohort_id/line_id, method = "REML")
summary(linear_normal)

s <- seq(0,150,length=150)
pred_lin_normal <- predict(linear_normal, cbind(s))
regplot(linear_normal, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,80),
        ylim = c(0, 2), pred = pred_lin_normal, xvals = s, main="Linear Regression")

#pch=NA_integer_,

##QUADRATIC REGRESSION

quad_normal <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=normal,
                       random = ~ 1 | cohort_id/line_id, method = "REML")
summary(quad_normal)

pred_quad_normal <- predict(quad_normal, newmods=cbind(s,s^2))
regplot(quad_normal, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,80),
        ylim = c(0, 2), pred = pred_quad_normal, xvals = s, main="Healthy weight range - Quadratic Regression")

predict(quad_normal, c(18,18^2),transf=exp)

-(-0.0180/(2*0.0005))

##RESTRICTED CUBIC SPLINE
knotsn <- quantile(normal$dose, c(.05, .35, .65, .95))

rcs_normal <- rma.mv(yi= lnor ~ rcs(dose, knotsn)+0, V=se^2, data=normal,
                      random = ~ 1 | cohort_id/line_id, method = "REML")
summary(rcs_normal)

pred_rcs_normal <- predict(rcs_normal, newmods=rcspline.eval(s, knotsn, inclx=TRUE))
regplot(rcs_normal, mod="rcs(dose, knotsn)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,80),
        ylim = c(0, 2), pred = pred_rcs_normal, xvals = s, main="RCS Regression")

waldtest(b = coef(rcs_normal), Sigma = vcov(rcs_normal), Terms = 1:nrow(vcov(rcs_normal)))

fitstats(linear_normal, quad_normal, rcs_normal)

#OVERWEIGHT

ov <- both %>%
  filter(BMI ==1)
dim(table(ov$results_id))

##LINEAR REGRESSION

linear_ov <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+0, data=ov,
                     random = ~ 1 | cohort_id/line_id, method = "REML")
summary(linear_ov)

s <- seq(0,150,length=150)
pred_lin_ov <- predict(linear_ov, cbind(s))
regplot(linear_ov, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,80),
        ylim = c(0, 2), pred = pred_lin_ov, xvals = s, main="Linear Regression")

##QUADRATIC REGRESSION

quad_ov <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=ov,
                   random = ~ 1 | cohort_id/line_id, method = "REML")
summary(quad_ov)

pred_quad_ov <- predict(quad_ov, newmods=cbind(s,s^2))
regplot(quad_ov, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,80),
        ylim = c(0, 2), pred = pred_quad_ov, xvals = s, main="Overweight range - Quadratic Regression")

predict(quad_ov, c(34,34^2),transf=exp)

-(-0.0202/(2*0.0003))

##RESTRICTED CUBIC SPLINE
knotsov <- quantile(ov$dose, c(.05, .35, .65, .95))

rcs_ov <- rma.mv(yi= lnor ~ rcs(dose, knotsov)+0, V=se^2, data=ov,
                  random = ~ 1 | cohort_id/line_id, method = "REML")
summary(rcs_ov)

pred_rcs_ov <- predict(rcs_ov, newmods=rcspline.eval(s, knotsov, inclx=TRUE))
regplot(rcs_ov, mod="rcs(dose, knotsov)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,80),
        ylim = c(0, 2), pred = pred_rcs_ov, xvals = s, main="RCS Regression")

waldtest(b = coef(rcs_ov), Sigma = vcov(rcs_ov), Terms = 1:nrow(vcov(rcs_ov)))

fitstats(linear_ov, quad_ov, rcs_ov)

#pch=NA_integer_,

#OBESE

ob <- both %>%
  filter(BMI ==2)
dim(table(ob$results_id))

##LINEAR REGRESSION

linear_ob <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+0, data=ob,
                     random = ~ 1 | results_id/line_id, method = "REML")
summary(linear_ob)

s <- seq(0,150,length=150)
pred_lin_ob <- predict(linear_ob, cbind(s))
regplot(linear_ob, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,80),
        ylim = c(0, 2), pred = pred_lin_ob, xvals = s, main="Linear Regression")

weights(linear_ob)

##QUADRATIC REGRESSION

quad_ob <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=ob,
                   random = ~ 1 | cohort_id/line_id, method = "REML")
summary(quad_ob)

pred_quad_ob <- predict(quad_ob, newmods=cbind(s,s^2))
regplot(quad_ob, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,80),
        ylim = c(0, 2), pred = pred_quad_ob, xvals = s, main="Obesity range - Quadratic Regression")

weights(quad_ob)

predict(quad_ob, c(36,36^2),transf=exp)

-(-0.0214/(2*0.0003))

##RESTRICTED CUBIC SPLINE
knotsob <- quantile(ob$dose, c(.05, .35, .65, .95))

rcs_ob <- rma.mv(yi= lnor ~ rcs(dose, knotsob)+0, V=se^2, data=ob,
                  random = ~ 1 | cohort_id/line_id, method = "REML")
summary(rcs_ob)

pred_rcs_ob <- predict(rcs_ob, newmods=rcspline.eval(s, knotsob, inclx=TRUE))
regplot(rcs_ob, mod="rcs(dose, knotsob)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,80),
        ylim = c(0, 2), pred = pred_rcs_ob, xvals = s, main="RCS Regression")

waldtest(b = coef(rcs_ob), Sigma = vcov(rcs_ob), Terms = 1:nrow(vcov(rcs_ob)))

weights(rcs_ob)

fitstats(linear_ob, quad_ob, rcs_ob)
