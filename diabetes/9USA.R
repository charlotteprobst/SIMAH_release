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
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric"))

####US SPECIFIC - MALE MODELS

male_us <- dataset %>%
  filter(analysis_id==0 & dose != 0.00 & sex ==1 & usa ==1)

dim(table(male_us$results_id))

##LINEAR REGRESSION  

linear_male_us <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+0, data=male_us,
                      random = ~ 1 | cohort_id/line_id, method = "REML")
summary(linear_male_us)

#graph
ms <- seq(0,150,length=150)
pred_lin_male_us <- predict(linear_male_us, cbind(ms))
regplot(linear_male_us, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), 
        ylim = c(0, 2), pred = pred_lin_male_us, xvals = ms, main="Male - Linear Regression")

weights(linear_male)
#pch=NA_integer_,

#test for linearity
waldtest(b = coef(linear_male_us), Sigma = vcov(linear_male_us), Terms = 1:nrow(vcov(linear_male_us)))

predict(linear_male_us, 100, transf=exp)

##QUADRATIC REGRESSION

quad_male_us <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=male_us, 
                    random = ~ 1 | cohort_id/line_id, method = "REML")
summary(quad_male_us)

pred_quad_male_us <- predict(quad_male_us, newmods=cbind(ms,ms^2))
regplot(quad_male_us, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100),
        ylim = c(0, 2), pred = pred_quad_male_us, xvals = ms, main="Quadratic Regression")

waldtest(b = coef(quad_male_us), Sigma = vcov(quad_male_us), Terms = 1:nrow(vcov(quad_male_us)))

##RESTRICTED CUBIC SPLINE

knotsmus <- quantile(male_us$dose, c(.05, .35, .65, .95))

rcs_male_us <- rma.mv(yi= lnor ~ rcs(dose, knotsmus)+0, V=se^2, data=male_us, 
                   random = ~ 1 | cohort_id/line_id, method = "REML")
summary(rcs_male_us)

pred_rcs_male_us <- predict(rcs_male_us, newmods=rcspline.eval(ms, knotsmus, inclx=TRUE))
regplot(rcs_male_us, mod="rcs(dose, knotsmus)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), pch=NA_integer_,
        ylim = c(0, 2), pred = pred_rcs_male_us, xvals = ms, main="Male - RCS Regression")
abline(v=knotsm, lty="dotted")

waldtest(b = coef(rcs_male_us), Sigma = vcov(rcs_male_us), Terms = 1:nrow(vcov(rcs_male_us)))

#ERASE ESTIMATES FROM GRAPH: pch=NA_integer_, 

weights(rcs)

#prediction rcs model
predict(rcs_male_us, newmods= rcspline.eval(0.57, knotsmus, inclx=TRUE), transf=exp)

##MODEL COMPARISON 
fitstats(linear_male_us, quad_male_us, rcs_male_us)
anova(linear_male_us, quad_male_us,refit=TRUE)

####FEMALE MODELS

female_us <- dataset %>%
  filter(analysis_id==0 & dose != 0.00 & sex ==0 & usa ==1)

dim(table(female_us$results_id))

##LINEAR REGRESSION

linear_female_us <- rma.mv(yi=lnor, V=se^2, mods = ~ dose-1, data=female_us,
                        random = ~ 1 | cohort_id/line_id, method = "REML")
summary(linear_female_us)

fs <- seq(0,150,length=150)
pred_lin_female_us <- predict(linear_female_us, cbind(fs))
regplot(linear_female_us, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), 
        ylim = c(0, 2), pred = pred_lin_female_us, xvals = fs, main="Female - Linear Regression")

waldtest(b = coef(linear_female_us), Sigma = vcov(linear_female_us), Terms = 1:nrow(vcov(linear_female_us)))

##QUADRATIC REGRESSION

quad_female_us <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=female_us, 
                      random = ~ 1 | cohort_id/line_id, method = "REML")
summary(quad_female_us)

pred_quad_female_us <- predict(quad_female_us, newmods=cbind(fs,fs^2))
regplot(quad_female_us, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), 
        ylim = c(0, 2), pred = pred_quad_female_us, xvals = fs, main="Female - Quadratic Regression")

##RESTRICTED CUBIC SPLINE

knotsfus <- quantile(female_us$dose, c(.05, .35, .65, .95))

rcs_female_us <- rma.mv(yi= lnor ~ rcs(dose, knotsfus)+0, V=se^2, data=female_us, 
                     random = ~ 1 | cohort_id/line_id, method = "REML")
summary(rcs_female_us)

pred_rcs_female_us <- predict(rcs_female_us, newmods=rcspline.eval(fs, knotsfus, inclx=TRUE))
regplot(rcs_female_us, mod="rcs(dose, knotsfus)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), 
        ylim = c(0, 2), pred = pred_rcs_female_us, xvals = fs, main="RCS Regression")

#use var.comp function
i2 <- var.comp(rcs_female)
summary(i2)
i2$results
i2$totalI2
i2$plot

#ERASE ESTIMATES FROM GRAPH: pch=NA_integer_, 

#prediction rcs model
predict(rcs_female_us, newmods= rcspline.eval(100, knotsfus, inclx=TRUE), transf=exp)

fitstats(linear_female_us, quad_female_us, rcs_female_us)

anova(quad_female_us, rcs_female_us, refit=TRUE)
