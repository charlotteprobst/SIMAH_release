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
                                    "numeric", "numeric", "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric", "numeric"))

####US SPECIFIC - MALE MODELS

male_us <- dataset %>%
  filter(analysis_id==0 & dose != 0.00 & sex ==1 & usa ==1)

dim(table(male_us$results_id))

##LINEAR REGRESSION  

linear_male_us <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+0, data=male_us, digits =6,
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

quad_female_us <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=female_us, digits =6, 
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
abline(h=1)
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


###FORMULAS

##MEN
#linear in the log

(exp(0-0.002661*x))
var(beta1) = 0.001506^2

##WOMEN
#quadratic

(exp(0-0.023792*x+0.000341*x^2))
var(beta1) = 0.004744^2
var(beta2) = 0.000099^2

#restrictive cubic spline

### VARIABLE BASED FUNCTIONS
knotsfus
sprintf("%.10f", knotsfus)

knots_T		<- c(2.20, 10.44, 24.60, 54.25)
kd			<- (knots_T[4] - knots_T[1])^(2/3)

DIST_1		<- function(alc_1){alc_1}
DIST_2		<- function(alc_1){ pmax((alc_1 - knots_T[1])/kd, 0)^3 + ((knots_T[3] - knots_T[1]) * pmax((alc_1 - knots_T[4])/kd, 0)^3 - (knots_T[4] - knots_T[1]) * 
                                                                    (pmax((alc_1 - knots_T[3])/kd, 0)^3))/ (knots_T[4] - knots_T[3]) }
DIST_3		<- function(alc_1){ pmax((alc_1 - knots_T[2])/kd, 0)^3 + ((knots_T[3] - knots_T[2]) * pmax((alc_1 - knots_T[4])/kd, 0)^3 - (knots_T[4] - knots_T[2]) * 
                                                                    (pmax((alc_1 - knots_T[3])/kd, 0)^3))/(knots_T[4] - knots_T[3]) }

### VALUE BASED FUNCTIONS
#DIST_2
( pmax((x - 2.20)/13.94057, 0)^3 + ((24.60 - 1) * pmax((x - 54.25)/13.94057, 0)^3 - (54.25 - 1) * (pmax((x - 24.60)/13.94057, 0)^3))  / (54.25 - 24.60)  )
#DIST_3
( pmax((x - 10.44)/13.94057, 0)^3 + ((24.60 - 10.44) * pmax((x - 54.25)/12.99405, 0)^3 - (54.25 - 10.44) * (pmax((x - 24.60)/13.94057, 0)^3)) / (54.25 - 24.60) )

