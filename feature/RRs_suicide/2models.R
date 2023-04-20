library(tidyverse)
library(ggplot2)
library(mvmeta)

library(dosresmeta)
library(meta)
library(metafor)
#for restrictive cubic spline
library(rms)

library(readxl)
dataset <- read_excel("CAMH/Suicide/SIMAH_workplace/2dataset.xlsx", 
                        col_types = c("numeric", "numeric", "numeric", 
                                      "numeric", "text", "numeric", "text", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric"))

####MALE MODELS

#suicide mortality
male <- dataset %>%
  filter(dose != 0.00 & sex ==0 & outcome ==0)

#including suicide attempts
#male <- dataset %>%
#  filter(dose != 0.00 & sex ==0)

dim(table(male$cohort_id))

##LINEAR REGRESSION

linear_male <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+0, data=male,
                 random = ~ 1 | cohort_id/line_id, digits = 6, method = "REML")
summary(linear_male)

#use var.comp function to check i2 - heterogeneity
i2 <- var.comp(linear_male)
summary(i2)
i2$results
i2$totalI2
i2$plot

predict(linear_male, 100, transf=exp)

#graph
ms <- seq(0,150,length=150)
pred_lin_male <- predict(linear_male, cbind(ms))
regplot(linear_male, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,50),
        ylim = c(0, 5), pred = pred_lin_male, xvals = ms, main="Linear Regression")

#test for linearity
waldtest(b = coef(linear_male), Sigma = vcov(linear_male), Terms = 1:nrow(vcov(linear_male)))

#residual vs fitter - linearity
res<-resid(linear_male)
plot(fitted(linear_male), res)
abline(0,0)
qqnorm(res)
qqline(res) 

plot(lm(lnor~ dose, data=male))

##QUADRATIC REGRESSION

quad_male <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=male, 
               random = ~ 1 | cohort_id/line_id, method = "REML")
summary(quad_male)

pred_quad_male <- predict(quad_male, newmods=cbind(ms,ms^2))
regplot(quad_male, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), 
        ylim = c(0, 6), pred = pred_quad_male, xvals = ms, main="Quadratic Regression")

weights(quad_male)
predict(quad_male, c(0.57,0.57^2))

##RESTRICTED CUBIC SPLINE

knotsm <- quantile(male$dose, c(.05, .35, .65, .95))

rcs_male <- rma.mv(yi= lnor ~ rcs(dose, knotsm)+0, V=se^2, data=male, 
              random = ~ 1 | cohort_id/line_id, method = "REML")
summary(rcs_male)

pred_rcs_male <- predict(rcs_male, newmods=rcspline.eval(ms, knotsm, inclx=TRUE))
regplot(rcs_male, mod="rcs(dose, knotsm)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), 
        ylim = c(0, 6), pred = pred_rcs_male, xvals = ms, main="RCS Regression")
abline(v=knotsm, lty="dotted")

#ERASE ESTIMATES FROM GRAPH: pch=NA_integer_, 

#prediction rcs model
predict(rcs_male, newmods= rcspline.eval(80, knotsm, inclx=TRUE), transf=exp)

##MODEL COMPARISON 
fitstats(linear_male, quad_male, rcs_male)

####FEMALE MODELS

female <- dataset %>%
  filter(dose != 0.00 & sex ==1)

dim(table(female$cohort_id))

##LINEAR REGRESSION

linear_female <- rma.mv(yi=lnor, V=se^2, mods = ~ dose-1, data=female,
                      random = ~ 1 | cohort_id/line_id, method = "REML")
summary(linear_female)

fs <- seq(0,150,length=150)
pred_lin_female <- predict(linear_female, cbind(fs))
regplot(linear_female, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0, 50), 
        ylim = c(0, 5), pred = pred_lin_female, xvals = fs, main="Linear Regression")

predict(linear_female, 80, transf=exp)

#residual vs fitter - linearity
res<-resid(linear_female)
plot(fitted(linear_female), res)
abline(0,0)
qqnorm(res)
qqline(res) 

plot(lm(lnor~ dose, data=female))

##QUADRATIC REGRESSION

quad_female <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=female, 
                    random = ~ 1 | cohort_id/line_id, digits = 8, method = "REML")
summary(quad_female)

pred_quad_female <- predict(quad_female, newmods=cbind(fs,fs^2))
regplot(quad_female, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,50), 
        ylim = c(0, 10), pred = pred_quad_female, xvals = fs, main="Quadratic Regression")

##RESTRICTED CUBIC SPLINE

knotsf <- quantile(female$dose, c(.05, .35, .65, .95))

rcs_female <- rma.mv(yi= lnor ~ rcs(dose, knotsf)+0, V=se^2, data=female, digits = 8, 
                   random = ~ 1 | cohort_id/line_id, method = "REML")
summary(rcs_female)

pred_rcs_female <- predict(rcs_female, newmods=rcspline.eval(fs, knotsf, inclx=TRUE))
regplot(rcs_female, mod="rcs(dose, knotsf)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,50), 
        ylim = c(0, 10), pred = pred_rcs_female, xvals = fs, main="RCS Regression")
abline(v=knotsf, lty="dotted")

weights(rcs_female)

#use var.comp function
i2 <- var.comp(rcs_female)
summary(i2)
i2$results
i2$totalI2
i2$plot

#ERASE ESTIMATES FROM GRAPH: pch=NA_integer_, 

#prediction rcs model
predict(rcs_female, newmods= rcspline.eval(100, knotsf, inclx=TRUE), transf=exp)

fitstats(linear_female, quad_female, rcs_female)

###NON-DRINKERS AS REFERENCE

library(readxl)
nondrinkers <- read_excel("CAMH/Suicide/SIMAH_workplace/3nondrinkers.xlsx", 
                      col_types = c("numeric", "numeric", "numeric", 
                                    "numeric", "text", "numeric", "text", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric"))


####MALE MODELS

#suicide mortality
male1 <- nondrinkers %>%
  filter(dose != 0.00 & sex ==0 & outcome ==0)

#including suicide attempts
#male1 <- nondrinkers %>%
#  filter(dose != 0.00 & sex ==0)

dim(table(male1$cohort_id))

##LINEAR REGRESSION

linear_male1 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+0, data=male1,
                      random = ~ 1 | cohort_id/line_id, digits = 6, method = "REML")
summary(linear_male1)

#use var.comp function to check i2 - heterogeneity
i2 <- var.comp(linear_male1)
summary(i2)
i2$results
i2$totalI2
i2$plot

predict(linear_male1, 100, transf=exp)

#graph
ms <- seq(0,150,length=150)
pred_lin_male1 <- predict(linear_male1, cbind(ms))
regplot(linear_male1, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), 
        ylim = c(0, 6), pred = pred_lin_male1, xvals = ms, main="Male - Linear Regression")

#LTA graph
regplot(linear_male, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), pch=NA_integer_,
        ylim = c(0, 6), pred = pred_lin_male, xvals = ms, main="Male - Linear Regression")
lines(exp(pred_lin_male1$pred), lwd = "4", col = "blue")
lines(exp(pred_lin_male1$ci.lb), lwd = "3", lty = "dotted", col = "blue")
lines(exp(pred_lin_male1$ci.ub), lwd = "3", lty = "dotted", col = "blue")
legend("topleft",inset =0.1, legend=c("LTA", "Non-drinkers"), lty=1:1, lwd=3:3, cex=1.2, col=c("black", "blue", "red"))

##QUADRATIC REGRESSION

quad_male1 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=male1, 
                    random = ~ 1 | cohort_id/line_id, method = "REML")
summary(quad_male1)

pred_quad_male1 <- predict(quad_male1, newmods=cbind(ms,ms^2))
regplot(quad_male1, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), 
        ylim = c(0, 6), pred = pred_quad_male1, xvals = ms, main="Male - Quadratic Regression")

#LTA graph
regplot(quad_male, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), pch=NA_integer_, 
        ylim = c(0, 6), pred = pred_quad_male, xvals = ms, main="Male - Quadratic Regression")
lines(exp(pred_quad_male1$pred), lwd = "4", col = "blue")
lines(exp(pred_quad_male1$ci.lb), lwd = "3", lty = "dotted", col = "blue")
lines(exp(pred_quad_male1$ci.ub), lwd = "3", lty = "dotted", col = "blue")
legend("topleft",inset =0.1, legend=c("LTA", "Non-drinkers"), lty=1:1, lwd=3:3, cex=1.2, col=c("black", "blue", "red"))

weights(quad_male)
predict(quad_male, c(0.57,0.57^2))

##RESTRICTED CUBIC SPLINE

knotsm1 <- quantile(male1$dose, c(.05, .35, .65, .95))

rcs_male1 <- rma.mv(yi= lnor ~ rcs(dose, knotsm1)+0, V=se^2, data=male1, 
                   random = ~ 1 | cohort_id/line_id, method = "REML")
summary(rcs_male1)

pred_rcs_male1 <- predict(rcs_male1, newmods=rcspline.eval(ms, knotsm1, inclx=TRUE))
regplot(rcs_male1, mod="rcs(dose, knotsm1)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,90), 
        ylim = c(0, 8), pred = pred_rcs_male1, xvals = ms, main="Male - RCS Regression")
abline(v=knotsm1, lty="dotted")

#LTA graph
regplot(rcs_male, mod="rcs(dose, knotsm)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), pch=NA_integer_, 
        ylim = c(0, 6), pred = pred_rcs_male, xvals = ms, main="Male - RCS Regression")
lines(exp(pred_rcs_male1$pred), lwd = "4", col = "blue")
lines(exp(pred_rcs_male1$ci.lb), lwd = "3", lty = "dotted", col = "blue")
lines(exp(pred_rcs_male1$ci.ub), lwd = "3", lty = "dotted", col = "blue")
legend("topleft",inset =0.1, legend=c("LTA", "Non-drinkers"), lty=1:1, lwd=3:3, cex=1.2, col=c("black", "blue", "red"))

#prediction rcs model
predict(rcs_male1, newmods= rcspline.eval(0.57, knotsm, inclx=TRUE), transf=exp)

##MODEL COMPARISON 
fitstats(linear_male1, quad_male1, rcs_male1)

####FEMALE MODELS

female1 <- nondrinkers %>%
  filter(dose != 0.00 & sex ==1)

dim(table(female1$cohort_id))

##LINEAR REGRESSION

linear_female1 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose-1, data=female1,
                        random = ~ 1 | cohort_id/line_id, method = "REML")
summary(linear_female1)

fs <- seq(0,150,length=150)
pred_lin_female1 <- predict(linear_female1, cbind(fs))
regplot(linear_female1, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0, 50), 
        ylim = c(0, 10), pred = pred_lin_female1, xvals = fs, main="Female - Linear Regression")

#LTA graph
regplot(linear_female, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0, 50), pch=NA_integer_, 
        ylim = c(0, 10), pred = pred_lin_female, xvals = fs, main="Female - Linear Regression")
lines(exp(pred_lin_female1$pred), lwd = "4", col = "blue")
lines(exp(pred_lin_female1$ci.lb), lwd = "3", lty = "dotted", col = "blue")
lines(exp(pred_lin_female1$ci.ub), lwd = "3", lty = "dotted", col = "blue")
legend("topleft",inset =0.1, legend=c("LTA", "Non-drinkers"), lty=1:1, lwd=3:3, cex=1.2, col=c("black", "blue", "red"))

##QUADRATIC REGRESSION

quad_female1 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=female1, 
                      random = ~ 1 | cohort_id/line_id, digits = 8, method = "REML")
summary(quad_female1)

pred_quad_female1 <- predict(quad_female1, newmods=cbind(fs,fs^2))
regplot(quad_female1, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,50), 
        ylim = c(0, 10), pred = pred_quad_female1, xvals = fs, main="Female - Quadratic Regression")

#LTA graph
regplot(quad_female, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,50), pch=NA_integer_, 
        ylim = c(0, 10), pred = pred_quad_female, xvals = fs, main="Female - Quadratic Regression")
lines(exp(pred_quad_female1$pred), lwd = "4", col = "blue")
lines(exp(pred_quad_female1$ci.lb), lwd = "3", lty = "dotted", col = "blue")
lines(exp(pred_quad_female1$ci.ub), lwd = "3", lty = "dotted", col = "blue")
legend("topleft",inset =0.1, legend=c("LTA", "Non-drinkers"), lty=1:1, lwd=3:3, cex=1.2, col=c("black", "blue", "red"))

##RESTRICTED CUBIC SPLINE

knotsf1 <- quantile(female1$dose, c(.05, .35, .65, .95))

rcs_female1 <- rma.mv(yi= lnor ~ rcs(dose, knotsf1)+0, V=se^2, data=female1, digits = 8, 
                     random = ~ 1 | cohort_id/line_id, method = "REML")
summary(rcs_female1)

pred_rcs_female1 <- predict(rcs_female1, newmods=rcspline.eval(fs, knotsf1, inclx=TRUE))
regplot(rcs_female1, mod="rcs(dose, knotsf1)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,50), 
        ylim = c(0, 10), pred = pred_rcs_female1, xvals = fs, main="Female - RCS Regression")
abline(v=knotsf, lty="dotted")

#LTA graph
regplot(rcs_female, mod="rcs(dose, knotsf)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,50), 
        ylim = c(0, 10), pred = pred_rcs_female, xvals = fs, main="Female - RCS Regression")
lines(exp(pred_rcs_female1$pred), lwd = "4", col = "blue")
lines(exp(pred_rcs_female1$ci.lb), lwd = "3", lty = "dotted", col = "blue")
lines(exp(pred_rcs_female1$ci.ub), lwd = "3", lty = "dotted", col = "blue")
legend("topleft",inset =0.1, legend=c("LTA", "Non-drinkers"), lty=1:1, lwd=3:3, cex=1.2, col=c("black", "blue", "red"))

weights(rcs_female1)

#prediction rcs model
predict(rcs_female, newmods= rcspline.eval(100, knotsf, inclx=TRUE), transf=exp)

fitstats(linear_female1, quad_female1, rcs_female1)
