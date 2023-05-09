library(dosresmeta)
library(tidyverse)
library(ggplot2)
library(rms)

library(readxl)
dataset <- read_excel("CAMH/Suicide/SIMAH_workplace/4dosresmeta.xlsx", 
                      col_types = c("numeric", "numeric", "numeric", "text", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric"))

####MALE

#LINEAR DOSE-RESPONSE

male <- dataset %>%
  filter(sex ==0 & first_author != "Grazioli")

linear_male <- dosresmeta(formula = logrr ~ dose, id = id, type = "ci", lb = rr.low, ub = rr.hi,
                          intercept = F, cases = cases, n = n, method = "reml", data = male)
summary(linear_male)
predict(linear_male, delta = 50, exp = TRUE)

dosex_bin <- data.frame(dose=seq(0, 50, 1))
with(predict(linear_male, dosex_bin, order=TRUE, exp=TRUE), 
     {plot(dose, pred, type="l", col="black",main="a) Men", las=1, lwd = "3", cex.lab=1.2, cex.axis=1.1,
           ylim=c(0.5, 5), ylab= "Relative risk", xlab="Alcohol intake, grams/day")
       lines(dose, ci.lb, lty=2, lwd = "2")
       lines(dose, ci.ub, lty=2, lwd = "2")})

#QUADRATIC DOSE-RESPONSE

quad_male <- dosresmeta(formula = logrr ~ dose + I(dose^2), id = id, type = "ci", lb = rr.low, ub = rr.hi,
                        intercept = F, cases = cases, n = n, method = "reml", data = male, proc = "1stage")
summary(quad_male)

predict(quad_male, expo = TRUE)

dosex_bin <- data.frame(dose=seq(0, 50, 1))
with(predict(quad_male, dosex_bin, order=TRUE, exp=TRUE), 
     {plot(dose, pred, type="l", col="blue", ylim=c(0.5, 5), ylab= "Relative risk", xlab="Alcohol intake, grams/day - Male")
       lines(dose, ci.lb, lty=2, lwd = "2")
       lines(dose, ci.ub, lty=2, lwd = "2")})

#RESTRICTIVE CUBIC SPLINES
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

#FEMALE

female <- dataset %>%
  filter(sex ==1)

lin_female <- dosresmeta(formula = logrr ~ dose, id = id, type = "ci", se = se, intercept = F,
                  cases = cases, n = n, method = "reml", data = female)
summary(lin_female)
predict(lin_female, delta = 50, exp = TRUE)

dosex_bin <- data.frame(dose=seq(0, 50, 1))
with(predict(lin_female, dosex_bin, order=TRUE, exp=TRUE), 
     {plot(dose, pred, type="l", col="black", main="b) Women", las=1, lwd = "3", cex.lab=1.2, cex.axis=1.1,
           ylim=c(0.5, 5), ylab= "Relative risk", xlab="Alcohol intake, grams/day")
       lines(dose, ci.lb, lty=2, lwd = "2")
       lines(dose, ci.ub, lty=2, lwd = "2")})


#QUADRATIC DOSE-RESPONSE

quad_female <- dosresmeta(formula = logrr ~ dose + I(dose^2), id = id, type = "ci", lb = rr.low, ub = rr.hi,
                        intercept = F, cases = cases, n = n, method = "reml", data = female, proc = "1stage")
summary(quad_female)

predict(quad_female, expo = TRUE)

dosex_bin <- data.frame(dose=seq(0, 50, 1))
with(predict(quad_female, dosex_bin, order=TRUE, exp=TRUE), 
     {plot(dose, pred, type="l", col="blue", ylim=c(0.5, 5), ylab= "Relative risk", xlab="Alcohol intake, grams/day")
       lines(dose, ci.lb, lty=2)
       lines(dose, ci.ub, lty=2)})

#RESTRICTIVE CUBIC SPLINES
knotsf <- quantile(female$dose, c(.05, .35, .65, .95))
splfemale <- dosresmeta(formula = logrr ~ rcs(dose, knotsf), id = id, type = "ci", 
                      cases = cases, n = n, data = female, se = se, proc = "1stage")
summary(splfemale)

dosex_bins <- data.frame(dose=seq(0, 50, 1))
xref <- 0
with(predict(splfemale, dosex_bins, xref, exp = TRUE),
     {plot(get("rcs(dose, knotsf)dose"), pred, type= "l", ylim= c(0.5,5), ylab= "Relative risk", 
           xlab= "Alcohol consumption, grams/day")
       matlines(get("rcs(dose, knotsf)dose"), cbind(ci.lb, ci.ub), col = 1, lty = "dashed")})

###NON-DRINKERS AS REFERENCE

library(readxl)
nondrinkers <- read_excel("CAMH/Suicide/SIMAH_workplace/5nondrinkers.xlsx", 
                      col_types = c("numeric", "numeric", "numeric", "text", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric"))

####MALE MODELS

#suicide mortality
male1 <- nondrinkers %>%
  filter(sex ==0 & first_author != "Grazioli")

linear_male1 <- dosresmeta(formula = logrr ~ dose, id = id, type = "ci", lb = rr.low, ub = rr.hi,
                          intercept = F, cases = cases, n = n, method = "reml", data = male1)
summary(linear_male1)
predict(linear_male1, delta = 50, exp = TRUE)
pred_lin_male1 <- predict(linear_male1, dosex_bin, order=TRUE)

dosex_bin <- data.frame(dose=seq(0, 50, 1))
with(predict(linear_male, dosex_bin, order=TRUE, exp=TRUE), 
     {plot(dose, pred, type="l", col="black",main="a) Men", las=1, lwd = "3", cex.lab=1.2, cex.axis=1.1,
           ylim=c(0.5, 5), ylab= "Relative risk", xlab="Alcohol intake, grams/day")
       lines(dose, ci.lb, lty=2, lwd = "2")
       lines(dose, ci.ub, lty=2, lwd = "2")})
lines(exp(pred_lin_male1$pred), lwd = "4", col = "blue")
lines(exp(pred_lin_male1$ci.lb), lwd = "3", lty = "dotted", col = "blue")
lines(exp(pred_lin_male1$ci.ub), lwd = "3", lty = "dotted", col = "blue")
legend("topleft",inset =0.1, legend=c("LTA", "Non-drinkers"), lty=1:1, lwd=3:3, cex=1, col=c("black", "blue"))


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
        ylim = c(0, 5), pred = pred_lin_female, xvals = fs, main="b) Women")
lines(exp(pred_lin_female1$pred), lwd = "4", col = "blue")
lines(exp(pred_lin_female1$ci.lb), lwd = "3", lty = "dotted", col = "blue")
lines(exp(pred_lin_female1$ci.ub), lwd = "3", lty = "dotted", col = "blue")
legend("topleft",inset =0.1, legend=c("LTA", "Non-drinkers"), lty=1:1, lwd=3:3, cex=1, col=c("black", "blue"))
abline(h=1)

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
