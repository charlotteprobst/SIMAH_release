library(tidyverse)
library(ggplot2)
library(mvmeta)
library(dosresmeta)

library(meta)
library(metafor)
library(dmetar)
install.packages("dmetar")

library(readxl)
dataset <- read_excel("CAMH/DIABETES/analysis/SIMAH_workplace/4dataset.xlsx", 
                      col_types = c("numeric", "numeric", "numeric", "numeric", "numeric", "text", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric"))

final <- dataset %>%
  filter(analysis_id==0 & dose != 0.00)

final <- final[-c(36,37,38,39,162,163,164,165,166),]

##LINEAR REGRESSION

linear <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+0, data=final,
                 random = ~ 1 | cohort_id/line_id, test = "t", method = "REML")
summary(linear)

s <- seq(0,150,length=150)
pred_lin <- predict(linear, cbind(s))
regplot(linear, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), 
        ylim = c(0, 2), pred = pred_lin, xvals = s, main="Linear Regression")

weights(linear)

##QUADRATIC REGRESSION

quad <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=final, 
               random = ~ 1 | cohort_id/line_id, test = "t", method = "REML")
summary(quad)

pred_quad <- predict(quad, newmods=cbind(s,s^2))
regplot(quad, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 3), pred = pred_quad, xvals = s, main="Quadratic Regression")

##RESTRICTED CUBIC SPLINE

library(rms)
s <- seq(0,150,length=150)
knots <- quantile(final$dose, c(.05, .35, .65, .95))

rcs <- rma.mv(yi= lnor ~ rcs(dose, knots)+0, V=se^2, data=final, 
              random = ~ 1 | cohort_id/line_id, test = "t", method = "REML")
summary(rcs)

pred_rcs <- predict(rcs, newmods=rcspline.eval(s, knots, inclx=TRUE))
regplot(rcs, mod="rcs(dose, knots)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), pch=NA_integer_,
        ylim = c(0, 2), pred = pred_rcs, xvals = s, main="RCS Regression")
abline(v=knots, lty="dotted")

#ERASE ESTIMATES FROM GRAPH: pch=NA_integer_, 

weights(rcs)

#prediction rcs model
predict(rcs, newmods= rcspline.eval(50, knots, inclx=TRUE), transf=exp)

##POLYNOMIAL MODEL

cp <- rma.mv(yi= lnor, V=se^2, mods = ~ poly(dose, degree=4, raw=TRUE)+0, data=final, 
             random = ~ 1 | cohort_id/line_id, test = "t", method = "REML")
summary(cp)

pred_cp <- predict(cp, newmods=unname(poly(s, degree=4, raw=TRUE)))
regplot(cp, mod=1, xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), pch=NA_integer_,
        ylim = c(0, 2), pred = pred_cp, xvals = s, main="Cubic Polynomial Regression")

#TO CHECK AND CORRECT: MULTIVARIATE FRACTIONAL POLYNOMIAL
library(mfp)
library(visreg)

#4 degrees of freedom
mfp <- mfp(formula = lnor ~ fp(dose, df = 4)+0 ,alpha=0.05, 
           data = final)
mfp
summary(mfp)

#plot for mfp
visual4<- glm(formula = lnor ~ I((dose/10)^1), data = final)
visreg(visual4,"dose", xlab="Alcohol intake, grams/day", trans=exp, ylab="Relative Risk")

#2 degrees of freedom
mfp2 <- mfp(formula = lnor ~ fp(dose, df = 2)+0,alpha=0.05, 
                 data = final)
mfp2
summary(mfp2)

visual2<- glm(formula = lnor ~ I((dose/10)^1), data = final)
visreg(visual2,"dose", xlab="Alcohol intake, grams/day", trans=exp, ylab="Relative Risk")

###doesnt run: FRACPOL - mfp using rma function with the selected p from the previous analysis
mfp_fracpol <- rma.mv(yi=lnor, V=se^2, mods = ~ fracpol(dose/10, p = c(0,1)) +0, data=final, 
                      random = ~ 1 | cohort_id/line_id, method = "REML")
summary(mfp_fracpol)

pred_fp <- predict(mfp_fracpol, newmods=unname(fracpol(s/10, p = c(0,1))))
regplot(mfp_fracpol, mod=1, xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), pch=NA_integer_,
        ylim = c(0, 2), pred = pred_fp, xvals = s, main="Polynomial Regression")

##MODEL COMPARISON 
fitstats(linear, quad, rcs, cp)

anova(rcs, cp)

####MALE MODELS

male <- dataset %>%
  filter(analysis_id==0 & dose != 0.00 & sex ==1)

dim(table(male$results_id))

##LINEAR REGRESSION

linear_male <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+0, data=male,
                 random = ~ 1 | cohort_id/line_id, method = "REML")
summary(linear_male)

ms <- seq(0,150,length=150)
pred_lin_male <- predict(linear_male, cbind(ms))
regplot(linear_male, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), 
        ylim = c(0, 3), pred = pred_lin_male, xvals = ms, main="Male - Linear Regression")

weights(linear_male)

##QUADRATIC REGRESSION

quad_male <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=male, 
               random = ~ 1 | cohort_id/line_id, method = "REML")
summary(quad_male)

pred_quad_male <- predict(quad_male, newmods=cbind(ms,ms^2))
regplot(quad_male, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), 
        ylim = c(0, 3), pred = pred_quad_male, xvals = ms, main="Male - Quadratic Regression")

##RESTRICTED CUBIC SPLINE

library(rms)
ms <- seq(0,150,length=150)
knotsm <- quantile(male$dose, c(.05, .35, .65, .95))

rcs_male <- rma.mv(yi= lnor ~ rcs(dose, knotsm)+0, V=se^2, data=male, 
              random = ~ 1 | cohort_id/line_id, method = "REML")
summary(rcs_male)

pred_rcs_male <- predict(rcs_male, newmods=rcspline.eval(ms, knotsm, inclx=TRUE))
regplot(rcs_male, mod="rcs(dose, knotsm)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), pch=NA_integer_,
        ylim = c(0, 3), pred = pred_rcs_male, xvals = ms, main="Male - RCS Regression")
abline(v=knotsm, lty="dotted")

#ERASE ESTIMATES FROM GRAPH: pch=NA_integer_, 

weights(rcs)

#prediction rcs model
predict(rcs_male, newmods= rcspline.eval(50, knotsm, inclx=TRUE), transf=exp)

##POLYNOMIAL MODEL

cp_male <- rma.mv(yi= lnor, V=se^2, mods = ~ poly(dose, degree=3, raw=TRUE)+0, data=male, 
             random = ~ 1 | cohort_id/line_id,  method = "REML")
summary(cp_male)

pred_cp_male <- predict(cp_male, newmods=unname(poly(ms, degree=3, raw=TRUE)))
regplot(cp_male, mod=1, xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), pch=NA_integer_,
        ylim = c(0, 2), pred = pred_cp_male, xvals = ms, main="Cubic Polynomial Regression")

##MODEL COMPARISON 
fitstats(linear_male, quad_male, rcs_male, cp_male)

anova(rcs_male, quad_male, refit=TRUE)


####FEMALE MODELS

female <- dataset %>%
  filter(analysis_id==0 & dose != 0.00 & sex ==0)

dim(table(female$results_id))

##LINEAR REGRESSION

linear_female <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+0, data=female,
                      random = ~ 1 | cohort_id/line_id, method = "ML")
summary(linear_female)

fs <- seq(0,150,length=150)
pred_lin_female <- predict(linear_female, cbind(fs))
regplot(linear_female, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), 
        ylim = c(0, 3), pred = pred_lin_female, xvals = fs, main="Female - Linear Regression")

weights(linear_male)

##QUADRATIC REGRESSION

quad_female <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=female, 
                    random = ~ 1 | cohort_id/line_id, method = "ML")
summary(quad_female)

pred_quad_female <- predict(quad_female, newmods=cbind(fs,fs^2))
regplot(quad_female, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), 
        ylim = c(0, 3), pred = pred_quad_female, xvals = fs, main="Female - Quadratic Regression")

##RESTRICTED CUBIC SPLINE

library(rms)
fs <- seq(0,150,length=150)
knotsf <- quantile(female$dose, c(.05, .35, .65, .95))

rcs_female <- rma.mv(yi= lnor ~ rcs(dose, knotsf)+0, V=se^2, data=female, 
                   random = ~ 1 | cohort_id/line_id, method = "ML")
summary(rcs_female)

pred_rcs_female <- predict(rcs_female, newmods=rcspline.eval(fs, knotsf, inclx=TRUE))
regplot(rcs_female, mod="rcs(dose, knotsf)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), 
        ylim = c(0, 2), pred = pred_rcs_female, xvals = fs, main="Female - RCS Regression")
abline(v=knotsf, lty="dotted")

i2 <- var.comp(rcs_female)
summary(i2)
i2$results
i2$totalI2
i2$plot

#ERASE ESTIMATES FROM GRAPH: pch=NA_integer_, 

#prediction rcs model
predict(rcs_female, newmods= rcspline.eval(50, knotsf, inclx=TRUE), transf=exp)

##POLYNOMIAL MODEL

cp_female <- rma.mv(yi= lnor, V=se^2, mods = ~ poly(dose, degree=3, raw=TRUE)+0, data=female, 
                  random = ~ 1 | cohort_id/line_id,  method = "ML")
summary(cp_female)

pred_cp_female <- predict(cp_female, newmods=unname(poly(fs, degree=3, raw=TRUE)))
regplot(cp_female, mod=1, xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), 
        ylim = c(0, 2), pred = pred_cp_female, xvals = fs, main="Polynomial Regression")

fitstats(linear_female, quad_female, rcs_female, cp_female)

