library(tidyverse)
library(ggplot2)
library(mvmeta)
library(dosresmeta)

library(meta)
library(metafor)

library(readxl)
dataset <- read_excel("CAMH/DIABETES/analysis/SIMAH_workplace/3dataset_escalated.xlsx", 
                      col_types = c("numeric", "numeric", "numeric", "numeric", "text", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric"))

dataset <- dataset %>%
  filter(analysis_id==0 & reference %in% c(0,1,2) & dose != 0.00)

######STEP 2: REGRESSION WITH ABSTAINERS AS REFERENCE - ALREADY SPLIT INTO LTA AND EXDRINKERS
##AND PAPERS WITH ABSTAINERS AS CATEGORY BUT ANOTHER AC CATEGORY AS REFERENCE

male <- dataset %>%
  filter(sex==1)
female <- dataset %>%
  filter(sex==0)

dim(table(male$cohort_id))
dim(table(female$cohort_id))

####MALE LINEAR REGRESSION

#metaanalysis
meta.male <- metagen(TE = lnor,
                     lower = ln.low,
                     upper = ln.hi,
                     level.ci = 0.95,
                     studlab = results_id,
                     data = male,
                     sm = "RR",
                     fixed = FALSE,
                     random = TRUE,
                     method.tau = "REML")
summary(meta.male)

forest.meta(meta.male,
            print.tau2 = FALSE, leftcols = c("studlab"), 
            leftlabs = c("Studies"), text.random = "Overall effect")

#metaregression
linear.male <- metareg(meta.male, ~dose, intercept = FALSE, method.tau = "REML")
summary(linear.male)

#plot metareg
ms <- seq(0,150,length=150)
predict.linear.male <- predict(linear.male ,cbind(ms)) 
plot(male$dose,male$rr, ylim = c(0, 4), xlim = c(0,100))
lines(exp(predict.linear.male$pred))

regplot(linear.male, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), 
        ylim = c(0, 4), pred = predict.linear.male, xvals = ms)

#plot regression
rma.lm <- rma(yi=lnor, sei=se, mods = ~ dose+0, data=male, method = "REML")
summary(rma.lm)

predict.rma.lm <- predict(rma.lm, cbind(ms))
regplot(rma.lm, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), 
        ylim = c(0, 4), pred = predict.rma.lm, xvals = ms)

#prediction linear model
predict(linear.male, 150, transf=exp)
predict(rma.lm, 150, transf=exp)

####MALE QUADRATIC REGRESSION

quad.male <- metareg(meta.male, ~dose + I(dose^2), intercept = FALSE, method.tau = "REML")
summary(quad.male)

#plot metareg
predict.quad.male <- predict(quad.male ,cbind(ms,ms^2)) 
plot(male$dose,male$rr, ylim = c(0, 4), xlim = c(0,100))
lines(exp(predict.quad.male$pred))

regplot(quad.male, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), 
        ylim = c(0, 4), pred = predict.quad.male, xvals = ms)

#plot rma
rma.quadm <- rma(yi=lnor, sei = se, mods = ~ dose + I(dose^2)+0, data=male, method = "REML")
rma.quadm

predict.rma.qm <- predict(rma.quadm, newmods=cbind(ms,ms^2))
regplot(rma.quadm, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), 
        ylim = c(0, 4), pred = predict.rma.qm, xvals = ms)

predict(quad.male, c(150,150^2), transf=exp)
predict(rma.quadm, c(150,150^2), transf=exp)

#MALE RESTRICTIVE CUBIC SPLINES

library(rms)
knotsm <- quantile(male$dose, c(.05, .35, .65, .95))

rma.rcsm <- rma(yi=lnor, sei=se, mods = ~ rcs(dose, knotsm)+0, data=male, method = "REML")
rma.rcsm

predict.rma.rcsm <- predict(rma.rcsm, newmods=rcspline.eval(ms, knotsm, inclx=TRUE))
regplot(rma.rcsm, mod="rcs(dose, knotsm)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), 
        ylim = c(0, 2), pred = predict.rma.rcsm, xvals = ms, main="Male RCS Regression")

predict(rma.rcsm, newmods=rcspline.eval(30, knotsm, inclx=TRUE), transf=exp)

#metareg
rcs.male <- metareg(meta.male, ~ rcs(dose, knotsm), intercept = FALSE, method.tau = "REML")
summary(rcs.male)
predict.rcsm <- predict(rcs.male, newmods=rcspline.eval(ms, knotsm, inclx=TRUE))
regplot(rcs.male, mod="rcs(dose, knotsm)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,80), 
        ylim = c(0, 2), pred = predict.rcsm, xvals = ms, main="Male RCS Regression")


####FEMALE LINEAR REGRESSION

meta.female <- metagen(TE = lnor,
                       lower = ln.low,
                       upper = ln.hi,
                       level.ci = 0.95,
                       studlab = results_id,
                       data = female,
                       sm = "RR",
                       fixed = FALSE,
                       random = TRUE,
                       method.tau = "REML")
summary(meta.female)

lin.female <- metareg(meta.female, ~dose, intercept = FALSE, method.tau = "REML")
summary(lin.female)

#plot metareg
fs <- seq(0,150,length=150)
predict.linear.female <- predict(lin.female ,cbind(fs)) 
plot(female$dose,female$rr, ylim = c(0, 4), xlim = c(0,80))
lines(exp(predict.linear.female$pred))

#plot regression
rma.linearf <- rma(yi=lnor, sei=se, mods = ~ dose+0, data=female, method = "REML")
summary(rma.linearf)

predict.rma.lf <- predict(rma.linearf, cbind(fs))
regplot(rma.linearf, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,80), 
        ylim = c(0, 4), pred = predict.rma.lf, xvals = fs)

##FEMALE QUADRATIC MODEL

quad.female <- metareg(meta.female, ~dose + I(dose^2), intercept = FALSE, method.tau = "REML")
summary(quad.female)

#plot metareg
predict.quad.female <- predict(quad.female ,cbind(fs,fs^2)) 
plot(female$dose,female$rr, ylim = c(0, 4), xlim = c(0,80))
lines(exp(predict.quad.female$pred))

regplot(quad.female, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,80), 
        ylim = c(0, 4), pred = predict.quad.female, xvals = fs)

#plot rma
rma.quadf <- rma(yi=lnor, sei=se, mods = ~ dose + I(dose^2)+0, data=female, method = "REML")
rma.quadf

predict.rma.quadf <- predict(rma.quadf, newmods=cbind(fs,fs^2))
regplot(rma.quadf, mod="dose", xlab="Alcohol intake, grams/day - Female", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,80), 
        ylim = c(0, 4), pred = predict.rma.quadf, xvals = fs)

#predict quadratic model
predict(quad.female, c(150,150^2), transf=exp)
predict(rma.quadf, c(150,150^2), transf=exp)

#FEMALE RESTRICTIVE CUBIC SPLINES

library(rms)
knotsf <- quantile(female$dose, c(.05, .35, .65, .95))

rma.rcsf <- rma(yi=lnor, sei=se, mods = ~ rcs(dose, knotsf)+0, data=female, method = "REML")
rma.rcsf

predict.rma.rcsf <- predict(rma.rcsf, newmods=rcspline.eval(fs, knotsf, inclx=TRUE))
regplot(rma.rcsf, mod="rcs(dose, knotsf)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,80), 
        ylim = c(0, 4), pred = predict.rma.rcsf, xvals = fs, main="Female RCS Regression")

predict(rma.rcsf, newmods=rcspline.eval(120, knotsf, inclx=TRUE), transf=exp)

#metareg
rcs.female <- metareg(meta.female, ~ rcs(dose, knotsf), intercept = FALSE, method.tau = "REML")
summary(rcs.female)
predict.rcsf <- predict(rcs.female, newmods=rcspline.eval(fs, knotsf, inclx=TRUE))
regplot(rcs.female, mod="rcs(dose, knotsf)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,80), 
        ylim = c(0, 2), pred = predict.rcsf, xvals = fs, main="Female RCS Regression")

###PREDICT FOR NEXT STEP

predict(linear.male, 6.9)
predict(lin.female, 6.9)
