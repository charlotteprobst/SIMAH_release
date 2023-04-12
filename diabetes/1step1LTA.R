library(tidyverse)
library(ggplot2)
library(mvmeta)
library(dosresmeta)

library(meta)
library(metafor)

library(readxl)
dataset <- read_excel("CAMH/DIABETES/analysis/SIMAH_workplace/1dataset.xlsx", 
                        col_types = c("numeric", "numeric", "numeric", "numeric", "numeric", "text", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric"))

dataset <- dataset %>%
  filter(analysis_id==0 & reference==0)

######STEP 1: STUDIES WITH LIFETIME ABSTAINERS (LTA) AS REFERENCE

LTA.male <- dataset %>%
  filter(sex %in% c(1) & dose != 0.00)
LTA.female <- dataset %>%
  filter(sex %in% c(0) & dose != 0.00)

####MALE LINEAR REGRESSION

#METAANALYSIS
LTA.mr.male <- metagen(TE = lnor,
                       seTE = se,
                       level.ci = 0.95,
                       studlab = results_id,
                       data = LTA.male,
                       sm = "RR",
                       fixed = FALSE,
                       random = TRUE,
                       method.tau = "REML")
summary(LTA.mr.male)

#METAREGRESSION
LTA.lin.male <- metareg(LTA.mr.male, ~dose, intercept = FALSE, method.tau = "REML")
summary(LTA.lin.male)

#prediction sequence for the plot
ms <- seq(0,150,length=150)
mpLTA <- predict(LTA.lin.male ,cbind(ms))

#plot with metareg
plot(LTA.male$dose,LTA.male$rr, ylim = c(0, 4), xlim = c(0,100))
lines(exp(mpLTA$pred))

regplot(LTA.lin.male, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), 
        ylim = c(0, 2), pred = mpLTA, xvals = ms, main="Male - Linear Regression")

#plot for regression with weights
LTA.rma.lm <- rma(yi=lnor, sei=se, mods = ~ dose+0, data=LTA.male, method = "REML")
LTA.rma.lm

mrpLTA <- predict(LTA.rma.lm, cbind(ms))

regplot(LTA.rma.lm, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), 
        ylim = c(0, 2), pred = mpLTA, xvals = ms, main="Male - Linear Regression")

#predict linear model
predict(LTA.lin.male, 12, transf=exp)
predict(LTA.rma.lm, 12, transf=exp)

#rma.mv
LTAlinear_male <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+0, data=LTA.male,
                 random = ~ 1 | cohort_id/line_id, test = "t", method = "REML")
summary(LTAlinear_male)

ms <- seq(0,150,length=150)
pred_LTAlin_male <- predict(LTAlinear_male, cbind(ms))
regplot(LTAlinear_male, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), 
        ylim = c(0.5, 2), pred = pred_LTAlin_male, xvals = ms, main="Male - Linear Regression")

#dosresmeta
LTA.male.dos <- dataset %>%
  filter(analysis_id==0 & reference==0 & sex ==1 & first_author != "Kerr")

linear_male <- dosresmeta(formula = lnor ~ dose, id = results_id, type = "cc", lb = rr.low, ub = rr.hi,
                          intercept = F, cases = n.outcome, n = n.alc, method = "reml", data = LTA.male.dos)
summary(linear_male)
dosex_bin <- data.frame(dose=seq(0, 100, 1))
with(predict(linear_male, dosex_bin, order=TRUE, exp=TRUE), 
     {plot(dose, pred, type="l", col="blue", ylim=c(0.5, 2), ylab= "Relative risk", xlab="Alcohol intake, grams/day - Male")
       lines(dose, ci.lb, lty=2)
       lines(dose, ci.ub, lty=2)})

#test rma.mv with no Kerr (missing N)

LTA.male <- LTA.male %>%
  filter(first_author != "Kerr")

LTA.male <- dataset %>%
  filter(sex %in% c(1) & dose != 0.00)

####MALE QUADRATIC REGRESSION

LTA.quad.male <- metareg(LTA.mr.male, ~dose + I(dose^2),intercept = FALSE, method.tau = "REML")
summary(LTA.quad.male)

#basic plot with metareg
mquadpLTA <- predict(LTA.quad.male ,cbind(ms,ms^2)) 
plot(LTA.male$dose,LTA.male$rr, ylim = c(0, 4), xlim = c(0,100))
lines(exp(mquadpLTA$pred))

#plot rma with weights
LTA.rma.quadm <- rma(yi=lnor, sei=se, mods = ~ dose + I(dose^2) +0, data=LTA.male, method = "REML")
LTA.rma.quadm

mrquadpLTA <- predict(LTA.rma.quadm, newmods=cbind(ms,ms^2))

regplot(LTA.rma.quadm, mod="dose", xlab="Alcohol intake, grams/day - Male", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), 
        ylim = c(0, 2), pred = mrquadpLTA, xvals = ms)

weights(LTA.rma.quadm)

#predict quadratic model
predict(LTA.quad.male, c(150,150^2), transf=exp)
predict(LTA.rma.quadm, c(150,150^2), transf=exp)

#rma.mv
LTAquad_male <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=LTA.male,
                         random = ~ 1 | cohort_id/line_id, test = "t", method = "REML")
summary(LTAquad_male)
pred_LTAquad_male <- predict(LTAquad_male, newmods=cbind(ms,ms^2))
regplot(LTAquad_male, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), 
        ylim = c(0, 2), pred = pred_LTAquad_male, xvals = ms, main="Male - Quadratic Regression")

#dosresmeta
quad_male <- dosresmeta(formula = lnor ~ dose + I(dose^2), id = results_id, type = "cc", lb = rr.low, ub = rr.hi,
                        intercept = F, cases = n.outcome, 
                        n = n.alc, method = "reml", data = LTA.male.dos)
summary(quad_male)

dosex_bin <- data.frame(dose=seq(0, 100, 1))
with(predict(quad_male, dosex_bin, order=TRUE, exp=TRUE), 
     {plot(dose, pred, type="l", col="blue", ylim=c(0.5, 2), ylab= "Relative risk", xlab="Alcohol intake, grams/day - Male")
       lines(dose, ci.lb, lty=2)
       lines(dose, ci.ub, lty=2)})

#test rma.mv with no Kerr (missing N)

LTA.male <- LTA.male %>%
  filter(first_author != "Kerr")

LTA.male <- dataset %>%
  filter(sex %in% c(1) & dose != 0.00)

#male rcs
library(rms)
knotsm <- quantile(LTA.male$dose, c(.05, .35, .65, .95))

rcs_m <- rma.mv(yi= lnor ~ rcs(dose, knotsm)+0, V=se^2, data=LTA.male, 
                random = ~ 1 | results_id/line_id, method = "REML")
summary(rcs_m)

pred_rcs_m <- predict(rcs_m, newmods=rcspline.eval(ms, knotsm, inclx=TRUE))
regplot(rcs_m, mod="rcs(dose, knotsm)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,80), 
        ylim = c(0, 2), pred = pred_rcs_m, xvals = ms, main="Male RCS Regression")
abline(v=knotsm, lty="dotted")

#dosresmeta
knotsm <- quantile(LTA.male.dos$dose, c(.05, .35, .65, .95))
splmale <- dosresmeta(formula = lnor ~ rcs(dose, knotsm), id = results_id, type = "cc", 
                      cases = n.outcome, n = n.alc, method = "reml",
                      data = LTA.male.dos, se = se, proc = "1stage")
summary(splmale)

dosex_bins <- data.frame(dose=seq(0, 100, 1))
xref <- 0
with(predict(splmale, dosex_bins, xref, exp = TRUE),
     {plot(get("rcs(dose, knotsm)dose"), pred, type= "l", ylim= c(0,2), ylab= "Relative risk", 
           xlab= "Alcohol consumption, grams/day - Male")
       matlines(get("rcs(dose, knotsm)dose"), cbind(ci.lb, ci.ub), col = 1, lty = "dashed")})


#####FEMALE LINEAR REGRESSION

LTA.mr.female <- metagen(TE = lnor,
                         seTE = se,
                         level.ci = 0.95,
                         studlab = results_id,
                         data = LTA.female,
                         sm = "RR",
                         fixed = FALSE,
                         random = TRUE,
                         method.tau = "REML")
summary(LTA.mr.female)

LTA.lin.female <- metareg(LTA.mr.female, ~dose, intercept = FALSE, method.tau = "REML")
summary(LTA.lin.female)

#SEQUENCE FOR PLOTTING FEMALE
fs <- seq(0,150,length=150)

#basic plot with metareg
fpLTA <- predict(LTA.lin.female ,cbind(fs)) 
plot(LTA.female$dose,LTA.female$rr, ylim = c(0, 4), xlim = c(0,100))
lines(exp(fpLTA$pred))

#plot regression
LTA.rma.lf <- rma(yi=lnor, sei=se, mods = ~ dose+0, data=LTA.female, method = "REML")
LTA.rma.lf

frpLTA <- predict(LTA.rma.lf, cbind(fs))
regplot(LTA.rma.lf, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,80), 
        ylim = c(0, 2), main="Females - Incident T2DM")

weights(LTA.rma.lf)

#rma.mv
LTAlinear_female <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+0, data=LTA.female,
                         random = ~ 1 | cohort_id/line_id, test = "t", method = "REML")
summary(LTAlinear_female)

fs <- seq(0,150,length=150)
pred_LTAlin_female <- predict(LTAlinear_female, cbind(fs))
regplot(LTAlinear_female, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), 
        ylim = c(0, 2), pred = pred_LTAlin_female, xvals = fs, main="Female - Linear Regression")

#dosresmeta
LTA.female.dos <- dataset %>%
  filter(analysis_id==0 & reference==0 & sex ==0 & first_author != "Kerr")

linear_female <- dosresmeta(formula = lnor ~ dose, id = results_id, type = "cc", lb = rr.low, ub = rr.hi,
                          intercept = F, cases = n.outcome, n = n.alc, method = "reml", data = LTA.female.dos)
summary(linear_female)
dosex_bin <- data.frame(dose=seq(0, 100, 1))
with(predict(linear_female, dosex_bin, order=TRUE, exp=TRUE), 
     {plot(dose, pred, type="l", col="blue", ylim=c(0, 2), ylab= "Relative risk", xlab="Alcohol intake, grams/day - Male")
       lines(dose, ci.lb, lty=2)
       lines(dose, ci.ub, lty=2)})


LTA.female <- LTA.female %>%
  filter(first_author != "Kerr")

#####FEMALE QUADRATIC REGRESSION

LTA.quad.female <- metareg(LTA.mr.female, ~dose + I(dose^2), intercept = FALSE, method.tau = "REML")
summary(LTA.quad.female)

#plot metareg
fquadpLTA <- predict(LTA.quad.female ,cbind(fs,fs^2)) 
plot(LTA.female$dose,LTA.female$rr, ylim = c(0, 4), xlim = c(0,100))
lines(exp(fquadpLTA$pred))

#plot rma
LTA.rma.quadf <- rma(yi=lnor, sei=se, mods = ~ dose + I(dose^2)+0, data=LTA.female, method = "REML")
summary(LTA.rma.quadf)

frquadpLTA <- predict(LTA.rma.quadf, newmods=cbind(fs,fs^2))
regplot(LTA.rma.quadf, mod="dose", xlab="Alcohol intake, grams/day - Female", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), 
        ylim = c(0, 2), pred = frquadpLTA, xvals = fs)

#rma.mv
LTAquad_female <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=LTA.female,
                       random = ~ 1 | cohort_id/line_id, test = "t", method = "REML")
summary(LTAquad_male)
pred_LTAquad_female <- predict(LTAquad_female, newmods=cbind(fs,fs^2))
regplot(LTAquad_female, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), 
        ylim = c(0, 2), pred = pred_LTAquad_female, xvals = fs, main="Female - Quadratic Regression")

#dosresmeta
quad_female <- dosresmeta(formula = lnor ~ dose + I(dose^2), id = results_id, type = "cc", lb = rr.low, ub = rr.hi,
                        intercept = F, cases = n.outcome, n = n.alc, method = "reml", data = LTA.female.dos)
summary(quad_female)

dosex_bin <- data.frame(dose=seq(0, 100, 1))
with(predict(quad_female, dosex_bin, order=TRUE, exp=TRUE), 
     {plot(dose, pred, type="l", col="blue", ylim=c(0.5, 2), ylab= "Relative risk", xlab="Alcohol intake, grams/day - Male")
       lines(dose, ci.lb, lty=2)
       lines(dose, ci.ub, lty=2)})

#female rcs

knotsf <- quantile(LTA.female$dose, c(.05, .35, .65, .95))

rcs_f <- rma.mv(yi= lnor ~ rcs(dose, knotsf)+0, V=se^2, data=LTA.female, 
              random = ~ 1 | results_id/line_id, method = "REML")
summary(rcs_f)

pred_rcs_f <- predict(rcs_f, newmods=rcspline.eval(fs, knotsf, inclx=TRUE))
regplot(rcs_f, mod="rcs(dose, knotsf)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,80), 
        ylim = c(0, 2), pred = pred_rcs_f, xvals = fs, main="Female RCS Regression")
abline(v=knotsf, lty="dotted")

#dosresmeta
knotsf <- quantile(LTA.female.dos$dose, c(.05, .35, .65, .95))
splfemale <- dosresmeta(formula = lnor ~ rcs(dose, knotsf), id = results_id, type = "cc", 
                      cases = n.outcome, n = n.alc, method = "reml",
                      data = LTA.female.dos, se = se, proc = "1stage")

summary(splfemale)

dosex_bins <- data.frame(dose=seq(0, 150, 1))
xref <- 0
with(predict(splfemale, dosex_bins, xref, exp = TRUE),
     {plot(get("rcs(dose, knotsf)dose"), pred, type= "l", ylim= c(0,2), ylab= "Relative risk", 
           xlab= "Alcohol consumption, grams/day - Female")
       matlines(get("rcs(dose, knotsf)dose"), cbind(ci.lb, ci.ub), col = 1, lty = "dashed")})
