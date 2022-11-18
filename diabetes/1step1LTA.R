library(tidyverse)
library(ggplot2)
library(mvmeta)
library(dosresmeta)

library(meta)
library(metafor)

library(readxl)
dataset <- read_excel("CAMH/DIABETES/analysis/SIMAH_workplace/1dataset.xlsx", 
                        col_types = c("numeric", "numeric", "numeric", "numeric", "text", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric"))

dataset <- dataset %>%
  filter(analysis_id==0 & reference==0 & dose != 0.00)

dataset$inver_se <- 1/dataset$se
ggplot(dataset, aes(dose, rr, size=inver_se)) + geom_point (shape=1, colour="black") + scale_size_area(max_size=20) + xlim(0, 100)

######STEP 1: STUDIES WITH LIFETIME ABSTAINERS (LTA) AS REFERENCE

dataset <- dataset[-c(239, 240, 241, 242, 243, 244, 245, 246, 247, 248, 249,250),]

LTA.male <- dataset %>%
  filter(sex %in% c(1,2) & reference==0 & analysis_id==0 & dose != 0.00)
LTA.female <- dataset %>%
  filter(sex %in% c(0,2) & reference==0 & analysis_id==0 & dose != 0.00)


####MALE LINEAR REGRESSION

ggplot(LTA.male, aes(dose, lnor, size=inver_se)) + geom_point (shape=1, colour="black") + scale_size_area(max_size=20)


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

forest.meta(LTA.mr.male,
            print.tau2 = FALSE, leftcols = c("studlab"), 
            leftlabs = c("Studies"), text.random = "Overall effect")

#METAREGRESSION
LTA.lin.male <- metareg(LTA.mr.male, ~dose, intercept = FALSE, method.tau = "REML")
summary(LTA.lin.male)

#prediction sequence for the plot
ms <- seq(0,150,length=150)
mpLTA <- predict(LTA.lin.male ,cbind(ms))

#basic plot with metareg
plot(LTA.male$dose,LTA.male$rr, ylim = c(0, 4), xlim = c(0,150))
lines(exp(mpLTA$pred))

#plot for regression with weights
LTA.rma.lm <- rma(yi=lnor, sei=se, mods = ~ dose+0, data=LTA.male, method = "REML")
LTA.rma.lm

mrpLTA <- predict(LTA.rma.lm, cbind(s))
regplot(LTA.rma.lm, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,80), 
        ylim = c(0, 2), pred = FALSE, shade =FALSE, ci = FALSE, main="Males - Incident T2DM")

#predict linear model
predict(LTA.lin.male, 12, transf=exp)
predict(LTA.rma.lm, 12, transf=exp)

####MALE QUADRATIC REGRESSION

LTA.quad.male <- metareg(LTA.mr.male, ~dose + I(dose^2),intercept = FALSE, method.tau = "REML")
summary(LTA.quad.male)

#basic plot with metareg
mquadpLTA <- predict(LTA.quad.male ,cbind(ms,ms^2)) 
plot(LTA.male$dose,LTA.male$rr, ylim = c(0, 40), xlim = c(0,150))
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


#####FEMALE LINEAR REGRESSION

ggplot(LTA.female, aes(dose, lnor, size=inver_se)) + geom_point (shape=1, colour="black") + scale_size_area(max_size=20)

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

forest.meta(LTA.mr.female,
            print.tau2 = FALSE, leftcols = c("studlab"), 
            leftlabs = c("Studies"), text.random = "Overall effect")

LTA.lin.female <- metareg(LTA.mr.female, ~dose, intercept = FALSE, method.tau = "REML")
summary(LTA.lin.female)

#SEQUENCE FOR PLOTTING FEMALE
fs <- seq(0,150,length=150)

#basic plot with metareg
fpLTA <- predict(LTA.lin.female ,cbind(fs)) 
plot(LTA.female$dose,LTA.female$rr, ylim = c(0, 40), xlim = c(0,150))
lines(exp(fpLTA$pred))

#plot regression
LTA.rma.lf <- rma(yi=lnor, sei=se, mods = ~ dose+0, data=LTA.female, method = "REML")
LTA.rma.lf

frpLTA <- predict(LTA.rma.lf, cbind(fs))
regplot(LTA.rma.lf, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,60), 
        ylim = c(0, 2), xvals = fs, pred = FALSE, shade =FALSE, ci = FALSE, main="Females - Incident T2DM")

weights(LTA.rma.lf)

#####FEMALE QUADRATIC REGRESSION

LTA.quad.female <- metareg(LTA.mr.female, ~dose + I(dose^2), intercept = FALSE, method.tau = "REML")
summary(LTA.quad.female)

#plot metareg
fquadpLTA <- predict(LTA.quad.female ,cbind(fs,fs^2)) 
plot(LTA.female$dose,LTA.female$rr, ylim = c(0, 40), xlim = c(0,150))
lines(exp(fquadpLTA$pred))

#plot rma
LTA.rma.quadf <- rma(yi=lnor, sei=se, mods = ~ dose + I(dose^2)+0, data=LTA.female, method = "REML")
summary(LTA.rma.quadf)

frquadpLTA <- predict(LTA.rma.quadf, newmods=cbind(fs,fs^2))
regplot(LTA.rma.quadf, mod="dose", xlab="Alcohol intake, grams/day - Female", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), 
        ylim = c(0, 2), pred = frquadpLTA, xvals = fs)


##FOR SIMAH

#both sexes

dataset <- dataset[-c(48, 49, 50, 51, 52, 53, 54, 55, 56, 57),]
ggplot(dataset, aes(dose, rr, size=inver_se)) + geom_point (shape=1, colour="black") + scale_size_area(max_size=20) + xlim(0, 100)

both <- dataset[-c(7,8,9,10),]
ggplot(both, aes(dose, rr, size=inver_se)) + geom_point (shape=1, colour="black") + scale_size_area(max_size=20) + xlim(0, 100)

LTA.both <- rma(yi=lnor, sei=se, mods = ~ dose+0, data=both, method = "REML")
LTA.both

s <- seq(0,150,length=150)
m.LTA.both <- predict(LTA.both, cbind(s))
regplot(linearb , mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,80), main="Both sexes - Incident T2DM", 
        ylim = c(0, 2.5), xvals = s, pred = FALSE, shade =FALSE, ci = FALSE)

linearb <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+0, data=both,
                  random = ~ 1 | results_id, method = "REML")

#male

LTA.male <- dataset %>%
  filter(sex == 1)

rma.LTA.male <- rma(yi=lnor, sei=se, mods = ~ dose+0, data=LTA.male, method = "REML")
rma.LTA.male

m.LTA.male <- predict(rma.LTA.male, cbind(s))
regplot(rma.LTA.male, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,80), main="Males - Incident T2DM",
        ylim = c(0, 2), xvals = s, pred = FALSE, shade =FALSE, ci = FALSE)

linearm <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+0, data=LTA.male,
                  random = ~ 1 | results_id, method = "REML")

#rcs
knotsm <- quantile(LTA.male$dose, c(.05, .35, .65, .95))

rcs_m <- rma.mv(yi= lnor ~ rcs(dose, knotsm)+0, V=se^2, data=LTA.male, 
                random = ~ 1 | results_id, method = "REML")
summary(rcs_m)

pred_rcs_m <- predict(rcs_m, newmods=rcspline.eval(s, knotsm, inclx=TRUE))
regplot(rcs_m, mod="rcs(dose, knotsm)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,80), 
        ylim = c(0, 2), pred = pred_rcs_m, xvals = s, main="Male RCS Regression")
abline(v=knotsm, lty="dotted")


LTA.rma.rcsm <- rma(yi=lnor, sei=se, mods = ~ rcs(dose, knotsm)+0, data=LTA.male, method = "REML")
LTA.rma.rcsm

pLTA.rma.rcsm <- predict(LTA.rma.rcsm, newmods=rcspline.eval(s, knotsm, inclx=TRUE))
regplot(LTA.rma.rcsm, mod="rcs(dose, knotsm)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,80), 
        ylim = c(0, 2), pred = pLTA.rma.rcsm, xvals = s, main="Male RCS Regression")

#female

LTA.female <- dataset %>%
  filter(sex == 0)

rma.LTA.female <- rma(yi=lnor, sei=se, mods = ~ dose+0, data=LTA.female, method = "REML")
rma.LTA.female

m.LTA.female <- predict(rma.LTA.female, cbind(s))
regplot(linearf, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,60), main="Females - Incident T2DM",
        ylim = c(0, 2), xvals = s, pred = FALSE, shade =FALSE, ci = FALSE)

linearf <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+0, data=LTA.female,
                 random = ~ 1 | results_id, method = "REML")


#rcs
knotsf <- quantile(LTA.female$dose, c(.05, .35, .65, .95))

rcs_f <- rma.mv(yi= lnor ~ rcs(dose, knotsf)+0, V=se^2, data=LTA.female, 
              random = ~ 1 | results_id, method = "REML")
summary(rcs_f)

pred_rcs_f <- predict(rcs_f, newmods=rcspline.eval(s, knotsf, inclx=TRUE))
regplot(rcs_f, mod="rcs(dose, knotsf)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,60), 
        ylim = c(0, 2), pred = pred_rcs_f, xvals = s, main="Female RCS Regression")
abline(v=knotsf, lty="dotted")

LTA.rma.rcsf <- rma(yi=lnor, sei=se, mods = ~ rcs(dose, knotsf)+0, data=LTA.female, method = "REML")
LTA.rma.rcsf

pLTA.rma.rcsf <- predict(LTA.rma.rcsf, newmods=rcspline.eval(s, knotsf, inclx=TRUE))
regplot(LTA.rma.rcsf, mod="rcs(dose, knotsf)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,60), 
        ylim = c(0, 2), pred = pLTA.rma.rcsf, xvals = s, main="Female RCS Regression")
