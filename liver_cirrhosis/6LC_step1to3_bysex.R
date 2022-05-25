library(tidyverse)
library(mvmeta)
library(dosresmeta)

library(meta)
library(metafor)

library(readxl)
step1to3 <- read_excel("CAMH/SIMAH/SIMAH_dataset/6LC_step1to3_bysex.xlsx", 
                  col_types = c("numeric", "numeric", "text", 
                                "numeric", "text", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric"))

step1to3$sex <- as.factor(step1to3$sex)

######STEP 1: STUDIES WITH LIFETIME ABSTAINERS (LTA) AS REFERENCE

LTA.male <- step1to3 %>%
  filter(sex == 1 & LTA ==1 & ref.nonzero==0 & dose != 0.00)
LTA.female <- step1to3 %>%
  filter(sex == 2 & LTA ==1 & ref.nonzero==0 & dose != 0.00)

dim(table(LTA.female$study))
####MALE LINEAR REGRESSION

#METAANALYSIS
LTA.mr.male <- metagen(TE = lnor,
                   lower = ci.lnl,
                   upper = ci.lnh,
                   level.ci = 0.95,
                   studlab = study,
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
plot(LTA.male$dose,LTA.male$or, ylim = c(0, 40), xlim = c(0,150))
lines(exp(mpLTA$pred))

#plot for regression with weights
LTA.rma.lm <- rma(yi=lnor, sei=se, mods = ~ dose+0, data=LTA.male, method = "REML")
LTA.rma.lm

mrpLTA <- predict(LTA.rma.lm, cbind(ms))
regplot(LTA.rma.lm, mod="dose", xlab="Alcohol intake, grams/day - Male", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 40))
lines(exp(mrpLTA0$pred), lwd = "4", col = "blue")
lines(exp(mrpLTA0$ci.lb), lwd = "3", lty = "dotted", col = "blue")
lines(exp(mrpLTA0$ci.ub), lwd = "3", lty = "dotted", col = "blue")

#predict linear model
predict(LTA.lin.male, 12, transf=exp)
predict(LTA.rma.lm, 12, transf=exp)

####MALE QUADRATIC REGRESSION

LTA.quad.male <- metareg(LTA.mr.male, ~dose + I(dose^2),intercept = FALSE, method.tau = "REML")
summary(LTA.quad.male)

#basic plot with metareg
mquadpLTA <- predict(LTA.quad.male ,cbind(ms,ms^2)) 
plot(LTA.male$dose,LTA.male$or, ylim = c(0, 40), xlim = c(0,150))
lines(exp(mquadpLTA$pred))

#plot rma with weights
LTA.rma.quadm <- rma(yi=lnor, sei=se, mods = ~ dose + I(dose^2) +0, data=LTA.male, method = "REML")
LTA.rma.quadm

mrquadpLTA <- predict(LTA.rma.quadm, newmods=cbind(ms,ms^2))
regplot(LTA.rma.quadm, mod="dose", xlab="Alcohol intake, grams/day - Male", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 40), pred = mrquadpLTA, xvals = ms)

#predict quadratic model
predict(LTA.quad.male, c(150,150^2), transf=exp)
predict(LTA.rma.quadm, c(150,150^2), transf=exp)

#####FEMALE LINEAR REGRESSION

LTA.mr.female <- metagen(TE = lnor,
                     lower = ci.lnl,
                     upper = ci.lnh,
                     level.ci = 0.95,
                     studlab = study,
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
plot(LTA.female$dose,LTA.female$or, ylim = c(0, 40), xlim = c(0,150))
lines(exp(fpLTA$pred))

#plot regression
LTA.rma.lf <- rma(yi=lnor, sei=se, mods = ~ dose+0, data=LTA.female, method = "REML")
LTA.rma.lf

frpLTA <- predict(LTA.rma.lf, cbind(fs))
regplot(LTA.rma.lf, mod="dose", xlab="Alcohol intake, grams/day - Female", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 40), pred = frpLTA, xvals = fs)
lines(exp(frpLTA0$pred), lwd = "4", col = "blue")
lines(exp(frpLTA0$ci.lb), lwd = "3", lty = "dotted", col = "blue")
lines(exp(frpLTA0$ci.ub), lwd = "3", lty = "dotted", col = "blue")

weights(LTA.rma.lf)

#####FEMALE QUADRATIC REGRESSION

LTA.quad.female <- metareg(LTA.mr.female, ~dose + I(dose^2), intercept = FALSE, method.tau = "REML")
summary(LTA.quad.female)

#plot metareg
fquadpLTA <- predict(LTA.quad.female ,cbind(fs,fs^2)) 
plot(LTA.female$dose,LTA.female$or, ylim = c(0, 40), xlim = c(0,150))
lines(exp(fquadpLTA$pred))

#plot rma
LTA.rma.quadf <- rma(yi=lnor, sei=se, mods = ~ dose + I(dose^2)+0, data=LTA.female, method = "REML")
summary(LTA.rma.quadf)

frquadpLTA <- predict(LTA.rma.quadf, newmods=cbind(fs,fs^2))
regplot(LTA.rma.quadf, mod="dose", xlab="Alcohol intake, grams/day - Female", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 40), pred = frquadpLTA, xvals = fs)

#predict quadratic model
predict(LTA.quad.female, c(150,150^2), transf=exp)
predict(LTA.rma.quadf, c(150,150^2), transf=exp)

#prediction linear FOR ASKGAARD - exdrinkers
predict(LTA.lin.male, 12)
predict(LTA.lin.female, 12)
        
######STEP 2: REGRESSION WITH ABSTAINERS AS REFERENCE - SPLIT INTO LTA AND EXDRINKERS

a.male <- step1to3 %>%
  filter(sex == 1 & ref.nonzero==0 & dose != 0.00)
a.female <- step1to3 %>%
  filter(sex == 2 & ref.nonzero==0 & dose != 0.00)

dim(table(a.female$study))
####MALE LINEAR REGRESSION

#metaanalysis
a.mr.male <- metagen(TE = lnor,
                       lower = ci.lnl,
                       upper = ci.lnh,
                       level.ci = 0.95,
                       studlab = study,
                       data = a.male,
                       sm = "RR",
                       fixed = FALSE,
                       random = TRUE,
                       method.tau = "REML")
summary(a.mr.male)

#metaregression
a.lin.male <- metareg(a.mr.male, ~dose, intercept = FALSE, method.tau = "REML")
summary(a.lin.male)

#plot metareg
ms <- seq(0,150,length=150)
a.mp <- predict(a.lin.male ,cbind(ms)) 
plot(a.male$dose,a.male$or, ylim = c(0, 80), xlim = c(0,150))
lines(exp(a.mp$pred))

#plot regression
a.rma.lm <- rma(yi=lnor, sei=se, mods = ~ dose+0, data=a.male, method = "REML")
summary(a.rma.lm)

a.rpm <- predict(a.rma.lm, cbind(ms))
regplot(a.rma.lm, mod="dose", xlab="Alcohol intake, grams/day - Male", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 80), pred = a.rpm, xvals = ms)
lines(exp(a.rpm0$pred), lwd = "4", col = "blue")
lines(exp(a.rpm0$ci.lb), lwd = "3", lty = "dotted", col = "blue")
lines(exp(a.rpm0$ci.ub), lwd = "3", lty = "dotted", col = "blue")

#prediction linear model
predict(a.lin.male, 150, transf=exp)
predict(a.rma.lm, 150, transf=exp)

####MALE QUADRATIC REGRESSION

a.quad.male <- metareg(a.mr.male, ~dose + I(dose^2), intercept = FALSE, method.tau = "REML")
summary(a.quad.male)

#plot metareg
a.mquadp <- predict(a.quad.male ,cbind(ms,ms^2)) 
plot(a.male$dose,a.male$or, ylim = c(0, 80), xlim = c(0,150))
lines(exp(a.mquadp$pred))

#plot rma
a.rma.quadm <- rma(yi=lnor, sei = se, mods = ~ dose + I(dose^2)+0, data=a.male, method = "REML")
a.rma.quadm

a.mrquadp <- predict(a.rma.quadm, newmods=cbind(ms,ms^2))
regplot(a.rma.quadm, mod="dose", xlab="Alcohol intake, grams/day - Male", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 80), pred = a.mrquadp, xvals = ms)

predict(a.quad.male, c(150,150^2), transf=exp)
predict(a.rma.quadm, c(150,150^2), transf=exp)

####FEMALE LINEAR REGRESSION

a.mr.female <- metagen(TE = lnor,
                         lower = ci.lnl,
                         upper = ci.lnh,
                         level.ci = 0.95,
                         studlab = study,
                         data = a.female,
                         sm = "RR",
                         fixed = FALSE,
                         random = TRUE,
                         method.tau = "REML")
summary(a.mr.female)

a.lin.female <- metareg(a.mr.female, ~dose, intercept = FALSE, method.tau = "REML")
summary(a.lin.female)

#plot metareg
fs <- seq(0,150,length=150)
a.fp <- predict(a.lin.female ,cbind(fs)) 
plot(a.female$dose,a.female$or, ylim = c(0, 80), xlim = c(0,150))
lines(exp(a.fp$pred))

#plot regression
a.rma.lf <- rma(yi=lnor, sei=se, mods = ~ dose+0, data=a.female, method = "REML")
summary(a.rma.lf)

a.rpf <- predict(a.rma.lf, cbind(fs))
regplot(a.rma.lf, mod="dose", xlab="Alcohol intake, grams/day - Female", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 80))
lines(exp(a.rpf0$pred), lwd = "4", col = "blue")
lines(exp(a.rpf0$ci.lb), lwd = "3", lty = "dotted", col = "blue")
lines(exp(a.rpf0$ci.ub), lwd = "3", lty = "dotted", col = "blue")

##FEMALE QUADRATIC MODEL

a.quad.female <- metareg(a.mr.female, ~dose + I(dose^2), intercept = FALSE, method.tau = "REML")
summary(a.quad.female)

#plot metareg
a.fquadp <- predict(a.quad.female ,cbind(fs,fs^2)) 
plot(a.female$dose,a.female$or, ylim = c(0, 80), xlim = c(0,150))
lines(exp(a.fquadp$pred))

#plot rma
a.rma.quadf <- rma(yi=lnor, sei=se, mods = ~ dose + I(dose^2)+0, data=a.female, method = "REML")
a.rma.quadf

a.rfquadp <- predict(a.rma.quadf, newmods=cbind(fs,fs^2))
regplot(a.rma.quadf, mod="dose", xlab="Alcohol intake, grams/day - Female", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 80), pred = a.rfquadp, xvals = fs)

#predict quadratic model
predict(a.quad.female, c(150,150^2), transf=exp)
predict(a.rma.quadf, c(150,150^2), transf=exp)

####STEP 3: REGRESSION WITH ALL STUDIES WHICH HAD AN ABSTAINER CATEGORY -  but not as the reference

b.male <- step1to3 %>%
  filter(sex == 1 & dose != 0.00)
b.female <- step1to3 %>%
  filter(sex == 2 & dose != 0.00)

dim(table(b.female$study))

####MALE LINEAR REGRESSION

##metaanalysis
b.mr.male <- metagen(TE = lnor,
                     lower = ci.lnl,
                     upper = ci.lnh,
                     level.ci = 0.95,
                     studlab = study,
                     data = b.male,
                     sm = "RR",
                     fixed = FALSE,
                     random = TRUE,
                     method.tau = "REML")
summary(b.mr.male)

##metaregression
b.lin.male <- metareg(b.mr.male, ~dose, intercept = FALSE, method.tau = "REML")
summary(b.lin.male)

#plot metareg
b.mp <- predict(b.lin.male ,cbind(ms)) 
plot(b.male$dose,b.male$or, ylim = c(0, 80), xlim = c(0,150))
lines(exp(b.mp$pred))

#plot regression
b.rma.lm <- rma(yi=lnor, sei=se, mods = ~ dose+0, data=b.male, method = "REML")
summary(b.rma.lm)

b.rpm <- predict(b.rma.lm, cbind(ms))
regplot(b.rma.lm, mod="dose", xlab="Alcohol intake, grams/day - Male", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 80), pred = b.rpm, xvals = ms)
lines(exp(b.rpm0$pred), lwd = "4", col = "blue")
lines(exp(b.rpm0$ci.lb), lwd = "3", lty = "dotted", col = "blue")
lines(exp(b.rpm0$ci.ub), lwd = "3", lty = "dotted", col = "blue")

#prediction linear model
predict(b.lin.male, 150, transf=exp)
predict(b.rma.lm, 150, transf=exp)

####MALE QUADRATIC REGRESSION

b.quad.male <- metareg(b.mr.male, ~dose + I(dose^2), intercept = FALSE, method.tau = "REML")
summary(b.quad.male)

#plot metareg
b.mqp <- predict(b.quad.male ,cbind(ms,ms^2)) 
plot(b.male$dose,b.male$or, ylim = c(0, 80), xlim = c(0,150))
lines(exp(b.mqp$pred))

#plot rma
b.rma.quadm <- rma(yi=lnor, sei=se, mods = ~ dose + I(dose^2)+0, data=b.male, method = "REML")
b.rma.quadm

b.mrquadp <- predict(b.rma.quadm, newmods=cbind(ms,ms^2))
regplot(b.rma.quadm, mod="dose", xlab="Alcohol intake, grams/day - Male", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 80), pred = b.mrquadp, xvals = ms)

#predict quadratic model
predict(b.quad.male, c(150,150^2), transf=exp)
predict(b.rma.quadm, c(150,150^2), transf=exp)

####FEMALE LINEAR REGRESSION

#metaanalysis
b.mr.female <- metagen(TE = lnor,
                       lower = ci.lnl,
                       upper = ci.lnh,
                       level.ci = 0.95,
                       studlab = study,
                       data = b.female,
                       sm = "RR",
                       fixed = FALSE,
                       random = TRUE,
                       method.tau = "REML")
summary(b.mr.female)

#metaregression
b.lin.female <- metareg(b.mr.female, ~dose, intercept = FALSE, method.tau = "REML")
summary(b.lin.female)

#plot metareg
b.fp <- predict(b.lin.female ,cbind(fs)) 
plot(b.female$dose,b.female$or, ylim = c(0, 80), xlim = c(0,150))
lines(exp(b.fp$pred))

#plot regression
b.rma.lf <- rma(yi=lnor, sei=se, mods = ~ dose+0, data=b.female, method = "REML")
summary(b.rma.lf) 

b.rpf <- predict(b.rma.lf, cbind(fs))
regplot(b.rma.lf, mod="dose", xlab="Alcohol intake, grams/day - Female", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 80), pred = b.rpf, xvals = fs)
lines(exp(b.rpf0$pred), lwd = "4", col = "blue")
lines(exp(b.rpf0$ci.lb), lwd = "3", lty = "dotted", col = "blue")
lines(exp(b.rpf0$ci.ub), lwd = "3", lty = "dotted", col = "blue")

#predict linear model
predict(b.lin.female, 150, transf=exp)
predict(b.rma.lf, 150, transf=exp)

####FEMALE QUADRATIC MODEL

b.quad.female <- metareg(b.mr.female, ~dose + I(dose^2), intercept = FALSE, method.tau = "REML")
summary(b.quad.female)

#plot metareg
b.fqp <- predict(b.quad.female ,cbind(fs,fs^2)) 
plot(b.female$dose,b.female$or, ylim = c(0, 80), xlim = c(0,150))
lines(exp(b.fqp$pred))

#plot rma
b.rma.quadf <- rma(yi=lnor, sei=se, mods = ~ dose + I(dose^2)+0, data=b.female, method = "REML")
b.rma.quadf

b.frquadp <- predict(b.rma.quadf, newmods=cbind(fs,fs^2))
regplot(b.rma.quadf, mod="dose", xlab="Alcohol intake, grams/day - Female", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 80), pred = b.frquadp, xvals = fs)

#predict quadratic model
predict(b.quad.female, c(150,150^2), transf=exp)
predict(b.rma.quadf, c(150,150^2), transf=exp)

##WEIGHTS
weights(b.resfq)
plot(weights(b.resfq150), b.female150$lnor)

###PREDICT FOR NEXT STEP

predict(b.lin.male, 10)
predict(b.lin.female, 1)
