library(tidyverse)
library(mvmeta)
library(dosresmeta)

library(meta)
library(metafor)

library(readxl)
step1to3 <- read_excel("CAMH/SIMAH/SIMAH_dataset/1LC_step1to3_all.xlsx", 
                  col_types = c("numeric", "numeric", "text", 
                                "numeric", "text", "numeric", "numeric", "numeric",
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric"))

step1to3$sex <- as.factor(step1to3$sex)

######STEP 1: STUDIES WITH LIFETIME ABSTAINERS (LTA) AS REFERENCE

LTA <- step1to3 %>%
  filter(LTA ==1 & ref.nonzero==0 & dose != 0.00)

dim(table(LTA$study))

####LINEAR REGRESSION

#METAANALYSIS
LTA_meta <- metagen(TE = lnor,
                   lower = ci.lnl,
                   upper = ci.lnh,
                   level.ci = 0.95,
                   studlab = study,
                   data = LTA,
                   sm = "RR",
                   fixed = FALSE,
                   random = TRUE,
                   method.tau = "REML")
summary(LTA_meta)

forest.meta(LTA_meta,print.tau2 = FALSE, leftcols = c("studlab"), 
            leftlabs = c("Studies"), text.random = "Overall effect")

#METAREGRESSION
LTA_linr <- metareg(LTA_meta, ~dose, intercept = FALSE, method.tau = "REML")
summary(LTA_linr)

#prediction sequence for the plot
s <- seq(0,150,length=150)
pLTA <- predict(LTA_linr ,cbind(s))

#basic plot with metareg
plot(LTA$dose,LTA$or, ylim = c(0, 40), xlim = c(0,150))
lines(exp(pLTA$pred))

#plot for regression with weights
LTA.rma <- rma(yi=lnor, sei=se, mods = ~ dose+0, data=LTA, method = "REML")
LTA.rma

rpLTA <- predict(LTA.rma, cbind(s))
regplot(LTA.rma, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 40))

#predict linear model
predict(LTA_linr, 12, transf=exp)

####QUADRATIC REGRESSION

LTA.quad <- metareg(LTA_meta, ~dose + I(dose^2),intercept = FALSE, method.tau = "REML")
summary(LTA.quad)

#basic plot with metareg
quadpLTA <- predict(LTA.quad ,cbind(s,s^2)) 
plot(LTA$dose,LTA$or, ylim = c(0, 40), xlim = c(0,150))
lines(exp(quadpLTA$pred))

#plot rma with weights
LTA.rma.quad <- rma(yi=lnor, sei=se, mods = ~ dose + I(dose^2) +0, data=LTA, method = "REML")
LTA.rma.quad

rquadpLTA <- predict(LTA.rma.quad, newmods=cbind(s,s^2))
regplot(LTA.rma.quad, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 40), pred = rquadpLTA, xvals = s)

#predict quadratic model
predict(LTA.quad, c(150,150^2), transf=exp)
        
######STEP 2: REGRESSION WITH ABSTAINERS AS REFERENCE - SPLIT INTO LTA AND EXDRINKERS

step2 <- step1to3 %>%
  filter(ref.nonzero==0 & dose != 0.00)

dim(table(a.female$study))

####LINEAR REGRESSION

#metaanalysis
a.reg <- metagen(TE = lnor,
                       lower = ci.lnl,
                       upper = ci.lnh,
                       level.ci = 0.95,
                       studlab = study,
                       data = step2,
                       sm = "RR",
                       fixed = FALSE,
                       random = TRUE,
                       method.tau = "REML")
summary(a.reg)

#metaregression
a.lin <- metareg(a.reg, ~dose, intercept = FALSE, method.tau = "REML")
summary(a.lin)

#plot metareg
s <- seq(0,150,length=150)
a.p <- predict(a.lin ,cbind(s)) 
plot(step2$dose,step2$or, ylim = c(0, 80), xlim = c(0,150))
lines(exp(a.p$pred))

#plot regression
a.rma <- rma(yi=lnor, sei=se, mods = ~ dose+0, data=step2, method = "REML")
summary(a.rma)

a.rp <- predict(a.rma, cbind(s))
regplot(a.rma, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 80), pred = a.rp, xvals = s)

#prediction linear model
predict(a.rma, 150, transf=exp)

####QUADRATIC REGRESSION

a.quad <- metareg(a.reg, ~dose + I(dose^2), intercept = FALSE, method.tau = "REML")
summary(a.quad)

#plot metareg
a.quadp <- predict(a.quad ,cbind(s,s^2)) 
plot(step2$dose,step2$or, ylim = c(0, 80), xlim = c(0,150))
lines(exp(a.quadp$pred))

#plot rma
a.rma.quad <- rma(yi=lnor, sei = se, mods = ~ dose + I(dose^2)+0, data=step2, method = "REML")
a.rma.quad

a.rquadp <- predict(a.rma.quad, newmods=cbind(s,s^2))
regplot(a.rma.quad, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 80), pred = a.rquadp, xvals = s)

predict(a.quad, c(150,150^2), transf=exp)

####STEP 3: REGRESSION WITH ALL STUDIES WHICH HAD AN ABSTAINER CATEGORY -  but not as the reference

step3 <- step1to3 %>%
  filter(dose != 0.00)

####LINEAR REGRESSION

##metaanalysis
b.reg <- metagen(TE = lnor,
                     lower = ci.lnl,
                     upper = ci.lnh,
                     level.ci = 0.95,
                     studlab = study,
                     data = step3,
                     sm = "RR",
                     fixed = FALSE,
                     random = TRUE,
                     method.tau = "REML")
summary(b.reg)

##metaregression
b.lin <- metareg(b.reg, ~dose, intercept = FALSE, method.tau = "REML")
summary(b.lin)

#plot metareg
b.p <- predict(b.lin ,cbind(s)) 
plot(step3$dose,step3$or, ylim = c(0, 80), xlim = c(0,150))
lines(exp(b.p$pred))

#plot regression
b.rma <- rma(yi=lnor, sei=se, mods = ~ dose+0, data=step3, method = "REML")
summary(b.rma)

b.rp <- predict(b.rma, cbind(s))
regplot(b.rma, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 40), pred = b.rp, xvals = s)

#prediction linear model
predict(b.lin, 140, transf=exp)

####QUADRATIC REGRESSION

b.quad <- metareg(b.reg, ~dose + I(dose^2), intercept = FALSE, method.tau = "REML")
summary(b.quad)

#plot metareg
b.qp <- predict(b.quad ,cbind(s,s^2)) 
plot(step3$dose,step3$or, ylim = c(0, 80), xlim = c(0,150))
lines(exp(b.qp$pred))

#plot rma
b.rma.quad <- rma(yi=lnor, sei=se, mods = ~ dose + I(dose^2)+0, data=step3, method = "REML")
b.rma.quad

b.rquadp <- predict(b.rma.quad, newmods=cbind(s,s^2))
regplot(b.rma.quad, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 80), pred = b.rquadp, xvals = s)

#predict quadratic model
predict(b.quad, c(150,150^2), transf=exp)


###PREDICT FOR NEXT STEP

predict(b.lin, 0.26)
