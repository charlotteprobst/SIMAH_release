library(tidyverse)
library(meta)
library(metafor)
library(dosresmeta)

library(mvmeta)

# Personal computer; specify locations 
data   <- "C:/Users/laura/Documents/CAMH/SIMAH/SIMAH_workplace/liver_cirrhosis/"    # Location of data

# load data
step4_all <- readRDS (paste0(data, "3LC_models_all.xlsx"))

library(readxl)
step4_all <- read_excel("CAMH/SIMAH/SIMAH_workplace/liver_cirrhosis/3LC_models_all.xlsx", 
                        col_types = c("numeric", "numeric", "text", 
                                      "numeric", "text", "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric","numeric", 
                                      "numeric", "text", "numeric", "numeric"))

final <- step4_all %>%
  filter(dose != 0.00)

##LINEAR REGRESSION

linear <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+0, data=final,
                    random = ~ 1 | study, method = "REML")
summary(linear)

s <- seq(0,150,length=150)
pred_lin <- predict(linear, cbind(s))
regplot(linear, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 30), pred = pred_lin, xvals = s, main="Linear Regression")

#basic plot to confirm regression line
plot(final$dose,final$or, ylim = c(0, 30), xlim = c(0,150))
lines(exp(pred_lin$pred))

##MQUADRATIC REGRESSION

quad <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=final, 
                     random = ~ 1 | study, method = "REML")
summary(quad)

pred_quad <- predict(quad, newmods=cbind(s,s^2))
regplot(quad, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 30), pred = pred_quad, xvals = s, main="Quadratic Regression")

#basic plot to confirm regression line
plot(final$dose,final$or, ylim = c(0, 30), xlim = c(0,150))
lines(exp(pred_quad$pred))

predict(pred_quad, c(50,50^2))

##RESTRICTED CUBIC SPLINE

library(rms)
s <- seq(0,150,length=150)
knots <- quantile(final$dose, c(.05, .35, .65, .95))

rcs <- rma.mv(yi= lnor ~ rcs(dose, knots)+0, V=se^2, data=final, 
                   random = ~ 1 | study, method = "REML")
summary(rcs)

pred_rcs <- predict(rcs, newmods=rcspline.eval(s, knots, inclx=TRUE))
regplot(rcs, mod="rcs(dose, knots)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 30), pred = pred_rcs, xvals = s, main="RCS Regression")
abline(v=knots, lty="dotted")
lines(exp(pred_quad$pred), lwd = "4", col = "blue")

weights(rcs)
plot(weights(rcs), final$dose)

#basic plot to confirm regression line
plot(final$dose,final$or, ylim = c(0, 30), xlim = c(0,150))
lines(exp(pred_rcs$pred))

#prediction rcs model
predict(rcs, newmods= rcspline.eval(100, knots, inclx=TRUE), transf=exp)

##CUBIC POLYNOMIAL MODEL

cp <- rma.mv(yi= lnor, V=se^2, mods = ~ poly(dose, degree=3, raw=TRUE)+0, data=final, 
                  random = ~ 1 | study, method = "REML")
summary(cp)

pred_cp <- predict(cp, newmods=unname(poly(s, degree=3, raw=TRUE)))
regplot(cp, mod=1, xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 30), pred = pred_cp, xvals = s, main="Cubic Polynomial Regression")

#MULTIVARIATE FRACTIONAL POLYNOMIAL
library(mfp)
library(visreg)

#4 degrees of freedom
mfp <- mfp(formula = lnor ~ fp(dose, df = 4)+0 ,alpha=0.05, 
               data = final)
mfp
summary(mfp)

#plot for mfp
visual4<- glm(formula = lnor ~ I((dose/100)^1) + I((dose/100)^3), data = final)
visreg(visual4,"dose", xlab="Alcohol intake, grams/day", trans=exp, ylab="Relative Risk")

#2 degrees of freedom
mfp2 <- mfp(formula = lnor ~ fp(dose, df = 2)+0,alpha=0.05, 
                 data = final)
mfp2
summary(mfp2)

visual2<- glm(formula = lnor ~ I((dose/100)^0.5), data = final)
visreg(visual2,"dose", xlab="Alcohol intake, grams/day", trans=exp, ylab="Relative Risk")

###FRACPOL - mfp using rma function with the selected p from the previous analysis
mfp_fracpol <- rma.mv(yi=lnor, V=se^2, mods = ~ fracpol(dose, p = c(1, 3)) +0, data=final, 
              random = ~ 1 | study, method = "REML")
summary(mfp_fracpol)

##MODEL COMPARISON 
fitstats(linear, quad, rcs, cp, mfp_fracpol)


###MORTALITY VS MORBIDITY
morbidity <- final %>%
  filter(mortality == 0)
mortality <- final %>%
  filter(mortality == 1)

s <- seq(0,150,length=150)

quad_morb <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=morbidity, 
               random = ~ 1 | study, method = "REML")
summary(quad_morb)

pred_quad_morb <- predict(quad_morb, newmods=cbind(s,s^2))
regplot(quad_morb, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 30), pred = pred_quad_morb, xvals = s)

quad_mort <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=mortality, 
                    random = ~ 1 | study, method = "REML")
summary(quad_mort)

pred_quad_mort <- predict(quad_mort, newmods=cbind(s,s^2))
regplot(quad_mort, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 30), pred = pred_quad_mort, xvals = s)

morbidity_m <- final %>%
  filter(sex== 1 & mortality == 0)
morbidity_f <- final %>%
  filter(sex== 2 & mortality == 0)
mortality_m <- final %>%
  filter(sex== 1 & mortality == 1)
mortality_f <- final %>%
  filter(sex== 2 & mortality == 1)

dim(table(mortality_m$study))

quad_morb_m <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=morbidity_m, 
                    random = ~ 1 | study, method = "REML")
summary(quad_morb_m)

pred_quad_morb_m <- predict(quad_morb_m, newmods=cbind(s,s^2))
regplot(quad_morb_m, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 30), pred = pred_quad_morb_m, xvals = s)

quad_morb_f <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=morbidity_f, 
                      random = ~ 1 | study, method = "REML")
summary(quad_morb_f)

pred_quad_morb_f <- predict(quad_morb_f, newmods=cbind(s,s^2))
regplot(quad_morb_f, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 30), pred = pred_quad_morb_f, xvals = s)

#using rcs
knots1 <- quantile(morbidity$dose, c(.05, .35, .65, .95))

rcs_morb <- rma.mv(yi= lnor ~ rcs(dose, knots1)+0, V=se^2, data=morbidity, 
              random = ~ 1 | study, method = "REML")
summary(rcs_morb)

pred_rcs_morb <- predict(rcs_morb, newmods=rcspline.eval(s, knots1, inclx=TRUE))
regplot(rcs_morb, mod="rcs(dose, knots1)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), 
        ylim = c(0, 30), pred = pred_rcs_morb, xvals = s, main="RCS Regression")
abline(v=knots1, lty="dotted")


knots2 <- quantile(mortality$dose, c(.05, .35, .65, .95))

rcs_mort <- rma.mv(yi= lnor ~ rcs(dose, knots2)+0, V=se^2, data=mortality, 
                   random = ~ 1 | study, method = "REML")
summary(rcs_mort)

pred_rcs_mort <- predict(rcs_mort, newmods=rcspline.eval(s, knots2, inclx=TRUE))
regplot(rcs_mort, mod="rcs(dose, knots2)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), 
        ylim = c(0, 30), pred = pred_rcs_mort, xvals = s, main="RCS Regression")
abline(v=knots2, lty="dotted")

##PURE MORBIDITY AND MORTALITY

puremorbidity <- final %>%
  filter(outcome %in% c(0,1))
puremortality <- final %>%
  filter(outcome == 2)

s <- seq(0,150,length=150)

knots3 <- quantile(puremorbidity$dose, c(.05, .35, .65, .95))

rcs_morb3 <- rma.mv(yi= lnor ~ rcs(dose, knots3)+0, V=se^2, data=puremorbidity, 
                   random = ~ 1 | study, method = "REML")
summary(rcs_morb3)

pred_rcs_morb3 <- predict(rcs_morb3, newmods=rcspline.eval(s, knots3, inclx=TRUE))
regplot(rcs_morb3, mod="rcs(dose, knots3)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 30), pred = pred_rcs_morb3, xvals = s, main="RCS Regression -pure")
abline(v=knots3, lty="dotted")


knots4 <- quantile(puremortality$dose, c(.05, .35, .65, .95))

rcs_mort4 <- rma.mv(yi= lnor ~ rcs(dose, knots4)+0, V=se^2, data=puremortality, 
                   random = ~ 1 | study, method = "REML")
summary(rcs_mort4)

pred_rcs_mort4 <- predict(rcs_mort4, newmods=rcspline.eval(s, knots4, inclx=TRUE))
regplot(rcs_mort4, mod="rcs(dose, knots4)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 30), pred = pred_rcs_mort4, xvals = s, main="RCS Regression-pure")
abline(v=knots4, lty="dotted")

##USA VS OTHERS

usa <- final %>%
  filter(usa == 1)
dim(table(usa$study))
s <- seq(0,150,length=150)

quad_usa <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=usa, 
                    random = ~ 1 | study, method = "REML")
summary(quad_usa)

pred_quad_usa <- predict(quad_usa, newmods=cbind(s,s^2))
regplot(quad_usa, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 30), pred = pred_quad_usa, xvals = s)

#using rcs
knots3 <- quantile(usa$dose, c(.05, .35, .65, .95))

rcs_usa <- rma.mv(yi= lnor ~ rcs(dose, knots3)+0, V=se^2, data=usa, 
                   random = ~ 1 | study, method = "REML")
summary(rcs_usa)

pred_rcs_usa <- predict(rcs_usa, newmods=rcspline.eval(s, knots3, inclx=TRUE))
regplot(rcs_usa, mod="rcs(dose, knots3)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 30), pred = pred_rcs_usa, xvals = s, main="RCS Regression")
abline(v=knots3, lty="dotted")

#models
usa$type <- as.factor(usa$type)
usa$mortality <- as.factor(usa$mortality)
usa$sex <- as.factor(usa$sex)
usa$qualitySC <- as.factor(usa$qualitySC)


usa1 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+dose*type, data=usa, 
                   random = ~ 1 | study, method = "REML")
usa1

anova(quad_usa, usa1)

usa1a <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+dose:type, data=usa, 
               random = ~ 1 | study, method = "REML")
usa1a

anova(usa1, usa1a)

usa2 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+dose:type + I(dose^2):type, data=usa, 
                random = ~ 1 | study, method = "REML")
usa2

anova(usa2, usa1a)

usa3 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+dose:type + dose*qualitySC, data=usa, 
               random = ~ 1 | study, method = "REML")
usa3

anova(usa3, usa1a)

usa3a <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+dose:type + dose:qualitySC, data=usa, 
               random = ~ 1 | study, method = "REML")
usa3a

anova(usa3, usa3a)

usa4 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+dose:type + dose:qualitySC+ I(dose^2):qualitySC, data=usa, 
                random = ~ 1 | study, method = "REML")
usa4

anova(usa4, usa3a)

usa5 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+dose:type + dose:qualitySC
                +dose*sex, data=usa, 
                random = ~ 1 | study, method = "REML")
usa5

anova(usa5, usa3a)

usa5a <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+dose:type + dose:qualitySC
               +dose:sex, data=usa, 
               random = ~ 1 | study, method = "REML")
usa5a

anova(usa5, usa5a)

usa6 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+dose:type + dose:qualitySC
                +dose:sex+ I(dose^2):sex, data=usa, 
                random = ~ 1 | study, method = "REML")
usa6

anova(usa6, usa5a)

###results by cause

#ALL LC
all <- final

library(rms)
s <- seq(0,150,length=150)
knots_all <- quantile(all$dose, c(.05, .35, .65, .95))

rcs_all <- rma.mv(yi= lnor ~ rcs(dose, knots_all)+0, V=se^2, data=all, 
                  random = ~ 1 | study, method = "REML")
summary(rcs_all)

pred_rcs_all <- predict(rcs_all, newmods=rcspline.eval(s, knots_all, inclx=TRUE))
regplot(rcs_all, mod="rcs(dose, knots_all)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 30), pred = pred_rcs_all, xvals = s, main="All LC RCS Regression")
abline(v=knots_all, lty="dotted")

predict(rcs_all, newmods= rcspline.eval(100, knots_all, inclx=TRUE), transf=exp)

#ALC 
alc <- final %>%
  filter(type == 1)

knots_alc <- quantile(alc$dose, c(.05, .35, .65, .95))

rcs_alc <- rma.mv(yi= lnor ~ rcs(dose, knots_alc)+0, V=se^2, data=alc, 
                  random = ~ 1 | study, method = "REML")
summary(rcs_alc)

pred_rcs_alc <- predict(rcs_alc, newmods=rcspline.eval(s, knots_alc, inclx=TRUE))
regplot(rcs_alc, mod="rcs(dose, knots_alc)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 100), pred = pred_rcs_alc, xvals = s, main="ALC RCS Regression")
abline(v=knots_alc, lty="dotted")

predict(rcs_alc, newmods= rcspline.eval(50, knots_alc, inclx=TRUE), transf=exp)

#HCV
hcv <- final %>%
  filter(type == 3)
hcv <- hcv[-c(14),]

knots_hcv <- quantile(hcv$dose, c(.05, .35, .65, .95))

rcs_hcv <- rma.mv(yi= lnor ~ rcs(dose, knots_hcv)+0, V=se^2, data=hcv, 
                  random = ~ 1 | study, method = "REML")
summary(rcs_hcv)

pred_rcs_hcv <- predict(rcs_hcv, newmods=rcspline.eval(s, knots_hcv, inclx=TRUE))
regplot(rcs_hcv, mod="rcs(dose, knots_hcv)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 30), pred = pred_rcs_hcv, xvals = s, main="HCV RCS Regression")
abline(v=knots_hcv, lty="dotted")
weights(rcs_hcv)

predict(rcs_hcv, newmods= rcspline.eval(50, knots_hcv, inclx=TRUE), transf=exp)

#graph types
pred_rcs_all <- predict(rcs_all, newmods=rcspline.eval(s, knots_all, inclx=TRUE))
regplot(rcs_all, mod="rcs(dose, knots_all)dose", xlab="Alcohol intake, grams/day", ylab="Relative risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,125), shade =FALSE, pch=NA_integer_, 
        ylim = c(0, 50), pred = pred_rcs_all, xvals = s, cex.lab=1.2, cex.axis=1.1)
lines(exp(pred_rcs_alc$pred), lwd = "4", col = "blue")
lines(exp(pred_rcs_alc$ci.lb), lwd = "3", lty = "dotted", col = "blue")
lines(exp(pred_rcs_alc$ci.ub), lwd = "3", lty = "dotted", col = "blue")
lines(exp(pred_rcs_hcv$pred), lwd = "4", col = "red")
lines(exp(pred_rcs_hcv$ci.lb), lwd = "3", lty = "dotted", col = "red")
lines(exp(pred_rcs_hcv$ci.ub), lwd = "3", lty = "dotted", col = "red")
legend("topleft",inset =0.1, legend=c("All Cause LC", "Alcohol-related LC", "HCV LC"), lty=1:1, lwd=3:3, cex=1.2, col=c("black", "blue", "red"))

#second sensitivity analysis
final2 <- final %>%  
  filter(studyyear != 1)

#RCS
s <- seq(0,150,length=150)
knots2 <- quantile(final2$dose, c(.05, .35, .65, .95))

rcs2 <- rma.mv(yi= lnor ~ rcs(dose, knots2)+0, V=se^2, data=final2, 
               random = ~ 1 | study, method = "REML")
summary(rcs2)

pred_rcs2 <- predict(rcs2, newmods=rcspline.eval(s, knots2, inclx=TRUE))
regplot(rcs2, mod="rcs(dose, knots2)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,125),
        ylim = c(0, 30), pred = pred_rcs2, xvals = s)

regplot(rcs, mod="rcs(dose, knots)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,125), shade =FALSE, pch=NA_integer_, 
        ylim = c(0, 25), pred = pred_rcs, xvals = s)
lines(exp(pred_rcs2$pred), lwd = "4", col = "blue")
lines(exp(pred_rcs2$ci.lb), lwd = "3", lty = "dotted", col = "blue")
lines(exp(pred_rcs2$ci.ub), lwd = "3", lty = "dotted", col = "blue")
legend("topleft",inset =0.1, legend=c("Main results", "Sensitivity analysis 2"), lty=1:1, lwd=3:3, cex=1, col=c("black", "blue", "red"))


#shade =FALSE, pch=NA_integer_,

model2 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2)
                 + dose:type + I(dose^2):type + 
                   + dose:region + I(dose^2):region + dose:mortality + I(dose^2):mortality, 
                 digits = 6,data=final2, random = ~ 1 | study, method = "REML")
model2


#third sensitivity analysis
final3 <- final %>%  
  filter(qualitySC != 1 & study != 54 & study != 55)

#RCS
s <- seq(0,150,length=150)
knots3 <- quantile(final3$dose, c(.05, .35, .65, .95))

rcs3 <- rma.mv(yi= lnor ~ rcs(dose, knots3)+0, V=se^2, data=final3, 
               random = ~ 1 | study, method = "REML")
summary(rcs3)

pred_rcs3 <- predict(rcs3, newmods=rcspline.eval(s, knots3, inclx=TRUE))
regplot(rcs3, mod="rcs(dose, knots3)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,125),
        ylim = c(0, 30), pred = pred_rcs3, xvals = s)

regplot(rcs, mod="rcs(dose, knots)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,125), shade =FALSE, pch=NA_integer_, 
        ylim = c(0, 25), pred = pred_rcs, xvals = s)
lines(exp(pred_rcs3$pred), lwd = "4", col = "blue")
lines(exp(pred_rcs3$ci.lb), lwd = "3", lty = "dotted", col = "blue")
lines(exp(pred_rcs3$ci.ub), lwd = "3", lty = "dotted", col = "blue")
legend("topleft",inset =0.1, legend=c("Main results", "Sensitivity analysis 3"), lty=1:1, lwd=3:3, cex=1, col=c("black", "blue", "red"))


model3 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2)
                 + dose:type + I(dose^2):type + 
                   + dose:region + I(dose^2):region + dose:mortality + I(dose^2):mortality, 
                 digits = 6,data=final3, random = ~ 1 | study, method = "REML")
model3

