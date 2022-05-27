library(tidyverse)
library(meta)
library(metafor)
library(dosresmeta)

library(mvmeta)

# Personal computer; specify locations 
data   <- "C:/Users/laura/Documents/CAMH/SIMAH/SIMAH_workplace/liver_cirrhosis/"    # Location of data

# load data
step4 <- readRDS (paste0(data, "7LC_models_bysex.xlsx"))

library(readxl)
step4 <- read_excel("CAMH/SIMAH/SIMAH_workplace/liver_cirrhosis/7LC_models_bysex.xlsx", 
                        col_types = c("numeric", "numeric", "text", 
                                      "numeric", "text", "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric","numeric", "numeric", "numeric"))

step4$sex <- as.factor(step4$sex)

male <- step4 %>%
  filter(sex == 1 & dose != 0.00)
female <- step4 %>%
  filter(sex == 2 & dose != 0.00)

dim(table(female$study))

#####MALE

#metaanalysis
metamale <- metagen(TE = lnor,
                          lower = ci.lnl,
                          upper = ci.lnh,
                          level.ci = 0.95,
                          studlab = study,
                          data = male,
                          sm = "RR",
                          fixed = FALSE,
                          random = TRUE,
                          method.tau = "REML")
summary(metamale)

##MALE LINEAR REGRESSION

linear_male <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+0, data=male,
                    random = ~ 1 | study, method = "REML")
summary(linear_male)

m <- seq(0,150,length=150)
pred_lin_male <- predict(linear_male, cbind(m))
regplot(linear_male, mod="dose", xlab="Alcohol intake, grams/day - Male", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 30), pred = pred_lin_male, xvals = m, main="Linear Regression")
lines(exp(pred_lin_male0$pred), lwd = "4", col = "blue")
lines(exp(pred_lin_male0$ci.lb), lwd = "3", lty = "dotted", col = "blue")
lines(exp(pred_lin_male0$ci.ub), lwd = "3", lty = "dotted", col = "blue")

predict(linear_male, 12, transf = exp)

#basic plot to confirm regression line
plot(male$dose,male$or, ylim = c(0, 30), xlim = c(0,150))
lines(exp(pred_lin_male$pred))

##MALE QUADRATIC REGRESSION

quad_male <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=male, 
                     random = ~ 1 | study, method = "REML")
summary(quad_male)

pred_quad_male <- predict(quad_male, newmods=cbind(m,m^2))
regplot(quad_male, mod="dose", xlab="Alcohol intake, grams/day - Male", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 30), pred = pred_quad_male, xvals = m, main="Quadratic Regression")
lines(exp(pred_quad_male0$pred), lwd = "4", col = "blue")
lines(exp(pred_quad_male0$ci.lb), lwd = "3", lty = "dotted", col = "blue")
lines(exp(pred_quad_male0$ci.ub), lwd = "3", lty = "dotted", col = "blue")

#plot 
pred_quad_male <- predict(quad_male, newmods=cbind(dose=m,type=t,usa=c))

pred_quad_male <- predict(quad_male, newmods=cbind(dose=m,m^2))
regplot(quad_male, mod="dose", xlab="Alcohol intake, grams/day - Male", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 30), pred = pred_quad_male, xvals = m, main="Quadratic Regression")

#basic plot to confirm regression line
plot(male$dose,male$or, ylim = c(0, 30), xlim = c(0,150))
lines(exp(pred_quad_male$pred))

predict(quad_male, c(50,50^2))

##RESTRICTED CUBIC SPLINE

library(rms)
m <- seq(0,150,length=150)
knots <- quantile(male$dose, c(.05, .35, .65, .95))

rcs_male <- rma.mv(yi= lnor ~ rcs(dose, knots)+0, V=se^2, data=male, 
                   random = ~ 1 | study, method = "REML")
summary(rcs_male)

pred_rcs_male <- predict(rcs_male, newmods=rcspline.eval(m, knots, inclx=TRUE))
regplot(rcs_male, mod="rcs(dose, knots)dose", xlab="Alcohol intake, grams/day - Male", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 30), pred = pred_rcs_male, xvals = m, main="RCS Regression")
abline(v=knots, lty="dotted")
lines(exp(pred_rcs_male0$pred), lwd = "4", col = "blue")
lines(exp(pred_rcs_male0$ci.lb), lwd = "3", lty = "dotted", col = "blue")
lines(exp(pred_rcs_male0$ci.ub), lwd = "3", lty = "dotted", col = "blue")


weights(rcs_male)
plot(weights(rcs_male), male$dose)

#basic plot to confirm regression line
plot(male$dose,male$or, ylim = c(0, 30), xlim = c(0,150))
lines(exp(pred_rcs_male$pred))

#prediction rcs model
predict(rcs_male, newmods= rcspline.eval(100, knots, inclx=TRUE), transf=exp)

##CUBIC POLYNOMIAL MODEL

cp_male <- rma.mv(yi= lnor, V=se^2, mods = ~ poly(dose, degree=3, raw=TRUE)+0, data=male, 
                  random = ~ 1 | study, method = "REML")
summary(cp_male)

pred_cp_male <- predict(cp_male, newmods=unname(poly(m, degree=3, raw=TRUE)))
regplot(cp_male, mod=1, xlab="Alcohol intake, grams/day - Male", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 30), pred = pred_cp_male, xvals = m, main="Cubic Polynomial Regression")

#MULTIVARIATE FRACTIONAL POLYNOMIAL
install.packages("mfp")
install.packages("visreg")
library(mfp)
library(visreg)

#4 degrees of freedom
mfp_male4 <- mfp(formula = lnor ~ fp(dose, df = 4)+0 ,alpha=0.05, 
               data = male)
mfp_male4
summary(mfp_male4)

#plot for mfp
visual4<- glm(formula = lnor ~ I((dose/100)^1) + I((dose/100)^3), data = male)
visreg(visual4,"dose", xlab="Alcohol intake, grams/day - Male", trans=exp, ylab="Relative Risk")

#2 degrees of freedom
mfp_male2 <- mfp(formula = lnor ~ fp(dose, df = 2)+0,alpha=0.05, 
                 data = male)
mfp_male2
summary(mfp_male2)

visual2<- glm(formula = lnor ~ I((dose/100)^0.5), data = male)
visreg(visual2,"dose", xlab="Alcohol intake, grams/day - Male", trans=exp, ylab="Relative Risk")

###FRACPOL - mfp using rma function with the selected p from the previous analysis
mfp <- rma.mv(yi=lnor, V=se^2, mods = ~ fracpol(dose, p = c(1, 3)) +0, data=male, 
              random = ~ 1 | study, method = "REML")
summary(mfp)

##MODEL COMPARISON MALE
fitstats(linear_male, quad_male, rcs_male, cp_male, mfp)

#####FEMALE

metafemale <- metagen(TE = lnor,
                    lower = ci.lnl,
                    upper = ci.lnh,
                    level.ci = 0.95,
                    studlab = study,
                    data = female,
                    sm = "RR",
                    fixed = FALSE,
                    random = TRUE,
                    method.tau = "REML")
summary(metafemale)

##FEMALE LINEAR REGRESSION

linear_female <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+0, data=female, 
                    random = ~ 1 | study, method = "REML")
summary(linear_female)

f <- seq(0,150,length=150)
pred_linear_female <- predict(linear_female, cbind(f))
regplot(linear_female, mod="dose", xlab="Alcohol intake, grams/day - Female", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 30), pred = pred_linear_female, xvals = f, main="Linear Regression")
lines(exp(pred_linear_female0$pred), lwd = "4", col = "blue")
lines(exp(pred_linear_female0$ci.lb), lwd = "3", lty = "dotted", col = "blue")
lines(exp(pred_linear_female0$ci.ub), lwd = "3", lty = "dotted", col = "blue")

#basic plot to confirm regression line
plot(female$dose,female$or, ylim = c(0, 30), xlim = c(0,150))
lines(exp(pred_linear_female$pred))

##FEMALE QUADRATIC MODEL

quad_female <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=female, 
                     random = ~ 1 | study, method = "REML")
summary(quad_female)
summary(quad_male)

pred_quad_female <- predict(quad_female, newmods=cbind(f,f^2))
regplot(quad_female, mod="dose", xlab="Alcohol intake, grams/day - Female", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 30), pred = pred_quad_female, xvals = f, main="Quadratic Regression")
lines(exp(pred_quad_female0$pred), lwd = "4", col = "blue")
lines(exp(pred_quad_female0$ci.lb), lwd = "3", lty = "dotted", col = "blue")
lines(exp(pred_quad_female0$ci.ub), lwd = "3", lty = "dotted", col = "blue")

predict(quad_female, c(150,150^2), transf=exp)

#basic plot to confirm regression line
plot(female$dose,female$or, ylim = c(0, 30), xlim = c(0,150))
lines(exp(pred_quad_female$pred))

##RESTRICTED CUBIC SPLINE
f <- seq(0,150,length=150)
knotsf <- quantile(female$dose, c(.05, .35, .65, .95))
rcs_female <- rma.mv(yi=lnor ~ rcs(dose, knotsf)+0, V=se^2, data=female, 
                     random = ~ 1 | study, method = "REML")
summary(rcs_female)

pred_rcs_female <- predict(rcs_female, newmods=rcspline.eval(f, knotsf, inclx=TRUE))
regplot(rcs_female, mod="rcs(dose, knotsf)dose", xlab="Alcohol intake, grams/day - Female", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 30), pred = pred_rcs_female, xvals = f, main="RCS Regression")
abline(v=knotsf, lty="dotted")
lines(exp(pred_rcs_female0$pred), lwd = "4", col = "blue")
lines(exp(pred_rcs_female0$ci.lb), lwd = "3", lty = "dotted", col = "blue")
lines(exp(pred_rcs_female0$ci.ub), lwd = "3", lty = "dotted", col = "blue")

#basic plot to confirm regression line
plot(female$dose,female$or, ylim = c(0, 30), xlim = c(0,150))
lines(exp(pred_rcs_female$pred))

#prediction rcs model
predict(rcs_female, newmods= rcspline.eval(80, knotsf, inclx=TRUE), transf=exp)

##CUBIC POLYNOMIAL MODEL

cp_female <- rma.mv(yi= lnor, V=se^2, mods = ~ poly(dose, degree=3, raw=TRUE)+0, data=female, 
                  random = ~ 1 | study, method = "REML")
summary(cp_female)

pred_cp_female <- predict(cp_female, newmods=unname(poly(m, degree=3, raw=TRUE)))
regplot(cp_female, mod=1, xlab="Alcohol intake, grams/day - Female", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 30), pred = pred_cp_female, xvals = f, main="Cubic Polynomial Regression")

#basic plot to confirm regression line
plot(female$dose,female$or, ylim = c(0, 30), xlim = c(0,150))
lines(exp(pred_cp_female$pred))

#MULTIVARIATE FRACTIONAL POLYNOMIAL
#4degrees of freedom
mfp_female4 <- mfp(formula = lnor ~ fp(dose, df = 4)+0 ,alpha=0.05, 
                 data = female)
mfp_female4
summary(mfp_female4)
plot(mfp_female4)
mfp_female4$pvalues
mfp_female4$fit

#plot
visualf4<- glm(formula = lnor ~ I((dose/100)^1) + I((dose/100)^2), data = female)
visreg(visualf4,"dose", xlab="Alcohol intake, grams/day - Female", trans=exp, ylab="Relative Risk")

#2degrees of freedom
mfp_female2 <- mfp(formula = lnor ~ fp(dose, df = 2),alpha=0.05, 
                 data = female)
mfp_female2
summary(mfp_female2)
plot(mfp_female2)
mfp_female2$pvalues
mfp_female2$fit

visualf2<- glm(formula = lnor ~ I((dose/100)^0.5), data = female)
visreg(visualf2,"dose", xlab="Alcohol intake, grams/day - Female", trans=exp, ylab="Relative Risk")

###FRACPOL using the p values obtained in the previous analysis
ffpf <- rma.mv(yi=lnor, V=se^2, mods = ~ fracpol(dose, p = c(1, 2)) +0, data=female, 
              random = ~ 1 | study, method = "REML")
summary(ffpf)

##MODEL COMPARISON FEMALE
fitstats(linear_female, quad_female, rcs_female, cp_female, ffpf)

#predictions
predict(rcs_male, newmods= rcspline.eval(100, knots, inclx=TRUE), transf=exp)
predict(rcs_female, newmods= rcspline.eval(100, knotsf, inclx=TRUE), transf=exp)

##morb vs mort

morb_m <- male %>%
  filter(mortality == 0)
mort_m <- male %>%
  filter(mortality == 1)
morb_f <- female %>%
  filter(mortality == 0)
mort_f <- female %>%
  filter(mortality == 1)

dim(table(mort_f$study))

quad_morb_m <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=morb_m, digits =5,
                      random = ~ 1 | study, method = "REML")
summary(quad_morb_m)
s <- seq(0,150,length=1500)
pred_quad_morb_m <- predict(quad_morb_m, newmods=cbind(s,s^2))
regplot(quad_morb_m, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 40), pred = pred_quad_morb_m, xvals = s, main="Morbidity - Men")

-(0.02845/(2*-0.00006))
(0.02845*237.08)+((-0.00006)*(237.08*237.08))
predict(quad_morb_m, c(237.08,237.08^2))

-(((0.0285*0.0285)-(4*-0.0001))/(4*-0.0001))

c <- seq(0,142.5,length=142.5)
d <- seq(142.5,150, length=50)
e <- rep(17.1794, times=50)
f <- rep(11.6054, times=50)
g <- rep(25.4305, times=50)

pred_quad_morb_m2 <- predict(quad_morb_m, newmods=cbind(c,c^2))
regplot(quad_morb_m, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), shade =FALSE,
        ylim = c(0, 40), pred = pred_quad_morb_m2, xvals = c, main="Morbidity - Men")
lines(d,e, lwd = "3", col = "black")
lines(d,f, lwd = "1", lty = "dashed", col = "black")
lines(d,g, lwd = "1", lty = "dashed", col = "black")

quad_mort_m <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=mort_m, digits =5,
                      random = ~ 1 | study, method = "REML")
summary(quad_mort_m)
s <- seq(0,200,length=200)
pred_quad_mort_m<- predict(quad_mort_m, newmods=cbind(s,s^2))
regplot(quad_mort_m, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,200), 
        ylim = c(0, 40), pred = pred_quad_mort_m, xvals = s, main="Mortality - Men")

-(0.03897/(2*-0.00011))
(0.03897*177.14)+((-0.00011)*(177.14*177.14))
predict(quad_mort_m, c(177.14,177.14^2))

quad_morb_f <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=morb_f, digits =5,
                      random = ~ 1 | study, method = "REML")
summary(quad_morb_f)
s <- seq(0,150,length=150)
pred_quad_morb_f <- predict(quad_morb_f, newmods=cbind(s,s^2))
regplot(quad_morb_f, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,200), 
        ylim = c(0, 40), pred = pred_quad_morb_f, xvals = s, main="Morbidity - Women")

-(0.03613/(2*-0.00010))
-(0.036134/(2*-0.000099))
(0.03613*180.65)+((-0.0001)*(180.5*180.65))
predict(quad_morb_f, c(180.65,180.65^2))

quad_mort_f <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=mort_f, digits =5, 
                      random = ~ 1 | study, method = "REML")
summary(quad_mort_f)

predict(quad_mort_f, c(91.7,91.7^2), transf=exp)

q <- seq(0,91.7,length=91.7)
r <- seq(91.7,150, length=50)
y <- rep(28.99933, times=50)
a <- rep(22.04425, times=50)
b <- rep(38.14879, times=50)

pred_quad_mort_f <- predict(quad_mort_f, newmods=cbind(q,q^2))
regplot(quad_mort_f, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), shade =FALSE,
        ylim = c(0, 40), pred = pred_quad_mort_f, xvals = q, main="Mortality - Women")
lines(r,y, lwd = "3", col = "black")
lines(r,a, lwd = "1", lty = "dashed", col = "black")
lines(r,b, lwd = "1", lty = "dashed", col = "black")

-(0.07336/(2*-0.00040))

(0.07336*91.7)+((-0.0004)*(91.7*91.7))

