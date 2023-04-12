library(tidyverse)
library(ggplot2)
library(mvmeta)

library(dosresmeta)
library(meta)
library(metafor)
#for restrictive cubic spline
library(rms)

library(readxl)
dataset <- read_excel("CAMH/DIABETES/analysis/SIMAH_workplace/4dataset.xlsx", 
                      col_types = c("numeric", "numeric", "numeric", "numeric", "numeric", "text", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric"))

####MALE MODELS

male <- dataset %>%
  filter(analysis_id==0 & dose != 0.00 & sex ==1)

#erase extreme value - Burke 2007
male <- male[-c(15),]

dim(table(male$results_id))

##LINEAR REGRESSION

linear_male <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+0, data=male,
                 random = ~ 1 | cohort_id/line_id, digits = 8, method = "REML")
summary(linear_male)

#use var.comp function to check i2 - heterogeneity
i2 <- var.comp(linear_male)
summary(i2)
i2$results
i2$totalI2
i2$plot


#comparing model to check if it is necessary to run a multilevel analysis
linear_male_2 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+0, data=male,
                        random = ~ 1 | cohort_id/line_id, method = "REML", sigma2 =  c(0, NA))
summary(linear_male_2)

anova(linear_male, linear_male_2)

predict(linear_male, 100, transf=exp)

#graph
ms <- seq(0,150,length=150)
pred_lin_male <- predict(linear_male, cbind(ms))
regplot(linear_male, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), pch=NA_integer_,
        ylim = c(0, 2), pred = pred_lin_male, xvals = ms, main="Male - Linear Regression")

#for figure 2
regplot(linear_male, mod="dose", ylab="Relative risk", 
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100),pch=NA_integer_, shade =FALSE,
        ylim = c(0.4, 2), lcol= "blue4", lwd = c(3.5,1.5),
        pred = pred_lin_male, xvals = ms)
abline(h=1)

#test for linearity
waldtest(b = coef(linear_male), Sigma = vcov(linear_male), Terms = 1:nrow(vcov(linear_male)))

##QUADRATIC REGRESSION

quad_male <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=male, 
               random = ~ 1 | cohort_id/line_id, method = "REML")
summary(quad_male)

pred_quad_male <- predict(quad_male, newmods=cbind(ms,ms^2))
regplot(quad_male, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), pch=NA_integer_,
        ylim = c(0, 2), pred = pred_quad_male, xvals = ms, main="Male - Quadratic Regression")

waldtest(b = coef(quad_male), Sigma = vcov(quad_male), Terms = 1:nrow(vcov(quad_male)))

predict(quad_male, c(0.57,0.57^2))

##RESTRICTED CUBIC SPLINE

knotsm <- quantile(male$dose, c(.05, .35, .65, .95))

rcs_male <- rma.mv(yi= lnor ~ rcs(dose, knotsm)+0, V=se^2, data=male, 
              random = ~ 1 | cohort_id/line_id, method = "REML")
summary(rcs_male)

pred_rcs_male <- predict(rcs_male, newmods=rcspline.eval(ms, knotsm, inclx=TRUE))
regplot(rcs_male, mod="rcs(dose, knotsm)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), pch=NA_integer_,
        ylim = c(0, 2), pred = pred_rcs_male, xvals = ms, main="Male - RCS Regression")
abline(v=knotsm, lty="dotted")

waldtest(b = coef(rcs_male), Sigma = vcov(rcs_male), Terms = 1:nrow(vcov(rcs_male)))

#ERASE ESTIMATES FROM GRAPH: pch=NA_integer_, 

#prediction rcs model
predict(rcs_male, newmods= rcspline.eval(0.57, knotsm, inclx=TRUE), transf=exp)

##MODEL COMPARISON 
fitstats(linear_male, quad_male, rcs_male)


####FEMALE MODELS

female <- dataset %>%
  filter(analysis_id==0 & dose != 0.00 & sex ==0)

#erase extreme value - Burke 2007
female <- female[-c(12),]
dim(table(female$results_id))

##LINEAR REGRESSION

linear_female <- rma.mv(yi=lnor, V=se^2, mods = ~ dose-1, data=female,
                      random = ~ 1 | cohort_id/line_id, method = "REML")
summary(linear_female)

fs <- seq(0,150,length=150)
pred_lin_female <- predict(linear_female, cbind(fs))
regplot(linear_female, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), pch=NA_integer_,
        ylim = c(0, 2), pred = pred_lin_female, xvals = fs, main="Female - Linear Regression")

waldtest(b = coef(linear_female), Sigma = vcov(linear_female), Terms = 1:nrow(vcov(linear_female)))

##QUADRATIC REGRESSION

quad_female <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=female, 
                    random = ~ 1 | cohort_id/line_id, digits = 8, method = "REML")
summary(quad_female)

pred_quad_female <- predict(quad_female, newmods=cbind(fs,fs^2))
regplot(quad_female, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100),pch=NA_integer_, 
        ylim = c(0, 3), pred = pred_quad_female, xvals = fs, main="Female - Quadratic Regression")

##RESTRICTED CUBIC SPLINE

knotsf <- quantile(female$dose, c(.05, .35, .65, .95))

rcs_female <- rma.mv(yi= lnor ~ rcs(dose, knotsf)+0, V=se^2, data=female, digits = 8, 
                   random = ~ 1 | cohort_id/line_id, method = "REML")
summary(rcs_female)

pred_rcs_female <- predict(rcs_female, newmods=rcspline.eval(fs, knotsf, inclx=TRUE))
regplot(rcs_female, mod="rcs(dose, knotsf)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), pch=NA_integer_,
        ylim = c(0, 2), pred = pred_rcs_female, xvals = fs, main="Female - RCS Regression")
abline(v=knotsf, lty="dotted")

#figure 2
regplot(rcs_female, mod="rcs(dose, knotsf)dose", xlab="Alcohol intake, grams/day", ylab="Relative risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), pch=NA_integer_,shade =FALSE,
        ylim = c(0.4, 2), lcol= "firebrick2", pred = pred_rcs_female, lwd = c(3.5,1.5),
        xvals = fs)
abline(h=1)

#use var.comp function
i2 <- var.comp(rcs_female)
summary(i2)
i2$results
i2$totalI2
i2$plot

#ERASE ESTIMATES FROM GRAPH: pch=NA_integer_, 

#prediction rcs model
predict(rcs_female, newmods= rcspline.eval(15, knotsf, inclx=TRUE), transf=exp)

fitstats(linear_female, quad_female, rcs_female)

######BOTH SEXES COMBINED

final <- dataset %>%
  filter(analysis_id==0 & dose != 0.00)
#erase male and female from two papers that provides estimates for both/male/female to avoid duplicate population
final <- final[-c(35,36,37,38,39,162,163,164,165,166),]

dim(table(final$cohort_id))

##LINEAR REGRESSION

linear <- rma.mv(yi=lnor, V=se^2, mods = ~ dose-1, data=final,
                 random = ~ 1 | cohort_id/line_id, method = "REML")
summary(linear)

#comparing model
linear_two <- rma.mv(yi=lnor, V=se^2, mods = ~ dose-1, data=final,
                     random = ~ 1 | cohort_id/line_id, method = "REML", sigma2 =  c(0, NA))
summary(linear_two)

anova(linear, linear_two)

#graph
s <- seq(0,150,length=150)
pred_lin <- predict(linear,cbind(s))
regplot(linear, mod="dose",  xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), pch=NA_integer_, 
        ylim = c(0, 2), pred = pred_lin, xvals = s, main="Linear Regression - both sexes")

#test linearity
waldtest(b = coef(linear), Sigma = vcov(linear), Terms = 1:nrow(vcov(linear)))

##QUADRATIC REGRESSION

quad <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=final, digits = 8,
               random = ~ 1 | cohort_id/line_id,  method = "REML")
summary(quad)

pred_quad <- predict(quad, newmods=cbind(s,s^2))
regplot(quad, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), pch=NA_integer_, 
        ylim = c(0, 2), pred = pred_quad, xvals = s, main="Quadratic Regression - both sexes")

#comparing model
quad_two <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)-1, data=final, 
                   random = ~ 1 | cohort_id/line_id, method = "REML", sigma2 =  c(0, NA))
summary(quad_two)

anova(quad, quad_two)

waldtest(b = coef(quad), Sigma = vcov(quad), Terms = 1:nrow(vcov(quad)))

##RESTRICTED CUBIC SPLINE

library(rms)
s <- seq(0,150,length=150)
knots <- quantile(final$dose, c(.05, .35, .65, .95))

rcs <- rma.mv(yi= lnor ~ rcs(dose, knots)+0, V=se^2, data=final, digits = 8, 
              random = ~ 1 | cohort_id/line_id, method = "REML")
summary(rcs)

pred_rcs <- predict(rcs, newmods=rcspline.eval(s, knots, inclx=TRUE))
regplot(rcs, mod="rcs(dose, knots)dose", xlab="Alcohol intake, grams/day", ylab="Relative risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100),pch=NA_integer_,
        ylim = c(0, 2), pred = pred_rcs, xvals = s)
abline(v=knots, lty="dotted")

waldtest(b = coef(rcs), Sigma = vcov(rcs), Terms = 1:nrow(vcov(rcs)))

#ERASE ESTIMATES FROM GRAPH: pch=NA_integer_, 

weights(rcs)

#prediction rcs model
predict(rcs, newmods= rcspline.eval(15, knots, inclx=TRUE), transf=exp)

#TEST I2

W <- diag(1/rcs$vi)
X <- model.matrix(rcs)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(rcs$sigma2) / (sum(rcs$sigma2) + (rcs$k-rcs$p)/sum(diag(P)))

100 * rcs$sigma2 / (sum(rcs$sigma2) + (rcs$k-rcs$p)/sum(diag(P)))

#######TO CHECK AND CORRECT: MULTIVARIATE FRACTIONAL POLYNOMIAL
library(mfp)
library(visreg)

#4 degrees of freedom
mfp <- mfp(formula = lnor ~ fp(dose, df = 4) ,alpha=0.05, 
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

visual2<- glm(formula = lnor ~ I((dose/10)^1)+0, data = final)
visreg(visual2,"dose", xlab="Alcohol intake, grams/day", trans=exp, ylab="Relative Risk")

###doesnt run: FRACPOL - mfp using rma function with the selected p from the previous analysis
mfp_fracpol <- rma.mv(yi=lnor, V=se^2, mods = ~I((dose/10)^1)+1, data=final, 
                      random = ~ 1 | cohort_id/line_id, method = "REML")
summary(mfp_fracpol)

pred_fp <- predict(mfp_fracpol, newmods=cbind((s/10)^1))
regplot(mfp_fracpol, mod="I((dose/10)^1)",xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,140), pch=NA_integer_,
        ylim = c(0, 2), pred = pred_fp, xvals = s, main="Fractional Polynomial Regression")

##MODEL COMPARISON 
fitstats(linear, quad, rcs)

########TEST TO COMPARE WITH DOSRESMETA#######

#male

male2 <- male %>%
  filter(first_author != "Kerr" & reference != 3)

##LINEAR REGRESSION

linear_male2 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+0, data=male2,
                      random = ~ 1 | cohort_id/line_id, method = "REML")
summary(linear_male2)

#graph
ms <- seq(0,150,length=150)
pred_lin_male2 <- predict(linear_male2, cbind(ms))
regplot(linear_male2, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), pch=NA_integer_,
        ylim = c(0, 2), pred = pred_lin_male2, xvals = ms, main="Male - Linear Regression")

#test for linearity
waldtest(b = coef(linear_male2), Sigma = vcov(linear_male2), Terms = 1:nrow(vcov(linear_male2)))

##QUADRATIC REGRESSION

quad_male2 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=male2, 
                    random = ~ 1 | cohort_id/line_id, method = "REML")
summary(quad_male2)

pred_quad_male2 <- predict(quad_male2, newmods=cbind(ms,ms^2))
regplot(quad_male2, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), pch=NA_integer_,
        ylim = c(0, 2), pred = pred_quad_male2, xvals = ms, main="Male - Quadratic Regression")

waldtest(b = coef(quad_male2), Sigma = vcov(quad_male2), Terms = 1:nrow(vcov(quad_male2)))

##RESTRICTED CUBIC SPLINE

knotsm2 <- quantile(male2$dose, c(.05, .35, .65, .95))

rcs_male2 <- rma.mv(yi= lnor ~ rcs(dose, knotsm2)+0, V=se^2, data=male2, 
                   random = ~ 1 | cohort_id/line_id, method = "REML")
summary(rcs_male2)

pred_rcs_male2 <- predict(rcs_male2, newmods=rcspline.eval(ms, knotsm2, inclx=TRUE))
regplot(rcs_male2, mod="rcs(dose, knotsm2)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), pch=NA_integer_,
        ylim = c(0, 2), pred = pred_rcs_male2, xvals = ms, main="Male - RCS Regression")
abline(v=knotsm2, lty="dotted")

waldtest(b = coef(rcs_male2), Sigma = vcov(rcs_male2), Terms = 1:nrow(vcov(rcs_male2)))

#ERASE ESTIMATES FROM GRAPH: pch=NA_integer_, 

weights(rcs)

#prediction rcs model
predict(rcs_male, newmods= rcspline.eval(50, knotsm, inclx=TRUE), transf=exp)

anova(linear_male, rcs_male)

##POLYNOMIAL MODEL

cp_male <- rma.mv(yi= lnor, V=se^2, mods = ~ poly(dose, degree=3, raw=TRUE)+0, data=male, 
                  random = ~ 1 | cohort_id/line_id,  method = "REML")
summary(cp_male)

pred_cp_male <- predict(cp_male, newmods=unname(poly(ms, degree=3, raw=TRUE)))
regplot(cp_male, mod=1, xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), pch=NA_integer_,
        ylim = c(0, 2), pred = pred_cp_male, xvals = ms, main="Male - Cubic Polynomial Regression")

##MODEL COMPARISON 
fitstats(linear_male2, quad_male2, rcs_male2)

#female

female2 <- female %>%
  filter(first_author != "Kerr" & first_author != "Rajaobelina")

#linear
linear_female2 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose-1, data=female2,
                        random = ~ 1 | cohort_id/line_id, method = "REML")
summary(linear_female2)

pred_lin_female2 <- predict(linear_female2, cbind(fs))
regplot(linear_female2, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), pch=NA_integer_,
        ylim = c(0, 2), pred = pred_lin_female2, xvals = fs, main="Female - Linear Regression")

waldtest(b = coef(linear_female2), Sigma = vcov(linear_female2), Terms = 1:nrow(vcov(linear_female2)))

##QUADRATIC REGRESSION

quad_female2 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=female2, 
                      random = ~ 1 | cohort_id/line_id, method = "ML")
summary(quad_female2)

pred_quad_female2 <- predict(quad_female2, newmods=cbind(fs,fs^2))
regplot(quad_female2, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), pch=NA_integer_,
        ylim = c(0, 2), pred = pred_quad_female2, xvals = fs, main="Female - Quadratic Regression")

##RESTRICTED CUBIC SPLINE

knotsf2 <- quantile(female2$dose, c(.05, .35, .65, .95))

rcs_female2 <- rma.mv(yi= lnor ~ rcs(dose, knotsf2)+0, V=se^2, data=female2, 
                     random = ~ 1 | cohort_id/line_id, method = "REML")
summary(rcs_female2)

fs <- seq(0,150,length=150)
pred_rcs_female2 <- predict(rcs_female2, newmods=rcspline.eval(fs, knotsf2, inclx=TRUE))
regplot(rcs_female2, mod="rcs(dose, knotsf2)dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), pch=NA_integer_,
        ylim = c(0, 2), pred = pred_rcs_female2, xvals = fs, main="Female - RCS Regression")
abline(v=knotsf2, lty="dotted")

predict(rcs_female2, newmods= rcspline.eval(17, knotsf2, inclx=TRUE), transf=exp)

#functions
knots_T<- c(1.000, 9.065, 20.815, 47.840)
kd <- (knots_T[4] - knots_T[1])^(2/3)
knotnk1

DIST_1<- function(alc_1){alc_1}

DIST_2<- function(alc_1){pmax((alc_1 - knots[1])/kd, 0)^3 + 
    ((knots_T[3] - knots_T[1]) * pmax((alc_1 - knots_T[4])/kd, 0)^3 - (knots_T[4] - knots_T[1]) * 
    (pmax((alc_1 - knots_T[3])/kd, 0)^3))/(knots_T[4] - knots_T[3]) }

DIST_3<- function(alc_1){pmax((alc_1 - knots[2])/kd, 0)^3 + ((knots_T[3] - knots_T[2]) * 
    pmax((alc_1 - knots_T[4])/kd, 0)^3 - (knots_T[4] - knots_T[2]) * 
      (pmax((alc_1 - knots_T[3])/kd, 0)^3))/(knots_T[4] - knots_T[3]) }
