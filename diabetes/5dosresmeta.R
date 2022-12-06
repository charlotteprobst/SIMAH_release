library(dosresmeta)
library(tidyverse)
library(ggplot2)

library(readxl)
dataset_dos <- read_excel("CAMH/DIABETES/analysis/SIMAH_workplace/5dosresmeta.xlsx", 
                      col_types = c("numeric","numeric", "numeric", "text", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric"))

#####BOTH

both_dos <- dataset_dos[-c(45,46,47,48,49,50,51,199,200,201,202,203,204,205),]

#LINEAR DOSE-RESPONSE

linear_dos <- dosresmeta(formula = logrr ~ dose, id = id, type = "ci", se = se,
                         intercept = F, cases = cases, n = n, method = "reml", data = both_dos)
summary(linear_dos)
predict(linear_dos, delta = 60, exp = T)

dosex_bin <- data.frame(dose=seq(0, 150, 1))
with(predict(linear_dos, dosex_bin, order=TRUE, exp=TRUE), 
     {plot(dose, pred, type="l", col="blue", ylim=c(0.5, 2), ylab= "Relative risk", xlab="Alcohol intake, grams/day")
       lines(dose, ci.lb, lty=2)
       lines(dose, ci.ub, lty=2)})

#graph with ggplot
pred <- data.frame(dose = c(xref, seq(0, 100, 1))) %>%
     predict(linear_dos, newdata = ., expo = T)

ggplot(pred, aes(dose, pred, ymin = ci.lb, ymax = ci.ub)) +
     geom_line() + geom_ribbon(alpha = .1) +
     scale_y_continuous(trans = "log", breaks = scales::pretty_breaks()) +
     labs(x = "Alcohol intake, grams/day", y = "Relative Risk")

#QUADRATIC DOSE-RESPONSE

quad_dos <- dosresmeta(formula = logrr ~ dose + I(dose^2), id = id, type = "ci", se = se,
                        intercept = F, cases = cases, n = n, method = "reml", data = both_dos, proc = "1stage")
summary(quad_dos)

dosex_bin <- data.frame(dose=seq(0, 150, 1))
with(predict(quad_dos, dosex_bin, order=TRUE, exp=TRUE), 
     {plot(dose, pred, type="l", col="blue", ylim=c(0.5, 8), ylab= "Relative risk", xlab="Alcohol intake, grams/day")
       lines(dose, ci.lb, lty=2)
       lines(dose, ci.ub, lty=2)})

#graph with ggplot
pred <- data.frame(dose = c(xref, seq(0, 100, 1))) %>%
  predict(quad_dos, newdata = ., expo = T)

ggplot(pred, aes(dose, pred, ymin = ci.lb, ymax = ci.ub)) +
  geom_line() + geom_ribbon(alpha = .1) +
  scale_y_continuous(trans = "log", breaks = scales::pretty_breaks()) +
  labs(x = "Alcohol intake, grams/day", y = "Relative Risk")

#RESTRICTIVE CUBIC SPLINES
library("rms")
knotsd <- quantile(both_dos$dose, c(.05, .35, .65, .95))
spl_dos <- dosresmeta(formula = logrr ~ rcs(dose, knotsd), id = id, type = "ci", 
                      cases = cases, n = n,
                      data = both_dos, se = se, proc = "1stage")
summary(spl_dos)

dosex_bins <- data.frame(dose=seq(0, 150, 1))
xref <- 0
with(predict(spl_dos, dosex_bins, xref, exp = TRUE),
     {plot(get("rcs(dose, knotsd)dose"), pred, type= "l", ylim= c(0,2), ylab= "Relative risk", 
           xlab= "Alcohol consumption, grams/day")
       matlines(get("rcs(dose, knotsd)dose"), cbind(ci.lb, ci.ub), col = 1, lty = "dashed")})

####MALE

#LINEAR DOSE-RESPONSE

male <- dataset_dos %>%
  filter(sex ==1)
male <- male[-c(19),]

linear_male <- dosresmeta(formula = logrr ~ dose, id = id, type = "ci", se=se,
                          intercept = F, cases = cases, n = n, method = "reml", data = male)
summary(linear_male)
predict(linear_male, delta = 12, exp = TRUE)

dosex_bin <- data.frame(dose=seq(0, 150, 1))
with(predict(linear_male, dosex_bin, order=TRUE, exp=TRUE), 
     {plot(dose, pred, type="l", col="blue", ylim=c(0, 2), ylab= "Relative risk", xlab="Alcohol intake, grams/day - Male")
       lines(dose, ci.lb, lty=2)
       lines(dose, ci.ub, lty=2)})

#QUADRATIC DOSE-RESPONSE

quad_male <- dosresmeta(formula = logrr ~ dose + I(dose^2), id = id, type = "ci", se=se,
                          intercept = F, cases = cases, n = n, method = "reml", data = male, proc = "1stage")
summary(quad_male)

predict(quad_male, expo = TRUE)

dosex_bin <- data.frame(dose=seq(0, 150, 1))
with(predict(quad_male, dosex_bin, order=TRUE, exp=TRUE), 
     {plot(dose, pred, type="l", col="blue", ylim=c(0, 2), ylab= "Relative risk", xlab="Alcohol intake, grams/day - Male")
       lines(dose, ci.lb, lty=2)
       lines(dose, ci.ub, lty=2)})

#RESTRICTIVE CUBIC SPLINES
library("rms")
knotsm <- quantile(male$dose, c(.05, .35, .65, .95))
splmale <- dosresmeta(formula = logrr ~ rcs(dose, knotsm), id = id, type = "ci", 
                      cases = cases, n = n,
                      data = male, se = se, proc = "1stage")
summary(splmale)

dosex_bins <- data.frame(dose=seq(0, 150, 1))
xref <- 0
with(predict(splmale, dosex_bins, xref, exp = TRUE),
     {plot(get("rcs(dose, knotsm)dose"), pred, type= "l", ylim= c(0,2), ylab= "Relative risk", 
           xlab= "Alcohol consumption, grams/day - Male")
       matlines(get("rcs(dose, knotsm)dose"), cbind(ci.lb, ci.ub), col = 1, lty = "dashed")})

#test non-linearity
waldtest(b=coef(splmale), Sigma=vcov(splmale), Terms=c(2,3))

#goodness of fit
models <- list(linear_male, quad_male, splmale)
data.frame(do.call("rbind", lapply(models, function(m) 
  unlist(gof(m)[c("deviance","R2", "R2adj")]))))

#not working
par(mfrow = c(1, 3))
lapply(models, function(m)
  with(gof(m)$tdata,{
    plot(male$dose[male$se != 0], residuals, ylim = -c(-5, 5), xlab = "Alcohol consumption, gr/day")
  lines(lowess(male$dose[male$se != 0], residuals), lwd = 4)
  })
)


#FEMALE

female <- dataset_dos %>%
  filter(sex ==0)

female <- female[-c(16),]

lin_female <- dosresmeta(formula = logrr ~ dose, id = id, type = "ci", se = se, intercept = F,
                  cases = cases, n = n, data = female)
summary(lin_female)
predict(lin_female, delta = 12, exp = TRUE)

dosex_bin <- data.frame(dose=seq(0, 150, 1))
with(predict(lin_female, dosex_bin, order=TRUE, exp=TRUE), 
     {plot(dose, pred, type="l", col="blue", ylim=c(0, 2), ylab= "Relative risk", xlab="Alcohol intake, grams/day - Female")
       lines(dose, ci.lb, lty=2)
       lines(dose, ci.ub, lty=2)})

#QUADRATIC DOSE-RESPONSE

quad_female <- dosresmeta(formula = logrr ~ dose + I(dose^2), id = id, type = "ci", se=se,
                        intercept = F, cases = cases, n = n, method = "reml", data = female, proc = "1stage")
summary(quad_female)

predict(quad_female, expo = TRUE)

dosex_bin <- data.frame(dose=seq(0, 150, 1))
with(predict(quad_female, dosex_bin, order=TRUE, exp=TRUE), 
     {plot(dose, pred, type="l", col="blue", ylim=c(0.5, 2), ylab= "Relative risk", xlab="Alcohol intake, grams/day - Male")
       lines(dose, ci.lb, lty=2)
       lines(dose, ci.ub, lty=2)})


#RESTRICTIVE CUBIC SPLINES

knotsf <- quantile(female$dose, c(.05, .35, .65, .95))
splfemale <- dosresmeta(formula = logrr ~ rcs(dose, knotsf), id = id, type = "ci", 
                      cases = cases, n = n,
                      data = female, se = se, proc = "1stage")
summary(splfemale)

dosex_bins <- data.frame(dose=seq(0, 150, 1))
xref <- 0
with(predict(splfemale, dosex_bins, xref, exp = TRUE),
     {plot(get("rcs(dose, knotsf)dose"), pred, type= "l", ylim= c(0,2), ylab= "Relative risk", 
           xlab= "Alcohol consumption, grams/day - Female")
       matlines(get("rcs(dose, knotsf)dose"), cbind(ci.lb, ci.ub), col = 1, lty = "dashed")})

waldtest(b=coef(splfemale), Sigma=vcov(splfemale), Terms=2:3)
