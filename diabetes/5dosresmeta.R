library(dosresmeta)
library(tidyverse)
library(ggplot2)

library(readxl)
dataset_dos <- read_excel("CAMH/DIABETES/analysis/SIMAH_workplace/5dosresmeta.xlsx", 
                      col_types = c("numeric","numeric", "numeric", "text", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric"))

####MALE

maled <- dataset_dos %>%
  filter(sex ==1 & first_author != "Okamura" & first_author != "Park")
maled <- maled[-c(19),]

#LINEAR DOSE-RESPONSE

linear_maled <- dosresmeta(formula = logrr ~ dose, id = id, type = "ci", se=se,
                          intercept = F, cases = cases, n = n, method = "reml", data = maled)
summary(linear_maled)
predict(linear_maled, delta = 12, exp = TRUE)

dosex_bin <- data.frame(dose=seq(0, 100, 1))
with(predict(linear_maled, dosex_bin, order=TRUE, exp=TRUE), 
     {plot(dose, pred, type="l", ylim=c(0, 2), ylab= "Relative risk", xlab="Alcohol intake, grams/day")
       lines(dose, ci.lb, lty=2)
       lines(dose, ci.ub, lty=2)})

#QUADRATIC DOSE-RESPONSE

quad_maled <- dosresmeta(formula = logrr ~ dose + I(dose^2), id = id, type = "ci", se=se,
                          intercept = F, cases = cases, n = n, method = "reml", data = maled, proc = "1stage")
summary(quad_maled)

dosex_bin <- data.frame(dose=seq(0, 150, 1))
with(predict(quad_maled, dosex_bin, order=TRUE, exp=TRUE), 
     {plot(dose, pred, type="l", col="blue", ylim=c(0, 2), ylab= "Relative risk", xlab="Alcohol intake, grams/day - Male")
       lines(dose, ci.lb, lty=2)
       lines(dose, ci.ub, lty=2)})

#RESTRICTIVE CUBIC SPLINES
library("rms")
knotsmd <- quantile(maled$dose, c(.05, .35, .65, .95))
splmaled <- dosresmeta(formula = logrr ~ rcs(dose, knotsmd), id = id, type = "ci", 
                      cases = cases, n = n,
                      data = maled, se = se, proc = "1stage")
summary(splmaled)

dosex_bins <- data.frame(dose=seq(0, 150, 1))
xref <- 0
with(predict(splmaled, dosex_bins, xref, exp = TRUE),
     {plot(get("rcs(dose, knotsmd)dose"), pred, type= "l", ylim= c(0,2), ylab= "Relative risk", 
           xlab= "Alcohol consumption, grams/day - Male")
       matlines(get("rcs(dose, knotsmd)dose"), cbind(ci.lb, ci.ub), col = 1, lty = "dashed")})

#test non-linearity
waldtest(b=coef(splmaled), Sigma=vcov(splmaled), Terms=c(2,3))

#goodness of fit
models <- list(linear_maled, quad_maled, splmaled)
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

femaled <- dataset_dos %>%
  filter(sex ==0)

femaled <- femaled[-c(16),]

lin_femaled <- dosresmeta(formula = logrr ~ dose, id = id, type = "ci", se = se, intercept = F,
                  cases = cases, n = n, data = femaled)
summary(lin_femaled)
predict(lin_femaled, delta = 12, exp = TRUE)

dosex_bin <- data.frame(dose=seq(0, 100, 1))
with(predict(lin_femaled, dosex_bin, order=TRUE, exp=TRUE), 
     {plot(dose, pred, type="l", col="blue", ylim=c(0, 2), ylab= "Relative risk", xlab="Alcohol intake, grams/day - Female")
       lines(dose, ci.lb, lty=2)
       lines(dose, ci.ub, lty=2)})

#QUADRATIC DOSE-RESPONSE

quad_femaled <- dosresmeta(formula = logrr ~ dose + I(dose^2), id = id, type = "ci", se=se,
                        intercept = F, cases = cases, n = n, method = "reml", data = femaled, proc = "1stage")
summary(quad_femaled)

dosex_bin <- data.frame(dose=seq(0, 100, 1))
with(predict(quad_femaled, dosex_bin, order=TRUE, exp=TRUE), 
     {plot(dose, pred, type="l", col="blue", ylim=c(0.5, 2), ylab= "Relative risk", xlab="Alcohol intake, grams/day - Female")
       lines(dose, ci.lb, lty=2)
       lines(dose, ci.ub, lty=2)})


#RESTRICTIVE CUBIC SPLINES

knotsfd <- quantile(femaled$dose, c(.05, .35, .65, .95))
splfemaled <- dosresmeta(formula = logrr ~ rcs(dose, knotsfd), id = id, type = "ci", 
                      cases = cases, n = n,
                      data = femaled, se = se, proc = "1stage")
summary(splfemaled)

dosex_bins <- data.frame(dose=seq(0, 100, 1))
xref <- 0
with(predict(splfemaled, dosex_bins, xref, exp = TRUE),
     {plot(get("rcs(dose, knotsfd)dose"), pred, type= "l", ylim= c(0,2), ylab= "Relative risk", 
           xlab= "Alcohol consumption, grams/day")
       matlines(get("rcs(dose, knotsfd)dose"), cbind(ci.lb, ci.ub), col = 1, lty = "dashed")})

waldtest(b=coef(splfemaled), Sigma=vcov(splfemaled), Terms=2:3)

predict(splfemaled, newmods= rcspline.eval(45, knotsfd, inclx=TRUE), exp=TRUE)

dataTab <- data.frame(dose = seq(0, 20, 1))
predSpl <- predict(splfemaled, dataTab, exp = TRUE)
predSpl

predict(splfemaled, 20, exp = TRUE)

#####BOTH - not for publication

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

