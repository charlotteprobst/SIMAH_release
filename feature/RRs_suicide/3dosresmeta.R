library(dosresmeta)
library(tidyverse)
library(ggplot2)

library(readxl)
dataset <- read_excel("CAMH/DIABETES/analysis/SIMAH_workplace/5dosresmeta.xlsx", 
                      col_types = c("numeric", "numeric", "text", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric"))

####MALE

#LINEAR DOSE-RESPONSE

male <- dataset %>%
  filter(sex ==1)

linear_male <- dosresmeta(formula = logrr ~ dose, id = id, type = "ci", lb = rr.low, ub = rr.hi,
                          intercept = F, cases = cases, n = n, method = "reml", data = male)
summary(linear_male)
predict(linear_male, delta = 12, exp = TRUE)

dosex_bin <- data.frame(dose=seq(0, 100, 1))
with(predict(linear_male, dosex_bin, order=TRUE, exp=TRUE), 
     {plot(dose, pred, type="l", col="blue", ylim=c(0.5, 2), ylab= "Relative risk", xlab="Alcohol intake, grams/day - Male")
       lines(dose, ci.lb, lty=2)
       lines(dose, ci.ub, lty=2)})

#FEMALE

female <- dataset %>%
  filter(sex ==0)

lin_female <- dosresmeta(formula = logrr ~ dose, id = id, type = "ci", se = se, intercept = F,
                  cases = cases, n = n, data = female)
summary(lin_female)
predict(lin_female, delta = 12, exp = TRUE)

dosex_bin <- data.frame(dose=seq(0, 150, 1))
with(predict(lin_female, dosex_bin, order=TRUE, exp=TRUE), 
     {plot(dose, pred, type="l", col="blue", ylim=c(0, 2), ylab= "Relative risk", xlab="Alcohol intake, grams/day - Female")
       lines(dose, ci.lb, lty=2)
       lines(dose, ci.ub, lty=2)})

