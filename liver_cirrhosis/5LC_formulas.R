library(tidyverse)
library(meta)
library(metafor)
library(dosresmeta)

library(mvmeta)

library(readxl)
dataset <- read_excel("CAMH/SIMAH/SIMAH_dataset/4LC_regression_all.xlsx", 
                      col_types = c("numeric", "numeric", "text", 
                                    "numeric", "text", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric","numeric", "numeric", "text", "numeric","numeric"))

dataset$sex <- as.factor(dataset$sex)
dataset$type <- as.factor(dataset$type)
dataset$mortality <- as.factor(dataset$mortality)

dataset <- dataset %>%
  filter(dose != 0.00)

#FEMALE
female <- dataset %>%
  filter(sex == 0)
dim(table(female$study))

q_female <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=female, digits = 6, 
                           random = ~ 1 | study, method = "REML")
summary(q_female)

a <- seq(0,150,length=150)
pred_female <- predict(q_female, newmods=cbind(a,a^2))
regplot(q_female, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 40), pred = pred_female, xvals = a, main="Female")

-(0.06696/(2*-0.000333))
(0.06696*100.54)+((-0.000333)*(100.54*100.54))
predict(q_female, c(100.54,100.54^2),transf=exp)

(0.060011*100.54)+((-0.000394)*(100.54*100.54))
(0.073909*100.54)+((-0.000273)*(100.54*100.54))

b <- seq(0,100.54,length=100.54)
c <- seq(100.54,150, length=49.46)
d <- rep(28.861987, times=50)
e <- rep(21.823106, times=50)
f <- rep(38.171208, times=50)

pred_female2 <- predict(q_female, newmods=cbind(b,b^2))
regplot(q_female, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), shade =FALSE,
        ylim = c(0, 40), pred = pred_female2, xvals = b, main="Female")
lines(c,d, lwd = "3", col = "black")
lines(c,e, lwd = "1", lty = "dashed", col = "black")
lines(c,f, lwd = "1", lty = "dashed", col = "black")

#MALE
male <- dataset %>%
  filter(sex == 1)
dim(table(male$study))

q_male <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=male, digits = 6, 
                   random = ~ 1 | study, method = "REML")
summary(q_male)

a <- seq(0,150,length=150)
pred_male <- predict(q_male, newmods=cbind(a,a^2))
regplot(q_male, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), shade =FALSE,
        ylim = c(0, 40), pred = pred_male, xvals = a, main="Male")

-(0.030119/(2*-0.000062))
(0.030119*242.9)+((-0.000062)*(242.9*242.9))
predict(q_male, c(242.9,242.9^2))


#MORBIDITY
morbidity <- dataset %>%
  filter(mortality == 0)
dim(table(morbidity$study))

q_morbidity <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=morbidity, digits = 6, 
                 random = ~ 1 | study, method = "REML")
summary(q_morbidity)

a <- seq(0,150,length=150)
pred_morbidity <- predict(q_morbidity, newmods=cbind(a,a^2))
regplot(q_morbidity, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), shade =FALSE,
        ylim = c(0, 40), pred = pred_morbidity, xvals = a, main="Morbidity")

-(0.031062/(2*-0.000102))
(0.031062*152.26)+((-0.000102)*(152.26*152.26))
predict(q_morbidity, c(152.26,152.26^2))

#MORTALITY
mortality <- dataset %>%
  filter(mortality == 1)
dim(table(mortality$study))

q_mortality <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=mortality, digits = 6, 
                      random = ~ 1 | study, method = "REML")
summary(q_mortality)

a <- seq(0,150,length=150)
pred_mortality <- predict(q_mortality, newmods=cbind(a,a^2))
regplot(q_mortality, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), shade =FALSE,
        ylim = c(0, 40), pred = pred_mortality, xvals = a, main="Mortality")

-(0.046635/(2*-0.000175))
(0.046635*133.24)+((-0.000175)*(133.24*133.24))
predict(q_mortality, c(133.24,133.24^2), transf=exp)

g <- seq(0,133.24,length=133.24)
h <- seq(133.24,150, length=17)
i <- rep(22.281989, times=17)
j <- rep(17.779877, times=17)
k <- rep(27.924099, times=17)

pred_mortality2 <- predict(q_mortality, newmods=cbind(g,g^2))
regplot(q_mortality, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), shade =FALSE,
        ylim = c(0, 40), pred = pred_mortality2, xvals = g, main="Mortality")
lines(h,i, lwd = "3", col = "black")
lines(h,j, lwd = "1", lty = "dashed", col = "black")
lines(h,k, lwd = "1", lty = "dashed", col = "black")

#ALL TYPE

alltype <- dataset %>%
  filter(type == 0)
dim(table(alltype$study))

q_alltype <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=alltype, digits = 6, 
                      random = ~ 1 | study, method = "REML")
summary(q_alltype)

a <- seq(0,150,length=150)
pred_alltype <- predict(q_alltype, newmods=cbind(a,a^2))
regplot(q_alltype, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), shade =FALSE,
        ylim = c(0, 40), pred = pred_alltype, xvals = a, main="All type LC")

-(0.031835/(2*-0.000091))
(0.031835*174.92)+((-0.000091)*(174.92*174.92))
predict(q_alltype, c(174.92,174.92^2))

#ALC
alc <- dataset %>%
  filter(type == 1)
dim(table(alc$study))

q_alc <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=alc, digits = 8, 
                    random = ~ 1 | study, method = "REML")
summary(q_alc)

a <- seq(0,150,length=150)
pred_alc <- predict(q_alc, newmods=cbind(a,a^2))
regplot(q_alc, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), shade =FALSE,
        ylim = c(0, 40), pred = pred_alc, xvals = a, main="ALC")

-(0.04148667/(2*-0.00006688))
(0.04148667*310.16)+((-0.00006688)*(310.16*310.16))
predict(q_alc, c(310.16,310.16^2))

#HCV
hcv <- dataset %>%
  filter(type == 3)
dim(table(hcv$study))

#monto
hcv <- hcv[-c(14),]

q_hcv <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=hcv, digits = 6, 
                random = ~ 1 | study, method = "REML")
summary(q_hcv)

a <- seq(0,150,length=150)
pred_hcv <- predict(q_hcv, newmods=cbind(a,a^2))
regplot(q_hcv, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), shade =FALSE,
        ylim = c(0, 10), pred = pred_hcv, xvals = a, main="HCV")

weights(q_hcv)

-(0.026035/(2*-0.000089))
(0.026035*146.26)+((-0.000089)*(146.26*146.26))
predict(q_hcv, c(146.26,146.26^2),transf=exp)

l <- seq(0,146.26,length=146.26)
m <- seq(146.26,150, length=4)
n <- rep(6.715305, times=4)
o <- rep(5.323907, times=4)
p <- rep(8.470343, times=4)

pred_hcv2 <- predict(q_hcv, newmods=cbind(l,l^2))
regplot(q_hcv, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), shade =FALSE,
        ylim = c(0,10), pred = pred_hcv2, xvals = l, main="HCV")
lines(m,n, lwd = "3", col = "black")
lines(m,o, lwd = "1", lty = "dashed", col = "black")
lines(m,p, lwd = "1", lty = "dashed", col = "black")

#FEMALE MORBIDITY
fem_morb <- dataset %>%
  filter(sex == 0 & mortality == 0)
dim(table(fem_morb$study))
fem_morb <- fem_morb[-(14),]

q_fem_morb <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=fem_morb, digits = 6, 
                   random = ~ 1 | study, method = "REML")
summary(q_fem_morb)

a <- seq(0,150,length=150)
pred_fem_morb <- predict(q_fem_morb, newmods=cbind(a,a^2))
regplot(q_fem_morb, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 60), pred = pred_fem_morb, xvals = a, main="Female Morbidity")

-(0.065975/(2*-0.000283))
(0.065975*116.56)+((-0.000283)*(116.56*116.56))
predict(q_fem_morb, c(116.56,116.56^2))

q <- seq(0,116.56,length=116.56)
r <- seq(116.56,150, length=34)
s <- rep(46.621853, times=34)
t <- rep(22.675973, times=34)
u <- rep(95.854638, times=34)

pred_fem_morb2 <- predict(q_fem_morb, newmods=cbind(q,q^2))
regplot(q_fem_morb, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), shade =FALSE,
        ylim = c(0, 60), pred = pred_fem_morb2, xvals = q, main="Female Morbidity")
lines(r,s, lwd = "3", col = "black")
lines(r,t, lwd = "1", lty = "dashed", col = "black")
lines(r,u, lwd = "1", lty = "dashed", col = "black")


#FEMALE MORTALITY

fem_mort <- dataset %>%
  filter(sex == 0 & mortality == 1)
dim(table(fem_mort$study))
fem_morb <- fem_morb[-(14),]

q_fem_mort <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=fem_mort, digits = 6, 
                     random = ~ 1 | study, method = "REML")
summary(q_fem_mort)

a <- seq(0,150,length=150)
pred_fem_mort <- predict(q_fem_mort, newmods=cbind(a,a^2))
regplot(q_fem_mort, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 40), pred = pred_fem_mort, xvals = a, main="Female Mortality")

-(0.069702/(2*-0.000370))
(0.069702*94.19)+((-0.000370)*(94.19*94.19))
predict(q_fem_mort, c(94.19,94.19^2), transf=exp)

v <- seq(0,94.19,length=94.19)
w <- seq(94.19,150, length=56)
x <- rep(26.612039, times=56)
y <- rep(19.433365, times=56)
z <- rep(36.442511, times=56)

pred_fem_mort2 <- predict(q_fem_mort, newmods=cbind(v,v^2))
regplot(q_fem_mort, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), shade =FALSE,
        ylim = c(0, 40), pred = pred_fem_mort2, xvals = v, main="Female Mortality")
lines(w,x, lwd = "3", col = "black")
lines(w,y, lwd = "1", lty = "dashed", col = "black")
lines(w,z, lwd = "1", lty = "dashed", col = "black")

#MALE MORBIDITY
male_morb <- dataset %>%
  filter(sex == 1 & mortality == 0)
dim(table(male_morb$study))

q_male_morb <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=male_morb, digits = 6, 
                     random = ~ 1 | study, method = "REML")
summary(q_male_morb)

a <- seq(0,150,length=150)
pred_male_morb <- predict(q_male_morb, newmods=cbind(a,a^2))
regplot(q_male_morb, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), shade =FALSE, 
        ylim = c(0, 40), pred = pred_male_morb, xvals = a, main="Male Morbidity")

-(0.028458/(2*-0.000060))
(0.028458*237.15)+((-0.000060)*(237.15*237.15))
predict(q_male_morb, c(237.15,237.15^2))

#MALE MORTALITY
male_mort <- dataset %>%
  filter(sex == 1 & mortality == 1)
dim(table(male_mort$study))

q_male_mort <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=male_mort, digits = 6, 
                      random = ~ 1 | study, method = "REML")
summary(q_male_mort)

a <- seq(0,150,length=150)
pred_male_mort <- predict(q_male_mort, newmods=cbind(a,a^2))
regplot(q_male_mort, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), shade =FALSE, 
        ylim = c(0, 40), pred = pred_male_mort, xvals = a, main="Male Mortality")

-(0.038963/(2*-0.000114))
(0.038963*170.89)+((-0.000114)*(170.89*170.89))
predict(q_male_mort, c(170.89,170.89^2))

#FEMALE ALL TYPE
fem_all <- dataset %>%
  filter(sex == 0 & type == 0)
dim(table(fem_all$study))

fem_all <- fem_all[-c(30),]

q_fem_all <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=fem_all, digits = 6, 
                         random = ~ 1 | study, method = "REML")
summary(q_fem_all)

a <- seq(0,150,length=150)
pred_fem_all <- predict(q_fem_all, newmods=cbind(a,a^2))
regplot(q_fem_all, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 40), pred = pred_fem_all, xvals = a, main="Female All Type")

-(0.061847/(2*-0.000308))
(0.061847*100.4)+((-0.000308)*(100.4*100.4))
predict(q_fem_all, c(100.4,100.4^2), transf=exp)

aa <- seq(0,100.4,length=100.4)
ab <- seq(100.4,150, length=50)
ac <- rep(22.338178, times=50)
ad <- rep(16.257911, times=50)
ae <- rep(30.692393, times=50)

pred_fem_all2 <- predict(q_fem_all, newmods=cbind(aa,aa^2))
regplot(q_fem_all, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), shade =FALSE,
        ylim = c(0, 40), pred = pred_fem_all2, xvals = aa, main="Female All Type")
lines(ab,ac, lwd = "3", col = "black")
lines(ab,ad, lwd = "1", lty = "dashed", col = "black")
lines(ab,ae, lwd = "1", lty = "dashed", col = "black")

#FEMALE ALC
fem_alc <- dataset %>%
  filter(sex == 0 & type == 1)
dim(table(fem_alc$study))

q_fem_alc <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=fem_alc, digits = 6, 
                    random = ~ 1 | study, method = "REML")
summary(q_fem_alc)

a <- seq(0,150,length=150)
pred_fem_alc <- predict(q_fem_alc, newmods=cbind(a,a^2))
regplot(q_fem_alc, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 60), pred = pred_fem_alc, xvals = a, main="Female ALC")

-(0.073719/(2*-0.000335))
(0.073719*110.03)+((-0.000335)*(110.03*110.03))
predict(q_fem_alc, c(110.03,110.03^2), transf=exp)

af <- seq(0,110.03,length=110.03)
ag <- seq(110.03,150, length=40)
ah <- rep(57.653961, times=40)
ai <- rep(18.712220, times=40)
aj <- rep(177.636819, times=40)

pred_fem_alc2 <- predict(q_fem_alc, newmods=cbind(af,af^2))
regplot(q_fem_alc, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), shade =FALSE,
        ylim = c(0, 60), pred = pred_fem_alc2, xvals = af, main="Female ALC")
lines(ag,ah, lwd = "3", col = "black")
lines(ag,ai, lwd = "1", lty = "dashed", col = "black")
lines(ag,aj, lwd = "1", lty = "dashed", col = "black")

predict(q_fem_alc, c(90,90^2), transf=exp)
ak <- seq(0,90,length=90)
pred_fem_alc3 <- predict(q_fem_alc, newmods=cbind(ak,ak^2))
al <- rep(27.8, times=10)
am <- seq(90,100, length=10)
regplot(q_fem_alc, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,100), shade =FALSE, ci =FALSE,
        ylim = c(0, 60), pred = pred_fem_alc2, xvals = af, main="Female ALC")
lines(exp(pred_fem_alc3$ci.lb), lwd = "1", lty = "dashed", col = "black")
lines(exp(pred_fem_alc2$ci.ub), lwd = "1", lty = "dashed", col = "black")
lines(am,al, lwd = "1", lty = "dashed", col = "black")

#FEMALE HCV
fem_hcv <- dataset %>%
  filter(sex == 0 & type == 3)
dim(table(fem_hcv$study))

#MALE ALL TYPE
male_all <- dataset %>%
  filter(sex == 1 & type == 0)
dim(table(male_all$study))

q_male_all <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=male_all, digits = 6, 
                    random = ~ 1 | study, method = "REML")
summary(q_male_all)

a <- seq(0,150,length=150)
pred_male_all <- predict(q_male_all, newmods=cbind(a,a^2))
regplot(q_male_all, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 40), pred = pred_male_all, xvals = a, main="Male All Type")

-(0.033091/(2*-0.000113))
(0.033091*146.42)+((-0.000113)*(146.42*146.42))
predict(q_male_all, c(146.42,146.42^2), transf=exp)

an <- seq(0,146.42,length=146.42)
ao <- seq(146.42,150, length=4)
ap <- rep(11.175155, times=4)
aq <- rep(8.136047, times=4)
ar <- rep(15.349481, times=4)

pred_male_all2 <- predict(q_male_all, newmods=cbind(an,an^2))
regplot(q_male_all, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), shade =FALSE,
        ylim = c(0, 20), pred = pred_male_all2, xvals = an, main="Male All Type")
lines(ao,ap, lwd = "3", col = "black")
lines(ao,aq, lwd = "1", lty = "dashed", col = "black")
lines(ao,ar, lwd = "1", lty = "dashed", col = "black")

#MALE ALC
male_alc <- dataset %>%
  filter(sex == 1 & type == 1)
dim(table(male_alc$study))

q_male_alc <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=male_alc, digits = 6, 
                     random = ~ 1 | study, method = "REML")
summary(q_male_alc)

a <- seq(0,150,length=150)
pred_male_alc <- predict(q_male_alc, newmods=cbind(a,a^2))
regplot(q_male_alc, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), 
        ylim = c(0, 40), pred = pred_male_alc, xvals = a, main="Male ALC")

-(0.021743/(2*0.000078))

#MALE HCV
male_hcv <- dataset %>%
  filter(sex == 1 & type == 3)
dim(table(male_hcv$study))

#MORBIDITY ALL TYPE
morb_all <- dataset %>%
  filter(mortality == 0 & type == 0)
dim(table(morb_all$study))

q_morb_all <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=morb_all, digits = 6, 
                     random = ~ 1 | study, method = "REML")
summary(q_morb_all)

a <- seq(0,150,length=150)
pred_morb_all <- predict(q_morb_all, newmods=cbind(a,a^2))
regplot(q_morb_all, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), shade =FALSE, 
        ylim = c(0, 40), pred = pred_morb_all, xvals = a, main="Morbidity All Type")

-(0.025850/(2*-0.000024))
(0.025850*538.54)+((-0.000024)*(538.54*538.54))
predict(q_morb_all, c(538.54,538.54^2))

#MORBIDITY ALC
morb_alc <- dataset %>%
  filter(mortality == 0 & type == 1)
dim(table(morb_alc$study))

#MORBIDITY HCV
morb_hcv <- dataset %>%
  filter(mortality == 0 & type == 3)
dim(table(morb_hcv$study))

q_morb_hcv <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=morb_hcv, digits = 6, 
                     random = ~ 1 | study, method = "REML")
summary(q_morb_hcv)

a <- seq(0,150,length=150)
pred_morb_hcv <- predict(q_morb_hcv, newmods=cbind(a,a^2))
regplot(q_morb_hcv, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), shade =FALSE, 
        ylim = c(0, 10), pred = pred_morb_hcv, xvals = a, main="Morbidity HCV")

weights(q_morb_hcv)
morb_hcv <- morb_hcv[-(14),]

-(0.026035/(2*-0.000089))
(0.026035*146.26)+((-0.000089)*(146.26*146.26))
predict(q_morb_hcv, c(146.26,146.26^2))

as <- seq(0,146.26,length=146.26)
at <- seq(146.26,150, length=4)
au <- rep(6.715305, times=4)
av <- rep(5.323907, times=4)
aw <- rep(8.470343, times=4)

pred_morb_hcv2 <- predict(q_morb_hcv, newmods=cbind(as,as^2))
regplot(q_morb_hcv, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), shade =FALSE,
        ylim = c(0, 10), pred = pred_morb_hcv2, xvals = as, main="Morbidity HCV")
lines(at,au, lwd = "3", col = "black")
lines(at,av, lwd = "1", lty = "dashed", col = "black")
lines(at,aw, lwd = "1", lty = "dashed", col = "black")

#MORTALITY ALL
mort_all <- dataset %>%
  filter(mortality == 1 & type == 0)
dim(table(mort_all$study))

q_mort_all <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=mort_all, digits = 6, 
                     random = ~ 1 | study, method = "REML")
summary(q_mort_all)

a <- seq(0,150,length=150)
pred_mort_all <- predict(q_mort_all, newmods=cbind(a,a^2))
regplot(q_mort_all, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), shade =FALSE, 
        ylim = c(0, 40), pred = pred_mort_all, xvals = a, main="Mortality All Type")

-(0.047936/(2*-0.000229))
(0.047936*104.66)+((-0.000229)*(104.66*104.66))
predict(q_mort_all, c(104.66,104.66^2), transf=exp)

ax <- seq(0,104.66,length=104.66)
ay <- seq(104.66,150, length=46)
az <- rep(12.335134, times=46)
ba <- rep(10.110520, times=46)
bb <- rep(15.049230, times=46)

pred_mort_all2 <- predict(q_mort_all, newmods=cbind(ax,ax^2))
regplot(q_mort_all, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), shade =FALSE,
        ylim = c(0, 20), pred = pred_mort_all2, xvals = ax, main="Morbidity All Type")
lines(ay,az, lwd = "3", col = "black")
lines(ay,ba, lwd = "1", lty = "dashed", col = "black")
lines(ay,bb, lwd = "1", lty = "dashed", col = "black")

#MORTALITY ALC
mort_alc <- dataset %>%
  filter(mortality == 1 & type == 1)
dim(table(mort_alc$study))

q_mort_alc <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=mort_alc, digits = 8, 
                     random = ~ 1 | study, method = "REML")
summary(q_mort_alc)

a <- seq(0,150,length=150)
pred_mort_alc <- predict(q_mort_alc, newmods=cbind(a,a^2))
regplot(q_mort_alc, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), shade =FALSE, 
        ylim = c(0, 40), pred = pred_mort_alc, xvals = a, main="Mortality ALC")

-(0.04148667/(2*-0.00006688))
(0.04148667*310.16)+((-0.00006688)*(310.16*310.16))
predict(q_mort_alc, c(310.16,310.16^2))

#MORTALITY HCV
mort_hcv <- dataset %>%
  filter(mortality == 1 & type == 3)
dim(table(mort_hcv$study))

#FEMALE_MORBIDITY_ALLTYPE -> SAME AS FEMALE MORBIDITY
fem_morb_all <- dataset %>%
  filter(sex == 0 & mortality == 0 & type == 0)
dim(table(fem_morb_all$study))

fem_morb_alc <- dataset %>%
  filter(sex == 0 & mortality == 0 & type == 1)
dim(table(fem_morb_alc$study))

fem_morb_hcv <- dataset %>%
  filter(sex == 0 & mortality == 0 & type == 3)
dim(table(fem_morb_hcv$study))

#FEMALE MORTALITY ALL TYPE
fem_mort_all <- dataset %>%
  filter(sex == 0 & mortality == 1 & type == 0)
dim(table(fem_mort_all$study))

q_fem_mort_all <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=fem_mort_all, digits = 6, 
                     random = ~ 1 | study, method = "REML")
summary(q_fem_mort_all)

a <- seq(0,150,length=150)
pred_fem_mort_all <- predict(q_fem_mort_all, newmods=cbind(a,a^2))
regplot(q_fem_mort_all, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), shade =FALSE, 
        ylim = c(0, 40), pred = pred_fem_mort_all, xvals = a, main="Female Mortality All Type")

-(0.060646/(2*-0.000312))
(0.060646*97.19)+((-0.000312)*(97.19*97.19))
predict(q_fem_mort_all, c(97.19,97.19^2),transf=exp)

bc <- seq(0,97.19,length=97.19)
bd <- seq(97.19,150, length=53)
be <- rep(19.085129, times=53)
bf <- rep(12.787579, times=53)
bg <- rep(28.484057, times=53)

pred_fem_mort_all2 <- predict(q_fem_mort_all, newmods=cbind(bc,bc^2))
regplot(q_fem_mort_all, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), shade =FALSE,
        ylim = c(0, 40), pred = pred_fem_mort_all2, xvals = bc, main="Female Mortality All Type")
lines(bd,be, lwd = "3", col = "black")
lines(bd,bf, lwd = "1", lty = "dashed", col = "black")
lines(bd,bg, lwd = "1", lty = "dashed", col = "black")

#FEMALE_MORTALITY_ALC -> SAME AS FEMALE ALC
fem_mort_alc <- dataset %>%
  filter(sex == 0 & mortality == 1 & type == 1)
dim(table(fem_mort_alc$study))

fem_mort_hcv <- dataset %>%
  filter(sex == 0 & mortality == 1 & type == 3)
dim(table(fem_mort_hcv$study))

#MALE MORBIDITY ALL
male_morb_all <- dataset %>%
  filter(sex == 1 & mortality == 0 & type == 0)
dim(table(male_morb_all$study))

q_male_morb_all <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=male_morb_all, digits = 6, 
                         random = ~ 1 | study, method = "REML")
summary(q_male_morb_all)

a <- seq(0,150,length=150)
pred_male_morb_all <- predict(q_male_morb_all, newmods=cbind(a,a^2))
regplot(q_male_morb_all, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), shade =FALSE, 
        ylim = c(0, 40), pred = pred_male_morb_all, xvals = a, main="Male Morbidity All Type")

-(0.027049/(2*-0.000045))
(0.027049*300.54)+((-0.000045)*(300.54*300.54))
predict(q_male_morb_all, c(300.54,300.54^2))


male_morb_alc <- dataset %>%
  filter(sex == 1 & mortality == 0 & type == 1)
dim(table(male_morb_alc$study))

male_morb_hcv <- dataset %>%
  filter(sex == 1 & mortality == 0 & type == 3)
dim(table(male_morb_hcv$study))

#MALE MORTALITY ALL
male_mort_all <- dataset %>%
  filter(sex == 1 & mortality == 1 & type == 0)
dim(table(male_mort_all$study))

q_male_mort_all <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=male_mort_all, digits = 6, 
                          random = ~ 1 | study, method = "REML")
summary(q_male_mort_all)

a <- seq(0,150,length=150)
pred_male_mort_all <- predict(q_male_mort_all, newmods=cbind(a,a^2))
regplot(q_male_mort_all, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), shade =FALSE, 
        ylim = c(0, 40), pred = pred_male_mort_all, xvals = a, main="Male Mortality All Type")

-(0.042913/(2*-0.000204))
(0.042913*105.18)+((-0.000204)*(105.18*105.18))
predict(q_male_mort_all, c(105.18,105.18^2),transf=exp)

bh <- seq(0,105.18,length=105.18)
bi <- seq(105.18,150, length=45)
bj <- rep(9.501212, times=45)
bk <- rep(7.181161, times=45)
bl <- rep(12.570812, times=45)

pred_male_mort_all2 <- predict(q_male_mort_all, newmods=cbind(bh,bh^2))
regplot(q_male_mort_all, mod="dose", xlab="Alcohol intake, grams/day", ylab="Relative Risk",
        transf=exp, digits=2L, las=1, bty="l", xlim = c(0,150), shade =FALSE,
        ylim = c(0, 20), pred = pred_male_mort_all2, xvals = bh, main="Male Mortality All Type")
lines(bi,bj, lwd = "3", col = "black")
lines(bi,bk, lwd = "1", lty = "dashed", col = "black")
lines(bi,bl, lwd = "1", lty = "dashed", col = "black")

male_mort_alc <- dataset %>%
  filter(sex == 1 & mortality == 1 & type == 1)
dim(table(male_mort_alc$study))

male_mort_hcv <- dataset %>%
  filter(sex == 1 & mortality == 1 & type == 3)
dim(table(male_mort_hcv$study))
