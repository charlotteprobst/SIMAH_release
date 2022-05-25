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
dataset$usa <- as.factor(dataset$usa)
dataset$qualitySC <- as.factor(dataset$qualitySC)
dataset$outcome <- as.factor(dataset$outcome)
dataset$mortality <- as.factor(dataset$mortality)

final <- dataset %>%
  filter(dose != 0.00)

#drop Alemy-Carreau
final <- final[-c(1),]

quad <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=final, 
               random = ~ 1 | study, method = "REML")
quad

model1 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose*sex, data=final, 
                 random = ~ 1 | study, method = "REML")
model1

anova(quad, model1)

model1a <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex, data=final, 
                  random = ~ 1 | study, method = "REML")
model1a

anova(model1, model1a)

model2 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex + I(dose^2):sex, data=final, 
                 random = ~ 1 | study, method = "REML")
model2

anova(model2, model1a)

model3 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex + I(dose^2):sex
                 + dose*type, data=final, random = ~ 1 | study, method = "REML")
model3

anova(model2, model3)

model3a <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex + I(dose^2):sex
                  + dose:type, data=final, random = ~ 1 | study, method = "REML")
model3a

anova(model3a, model3)

model4 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex+ I(dose^2):sex
                 + dose:type + I(dose^2):type, data=final, random = ~ 1 | study, method = "REML")
model4

anova(model4, model3a)

model5 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex + I(dose^2):sex
                 + dose:type + I(dose^2):type + dose*qualitySC, data=final, random = ~ 1 | study, method = "REML")
model5

anova(model4, model5)

model5a <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex + I(dose^2):sex
                  + dose:type + I(dose^2):type + dose:qualitySC, data=final, random = ~ 1 | study, method = "REML")
model5a

anova(model5a, model5)

model6 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex + I(dose^2):sex
                 + dose:type + I(dose^2):type + dose:qualitySC
                 + I(dose^2):qualitySC, data=final, random = ~ 1 | study, method = "REML")
model6

anova(model5a, model6)

model7 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex + I(dose^2):sex
                 + dose:type + I(dose^2):type + dose:qualitySC
                 + dose*usa, data=final, random = ~ 1 | study, method = "REML")
model7

anova(model5a, model7)

model7a <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex + I(dose^2):sex
                  + dose:type + I(dose^2):type + dose:qualitySC
                  + dose:usa, data=final, random = ~ 1 | study, method = "REML")
model7a

anova(model7a, model7)

model8 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex+ I(dose^2):sex
                 + dose:type + I(dose^2):type + dose:qualitySC
                 + dose:usa+ I(dose^2):usa, data=final, random = ~ 1 | study, method = "REML")
model8

anova(model7a, model8)

model9 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex + I(dose^2):sex
                 + dose:type + I(dose^2):type + dose:qualitySC
                 + dose:usa + dose*mortality, data=final, random = ~ 1 | study, method = "REML")
model9

anova(model7a, model9)

model9a <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex + I(dose^2):sex
                  + dose:type + I(dose^2):type + dose:qualitySC
                  + dose:usa + dose:mortality, data=final, random = ~ 1 | study, method = "REML")
model9a

anova(model9a, model9)

#final model
model10 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex+ I(dose^2):sex
                  + dose:type + I(dose^2):type + dose:qualitySC
                  + dose:usa + dose:mortality + I(dose^2):mortality, data=final, random = ~ 1 | study, method = "REML")
model10

anova(model9a, model10)
