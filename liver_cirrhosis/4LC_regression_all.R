library(tidyverse)
library(meta)
library(metafor)
library(dosresmeta)
library(mvmeta)

# Personal computer; specify locations 
data   <- "C:/Users/laura/Documents/CAMH/SIMAH/SIMAH_workplace/liver_cirrhosis/"    # Location of data

# load data
dataset <- readRDS (paste0(data, "4LC_regression_all.xlsx"))

library(readxl)
dataset <- read_excel("CAMH/CIRROSIS/SIMAH/SIMAH_dataset/4LC_regression_all.xlsx", 
                      col_types = c("numeric", "numeric", "text", 
                                    "numeric", "text", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric","numeric", "numeric", 
                                    "text", "numeric","numeric",
                                    "numeric","numeric","text", "numeric", "numeric", "numeric"))

dataset$sex <- as.factor(dataset$sex)
dataset$type <- as.factor(dataset$type)
dataset$usa <- as.factor(dataset$usa)
dataset$qualitySC <- as.factor(dataset$qualitySC)
dataset$qualitySC2 <- as.factor(dataset$qualitySC2)
dataset$outcome <- as.factor(dataset$outcome)
dataset$mortality <- as.factor(dataset$mortality)
dataset$over_55 <- as.factor(dataset$over_55)
dataset$studyyear <- as.factor(dataset$studyyear)
dataset$type_updated <- as.factor(dataset$type_updated)

final <- dataset %>%
  filter(dose != 0.00)

dim(table(final$sex))

hcv <- dataset %>%
  filter(type == 3)

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
                  + dose:usa + dose:mortality + I(dose^2):mortality, 
                  digits = 6,data=final, random = ~ 1 | study, method = "REML")
model10

anova(model9a, model10)

#age models
model_age <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2) + dose:age, data=final, 
                    random = ~ 1 | study, method = "REML")
model_age

model_age2 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2) + dose:age +I(dose^2):age, data=final, 
                     random = ~ 1 | study, method = "REML")
model_age2

anova(model_age, model_age2)

model_age55<- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2) + dose:over_55, data=final, 
                     random = ~ 1 | study, method = "REML")
model_age55

#final model
model_age55 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex+ I(dose^2):sex
                      + dose:type + I(dose^2):type + dose:qualitySC
                      + dose:usa + dose:mortality + I(dose^2):mortality + dose:over_55, 
                      digits = 6,data=final, random = ~ 1 | study, method = "REML")

model_age55

vcov(model_age55, type="fixed")


model11 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex+ I(dose^2):sex
                  + dose:type + I(dose^2):type + dose:qualitySC
                  + dose:usa + dose:mortality + I(dose^2):mortality + dose:over_55 , 
                  digits = 5,data=final, random = ~ 1 | study, method = "REML")
model11

anova(model11, model10)


#study year

year90 <- final %>%
  filter(studyyear == 1)
year00 <- final %>%
  filter(studyyear == 0)

model90 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex+ I(dose^2):sex
                  + dose:type + I(dose^2):type + dose:qualitySC
                  + dose:usa + dose:mortality + I(dose^2):mortality, 
                  digits = 6,data=year90, random = ~ 1 | study, method = "REML")
model90

model00 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex+ I(dose^2):sex
                  + dose:type + I(dose^2):type + dose:qualitySC
                  + dose:usa + dose:mortality + I(dose^2):mortality, 
                  digits = 6,data=year00, random = ~ 1 | study, method = "REML")
model00

dim(table(year90$id))


0.058164-0.057926
(0.000238/0.058164)*100

((0.031514-0.031477)/0.031514)*100

((0.003958-0.003743)/0.003958)*100

(0.016076-0.013049)/0.016076*100

(0.006776-0.006268)/0.006268*100

(0.010297-0.010097)/0.010297*100

#updated
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
                 + dose*type_updated, data=final, random = ~ 1 | study, method = "REML")
model3

anova(model2, model3)

model3a <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex + I(dose^2):sex
                  + dose:type_updated, data=final, random = ~ 1 | study, method = "REML")
model3a

anova(model3a, model3)

model4 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex+ I(dose^2):sex
                 + dose:type_updated + I(dose^2):type_updated, data=final, random = ~ 1 | study, method = "REML")
model4

anova(model4, model3a)

model5 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex + I(dose^2):sex
                 + dose:type_updated + I(dose^2):type_updated + dose*qualitySC, data=final, random = ~ 1 | study, method = "REML")
model5

anova(model4, model5)

model5a <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex + I(dose^2):sex
                  + dose:type_updated + I(dose^2):type_updated + dose:qualitySC, data=final, random = ~ 1 | study, method = "REML")
model5a

anova(model5a, model5)

model6 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex + I(dose^2):sex
                 + dose:type_updated + I(dose^2):type_updated + dose:qualitySC
                 + I(dose^2):qualitySC, data=final, random = ~ 1 | study, method = "REML")
model6

anova(model5a, model6)

model7 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex + I(dose^2):sex
                 + dose:type_updated + I(dose^2):type_updated + dose:qualitySC
                 + dose*usa, data=final, random = ~ 1 | study, method = "REML")
model7

anova(model5a, model7)

model7a <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex + I(dose^2):sex
                  + dose:type_updated + I(dose^2):type_updated + dose:qualitySC
                  + dose:usa,digits = 6, data=final, random = ~ 1 | study, method = "REML")
model7a

anova(model7a, model7)

model8 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex+ I(dose^2):sex
                 + dose:type_updated + I(dose^2):type_updated + dose:qualitySC
                 + dose:usa+ I(dose^2):usa, data=final, random = ~ 1 | study, method = "REML")
model8

anova(model7a, model8)

model9 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex + I(dose^2):sex
                 + dose:type_updated + I(dose^2):type_updated + dose:qualitySC
                 + dose:usa + dose*mortality, data=final, random = ~ 1 | study, method = "REML")
model9

anova(model7a, model9)

model9a <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex + I(dose^2):sex
                  + dose:type_updated + I(dose^2):type_updated + dose:qualitySC
                  + dose:usa + dose:mortality, data=final, random = ~ 1 | study, method = "REML")
model9a

anova(model9a, model9)

#final model
model10 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex+ I(dose^2):sex
                  + dose:type_updated + I(dose^2):type_updated + dose:qualitySC
                  + dose:usa + dose:mortality + I(dose^2):mortality, 
                  digits = 6,data=final, random = ~ 1 | study, method = "REML")
model10

anova(model9a, model10)

##Not US
model10_us <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex+ I(dose^2):sex
                     + dose:type_updated + I(dose^2):type_updated + dose:qualitySC +
                       dose:mortality + I(dose^2):mortality, 
                     digits = 6,data=final, random = ~ 1 | study, method = "REML")
model10_us

model11_us <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex+ I(dose^2):sex
                     + dose:type_updated + I(dose^2):type_updated + dose:qualitySC, 
                     digits = 6,data=final, random = ~ 1 | study, method = "REML")
model11_us

#sexes combined
model10_both <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:type_updated + I(dose^2):type_updated + dose:qualitySC +
                         dose:mortality + I(dose^2):mortality + dose:usa, 
                       digits = 6,data=final, random = ~ 1 | study, method = "REML")
model10_both

model11_both <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:usa
                       + dose:type_updated + I(dose^2):type_updated + dose:qualitySC, 
                       digits = 6,data=final, random = ~ 1 | study, method = "REML")
model11_both

