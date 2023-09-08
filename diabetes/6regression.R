library(tidyverse)
library(ggplot2)
library(mvmeta)

library(dosresmeta)
library(meta)
library(metafor)

library(readxl)
dataset <- read_excel("CAMH/DIABETES/analysis/SIMAH_workplace/6dataset.xlsx", 
                      col_types = c("numeric", "numeric", "numeric", "numeric", "numeric", "text", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric"))

dataset$sex <- as.factor(dataset$sex)
dataset$usa <- as.factor(dataset$usa)
dataset$over_45 <- as.factor(dataset$over_45)
dataset$outcome.ascertaiment <- as.factor(dataset$outcome.ascertaiment)
dataset$NOS_cat <- as.factor(dataset$NOS_cat)
dataset$NOS_cat_new <- as.factor(dataset$NOS_cat_new)
dataset$asia <- as.factor(dataset$asia)

#ALL STUDIES
final <- dataset %>%
  filter(analysis_id==0 & dose != 0.00)
final <- final[-c(35,36,37,38,39,162,163,164,165,166,249,250,251),]

quad <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=final, 
               random = ~ 1 | cohort_id/line_id,  method = "REML")
summary(quad)

model1 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose*sex, data=final, 
                 random = ~ 1 | cohort_id/line_id, method = "REML")
model1

anova(quad, model1,refit=TRUE)

model1a <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex, data=final, 
                  random = ~ 1 | cohort_id/line_id, method = "REML")
model1a

anova(model1, model1a,refit=TRUE)

model2 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex + I(dose^2):sex, data=final, 
                 random = ~ 1 | cohort_id/line_id, method = "REML")
model2

anova(model2, model1a,refit=TRUE)

model3 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex + I(dose^2):sex
                 + dose*over_45, data=final, random = ~ 1 | cohort_id/line_id,  method = "REML")
model3

anova(model2, model3)

model3a <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex + I(dose^2):sex
                  + dose:over_45, data=final, random = ~ 1 | cohort_id/line_id, method = "REML")
model3a

anova(model3a, model3,refit=TRUE)

model4 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex+ I(dose^2):sex
                 + dose:over_45 + I(dose^2):over_45, data=final, random = ~ 1 | cohort_id/line_id, method = "REML")
model4

anova(model4, model3a,refit=TRUE)

model5 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex + I(dose^2):sex
                 + dose:over_45 + dose*usa, data=final, random = ~ 1 | cohort_id/line_id, method = "REML")
model5

anova(model4, model5)

#final model - waiting for risk of bias
model5a <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex + I(dose^2):sex
                  + dose:over_45 + dose:usa, data=final, random = ~ 1 | cohort_id/line_id, method = "REML")
model5a

anova(model5a, model5,refit=TRUE)

model6 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex + I(dose^2):sex
                 + dose:over_45 +  dose:usa
                 + I(dose^2):usa, data=final, random = ~ 1 | cohort_id/line_id, method = "REML")
model6

anova(model5a, model6,refit=TRUE)

#age as continuous
model7 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex + I(dose^2):sex
                 + dose:age_all + I(dose^2):age_all + dose:usa, data=final, random = ~ 1 | cohort_id/line_id, method = "REML")
model7

##risk of bias - pending final numbers
model8 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex + I(dose^2):sex
                 + dose:age_all + I(dose^2):age_all + dose:usa + dose:NOS_cat, data=final, digits = 6, 
                 random = ~ 1 | cohort_id/line_id, method = "REML")
model8

model9 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex + I(dose^2):sex
                 + dose:age_all + I(dose^2):age_all + dose:usa + dose:NOS_cat + I(dose^2):NOS_cat, data=final, random = ~ 1 | cohort_id/line_id, method = "REML")
model9

anova(model8, model9,refit=TRUE)

#FINAL MODEL
model10 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex + I(dose^2):sex
                  + dose:age_all + I(dose^2):age_all + dose:usa + I(dose^2):usa
                  + dose:NOS_cat_new + I(dose^2):NOS_cat_new, data=final, digits = 6, 
                  random = ~ 1 | cohort_id/line_id, method = "REML")
model10

#MODEL WITH ASIAN COUNTRIES
model11 <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) + dose:sex + I(dose^2):sex
                  + dose:age_all + I(dose^2):age_all + dose:asia + I(dose^2):asia
                  + dose:NOS_cat_new + I(dose^2):NOS_cat_new, data=final, digits = 6, 
                  random = ~ 1 | cohort_id/line_id, method = "REML")
model11

#MALE MODEL
male <- dataset %>%
  filter(analysis_id==0 & dose != 0.00 & sex ==1)

male <- male[-c(15),]

quad_male <- rma.mv(yi=lnor, V=se^2, mods = ~ dose +0, data=male, 
                    random = ~ 1 | cohort_id/line_id, method = "REML")
summary(quad_male)

model3m <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+  
                 + dose*over_45, data=male, random = ~ 1 | cohort_id/line_id,  method = "REML")
model3m

anova(quad_male, model3m)

model3am <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+  
                  + dose:over_45, data=male, random = ~ 1 | cohort_id/line_id, method = "REML")
model3am

anova(model3am, model3m,refit=TRUE)

model5m <- rma.mv(yi=lnor, V=se^2, mods = ~ dose 
                 + dose:over_45 + dose*usa, data=male, random = ~ 1 | cohort_id/line_id, method = "REML")
model5m

anova(model4m, model5m)

#final model - waiting for risk of bias
model5am <- rma.mv(yi=lnor, V=se^2, mods = ~ dose 
                  + dose:over_45 + dose:usa, data=male, random = ~ 1 | cohort_id/line_id, method = "REML")
model5am

anova(model5am, model5m,refit=TRUE)

#age 
model6am <- rma.mv(yi=lnor, V=se^2, mods = ~ dose 
                   + dose:age_all + dose:usa, data=male, random = ~ 1 | cohort_id/line_id, method = "REML")
model6am

##final
model7am <- rma.mv(yi=lnor, V=se^2, mods = ~ dose 
                   + dose:age_all + dose:usa + dose:NOS_cat_new
                   , data=male, random = ~ 1 | cohort_id/line_id, method = "REML")
model7am

#FEMALE MODEL
female <- dataset %>%
  filter(analysis_id==0 & dose != 0.00 & sex ==0)

female <- female[-c(12),]

quad_female <- rma.mv(yi=lnor, V=se^2, mods = ~ dose + I(dose^2)+0, data=female, 
                    random = ~ 1 | cohort_id/line_id, method = "REML")
summary(quad_female)

model3f <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) 
                  + dose*over_45, data=female, random = ~ 1 | cohort_id/line_id,  method = "REML")
model3f

anova(quad_female, model3f)

model3af <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) 
                   + dose:over_45, data=female, random = ~ 1 | cohort_id/line_id, method = "REML")
model3af

anova(model3af, model3f,refit=TRUE)

model4f <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) 
                  + dose:over_45 + I(dose^2):over_45, data=female, random = ~ 1 | cohort_id/line_id, method = "REML")
model4f

anova(model4f, model3af,refit=TRUE)

model5f <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) 
                  + dose:over_45 + dose*usa, data=female, random = ~ 1 | cohort_id/line_id, method = "REML")
model5f

anova(model4f, model5f)

#final model - waiting for risk of bias
model5af <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) 
                   + dose:over_45 + dose:usa, data=female, random = ~ 1 | cohort_id/line_id, method = "REML")
model5af

anova(model5af, model5f,refit=TRUE)

model6f <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) 
                  + dose:over_45 +  dose:usa
                  + I(dose^2):usa, data=female, random = ~ 1 | cohort_id/line_id, method = "REML")
model6f

anova(model5af, model6f,refit=TRUE)

model7f <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) 
                   + dose:age_all + dose:usa, data=female, random = ~ 1 | cohort_id/line_id, method = "REML")
model7f

model8f <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) 
                  + dose:age_all + I(dose^2):age_all + dose:usa, data=female, random = ~ 1 | cohort_id/line_id, method = "REML")
model8f

anova(model7f, model8f,refit=TRUE)

##final
model9f <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) 
                  + dose:age_all + I(dose^2):age_all + dose:usa + dose:NOS_cat_new
                  , data=female, random = ~ 1 | cohort_id/line_id, method = "REML")
model9f

model10f <- rma.mv(yi=lnor, V=se^2, mods = ~ dose+ I(dose^2) 
                   + dose:age_all + I(dose^2):age_all + dose:usa + dose:NOS_cat_new + I(dose^2):NOS_cat_new
                   , data=female, random = ~ 1 | cohort_id/line_id, method = "REML")
model10f

anova(model10f, model9f,refit=TRUE)