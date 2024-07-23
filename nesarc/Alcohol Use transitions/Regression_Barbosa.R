
# SIMAH - NESARC Alcohol Transitions
# Regression model estimation

# Load packages and data
library(tidyverse)  # data management
library(ZIM)
library(nnet)

# Specify the data and output file locations
data    <- "~/Google Drive/SIMAH Sheffield/SIMAH_workplace/nesarc/Processed data/"  # Location of data
models  <- "~/Google Drive/SIMAH Sheffield/SIMAH_workplace/nesarc/Models/"          # Location of saved MSM models

# Load data / functions
nesarc_expanded <- readRDS(paste0(data, "nesarc_clean_new.rds")) 

# select the variables that are needed 
nesarc_selected <- nesarc_expanded %>% 
  dplyr::select(idnum, wave, weight_wave2, years, age,
                female.factor, race.factor,
                edu3, alc_daily_g, alc4.factor, alc5.factor) %>% 
  mutate(alc_rounded = ceiling(alc_daily_g),
         alc_rounded = ifelse(alc_rounded>200, 200, alc_rounded),
         age3 = cut(age, 
                    breaks=c(0,24,64,100),
                    labels=c("18-24","25-64","65+")),
         abstainer = ifelse(alc4.factor=="Non-drinker",1,0),
         formerdrinker = ifelse(alc5.factor=="Former", 1,0),
         cat1 = ifelse(alc4.factor=="Category I", 1,0),
         cat2 = ifelse(alc4.factor=="Category II", 1,0),
         cat3 = ifelse(alc4.factor=="Category III", 1,0)) %>% 
  dplyr::select(idnum, wave, weight_wave2, years, age3, female.factor, race.factor, edu3,alc4.factor) %>% 
  pivot_wider(names_from=wave, 
              values_from=c(years, age3, female.factor, race.factor, edu3,alc4.factor)) %>% 
  mutate(cat1_1 = ifelse(alc4.factor_1=="Category I", 1,0),
         cat2_1 = ifelse(alc4.factor_1=="Category II", 1,0),
         cat3_1 = ifelse(alc4.factor_1=="Category III", 1,0),
         Black = ifelse(race.factor_2=="Black, non-Hispanic",1,0),
         White = ifelse(race.factor_2=="White, non-Hispanic",1,0),
         Hispanic = ifelse(race.factor_2=="Hispanic", 1,0),
         Other=ifelse(race.factor_2=="Other, non-Hispanic",1,0),
         Women = ifelse(female.factor_2=="Women", 1,0),
         edulow = ifelse(edu3_2 =="Low",1,0),
         edumed = ifelse(edu3_2 =="Med",1,0),
         age2564 = ifelse(age3_2=="25-64",1,0),
         age65 = ifelse(age3_2=="65+",1,0))

fit <- multinom(alc4.factor_2 ~ cat1_1 + cat2_1 + cat3_1 + age3_2 + female.factor_2 + 
              race.factor_2 + edu3_2, data=nesarc_selected)

summary(fit)

nesarc_selected$transition_probabilities <- predict(fit, type = "probs")


# Extract coefficients
# Extract coefficients and intercepts
coefficients <- coef(fit)

forprediction <- expand.grid(alc4.factor_1 = unique(nesarc_selected$alc4.factor_1),
                            female.factor_2= unique(nesarc_selected$female.factor_2),
                            age3_2 =unique(nesarc_selected$age3_2),
                            race.factor_2 = unique(nesarc_selected$race.factor_2),
                            edu3_2 =  unique(nesarc_selected$edu3_2)) %>% 
  mutate(cat1_1 = ifelse(alc4.factor_1=="Category I", 1,0),
         cat2_1 = ifelse(alc4.factor_1=="Category II", 1,0),
         cat3_1 = ifelse(alc4.factor_1=="Category III", 1,0),
         Black = ifelse(race.factor_2=="Black, non-Hispanic",1,0),
         White = ifelse(race.factor_2=="White, non-Hispanic",1,0),
         Hispanic = ifelse(race.factor_2=="Hispanic", 1,0),
         Other=ifelse(race.factor_2=="Other, non-Hispanic",1,0),
         Women = ifelse(female.factor_2=="Women", 1,0),
         edulow = ifelse(edu3_2 =="Low",1,0),
         edumed = ifelse(edu3_2 =="Med",1,0),
         age2564 = ifelse(age3_2=="25-64",1,0),
         age65 = ifelse(age3_2=="65+",1,0))

# set up data to predict, unique combo of everything 
predict_categoryI <- coefficients[1, "(Intercept)"] + 
  coefficients[1, "cat1_1"] * forprediction$cat1_1 + 
  coefficients[1, "cat2_1"] * forprediction$cat2_1 + 
  coefficients[1, "cat3_1"] * forprediction$cat3_1 + 
  coefficients[1, "age3_225-64"] * forprediction$age2564 + 
  coefficients[1, "age3_265+"] * forprediction$age65 + 
  coefficients[1, "female.factor_2Women"] * forprediction$Women + 
  coefficients[1, "race.factor_2Black, non-Hispanic"] * forprediction$Black + 
  coefficients[1, "race.factor_2Hispanic"] * forprediction$Hispanic + 
  coefficients[1, "race.factor_2Other, non-Hispanic"] * forprediction$Other + 
  coefficients[1, "edu3_2Low"] * forprediction$edulow + 
  coefficients[1, "edu3_2Med"] * forprediction$edumed

predict_categoryII <- coefficients[2, "(Intercept)"] + 
  coefficients[2, "cat1_1"] * forprediction$cat1_1 + 
  coefficients[2, "cat2_1"] * forprediction$cat2_1 + 
  coefficients[2, "cat3_1"] * forprediction$cat3_1 + 
  coefficients[2, "age3_225-64"] * forprediction$age2564 + 
  coefficients[2, "age3_265+"] * forprediction$age65 + 
  coefficients[2, "female.factor_2Women"] * forprediction$Women + 
  coefficients[2, "race.factor_2Black, non-Hispanic"] * forprediction$Black + 
  coefficients[2, "race.factor_2Hispanic"] * forprediction$Hispanic + 
  coefficients[2, "race.factor_2Other, non-Hispanic"] * forprediction$Other + 
  coefficients[2, "edu3_2Low"] * forprediction$edulow + 
  coefficients[2, "edu3_2Med"] * forprediction$edumed

predict_categoryIII <- coefficients[3, "(Intercept)"] + 
  coefficients[3, "cat1_1"] * forprediction$cat1_1 + 
  coefficients[3, "cat2_1"] * forprediction$cat2_1 + 
  coefficients[3, "cat3_1"] * forprediction$cat3_1 + 
  coefficients[3, "age3_225-64"] * forprediction$age2564 + 
  coefficients[3, "age3_265+"] * forprediction$age65 + 
  coefficients[3, "female.factor_2Women"] * forprediction$Women + 
  coefficients[3, "race.factor_2Black, non-Hispanic"] * forprediction$Black + 
  coefficients[3, "race.factor_2Hispanic"] * forprediction$Hispanic + 
  coefficients[3, "race.factor_2Other, non-Hispanic"] * forprediction$Other + 
  coefficients[3, "edu3_2Low"] * forprediction$edulow + 
  coefficients[3, "edu3_2Med"] * forprediction$edumed

sum_exp_linear_predictors <- 1 + exp(predict_categoryI) + 
  exp(predict_categoryII) + 
  exp(predict_categoryIII)

probabstainer <- 1 / sum_exp_linear_predictors
probcategoryI <- exp(predict_categoryI) / sum_exp_linear_predictors
probcategoryII <- exp(predict_categoryII) / sum_exp_linear_predictors
probcategoryIII <- exp(predict_categoryIII) / sum_exp_linear_predictors

forprediction$probabstainer <- probabstainer
forprediction$probcategoryI <- probcategoryI
forprediction$probcategoryII <- probcategoryII
forprediction$probcategoryIII <- probcategoryIII


test <- forprediction %>% 
  dplyr::select(age3_2, female.factor_2, race.factor_2, edu3_2,
                alc4.factor_1, probabstainer, 
                probcategoryI,
                probcategoryII, probcategoryIII)

# now convert them to 1 year probabilities from 3 year probabilities
problist <- test %>% mutate(female.factor_2=ifelse(female.factor_2=="Women","f","m"),
                            race.factor_2=recode(race.factor_2, 
                                                 "Black, non-Hispanic" = "BLA" ,
                                                 "White, non-Hispanic" = "WHI",
                                                 "Hispanic" = "SPA",
                                                 "Other, non-Hispanic" = "OTH"),
                            edu3_2 = recode(edu3_2,
                                            "Low"="LEHS","Med"="SomeC","High"="College"),
  cat = paste(age3_2, female.factor_2, race.factor_2,
                                         edu3_2, sep="_"),
                            alc4.factor_1 = factor(alc4.factor_1, levels=c("Non-drinker","Category I","Category II","Category III")),
                         samplenum=1) %>% 
  arrange(alc4.factor_1) %>% 
  dplyr::select(samplenum,cat,alc4.factor_1, probabstainer, probcategoryI, probcategoryII, probcategoryIII) %>% 
  distinct()

oneyearprob <- convert_to_one_year(problist)

# save the one year and 3 year probabilities
write.csv(problist, paste0(models, "3yrprob_multinomial.csv"),row.names=F)

write.csv(oneyearprob, paste0(models, "1yrprob_multinomial.csv"),row.names=F)

# get ready for modelling 
oneyearprob_model <- oneyearprob %>% 
  mutate(alc4.factor = case_when(alc4.factor_1 == "Non-drinker" ~ "Non-drinker",
                                 alc4.factor_1 == "Category I" ~ "Low risk",
                                 alc4.factor_1 =="Category II" ~ "Medium risk",
                                 alc4.factor_1 == "Category III" ~ "High risk")) %>% 
  mutate(cat = paste(cat, alc4.factor, sep="_")) %>% 
  pivot_longer(probabstainer:probcategoryIII) %>% 
  group_by(cat) %>% mutate(cumsum=cumsum(value)) %>% 
  dplyr::select(cat, name, cumsum) %>% 
  rename(StateTo=name) %>% 
  mutate(StateTo = case_when(StateTo=="probabstainer" ~ "Non-drinker",
                             StateTo=="probcategoryI" ~ "Low risk",
                             StateTo=="probcategoryII" ~ "Medium risk",
                             StateTo=="probcategoryIII" ~ "High risk"))

write.csv(oneyearprob_model, paste0(models, "1yrprob_multinomial_model.csv"),row.names=F)
