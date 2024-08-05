
# SIMAH - NESARC Alcohol Transitions
# Regression model estimation

# Load packages and data
library(tidyverse)  # data management
library(ZIM)

# Specify the data and output file locations
data    <- "~/Google Drive/SIMAH Sheffield/SIMAH_workplace/nesarc/Processed data/"  # Location of data
models  <- "~/Google Drive/SIMAH Sheffield/SIMAH_workplace/nesarc/Models/"          # Location of saved MSM models

# Load data / functions
nesarc_expanded <- readRDS(paste0(data, "nesarc_clean_new.rds")) 

# select the variables that are needed 
nesarc_selected <- Pop %>% 
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
  dplyr::select(idnum, wave, weight_wave2, years, age3, female.factor, race.factor, edu3, alc_daily_g,
                alc_rounded, abstainer, formerdrinker, cat1, cat2, cat3) %>% 
  pivot_wider(names_from=wave, 
              values_from=c(years,age3,female.factor,race.factor, edu3,
                            alc_daily_g, alc_rounded, abstainer, formerdrinker, cat1, cat2, cat3)) %>% 
  mutate(microsim.init.alc.gpd=alc_rounded_1,
         formerdrinker = formerdrinker_1,
         microsim.init.sex = ifelse(female.factor_1=="Women", "f","m"),
         medorhigh = ifelse(cat2_1==1 | cat3_1==1, 1, 0))

nesarc_selected <- code_alcohol_categories(nesarc_selected)
nesarc_selected$oldcat <- nesarc_selected$AlcCAT
# nesarc_selected$oldcat <- ifelse(nesarc_selected$oldcat=="Medium risk" |
#                                    nesarc_selected$oldcat=="High risk", "MedorHigh", 
#                                  nesarc_selected$oldcat)
nesarc_selected$microsim.init.alc.gpd <- nesarc_selected$alc_rounded_2
nesarc_selected <- code_alcohol_categories(nesarc_selected)
# nesarc_selected$AlcCAT <- ifelse(nesarc_selected$AlcCAT=="Medium risk" |
#                                    nesarc_selected$AlcCAT=="High risk", "MedorHigh", 
#                                  nesarc_selected$AlcCAT)

test <- nesarc_selected %>% group_by(oldcat, AlcCAT) %>% tally() %>% 
  ungroup() %>% group_by(oldcat) %>% mutate(prop = n/sum(n))

nesarc_selected %>% group_by(oldcat) %>% tally() %>% 
  ungroup() %>% mutate(prop=n/sum(n))

nesarc_selected %>% group_by(AlcCAT) %>% tally() %>% 
  ungroup() %>% mutate(prop=n/sum(n))

library(pscl)

# round and scale the weight (to keep integers)

nesarc_selected$alcsq <- nesarc_selected$alc_rounded_1^2

predictors <- nesarc_selected %>% 
  dplyr::select(age3_2, female.factor_2, race.factor_2, 
                edu3_2, alc_rounded_1, abstainer_1,abstainer_2,
                formerdrinker_1, cat1_1, cat2_1, cat3_1,
                medorhigh)

# regression predicting the prob of being an abstainer - to decide on predictors
# init_mod <- glm(abstainer_2 ~ ., data = predictors, family=binomial,
#                 weights=nesarc_selected$scaledweight)
# stepped <- step(init_mod, scope = . ~ .^2, direction = 'both')
# summary(stepped)

nesarc_selected$scaledweight2 = as.integer(nesarc_selected$weight_wave2/nesarc_selected$years_2)
nesarc_selected$scaledweight2 <- as.integer(nesarc_selected$scaledweight2/135)
nesarc_selected$scaledweight1 <- as.integer(nesarc_selected$weight_wave2/455)

# nesarc_selected$scaledweight2 <- as.integer(nesarc_selected$scaledweight/nesarc_selected$years_2)

# predictors are previous alcohol use, all demographics and some interactions effects 
# count model parameters | zero model parameters 
m1 <-  zeroinfl(alc_rounded_2 ~ age3_2 + female.factor_2 +
                  race.factor_2 + edu3_2 + alc_rounded_1 + abstainer_1 + 
                  abstainer_1*age3_2 + alc_rounded_1*cat2_1 + alc_rounded_1*cat3_1 | 
                  age3_2 + female.factor_2 +
                  race.factor_2 + edu3_2 + alc_rounded_1 + abstainer_1 + 
                  abstainer_1*age3_2 + alc_rounded_1*cat2_1 + alc_rounded_1*cat3_1,
                data = nesarc_selected,
                weights = scaledweight1,
                dist = "poisson")
summary(m1)

Pop$microsim.init.alc.gpd_2003 <- round(Pop$microsim.init.alc.gpd_2003)
Pop$microsim.init.alc.gpd_2004 <- round(Pop$microsim.init.alc.gpd_2004)
Pop$agecat <- cut(Pop$microsim.init.age_2004,
                  breaks=c(0,24,64,100),
                  labels=c("18-24","25-64","65+"))
Pop$abstainer <- ifelse(Pop$microsim.init.alc.gpd_2003==0, 1,0)


m1 <-  zeroinfl(microsim.init.alc.gpd_2004 ~ agecat + microsim.init.sex_2003 +
                  microsim.init.race_2003 + microsim.init.education_2003 +
                  microsim.init.alc.gpd_2003 + abstainer | 
                  agecat + microsim.init.sex_2003 +
                  microsim.init.race_2003 + microsim.init.education_2003 +
                  microsim.init.alc.gpd_2003 + abstainer,
                data = Pop,
                # weights = scaledweight1,
                dist = "poisson")
summary(m1)


# Extract coefficients
zero_part_coef <- coef(m1, model = "zero")
count_part_coef <- coef(m1, model = "count")

# Extract standard errors for the zero part
coefSE <- rbind(c("PE","zero",zero_part_coef),
                c("PE","count", count_part_coef),
                c("SE","zero", summary(m1)$coefficients$zero[, "Std. Error"]),
                c("SE","count", summary(m1)$coefficients$count[, "Std. Error"]))
coefSE <- data.frame(coefSE)
names(coefSE)[1:2] <- c("estimate","type")
# coefSE <- coefSE[,-ncol(coefSE)] 

# save a copy of the coefficients 
write.csv(coefSE, paste0(models, "regression_NESARC.csv"), row.names=F)

# Display the coefficients
print("Zero part coefficients (logistic regression):")
print(zero_part_coef)

print("Count part coefficients (Negative Binomial regression):")
print(count_part_coef)

# dummy code and set up data for prediction
data_prediction <- nesarc_selected %>% 
  dplyr::select(age3_2, female.factor_2,
                  race.factor_2, edu3_2, alc_rounded_1, abstainer_1,
                cat1_1, cat2_1, cat3_1, formerdrinker_1, medorhigh) %>%
  mutate(Women = ifelse(female.factor_2=="Women", 1,0),
         age2564 = ifelse(age3_2=="25-64", 1,0),
         age65 = ifelse(age3_2=="65+", 1,0),
         raceblack = ifelse(race.factor_2=="Black, non-Hispanic",1,0),
         racehispanic = ifelse(race.factor_2=="Hispanic",1,0),
         raceother = ifelse(race.factor_2=="Other, non-Hispanic",1,0),
         edulow = ifelse(edu3_2=="Low", 1,0),
         edumed = ifelse(edu3_2=="Med", 1,0),
         age25abstainer = ifelse(abstainer_1==1 & age2564==1, 1,0),
         age65abstainer = ifelse(abstainer_1==1 & age65==1, 1,0),
         medorhighgpd = medorhigh*alc_rounded_1)
         
# calculate zero part regression 
zero_part_lp <- zero_part_coef['(Intercept)'] +
  zero_part_coef['age3_225-64'] * data_prediction$age2564 + 
  zero_part_coef['age3_265+'] * data_prediction$age65 + 
  zero_part_coef['female.factor_2Women'] * data_prediction$Women + 
  zero_part_coef['race.factor_2Black, non-Hispanic'] * data_prediction$raceblack + 
  zero_part_coef['race.factor_2Hispanic'] * data_prediction$racehispanic + 
  zero_part_coef['race.factor_2Other, non-Hispanic'] * data_prediction$raceother + 
  zero_part_coef['edu3_2Low'] * data_prediction$edulow + 
  zero_part_coef['edu3_2Med'] * data_prediction$edumed + 
  zero_part_coef['alc_rounded_1'] * data_prediction$alc_rounded_1 +
  zero_part_coef['abstainer_1'] * data_prediction$abstainer_1 +
  zero_part_coef['age3_225-64:abstainer_1'] * data_prediction$age25abstainer +
  zero_part_coef['age3_265+:abstainer_1'] * data_prediction$age65abstainer
  
# Compute the probability of zero using the logistic function
zero_prob <- 1 / (1+exp(-zero_part_lp))

# count part regression coefficients
count_part_lp <- count_part_coef['(Intercept)'] +
  count_part_coef['age3_225-64']*data_prediction$age2564 + 
  count_part_coef['age3_265+']*data_prediction$age65 + 
  count_part_coef['female.factor_2Women']*data_prediction$Women + 
  count_part_coef['race.factor_2Black, non-Hispanic']*data_prediction$raceblack + 
  count_part_coef['race.factor_2Hispanic']*data_prediction$racehispanic + 
  count_part_coef['race.factor_2Other, non-Hispanic']*data_prediction$raceother + 
  count_part_coef['edu3_2Low']*data_prediction$edulow + 
  count_part_coef['edu3_2Med']*data_prediction$edumed + 
  count_part_coef['alc_rounded_1']*data_prediction$alc_rounded_1 + 
  count_part_coef['abstainer_1'] * data_prediction$abstainer_1 +
  count_part_coef['age3_225-64:abstainer_1'] * data_prediction$age25abstainer +
  count_part_coef['age3_265+:abstainer_1'] * data_prediction$age65abstainer

# calculate expected counts
expected_counts <- exp(count_part_lp)
set.seed(123) # For reproducibility
# calculate the predicted values (manual method)
predicted_values_manual <- ifelse(runif(nrow(data_prediction)) < zero_prob, 0, expected_counts)

data_prediction$predicted_values <- predicted_values_manual  

# now use built in functions to compare predicted count and zeros
predicted_counts <- predict(m1, type = "count")
predicted_zeros <- predict(m1, type = "zero")

# Combine the predictions
set.seed(123) # For reproducibility
predicted_values_builtin <- ifelse(runif(nrow(data_prediction)) < predicted_zeros, 0, predicted_counts)

data_prediction$builtinpred <- predicted_values_builtin

# check that the built in prediction matches the manual (important when adding / removing covariates)
head(data_prediction$builtinpred==data_prediction$predicted_values)

# check how many abstainers remain abstainers 
# for this to work in the microsimulation - this needs to be a high prop
abstainers <- data_prediction %>% mutate(abstainer1=ifelse(alc_rounded_1==0, 1,0),
                                         abstainer2=ifelse(builtinpred==0,1,0)) %>% 
  group_by(abstainer1, abstainer2) %>% tally() %>% 
  ungroup() %>% group_by(abstainer1) %>% mutate(prop=n/sum(n))

abstainers

data_prediction$microsim.init.alc.gpd <- data_prediction$alc_rounded_1
data_prediction$microsim.init.sex = ifelse(data_prediction$Women==1, "f","m")
data_prediction$formerdrinker <- data_prediction$formerdrinker_1
data_prediction <- code_alcohol_categories(data_prediction)
data_prediction$oldcat <- data_prediction$AlcCAT
data_prediction$microsim.init.alc.gpd <- data_prediction$builtinpred
data_prediction <- code_alcohol_categories(data_prediction)

test <- data_prediction %>% group_by(oldcat, AlcCAT) %>% tally() %>% 
  ungroup() %>% group_by(oldcat) %>% mutate(prop=n/sum(n))

test
data_prediction %>% group_by(oldcat) %>% tally() %>% mutate(prop=n/sum(n))
data_prediction %>% group_by(AlcCAT) %>% tally() %>% mutate(prop=n/sum(n))

nesarc_selected$predicted <- data_prediction$builtinpred

histograms <- data_prediction %>% 
  dplyr::select(alc_rounded_1, builtinpred) %>% 
  mutate(id=1:nrow(.)) %>% 
  pivot_longer(alc_rounded_1:builtinpred)
ggplot(histograms, aes(x=value, colour=name, fill=name)) + 
  geom_histogram(aes(y=..density..), alpha=0.2) + 
  # geom_density() + 
  # geom_density() + 
  # facet_grid(cols=vars(name)) + 
  xlim(0,200)

cor.test(nesarc_selected$predicted, nesarc_selected$alc_rounded_2, method="spearman")

