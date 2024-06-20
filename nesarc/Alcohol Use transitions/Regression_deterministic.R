
# SIMAH - NESARC Alcohol Transitions
# Regression model estimation

# Load packages and data
library(tidyverse)  # data management
library(ZIM)

# Load data / functions
deterministic_pop <- read_csv("/Users/charlottebuckley/Google Drive/SIMAH Sheffield/SIMAH_workplace/microsim/2_output_data/alcohol_calibration/NESARC_optimisation_output/full_pop_deterministic.csv") 

deterministic_selected <- deterministic_pop %>% 
  mutate(abstainer=ifelse(AlcCAT=="Non-drinker",1,0)) %>% 
  dplyr::select(microsim.init.id, year, agecat, 
                microsim.init.race, microsim.init.sex,
                microsim.init.education, microsim.init.alc.gpd,
                AlcCAT, abstainer) %>% 
  mutate(alc_rounded = ceiling(microsim.init.alc.gpd),
         alc_rounded = ifelse(alc_rounded>200, 200, alc_rounded)) %>% 
  arrange(microsim.init.id, year) %>% 
  mutate(lagged_alcohol = lag(microsim.init.alc.gpd, n = 1),
         lagged_abstainer = lag(abstainer, n=1)) %>% 
  rename(age3_2 = agecat,
         female.factor_2=microsim.init.sex,
         race.factor_2=microsim.init.race,
         edu3_2 = microsim.init.education,
         alc_daily_g_1 = lagged_alcohol,
         abstainer_1 = lagged_abstainer) %>% 
  mutate(female.factor_2=ifelse(female.factor_2=="m","Men","Women"),
         race.factor_2 = ifelse(race.factor_2=="BLA","Black, non-Hispanic",
                                ifelse(race.factor_2=="WHI","White, non-Hispanic",
                                       ifelse(race.factor_2=="OTH","Other, non-Hispanic",
                                              ifelse(race.factor_2=="SPA","Hispanic", NA)))),
         edu3_2 = ifelse(edu3_2=="LEHS","Low",
                         ifelse(edu3_2=="SomeC","Med","High")),
         race.factor_2 = factor(race.factor_2, levels=c("Black, non-Hispanic",
                                                        "Hispanic","Other, non-Hispanic",
                                                        "White, non-Hispanic")))
deterministic_selected$race.factor_2 <- relevel(deterministic_selected$race.factor_2,
                                                ref="White, non-Hispanic")

m1 <-  zeroinfl(alc_rounded ~ age3_2 + female.factor_2 +
                  race.factor_2 + edu3_2 + alc_daily_g_1 + abstainer_1 | 
                  age3_2 + female.factor_2 +
                  race.factor_2 + edu3_2 + alc_daily_g_1 + abstainer_1,
                data = deterministic_selected,
                dist = "poisson")
summary(m1)

zero_part_coef <- data.frame(type="abstainermod", 
                             parameter=names(coef(m1, model="zero")),
                             Estimate=summary(m1)$coefficients$zero[, "Estimate"],
                             Std..Error=summary(m1)$coefficients$zero[, "Std. Error"])
count_part_coef <- data.frame(type="drinkermod", 
                              parameter=names(summary(m1)$coefficients$count[, "Estimate"]),
                              Estimate=summary(m1)$coefficients$count[, "Estimate"],
                              Std..Error=summary(m1)$coefficients$count[, "Std. Error"])
coefSE <- rbind(zero_part_coef, count_part_coef)

write.csv(coefSE, paste0(models, "zeroinfl_regression_poisson_BRFSS.csv"), row.names=F)

# now try Barbosa method for multinom log regs
deterministic_selected <- deterministic_selected %>% 
  arrange(microsim.init.id, year) %>% 
  #create a lagged variable for alcohol category
  mutate(lagged_cat = lag(AlcCAT),
         cat1_lag = ifelse(lagged_cat=="Low risk", 1,0),
         cat2_lag = ifelse(lagged_cat=="Medium risk", 1,0),
         cat3_lag = ifelse(lagged_cat=="High risk", 1,0))

deterministic_selected$AlcCAT <- relevel(factor(deterministic_selected$AlcCAT),
                                         ref='Non-drinker')

fit <- multinom(AlcCAT ~ cat1_lag + cat2_lag + cat3_lag + age3_2 + 
                  female.factor_2 + race.factor_2 + edu3_2, 
                data=deterministic_selected)

summary(fit)
coef(fit)


library(GLMMadaptive)

# do a zero inflated lagged model 
mixed <- mixed_model(fixed = alc_rounded ~ age3_2 + female.factor_2 +
                       race.factor_2 + edu3_2 + 
                       alc_daily_g_1 + abstainer_1, 
                     random= ~ 1 | microsim.init.id, 
                     data=deterministic_selected,
                     family=zi.poisson(),
                     zi_fixed = ~ age3_2 + female.factor_2 +
                       race.factor_2 + edu3_2 + 
                       alc_daily_g_1 + abstainer_1, 
                     control = list(max_coef_value = 500, iter_EM = 0))



# predictors are previous alcohol use, all demographics and some interactions effects 
# count model parameters | zero model parameters 
abstainermod <- glmer(abstainer ~ age3_2 + female.factor_2 +
                      race.factor_2 + edu3_2 + 
                      alc_daily_g_1 + abstainer_1 + (1|year),
                    family=binomial,
                    data=deterministic_selected) 

summary(abstainermod)

drinkers <- deterministic_selected %>% filter(microsim.init.alc.gpd!=0)
drinkers <- drinkers %>% 
  mutate(logalc = log(microsim.init.alc.gpd),
    logalcscaled = as.numeric(scale(logalc)))

drinkermod <- lm(logalc ~ age3_2 + female.factor_2 +
                   race.factor_2 + edu3_2 + 
                   alc_daily_g_1 + abstainer_1,
                   data=drinkers) 

summary(drinkermod)

summaryabstainer <- summary(abstainermod)

coefabstainers <- data.frame(summaryabstainer$coefficients) %>% 
  mutate(type="abstainermod",
         parameter = rownames(.)) %>% 
  dplyr::select(type, parameter, Estimate, Std..Error, Pr...z..) %>% 
  rename(p = Pr...z..)

summarydrinker <- summary(drinkermod)
coefdrinkers <- data.frame(summarydrinker$coefficients) %>% 
  mutate(type="drinkermod",
         parameter = rownames(.)) %>% 
  dplyr::select(type, parameter, Estimate, Std..Error, Pr...t..) %>% 
  rename(p = Pr...t..)

rownames(coefabstainers) <- NULL
rownames(coefdrinkers) <- NULL

coefSE <- rbind(coefabstainers, coefdrinkers)

# save a copy of the coefficients 
write.csv(coefSE, paste0(models, "regression_deterministicpop.csv"), row.names=F)


m1 <-  zeroinfl(alc_rounded ~ agecat + microsim.init.sex +
                  microsim.init.race + microsim.init.education + 
                  lagged_alcohol + lagged_abstainer | 
                  agecat + microsim.init.sex +
                  microsim.init.race + microsim.init.education + 
                  lagged_alcohol + lagged_abstainer,
                data = deterministic_selected,
                dist = "poisson")
summary(m1)

# mixed <- mixed_model(fixed = alc_rounded_2 ~ age3_2 + female.factor_2 + race.factor_2 + edu3_2 +
#                        alc_rounded_1 + abstainer_1, 
#                      random= ~ 1 | idnum, 
#                      data=nesarc_selected,
#                      family=zi.poisson,
#                      # weights=scaledweight1, 
#                      zi_fixed = ~ age3_2 + female.factor_2 + race.factor_2 + edu3_2 +
#                        alc_rounded_1 + abstainer_1)
# summary(mixed)

coef(mixed, model="zero")
coef(mixed, model="count")


# Extract coefficients
zero_part_coef <- data.frame(type="abstainermod", 
                             parameter=names(coef(m1, model="zero")),
                             Estimate=summary(m1)$coefficients$zero[, "Estimate"],
                             Std..Error=summary(m1)$coefficients$zero[, "Std. Error"])
count_part_coef <- data.frame(type="drinkermod", 
                             parameter=names(summary(m1)$coefficients$count[, "Estimate"]),
                             Estimate=summary(m1)$coefficients$count[, "Estimate"],
                             Std..Error=summary(m1)$coefficients$count[, "Std. Error"])
coefSE <- rbind(zero_part_coef, count_part_coef)

write.csv(coefSE, paste0(models, "zeroinfl_regression_negbin_NESARC.csv"), row.names=F)


# Extract standard errors for the zero part
coefSE <- rbind(c("PE","zero",zero_part_coef),
                c("PE","count", count_part_coef),
                c("SE","zero", summary(m1)$coefficients$zero[, "Std. Error"]),
                c("SE","count", summary(m1)$coefficients$count[, "Std. Error"]))
coefSE <- data.frame(coefSE)
names(coefSE)[1:2] <- c("estimate","type")
# coefSE <- coefSE[,-ncol(coefSE)] 

# save a copy of the coefficients 

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

