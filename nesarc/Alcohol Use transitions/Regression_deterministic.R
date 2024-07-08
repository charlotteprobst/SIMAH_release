
# SIMAH - NESARC Alcohol Transitions
# Regression model estimation

# Load packages and data
library(tidyverse)  # data management
library(ZIM)

models  <- "~/Google Drive/SIMAH Sheffield/SIMAH_workplace/nesarc/Models/"          # Location of saved MSM models

# Load data / functions
deterministic_pop <- read_csv("/Users/charlottebuckley/Google Drive/SIMAH Sheffield/SIMAH_workplace/microsim/2_output_data/alcohol_calibration/education_reps/full_pop_deterministic10000_withID.csv") 

deterministic_selected <- deterministic_pop %>% 
  mutate(abstainer=ifelse(AlcCAT=="Non-drinker",1,0)) %>% 
  mutate(agecat = cut(microsim.init.age,
                      breaks=c(0,24,64,100),
                      labels=c("18-24","25-64","65+"))) %>% 
  dplyr::select(brfssID, microsim.init.id, year, agecat, 
                microsim.init.race, microsim.init.sex,
                microsim.init.education, microsim.init.alc.gpd,
                AlcCAT, abstainer) %>% 
  mutate(alc_rounded = ceiling(microsim.init.alc.gpd),
         alc_rounded = ifelse(alc_rounded>200, 200, alc_rounded)) %>% 
  arrange(microsim.init.id, year) %>% 
  group_by(microsim.init.id) %>% 
  mutate(lagged_alcohol = lag(microsim.init.alc.gpd, n = 1),
         lagged_abstainer = lag(abstainer, n=1)) %>% 
  ungroup() %>% 
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
         edu3_2 = ifelse(edu3_2 =="High" & age3_2=="18-24", "Med", edu3_2),
         race.factor_2 = factor(race.factor_2, levels=c("Black, non-Hispanic",
                                                        "Hispanic","Other, non-Hispanic",
                                                        "White, non-Hispanic")))
deterministic_selected$race.factor_2 <- relevel(deterministic_selected$race.factor_2,
                                                ref="White, non-Hispanic")

# now try Barbosa method for multinom log regs
deterministic_selected <- deterministic_selected %>% 
  group_by(microsim.init.id) %>% 
  arrange(microsim.init.id, year) %>% 
  #create a lagged variable for alcohol category
  mutate(lagged_cat = lag(AlcCAT),
         cat1_lag = ifelse(lagged_cat=="Low risk", 1,0),
         cat2_lag = ifelse(lagged_cat=="Medium risk", 1,0),
         cat3_lag = ifelse(lagged_cat=="High risk", 1,0),
         cat1 = ifelse(AlcCAT=="Low risk", 1,0),
         cat2 = ifelse(AlcCAT=="Medium risk", 1,0),
         cat3 = ifelse(AlcCAT=="High risk", 1,0),
         lagged_age = lag(age3_2),
         lagged_education = lag(edu3_2)) %>% ungroup()

deterministic_selected$AlcCAT <- relevel(factor(deterministic_selected$AlcCAT),
                                         ref='Non-drinker')

deterministic_selected$time <- deterministic_selected$year-2000

library(nnet)
deterministic_selected$AlcCAT <- factor(deterministic_selected$AlcCAT,
                                        levels=c("Non-drinker","Low risk",
                                                 "Medium risk","High risk"))
library(MASS)
fit <- polr(AlcCAT ~ cat1_lag + cat2_lag + cat3_lag +
              female.factor_2*lagged_age + female.factor_2*lagged_education + 
              female.factor_2*race.factor_2, 
            data=deterministic_selected, Hess=TRUE)
summary(fit)

TPmodel <- data.frame(summary(fit)$coefficients)
TPmodel$name <- rownames(TPmodel)
write.csv(TPmodel, paste0(models, "ordinal_model.csv"), row.names=F)


deterministic_selected$probs <- predict(fit, deterministic_selected,
                                        type="probs")

probabilities <- deterministic_selected %>% dplyr::select(female.factor_2, race.factor_2,
                               lagged_age, lagged_education, lagged_cat)
probabilities$`Non-drinker` <- deterministic_selected$probs[,1]
probabilities$`Low risk` <- deterministic_selected$probs[,2]
probabilities$`Medium risk` <- deterministic_selected$probs[,3]
probabilities$`High risk` <- deterministic_selected$probs[,4]
probabilities <- probabilities %>% drop_na() %>% 
  distinct() %>% pivot_longer(`Non-drinker`:`High risk`) %>% 
  rename(from=lagged_cat, to=name)

analysis <- probabilities %>% group_by(from, to) %>% 
  summarise(min = min(value), max=max(value))

ggplot(data=subset(probabilities, from==to), 
       aes(x=lagged_age, y=value, fill=race.factor_2)) + 
         geom_bar(stat="identity",position="dodge",colour="black") + 
  facet_grid(cols=vars(from), 
             rows=vars(lagged_education, female.factor_2))







fit <- multinom(AlcCAT ~ cat1_lag + cat2_lag + cat3_lag + alc_daily_g_1  + 
                  female.factor_2*lagged_age + female.factor_2*lagged_education + 
                  female.factor_2*race.factor_2, 
                data=deterministic_selected)

summary(fit)
TPmodel <- data.frame(coef(fit))
# TPmodel <- exp(TPmodel)
TPmodel$cat <- row.names(TPmodel)
write.csv(TPmodel, paste0(models, "multinom_model_race_cont.csv"), row.names=F)

# standard errors for TP model 
ses <- data.frame(summary(fit)$standard.errors)
ses$cat <- row.names(ses)
write.csv(ses, paste0(models, "multinom_model_ses_race_cont.csv"), row.names=F)

# now do categorical to continuous model for each category 


ggplot(data=subset(deterministic_selected, microsim.init.alc.gpd!=0), 
       aes(x=microsim.init.alc.gpd)) + 
  geom_histogram() + 
  facet_grid(rows=vars(AlcCAT), scales="free")

ggplot(data=subset(deterministic_selected, microsim.init.alc.gpd!=0), 
       aes(x=log(microsim.init.alc.gpd))) + 
  geom_histogram() + 
  facet_grid(rows=vars(AlcCAT), scales="free")


deterministic_selected$logalc <- log(deterministic_selected$microsim.init.alc.gpd)
lowrisk <- deterministic_selected %>% filter(AlcCAT=="Low risk")
mediumrisk <- deterministic_selected %>% filter(AlcCAT=="Medium risk")
highrisk <- deterministic_selected %>% filter(AlcCAT=="High risk")

lr_model <- lm(logalc ~ alc_daily_g_1 + cat1_lag + cat2_lag + cat3_lag + 
                 female.factor_2*alc_daily_g_1 + 
                 lagged_education + 
                 lagged_age + race.factor_2, data = lowrisk)

summary(lr_model)
lowrisk$predicted <- exp(predict(lr_model, lowrisk))

lowrisk$predicted <- ifelse(lowrisk$female.factor_2=="Men" & lowrisk$predicted>40, 40,
                            ifelse(lowrisk$female.factor_2=="Women" & lowrisk$predicted>20, 20, lowrisk$predicted))


mr_model <- lm(logalc ~ alc_daily_g_1 + cat1_lag + cat2_lag + 
                 female.factor_2*alc_daily_g_1 + 
                 lagged_education + 
                 lagged_age + race.factor_2, data = mediumrisk)

summary(mr_model)
mediumrisk$predicted <- exp(predict(mr_model, mediumrisk))

mediumrisk$predicted <- ifelse(mediumrisk$female.factor_2=="Men" & mediumrisk$predicted<=40, 40,
                               ifelse(mediumrisk$female.factor_2=="Men" & mediumrisk$predicted>60, 60,
                                      ifelse(mediumrisk$female.factor_2=="Women" & mediumrisk$predicted<=20, 20,
                                             ifelse(mediumrisk$female.factor_2=="Women" & mediumrisk$predicted>60, 60,
                                                    mediumrisk$predicted))))
                              
hr_model <- lm(logalc ~ alc_daily_g_1 + cat1_lag + cat2_lag + 
                 female.factor_2*alc_daily_g_1 + 
                 lagged_education + 
                 lagged_age + race.factor_2, data = highrisk)

summary(hr_model)
highrisk$predicted <- exp(predict(hr_model, highrisk))

highrisk$predicted <- ifelse(highrisk$female.factor_2=="Men" & highrisk$predicted<=60, 60,
                               ifelse(highrisk$female.factor_2=="Men" & highrisk$predicted>200, 200,
                                      ifelse(highrisk$female.factor_2=="Women" & highrisk$predicted<=40, 40,
                                             ifelse(highrisk$female.factor_2=="Women" & highrisk$predicted>200, 200,
                                                    highrisk$predicted))))

predicted <- rbind(lowrisk, mediumrisk, highrisk) %>% 
  dplyr::select(AlcCAT, female.factor_2, lagged_age, lagged_education, 
                race.factor_2, microsim.init.alc.gpd,predicted) %>% 
  pivot_longer(microsim.init.alc.gpd:predicted) %>% 
  mutate(AlcCAT=factor(AlcCAT, levels=c("Low risk","Medium risk","High risk")),
         name = ifelse(name=="microsim.init.alc.gpd","observed",name)) %>% drop_na()

ggplot(data=predicted, 
       aes(x=value, fill=name)) + 
  geom_histogram(colour="black") +
  facet_grid(cols=vars(AlcCAT), rows=vars(female.factor_2, lagged_age), scales="free") +
  theme_bw() + 
  theme(legend.title=element_blank(), legend.position="bottom")

ggplot(data=predicted, 
       aes(x=value, fill=name)) + 
  geom_density(aes(y=..scaled..)) +
  facet_grid(cols=vars(AlcCAT), rows=vars(female.factor_2, lagged_education), scales="free") +
  theme_bw() + 
  theme(legend.title=element_blank(), legend.position="bottom")
ggsave(paste0(models, "plots/predicted_educ_sex.png"), dpi=300, width=33, height=19,
       units="cm")

# extract the model coefficients
lr_coefs <- data.frame(coef(lr_model))
lr_coefs$cat <- "Low risk"
names(lr_coefs)[1] <- "coef"
lr_coefs$variable <- row.names(lr_coefs)

mr_coefs <- data.frame(coef(mr_model))
mr_coefs$cat <- "Medium risk"
names(mr_coefs)[1] <- "coef"
mr_coefs$variable <- row.names(mr_coefs)

hr_coefs <- data.frame(coef(hr_model))
hr_coefs$cat <- "High risk"
names(hr_coefs)[1] <- "coef"
hr_coefs$variable <- row.names(hr_coefs)

coefs <- rbind(lr_coefs, mr_coefs, hr_coefs) %>% 
  pivot_wider(names_from=variable, values_from=coef)
write.csv(coefs, paste0(models, "cat_cont_model.csv"), row.names=F)


# library(GLMMadaptive)
# # deterministic_selected <- deterministic_selected %>% filter(year>=2001)
# deterministic_selected$time <- deterministic_selected$year-2000
# summary(deterministic_selected$time)
# deterministic_selected$strata <- paste(deterministic_selected$lagged_age,
#                                         deterministic_selected$female.factor_2,
#                                         deterministic_selected$race.factor_2,
#                                         deterministic_selected$lagged_education, sep="_")
# 
# # model for abstainers
# 
# library(lme4)
# 
# abstainermod <- glmer(abstainer ~ time + abstainer_1 + cat1_lag + 
#                         + (1|strata), family=binomial, 
#                       data=deterministic_selected) 
# 
# summary(abstainermod)
# amodel <- coef(abstainermod)[[1]]
# amodel$strata <- row.names(amodel)
# amodel <- amodel %>% pivot_longer("(Intercept)":cat1_lag) %>% 
#   rename(parameter=name, Estimate=value) %>% 
#   mutate(type="abstainermod")
# 
# # now do the drinker model 
# drinkers <- deterministic_selected %>% filter(microsim.init.alc.gpd!=0)
# drinkers <- drinkers %>% 
#   mutate(logalc = log(microsim.init.alc.gpd),
#          logalcscaled = as.numeric(scale(logalc)))
# 
# # drinker model
# 
# drinkermod <- lmer(logalc ~ 
#                      alc_daily_g_1*lagged_cat + 
#                      (1 |strata), 
#                    data=drinkers) 
# summary(drinkermod)
# coef(drinkermod)
# ranef(drinkermod)
# # drinkers$predicted <- exp(predict(drinkermod))
# # drinkers$error <- drinkers$predicted - drinkers$alc_daily_g_1
# # 
# # drinkers$microsim.init.alc.gpd <- drinkers$predicted
# # drinkers$formerdrinker <- 0
# # drinkers$oldcat <- drinkers$AlcCAT
# # drinkers$microsim.init.sex <- ifelse(drinkers$female.factor_2=="Women","f","m")
# # drinkers <- code_alcohol_categories(drinkers)
# 
# # test1 <- drinkers %>% group_by(oldcat,strata) %>% tally() %>% ungroup() %>% mutate(prop=n/sum(n)) %>% dplyr::select(-n) %>% 
# #   rename(AlcCAT=oldcat)
# # test2 <- drinkers %>% group_by(AlcCAT,strata) %>% tally() %>% ungroup() %>% mutate(prop_predicted=n/sum(n)) %>% dplyr::select(-n) 
# # test1 <- left_join(test1, test2) %>% mutate(error = abs(prop-prop_predicted))
# # summary(test1$error)
# 
# # drinkers$predictedunscaled <- (drinkers$predicted - mean(drinkers$logalc))*sd(drinkers$logalc)
# # drinkers$predictedunscaledexp <- exp(drinkers$predictedunscaled)
# library(lmerTest)
# summary(drinkermod)
# coef(summary(as(drinkermod,"merModLmerTest")))
# dmodel <- coef(drinkermod)[[1]]
# dmodel$strata <-row.names(dmodel)
# dmodel <- dmodel %>% pivot_longer("(Intercept)":`alc_daily_g_1:lagged_catMedium risk`) %>% 
#   rename(parameter=name, Estimate=value) %>% 
#   mutate(type="drinkermod")
# 
# model <- rbind(amodel,dmodel)
# 
# write.csv(model, paste0(models, "deterministic_mixedeff_stratari.csv"), row.names=F)
# 
# strata <- deterministic_selected %>% dplyr::select(microsim.init.id, strata) %>% 
#   rename(strata=microsim.init.id, cat=strata) %>% distinct()
# 
# 
# deterministic_selected$predicted <- predict(abstainermod,type="response")
# deterministic_selected$random <- runif(nrow(deterministic_selected))
# deterministic_selected$predicted_abstainer <- ifelse(deterministic_selected$random<=deterministic_selected$predicted,1,0)
# summary(deterministic_selected$abstainer)
# summary(deterministic_selected$predicted_abstainer)
# 
# test <- deterministic_selected %>% group_by(strata) %>% summarise(abstainer=mean(abstainer),
#                                                                   predicted = mean(predicted_abstainer),
#                                                                   error = abstainer-predicted)
# 
# 
# 
# 
# 
# 
# deterministic_selected <- deterministic_selected %>% drop_na()
# # trying to find which are the best predictors and interaction effects to include 
# full_model <- multinom(AlcCAT ~ cat1_lag + cat2_lag + cat3_lag + 
#                          lagged_age*female.factor_2 + 
#                          lagged_education*female.factor_2 + 
#                          lagged_education*lagged_age + 
#                          time*lagged_age + 
#                          time*lagged_education + 
#                          time*female.factor_2 + 
#                          time*cat1_lag + time*cat2_lag + time*cat3_lag +
#                          female.factor_2*cat1_lag + female.factor_2*cat2_lag + female.factor_2*cat3_lag +
#                          lagged_age + female.factor_2 + lagged_education + race.factor_2 + time, 
#                        data=deterministic_selected, na.action=na.fail)
# summary(full_model)
# 
# global_model <- full_model
# 
# # Generate a model selection table
# model_selection <- dredge(global_model)
# 
# # Display the model selection table
# print(model_selection)
# 
# # Get the best model based on AIC
# best_model <- get.models(model_selection, 1)[[1]]
# 
# exp(coef(fit))
# deterministic_selected$predicted <- predict(fit, deterministic_selected)
# 
# deterministic_selected$transition_probabilities <- predict(fit, deterministic_selected, type="probs")
# 
# tps <- deterministic_selected %>% filter(time!=0) %>% 
#   dplyr::select(strata, time, transition_probabilities) %>% distinct()
# 
# test <- deterministic_selected %>% 
#   group_by(year,age3_2, race.factor_2, female.factor_2, edu3_2, AlcCAT) %>% 
#   tally() %>% 
#   ungroup() %>% 
#   group_by(year, age3_2, race.factor_2, female.factor_2, edu3_2) %>% 
#   mutate(prop=n/sum(n)) %>% 
#   dplyr::select(-n)
# 
# test2 <- deterministic_selected %>% 
#   group_by(year,age3_2, race.factor_2, female.factor_2, edu3_2, predicted) %>% 
#   tally() %>% 
#   ungroup() %>% 
#   group_by(year, age3_2, race.factor_2, female.factor_2, edu3_2) %>% 
#   mutate(proppredicted=n/sum(n)) %>% 
#   rename(AlcCAT=predicted) %>% 
#   dplyr::select(-n)
# 
# test <- left_join(test, test2)
# 
# # predictors are previous alcohol use, all demographics and some interactions effects 
# # count model parameters | zero model parameters 
# library(lme4)
# 
# deterministic_formodel <- deterministic_selected %>% 
#   dplyr::select(year, abstainer, age3_2, race.factor_2, female.factor_2,
#                 edu3_2, alc_daily_g_1, lagged_cat)
# 
# fsa.abstainer <- FSA(abstainer ~ lagged_cat + age3_2 + female.factor_2 + 
#                        race.factor_2 + edu3_2, data=deterministic_formodel,
#                      fitfunc=glm, quad=FALSE, m=2, numrs=10,
#                      interactions=T)
# summary(fsa.abstainer)
# 
# drinkers <- deterministic_selected %>% filter(microsim.init.alc.gpd!=0) %>% 
#   dplyr::select(year, microsim.init.alc.gpd, age3_2, race.factor_2, female.factor_2,
#                 edu3_2, alc_daily_g_1, lagged_cat, alc_daily_g_1) %>% 
#   mutate(logalc = log(microsim.init.alc.gpd))
# 
# fsa.drinker <- FSA(logalc ~ lagged_cat + alc_daily_g_1 + age3_2 + female.factor_2 + 
#                        race.factor_2 + edu3_2, data=drinkers,
#                      fitfunc=lm, quad=FALSE, m=2, numrs=10,
#                      interactions=T)
# summary(fsa.drinker)
# 
# 
# abstainermod <- glmer(abstainer ~ age3_2 + female.factor_2 +
#                         race.factor_2 + edu3_2 + 
#                         abstainer_1 + alc_daily_g_1 + (1|year),
#                       family=binomial,
#                       data=deterministic_selected) 
# summary(abstainermod)
# coef(abstainermod)
# saveRDS(abstainermod, paste0(models, "abstainermod_yearrandom.RDS"))
# 
# 
# 
# coef(abstainermod)
# 
# abstainermod <- glmer(abstainer ~ age3_2 + female.factor_2 +
#                         race.factor_2 + edu3_2 + 
#                         alc_daily_g_1 + (1|lagged_cat),
#                       family=binomial,
#                       data=deterministic_selected) 
# summary(abstainermod)
# saveRDS(abstainermod, paste0(models, "abstainermod_alccatrandom.RDS"))
# coef(abstainermod)
#   
# drinkers <- deterministic_selected %>% filter(microsim.init.alc.gpd!=0)
# drinkers <- drinkers %>% 
#   mutate(logalc = log(microsim.init.alc.gpd),
#          logalcscaled = as.numeric(scale(logalc)))
# 
# drinkermod <- lmer(logalc ~ age3_2 + female.factor_2 +
#                    race.factor_2 + edu3_2 + 
#                    alc_daily_g_1 + abstainer_1 + (1|year),
#                  data=drinkers) 
# saveRDS(drinkermod, paste0(models, "drinkermod_yearrandom.RDS"))
# 
# drinkermod <- lmer(logalc ~ age3_2 + female.factor_2 +
#                      race.factor_2 + edu3_2 + 
#                      alc_daily_g_1 + (1|lagged_cat),
#                    data=drinkers) 
# saveRDS(drinkermod, paste0(models, "drinkermod_alccatrandom.RDS"))
# 
# abstainermod <- read_rds(paste0(models, "abstainermod_yearrandom.RDS"))
# drinkermod <- read_rds(paste0(models, "drinkermod_yearrandom.RDS"))
# 
# coefsabstainer <- (coef(abstainermod))[[1]] %>% 
#   mutate(year = rownames(.)) %>% 
#   pivot_longer(`(Intercept)`:alc_daily_g_1) %>% 
#   pivot_wider(names_from=name, values_from=value) %>% 
#   mutate(type="abstainermod")
# 
# coefsdrinker <- (coef(drinkermod))[[1]] %>% 
#   mutate(year = rownames(.)) %>% 
#   pivot_longer(`(Intercept)`:alc_daily_g_1) %>% 
#   pivot_wider(names_from=name, values_from=value) %>% 
#   mutate(type="drinkermod")
# 
# coefs <- rbind(coefsabstainer, coefsdrinker)
# write.csv(coefs, paste0(models, "regression_yearrandom_deterministicpop.csv"), row.names=F)
# 
# coef(drinkermod)
# library(pscl)
# m1 <-  zeroinfl(alc_rounded ~ lagged_age + female.factor_2 + 
#                   race.factor_2 + lagged_education + alc_daily_g_1 +
#                   cat1_lag + cat2_lag*alc_daily_g_1 + 
#                   cat3_lag*alc_daily_g_1 + time |
#                   lagged_age + female.factor_2 + 
#                   race.factor_2 + lagged_education + abstainer_1 + 
#                   cat1_lag + cat2_lag + time,
#                 data = deterministic_selected,
#                 dist = "poisson")
# summary(m1)
# 
# zero_part_coef <- data.frame(type="abstainermod", 
#                              parameter=names(coef(m1, model="zero")),
#                              Estimate=summary(m1)$coefficients$zero[, "Estimate"],
#                              Std..Error=summary(m1)$coefficients$zero[, "Std. Error"])
# count_part_coef <- data.frame(type="drinkermod", 
#                               parameter=names(summary(m1)$coefficients$count[, "Estimate"]),
#                               Estimate=summary(m1)$coefficients$count[, "Estimate"],
#                               Std..Error=summary(m1)$coefficients$count[, "Std. Error"])
# coefSE <- rbind(zero_part_coef, count_part_coef)
# 
# write.csv(coefSE, paste0(models, "zeroinfl_regression_poisson_deterministic.csv"), row.names=F)
# 
# 
# 
# 
# 
# 
# drinkers <- deterministic_selected %>% filter(microsim.init.alc.gpd!=0)
# drinkers <- drinkers %>% 
#   mutate(logalc = log(microsim.init.alc.gpd),
#     logalcscaled = as.numeric(scale(logalc)))
# 
# drinkermod <- lm(logalc ~ age3_2 + female.factor_2 +
#                    race.factor_2 + edu3_2 + 
#                    alc_daily_g_1 + abstainer_1,
#                    data=drinkers) 
# 
# summary(drinkermod)
# 
# summaryabstainer <- summary(abstainermod)
# 
# coefabstainers <- data.frame(summaryabstainer$coefficients) %>% 
#   mutate(type="abstainermod",
#          parameter = rownames(.)) %>% 
#   dplyr::select(type, parameter, Estimate, Std..Error, Pr...z..) %>% 
#   rename(p = Pr...z..)
# 
# summarydrinker <- summary(drinkermod)
# coefdrinkers <- data.frame(summarydrinker$coefficients) %>% 
#   mutate(type="drinkermod",
#          parameter = rownames(.)) %>% 
#   dplyr::select(type, parameter, Estimate, Std..Error, Pr...t..) %>% 
#   rename(p = Pr...t..)
# 
# rownames(coefabstainers) <- NULL
# rownames(coefdrinkers) <- NULL
# 
# coefSE <- rbind(coefabstainers, coefdrinkers)
# 
# # save a copy of the coefficients 
# write.csv(coefSE, paste0(models, "regression_deterministicpop.csv"), row.names=F)
# 
# 
# m1 <-  zeroinfl(alc_rounded ~ agecat + microsim.init.sex +
#                   microsim.init.race + microsim.init.education + 
#                   lagged_alcohol + lagged_abstainer | 
#                   agecat + microsim.init.sex +
#                   microsim.init.race + microsim.init.education + 
#                   lagged_alcohol + lagged_abstainer,
#                 data = deterministic_selected,
#                 dist = "poisson")
# summary(m1)
# 
# # mixed <- mixed_model(fixed = alc_rounded_2 ~ age3_2 + female.factor_2 + race.factor_2 + edu3_2 +
# #                        alc_rounded_1 + abstainer_1, 
# #                      random= ~ 1 | idnum, 
# #                      data=nesarc_selected,
# #                      family=zi.poisson,
# #                      # weights=scaledweight1, 
# #                      zi_fixed = ~ age3_2 + female.factor_2 + race.factor_2 + edu3_2 +
# #                        alc_rounded_1 + abstainer_1)
# # summary(mixed)
# 
# coef(mixed, model="zero")
# coef(mixed, model="count")
# 
# 
# # Extract coefficients
# zero_part_coef <- data.frame(type="abstainermod", 
#                              parameter=names(coef(m1, model="zero")),
#                              Estimate=summary(m1)$coefficients$zero[, "Estimate"],
#                              Std..Error=summary(m1)$coefficients$zero[, "Std. Error"])
# count_part_coef <- data.frame(type="drinkermod", 
#                              parameter=names(summary(m1)$coefficients$count[, "Estimate"]),
#                              Estimate=summary(m1)$coefficients$count[, "Estimate"],
#                              Std..Error=summary(m1)$coefficients$count[, "Std. Error"])
# coefSE <- rbind(zero_part_coef, count_part_coef)
# 
# write.csv(coefSE, paste0(models, "zeroinfl_regression_negbin_NESARC.csv"), row.names=F)
# 
# 
# # Extract standard errors for the zero part
# coefSE <- rbind(c("PE","zero",zero_part_coef),
#                 c("PE","count", count_part_coef),
#                 c("SE","zero", summary(m1)$coefficients$zero[, "Std. Error"]),
#                 c("SE","count", summary(m1)$coefficients$count[, "Std. Error"]))
# coefSE <- data.frame(coefSE)
# names(coefSE)[1:2] <- c("estimate","type")
# # coefSE <- coefSE[,-ncol(coefSE)] 
# 
# # save a copy of the coefficients 
# 
# # Display the coefficients
# print("Zero part coefficients (logistic regression):")
# print(zero_part_coef)
# 
# print("Count part coefficients (Negative Binomial regression):")
# print(count_part_coef)
# 
# # dummy code and set up data for prediction
# data_prediction <- nesarc_selected %>% 
#   dplyr::select(age3_2, female.factor_2,
#                   race.factor_2, edu3_2, alc_rounded_1, abstainer_1,
#                 cat1_1, cat2_1, cat3_1, formerdrinker_1, medorhigh) %>%
#   mutate(Women = ifelse(female.factor_2=="Women", 1,0),
#          age2564 = ifelse(age3_2=="25-64", 1,0),
#          age65 = ifelse(age3_2=="65+", 1,0),
#          raceblack = ifelse(race.factor_2=="Black, non-Hispanic",1,0),
#          racehispanic = ifelse(race.factor_2=="Hispanic",1,0),
#          raceother = ifelse(race.factor_2=="Other, non-Hispanic",1,0),
#          edulow = ifelse(edu3_2=="Low", 1,0),
#          edumed = ifelse(edu3_2=="Med", 1,0),
#          age25abstainer = ifelse(abstainer_1==1 & age2564==1, 1,0),
#          age65abstainer = ifelse(abstainer_1==1 & age65==1, 1,0),
#          medorhighgpd = medorhigh*alc_rounded_1)
#          
# # calculate zero part regression 
# zero_part_lp <- zero_part_coef['(Intercept)'] +
#   zero_part_coef['age3_225-64'] * data_prediction$age2564 + 
#   zero_part_coef['age3_265+'] * data_prediction$age65 + 
#   zero_part_coef['female.factor_2Women'] * data_prediction$Women + 
#   zero_part_coef['race.factor_2Black, non-Hispanic'] * data_prediction$raceblack + 
#   zero_part_coef['race.factor_2Hispanic'] * data_prediction$racehispanic + 
#   zero_part_coef['race.factor_2Other, non-Hispanic'] * data_prediction$raceother + 
#   zero_part_coef['edu3_2Low'] * data_prediction$edulow + 
#   zero_part_coef['edu3_2Med'] * data_prediction$edumed + 
#   zero_part_coef['alc_rounded_1'] * data_prediction$alc_rounded_1 +
#   zero_part_coef['abstainer_1'] * data_prediction$abstainer_1 +
#   zero_part_coef['age3_225-64:abstainer_1'] * data_prediction$age25abstainer +
#   zero_part_coef['age3_265+:abstainer_1'] * data_prediction$age65abstainer
#   
# # Compute the probability of zero using the logistic function
# zero_prob <- 1 / (1+exp(-zero_part_lp))
# 
# # count part regression coefficients
# count_part_lp <- count_part_coef['(Intercept)'] +
#   count_part_coef['age3_225-64']*data_prediction$age2564 + 
#   count_part_coef['age3_265+']*data_prediction$age65 + 
#   count_part_coef['female.factor_2Women']*data_prediction$Women + 
#   count_part_coef['race.factor_2Black, non-Hispanic']*data_prediction$raceblack + 
#   count_part_coef['race.factor_2Hispanic']*data_prediction$racehispanic + 
#   count_part_coef['race.factor_2Other, non-Hispanic']*data_prediction$raceother + 
#   count_part_coef['edu3_2Low']*data_prediction$edulow + 
#   count_part_coef['edu3_2Med']*data_prediction$edumed + 
#   count_part_coef['alc_rounded_1']*data_prediction$alc_rounded_1 + 
#   count_part_coef['abstainer_1'] * data_prediction$abstainer_1 +
#   count_part_coef['age3_225-64:abstainer_1'] * data_prediction$age25abstainer +
#   count_part_coef['age3_265+:abstainer_1'] * data_prediction$age65abstainer
# 
# # calculate expected counts
# expected_counts <- exp(count_part_lp)
# set.seed(123) # For reproducibility
# # calculate the predicted values (manual method)
# predicted_values_manual <- ifelse(runif(nrow(data_prediction)) < zero_prob, 0, expected_counts)
# 
# data_prediction$predicted_values <- predicted_values_manual  
# 
# # now use built in functions to compare predicted count and zeros
# predicted_counts <- predict(m1, type = "count")
# predicted_zeros <- predict(m1, type = "zero")
# 
# # Combine the predictions
# set.seed(123) # For reproducibility
# predicted_values_builtin <- ifelse(runif(nrow(data_prediction)) < predicted_zeros, 0, predicted_counts)
# 
# data_prediction$builtinpred <- predicted_values_builtin
# 
# # check that the built in prediction matches the manual (important when adding / removing covariates)
# head(data_prediction$builtinpred==data_prediction$predicted_values)
# 
# # check how many abstainers remain abstainers 
# # for this to work in the microsimulation - this needs to be a high prop
# abstainers <- data_prediction %>% mutate(abstainer1=ifelse(alc_rounded_1==0, 1,0),
#                                          abstainer2=ifelse(builtinpred==0,1,0)) %>% 
#   group_by(abstainer1, abstainer2) %>% tally() %>% 
#   ungroup() %>% group_by(abstainer1) %>% mutate(prop=n/sum(n))
# 
# abstainers
# 
# data_prediction$microsim.init.alc.gpd <- data_prediction$alc_rounded_1
# data_prediction$microsim.init.sex = ifelse(data_prediction$Women==1, "f","m")
# data_prediction$formerdrinker <- data_prediction$formerdrinker_1
# data_prediction <- code_alcohol_categories(data_prediction)
# data_prediction$oldcat <- data_prediction$AlcCAT
# data_prediction$microsim.init.alc.gpd <- data_prediction$builtinpred
# data_prediction <- code_alcohol_categories(data_prediction)
# 
# test <- data_prediction %>% group_by(oldcat, AlcCAT) %>% tally() %>% 
#   ungroup() %>% group_by(oldcat) %>% mutate(prop=n/sum(n))
# 
# test
# data_prediction %>% group_by(oldcat) %>% tally() %>% mutate(prop=n/sum(n))
# data_prediction %>% group_by(AlcCAT) %>% tally() %>% mutate(prop=n/sum(n))
# 
# nesarc_selected$predicted <- data_prediction$builtinpred
# 
# histograms <- data_prediction %>% 
#   dplyr::select(alc_rounded_1, builtinpred) %>% 
#   mutate(id=1:nrow(.)) %>% 
#   pivot_longer(alc_rounded_1:builtinpred)
# ggplot(histograms, aes(x=value, colour=name, fill=name)) + 
#   geom_histogram(aes(y=..density..), alpha=0.2) + 
#   # geom_density() + 
#   # geom_density() + 
#   # facet_grid(cols=vars(name)) + 
#   xlim(0,200)
# 
# cor.test(nesarc_selected$predicted, nesarc_selected$alc_rounded_2, method="spearman")

