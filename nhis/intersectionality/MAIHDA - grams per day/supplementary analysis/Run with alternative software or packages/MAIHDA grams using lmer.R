setwd("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_code/nhis/intersectionality/")

transformed_drinkers <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/cleaned_data/log_transformed_drinkers.RDS")

# Load necessary packages
library(lme4)
library(tidyr)
library(dplyr)
library(sjPlot)
library(merTools)
library(ggplot2)

# Bias toward non-scientific notation
options(scipen=10)

# Create a reference table of the intersectional groups - numbers and names
data_intersections_MAIHDA <- transformed_drinkers %>% 
  group_by(SEX, race_5_cats, education_3_cats, age_3_cats, decade) %>% 
  mutate(intersections = cur_group_id()) %>%
  group_by(intersections) %>%
  mutate(count=n())

temp <- data_intersections_MAIHDA %>%
  mutate(sex = dplyr::recode(SEX, Male = "M", Female = "F"),
         race_5_cats = dplyr::recode(race_5_cats, 
                                     "Black/African American" = "Black", 
                                     "Hispanic, White" = "Hispanic"),
         education_3_cats = dplyr::recode(education_3_cats, 
                                          "high school or less" = "low edu.", 
                                          "some college" = "med. edu.", 
                                          "4+ years college" = "high edu."))

temp2 <- temp %>% 
  mutate(intersectional_names = as.character(paste(sex, age_3_cats, race_5_cats, education_3_cats, decade)))

MAIHDA_intersections_reference <- distinct(temp2, intersections, .keep_all = TRUE) %>% dplyr::select(intersections, intersectional_names)


# Run the null two-level model and calculate the variance partition coefficient (VPC)
MAIHDA_null <- lmer(capped_daily_grams_log ~ 1 + (1 | intersections),
                       data = data_intersections_MAIHDA,
                       control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
performance::icc(MAIHDA_null)

#  Add in main effects one at a time & recalculate VPC to see the contribution of each to the overall variance

MAIHDA_sex <- lmer(capped_daily_grams_log ~ 1 + SEX + (1 | intersections),
                       data = data_intersections_MAIHDA,
                       control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
performance::icc(MAIHDA_sex)

MAIHDA_age <- lmer(capped_daily_grams_log ~ 1 + age_3_cats + (1 | intersections),
                       data = data_intersections_MAIHDA,
                       control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
performance::icc(MAIHDA_age)

MAIHDA_race <- lmer(capped_daily_grams_log ~ 1 + race_5_cats + (1 | intersections),
                       data = data_intersections_MAIHDA,
                       control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
performance::icc(MAIHDA_race)

MAIHDA_education <- lmer(capped_daily_grams_log ~ 1 + education_3_cats + (1 | intersections),
                       data = data_intersections_MAIHDA,
                       control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
performance::icc(MAIHDA_education)

MAIHDA_decade <- lmer(capped_daily_grams_log ~ 1 + decade + (1 | intersections),
                       data = data_intersections_MAIHDA,
                       control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
performance::icc(MAIHDA_decade)

# Run the full model
MAIHDA_main_effects <- lmer(capped_daily_grams_log ~ 1 + SEX + age_3_cats + race_5_cats + education_3_cats + decade + (1 | intersections),
                       data = data_intersections_MAIHDA,
                       control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
performance::icc(MAIHDA_main_effects)

# In the full model note that each intersectional group has a random intercept that varies by intersectional group 
# plus a fixed intercept for each of sex, age, race, education and decade. 

# Compare null and full models
summary(MAIHDA_null)
VPC_null <- performance::icc(MAIHDA_null) 

summary(MAIHDA_main_effects)
VPC_full <- performance::icc(MAIHDA_main_effects) 

# Combine null and full models into a single table
coeffs_and_variance <- tab_model(MAIHDA_null, MAIHDA_main_effects, digits=3, digits.re=3)
coeffs_and_variance
# Save the output
tab_model(MAIHDA_null, MAIHDA_main_effects, digits=3, digits.re=3, file = "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/supplementary analysis/daily grams - model coefs and variance - lmer.html")


#calculate reduction in strata-level variance
percent_reduction_in_strata_variance <- (VPC_null-VPC_full$ICC_unadjusted)/VPC_null*100
percent_multiplicative_variance <- (1 - reduction_in_strata_variance)*100

# The question, now, is whether that remaining multiplicative variance is significant. If it isn’t, it would make sense to simplify the model to a single level model, since there is no significant additional variance at level 2. 
# To judge this, we need to run the same model as a single level model, using the lm function

single_level_main_effects <- lm(capped_daily_grams_log ~ SEX + age_3_cats + race_5_cats + education_3_cats + decade, data=data_intersections_MAIHDA)

# We can then compare the model fit of these two models
anova(MAIHDA_main_effects, single_level_main_effects)
# MAIHDA is a significantly better fit than the single level model


# It would now be useful to plot our residuals to identify which intersectional strata have particularly higher/lower levels of consumption than expected. 
# To do this we will use the REsim command, to produce residuals, and associated confidence intervals, which we can then plot with the plotREsim command. 
# First, we will create a new dataframe with the residuals and uncertainty in them

reEX <- REsim(MAIHDA_null)
plotREsim(reEX, labs=T) + coord_flip()
# These are the overall differences between groups
# Note large CIs for some strata 
# This is also why this strata is close to the middle - it has been shrunk in a lot because of its unreliability.

reEX2 <- REsim(MAIHDA_main_effects)
plotREsim(reEX2, labs=T) + coord_flip()
#and these are what remains when additive effects are removed i.e. the multiplicative effects

# Generate estimates for each intersectional group (full model) using the predict function
data_intersections_MAIHDA$yhat <- predict(MAIHDA_main_effects) # the predicted expected value
data_intersections_MAIHDA$estimated_grams <- exp(data_intersections_MAIHDA$yhat)
estimated_grams_lmer <- data_intersections_MAIHDA %>% 
  dplyr::select(intersections, count, yhat, estimated_grams) %>% 
  distinct(intersections, .keep_all = TRUE)

estimates_table_grams_lmer <- inner_join(data_intersections_MAIHDA, MAIHDA_intersections_reference)
estimates_table_grams_lmer <- estimates_table_grams_lmer %>% 
  dplyr::select(intersections, count, age_3_cats, race_5_cats, SEX, intersectional_names, yhat, estimated_grams)%>% 
  distinct(intersections, .keep_all = TRUE)

## Save the estimates
saveRDS(estimates_table_grams_lmer,"C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/supplementary analysis/estiamtes_grams_lmer.rds" )

# Obtain confidence intervals around the estimates (not working)
ci <- confint(MAIHDA_main_effects, newdata = data_intersections_MAIHDA, level=0.95, method="boot")