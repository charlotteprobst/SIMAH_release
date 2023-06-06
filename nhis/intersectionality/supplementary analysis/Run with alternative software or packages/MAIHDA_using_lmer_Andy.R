library(tidyverse)
library(lme4)
library(sjPlot)
library(merTools)
library(reshape2)

#load the data
transformed_drinkers <- readRDS("U:/SIMAH/SIMAH_workplace/nhis/intersectionality/cleaned_data/6_nhis_alc_clean_transformed_drinkers_log.RDS")

##Prep data
# Generate intersectional groups
data_intersections_MAIHDA <- transformed_drinkers %>% 
  group_by(SEX, race_5_cats, education_3_cats, age_3_cats, decade) %>% 
  mutate(intersections = cur_group_id()) %>%
  group_by(intersections) %>%
  mutate(count=n())

# Add a column of the observed mean grams per day for each intersection
data_intersections_MAIHDA <- data_intersections_MAIHDA %>%
  group_by(intersections) %>%
  mutate(mean_observed_grams = mean(alc_daily_g_capped_200))

data_intersections_MAIHDA$strata <- paste(data_intersections_MAIHDA$SEX,
                                          data_intersections_MAIHDA$age_3_cats,
                                          data_intersections_MAIHDA$race_5_cats,
                                          data_intersections_MAIHDA$education_3_cats,
                                          data_intersections_MAIHDA$decade, sep="_")
table(data_intersections_MAIHDA$strata)
#We can see that some strata are bigger than others - eg there is one strata with only one observation!


# Generate intersectional groups
temp <- data_intersections_MAIHDA %>%
  mutate(sex = dplyr::recode(SEX, Male = "M", Female = "F"),
         race_5_cats = dplyr::recode(race_5_cats, 
                                     "Black/African American" = "Black", 
                                     "Hispanic, White" = "Hispanic"),
         education_3_cats = dplyr::recode(education_3_cats, 
                                          "high school or less" = "low edu.", 
                                          "some college" = "med. edu.", 
                                          "4+ years college" = "high edu."))

#run models
model1 <- lmer(capped_daily_grams_log ~ (1|intersections), data=data_intersections_MAIHDA)
summary(model1)
0.4538/(0.4538+3.2515) # 0.122

model2 <- lmer(capped_daily_grams_log ~ SEX + age_3_cats + race_5_cats + education_3_cats + decade + (1|intersections), data=data_intersections_MAIHDA)
summary(model2)
0.0526/(0.0526+3.2515) # 0.0159

#combine both models into a single table
results_table <- tab_model(model1, model2, digits=3, digits.re=3)

#calculate reduction in strata-level variance
(0.122-0.0159)/(0.122) # 0.869
#ie. 87% reduction in strata level variance, 13% of intersectional difference is multiplicative

# The question, now, is whether that remaining multiplicative variance is significant. If it isn’t, it would make sense to simplify the model to a single level model, since there is no significant additional variance at level 2. 
# To judge this, we need to run the same model as a single level model, using the lm function

model3 <- lm(capped_daily_grams_log ~ SEX + age_3_cats + race_5_cats + education_3_cats + decade, data=data_intersections_MAIHDA)

# We can then compare the model fit of these two models

anova(model2, model3)

# Model 2 is a significantly better fit than model 3 (i.e. multilevel model is better than single level)

# It would now be useful to plot our residuals to identify which intersectional strata have particularly higher/lower levels of consumption than expected. 
# To do this we will use the REsim command, to produce residuals, and associated confidence intervals, which we can then plot with the plotREsim command. 
# First, we will create a new dataframe with the residuals and uncertainty in them

reEX <- REsim(model1)
plotREsim(reEX, labs=T) + coord_flip()
#these are the overall differences between groups
#note large CIs for some strata 
#This is also why this strata is close to the middle - it has been shrunk in a lot because of its unreliability.

reEX2 <- REsim(model2)
plotREsim(reEX2, labs=T) + coord_flip()
#and these are what remains when additive effects are removed 9i.e. the multiplicative effects

# Generate prediction data ?????
prediction_data <- data_intersections_MAIHDA %>%
  mutate(intersectional_names = as.character(paste(SEX, age_3_cats, race_5_cats, education_3_cats, decade))) %>%
  distinct(intersections, .keep_all = TRUE) %>% dplyr::select(intersections, intersectional_names, SEX, age_3_cats, race_5_cats, education_3_cats, decade, capped_daily_grams_log, mean_observed_grams)

added_preds <- add_predictions(prediction_data, model2, var = "pred", type = NULL)

added_preds <- added_preds %>% mutate(predicted_average_grams = exp(pred))