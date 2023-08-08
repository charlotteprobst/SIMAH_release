setwd("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_code/nhis/intersectionality/")

full_sample_intersections <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/cleaned_data/full_sample_new_spec.RDS")

# Load necessary packages
library(lme4)
library(tidyr)
library(dplyr)
library(sjPlot)
library(merTools)
library(ggplot2)
library(scales)

# Bias toward non-scientific notation
options(scipen=10)

# Create a reference table of the intersectional groups - numbers and names
data_intersections_MAIHDA <- full_sample_intersections %>% 
  group_by(SEX, race_6_cats, education_3_cats, age_3_cats) %>% 
  mutate(intersections = cur_group_id()) %>%
  group_by(intersections) %>%
  mutate(count=n())
data_intersections_reference <- data_intersections_MAIHDA %>% distinct(intersections, .keep_all=TRUE)

# Run the null model and calculate the VPC
MAIHDA_null <- lmer(capped_daily_grams_log ~ 1 + (1 | intersections),
                       data = data_intersections_MAIHDA,
                       control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
MAIHDA_null_ICC_new_spec <- performance::icc(MAIHDA_null) # 15.7%

# Predict the shrunken (i.e., empirical Bayes) intersectional random effects and their standard errors
u0 <- as.data.frame(ranef(MAIHDA_null)) # u0 is a new data frame with one row per intersection
# Where condval is the estimated random intercepts and condsd is the conditional standard deviation
u0$condvalstd <- scale(u0$condval) # condval rescaled to have a mean of 0 and SD of 1

######################################################################################
# Random effect exploration

# Check normality assumption of the random effects
# Plot a histogram of the standardised mother effects and superimpose an appropriately scaled normal density in order to assess the normality assumption
ggplot(u0, aes(x = condvalstd)) + 
  geom_histogram(aes(y =..density..), colour = "black", fill = "white") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(u0$condvalstd), sd = sd(u0$condvalstd)))
ggplot(u0, aes(sample = condvalstd)) + 
  stat_qq() + 
  stat_qq_line()

# Plot a caterpillar plot of the intersectional random effects
u0$lower<-u0$condval - 1.96*u0$condsd
u0$upper<-u0$condval + 1.96*u0$condsd
u0$rank = rank(u0$condval)
ggplot(u0, aes(x = rank, y = condval, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0) +
  geom_errorbar() +
  geom_point()

####################################################################################

# Estimate the overall mean combining fixed effects and random effects
fem <- fixef(MAIHDA_null) # Fixed effects 
fem_SE <- se.fixef(MAIHDA_null)
u0$b0condval <- fem["(Intercept)"] + u0$condval # estimate = intercept + random effect for a given intersection

## ESTIMATE CIs
u0$b0lower <- u0$b0condval - 1.96*u0$condsd #doesn't seem to be incorporating uncertainty from the fixed effect? ASK ANDY
u0$b0upper <- u0$b0condval + 1.96*u0$condsd

# Back transformed
u0$estimated_grams <- exp(u0$b0condval)
u0$estimated_grams_lwr <- exp(u0$b0lower)
u0$estimated_grams_upr <- exp(u0$b0upper)
plot_one <- ggplot(u0, aes(x = grp, y = estimated_grams, ymin = estimated_grams_lwr, ymax = estimated_grams_upr)) +
  geom_hline(yintercept = exp(fem["(Intercept)"])) +
  geom_errorbar() +
  geom_point()

# Alternate CIs:
u0$total_se <- (sqrt((fem_SE*fem_SE)+(u0$condsd*u0$condsd))) # not currently dividing by 2?
u0$total_lower <- u0$b0condval - 1.96*u0$total_se
u0$total_upper <- u0$b0condval + 1.96*u0$total_se
# Back transformed alternate CIs
u0$estimated_grams_lwr_alt <- exp(u0$total_lower)
u0$estimated_grams_upr_alt <- exp(u0$total_upper)
plot_alternate <- ggplot(u0, aes(x = grp, y = estimated_grams, ymin = estimated_grams_lwr_alt, ymax = estimated_grams_upr_alt)) +
  geom_hline(yintercept = exp(fem["(Intercept)"])) +
  geom_errorbar() +
  geom_point()

mutate(
  back_transformed_estimate = back_transform_log(estimate),
  back_transformed_CI_lower = back_transform_log(estimate - 1.96*SE),
  back_transformed_CI_upper = back_transform_log(estimate + 1.96*SE))


# Join estimates and intersections
u0 <- rename(u0, "intersections"=grp)
results_null <- merge(u0, data_intersections_reference)
results_null <- results_null %>% dplyr::select(intersections, SEX, race_6_cats, age_3_cats, education_3_cats, estimated_grams, estimated_grams_lwr, estimated_grams_upr)



# Run the full model
MAIHDA_main_effects <- lmer(capped_daily_grams_log ~ 1 + SEX + age_3_cats + race_6_cats + education_3_cats + (1 | intersections),
                       data = data_intersections_MAIHDA,
                       REML=FALSE,
                       control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
MAIHDA_full_ICC_new_spec <- performance::icc(MAIHDA_main_effects)

# Combine null and full models coeffs into a single table
coeffs_and_variance <- tab_model(MAIHDA_null, MAIHDA_main_effects, digits=3, digits.re=3)
coeffs_and_variance
# Save the output
tab_model(MAIHDA_null, MAIHDA_main_effects, digits=3, digits.re=3, file = "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/supplementary analysis/daily grams - model coefs and variance - lmer - new spec.html")

#calculate reduction in strata-level variance
VPC_null <- MAIHDA_null_ICC_new_spec$ICC_unadjusted
VPC_full <- MAIHDA_full_ICC_new_spec$ICC_unadjusted
percent_reduction_in_strata_variance <- (VPC_null-VPC_full)/VPC_null*100
percent_multiplicative_variance <- (100 - percent_reduction_in_strata_variance)
 
# The question, now, is whether that remaining multiplicative variance is significant. If it isn’t, it would make sense to simplify the model to a single level model, since there is no significant additional variance at level 2. 
# To judge this, we need to run the same model as a single level model, using the lm function
single_level_main_effects <- lm(capped_daily_grams_log ~ SEX + age_3_cats + race_6_cats + education_3_cats, data=data_intersections_MAIHDA)

# We can then compare the model fit of these two models
anova(MAIHDA_main_effects, single_level_main_effects)
# MAIHDA is a significantly better fit than the single level model

plot_women <- test %>%
  filter(SEX=="Female")%>%
  ggplot(aes(x=education_3_cats, y=estimated_grams)) +
  geom_point(position=position_dodge(width=0.9), size=2) +
  geom_errorbar(aes(ymin = estimated_grams_lwr,
                    ymax = estimated_grams_upr),
                position=position_dodge(width=0.9), linewidth=0.8) +
  facet_grid(cols=vars(race_6_cats),rows=vars(age_3_cats)) +
  theme(axis.title.x = element_blank(), legend.position = "bottom", 
        axis.text=element_text(size=12),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        axis.title.y =element_text(size=12, margin = margin(t = 0, r = 20, b = 0, l = 0)),
        plot.title = element_text(size=18))+
  scale_x_discrete(labels = label_wrap(width = 7))+ 
  ggtitle("Women") +
  labs(y= "Estimated grams per day", colour = "Educational attainment")
plot_women

plot_men <- test %>%
  filter(SEX=="Male")%>%
  ggplot(aes(x=education_3_cats, y=estimated_grams)) +
  geom_point(position=position_dodge(width=0.9), size=2) +
  geom_errorbar(aes(ymin = estimated_grams_lwr,
                    ymax = estimated_grams_upr),
                position=position_dodge(width=0.9), linewidth=0.8) +
  facet_grid(cols=vars(race_6_cats),rows=vars(age_3_cats)) +
  theme(axis.title.x = element_blank(), legend.position = "bottom", 
        axis.text=element_text(size=12),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        axis.title.y =element_text(size=12, margin = margin(t = 0, r = 20, b = 0, l = 0)),
        plot.title = element_text(size=18))+
  scale_x_discrete(labels = label_wrap(width = 7))+ 
  ggtitle("men") +
  labs(y= "Estimated grams per day", colour = "Educational attainment")
plot_men






#### workings - ignore for now

## Part a
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
 
# Generate estimates for each intersectional group (full model and null model produce identical estimates) using the predict function
data_intersections_MAIHDA$yhat <- predict(MAIHDA_main_effects) # the predicted expected value
data_intersections_MAIHDA$estimated_grams <- exp(data_intersections_MAIHDA$yhat)
estimated_grams_lmer <- data_intersections_MAIHDA %>% 
dplyr::select(intersections, count, yhat, estimated_grams) %>% 
distinct(intersections, .keep_all = TRUE)

estimates_table_grams_lmer <- inner_join(data_intersections_MAIHDA, MAIHDA_intersections_reference)
estimates_table_grams_lmer <- estimates_table_grams_lmer %>% 
dplyr::select(intersections, count, age_3_cats, race_6_cats, SEX, intersectional_names, yhat, estimated_grams)%>% 
distinct(intersections, .keep_all = TRUE)



# Working part b - ? need to include something to state whether the group should have each effect applied

# Predict the shrunken (i.e., empirical Bayes) intersectional random effects and their standard errors
u02 <- as.data.frame(ranef(MAIHDA_main_effects)) # u0 is a new data frame with one row per intersection.
u02$condvalstd <- scale(u02$condval)
# condval = estimated random intercepts
# condsd = estimated standard errors for the random intercepts.
# condvalstd = condval rescaled to have a mean of 0 and SD of 1

# Check normality assumption of the random effects
# Plot a histogram of the standardised mother effects and superimpose an appropriately scaled normal density in order to assess the normality assumption
ggplot(u02, aes(x = condvalstd)) + 
  geom_histogram(aes(y =..density..), colour = "black", fill = "white") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(u0$condvalstd), sd = sd(u02$condvalstd)))

ggplot(u02, aes(sample = condvalstd)) + 
  stat_qq() + 
  stat_qq_line()

# Plot a caterpillar plot of the intersectional random effects
u02$lower<-u02$condval - 1.96*u02$condsd
u02$upper<-u02$condval + 1.96*u02$condsd

# Predict the mean grams per intersectional group and CIs around these estimates 
fem2 <- fixef(MAIHDA_main_effects) # Fixed effect i.e. intercept = -1.37
u02$b0condval <- fem2["(Intercept)"] + fem2["SEXFemale"] + 
  fem2["age_3_cats25-69"] + 
  fem2["age_3_cats70+"] + 
  fem2["race_6_catsHispanic, White only"] + 
  fem2["race_6_catsNon-hispanic, Black/African American only"] + 
  fem2["race_6_catsNon-hispanic, Asian only"] + 
  fem2["race_6_catsNon-hispanic, Multiple race"] + 
  fem2["race_6_catsAmerican Indian/Alaska Native only"] + 
  fem2["education_3_catssome college"] + 
  fem2["education_3_cats4+ years college"] + 
  u02$condval
u02$b0lower <- u02$b0condval - 1.96*u02$condsd 
u02$b0upper <- u02$b0condval + 1.96*u02$condsd
# Back transformed
u02$estimated_grams <- exp(u02$b0condval)
u02$estimated_grams_lwr <- exp(u02$b0lower)
u02$estimated_grams_upr <- exp(u02$b0upper)
ggplot(u02, aes(x = rank, y = estimated_grams, ymin = estimated_grams_lwr, ymax = estimated_grams_upr)) +
  geom_hline(yintercept = exp(fem2["(Intercept)"])) +
  geom_errorbar() +
  geom_point()


###################



# ## Save the estimates
# saveRDS(estimates_table_grams_lmer,"C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/supplementary analysis/estiamtes_grams_lmer.rds" )
# 
# # NB. The confidence intervals generated below are much much wider than those generated with R2MLWin (even though the estimates are similar)
# # Generate estimates for each intersectional group (full model) using the predictInterval function
# int_fit <- predictInterval(MAIHDA_main_effects, data_intersections_MAIHDA) # the predicted expected values and SEs
# data_intersections_MAIHDA$estimated_grams_2 <- exp(int_fit$fit)
# data_intersections_MAIHDA$estimated_lower_CI <- exp(int_fit$lwr)
# data_intersections_MAIHDA$estimated_upper_CI <- exp(int_fit$upr)
#   
# estimated_grams_lmer_predictInterval <- data_intersections_MAIHDA %>% 
#   dplyr::select(intersections, count, estimated_grams_2, estimated_lower_CI, estimated_upper_CI) %>% 
#   distinct(intersections, .keep_all = TRUE)
# 
# estimates_table_both <- inner_join(estimates_table_grams_lmer, estimated_grams_lmer_predictInterval)
