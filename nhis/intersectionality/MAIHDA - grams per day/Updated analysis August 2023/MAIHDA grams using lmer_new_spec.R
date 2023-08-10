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

# rename race categories
full_sample_intersections <- full_sample_intersections %>%
  mutate(
    education_3_cats = case_when(
    education_3_cats == "4+ years college" ~ "College+",
    education_3_cats == "high school or less" ~ "High school max",
    education_3_cats == "some college" ~ "some college")
    )

# Create a reference table of the intersectional groups - numbers and names
data_intersections_MAIHDA <- full_sample_intersections %>% 
  group_by(SEX, race_6_cats, education_3_cats, age_3_cats) %>% 
  mutate(intersections = cur_group_id()) %>%
  group_by(intersections) %>%
  mutate(count=n())
data_intersections_reference <- data_intersections_MAIHDA %>% distinct(intersections, .keep_all=TRUE)

##### 1. RUN MAIHDA

# NULL model
MAIHDA_null <- lmer(capped_daily_grams_log ~ 1 + (1 | intersections),
                       data = data_intersections_MAIHDA,
                       control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

# FULL model
MAIHDA_main_effects <- lmer(capped_daily_grams_log ~ 1 + SEX + age_3_cats + race_6_cats + education_3_cats + (1 | intersections),
                            data = data_intersections_MAIHDA,
                            REML=FALSE,
                            control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

tab_model(MAIHDA_null, MAIHDA_main_effects, digits=3, digits.re=3, file = "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/supplementary analysis/daily grams - model coefs and variance - lmer - new spec.html")


#####  2. CALCULATE VPCs AND PCV

VPC_null <- performance::icc(MAIHDA_null)$ICC_unadjusted
VPC_full <- performance::icc(MAIHDA_main_effects)$ICC_unadjusted
PCV <- (VPC_null-VPC_full)/VPC_null

### COMPARE MULTILEVEL TO SINGLE LEVEL MODEL
single_level_main_effects <- lm(capped_daily_grams_log ~ SEX + age_3_cats + race_6_cats + education_3_cats, data=data_intersections_MAIHDA)
anova(MAIHDA_main_effects, single_level_main_effects) # MAIHDA is a significantly better fit than the single level model


#####  3. ESTIMATE AVERAGE CONSUMPTION PER STRATA BASED ON THE NULL MODEL

# Estimate the overall mean combining fixed effects and random effects

# Fixed effects
fem <- fixef(MAIHDA_null)
fem_SE <- se.fixef(MAIHDA_null)
# Random effects
u0 <- as.data.frame(ranef(MAIHDA_null)) # Where condval is the estimated random intercepts, condsd is the conditional standard deviation

# Generate the estimates for each intersection
u0$b0condval <- fem["(Intercept)"] + u0$condval # intercept + random effect for that intersection

# and confidence intervals:
u0$b0lower <- u0$b0condval - 1.96*u0$condsd # intersectional estimate plus - 1.96*random effects SEs
u0$b0upper <- u0$b0condval + 1.96*u0$condsd
# nb. this doesn't seem to be incorporating uncertainty from the fixed effects? ASK ANDY - see workings 1

# Back transform
u0$estimated_grams <- exp(u0$b0condval)
u0$grams_lwr <- exp(u0$b0lower)
u0$grams_upr <- exp(u0$b0upper)

# Join estimates and intersections
u0 <- rename(u0, "intersections"=grp)
estimates_null <- merge(u0, data_intersections_reference)
estimates_null <- estimates_null %>% dplyr::select(intersections, SEX, race_6_cats, age_3_cats, education_3_cats, estimated_grams, grams_lwr, grams_upr)


##### 4. PLOTs of ESTIMATES

# Prep data for plots
plot_data <- estimates_null %>%
  mutate(intersection_names = as.character(paste(SEX, race_6_cats, age_3_cats, education_3_cats)))

# Caterpillar plot
ggplot(plot_data, aes(x = fct_reorder(intersection_names, estimated_grams), y = estimated_grams, ymin = grams_lwr, ymax = grams_upr)) +
  theme(axis.text.x = element_text(angle = 90)) +
        geom_hline(yintercept = exp(fem["(Intercept)"])) +
  geom_errorbar() +
  geom_point() +
  labs(x= "Strata", y= "Estimated grams per day")

# Facet grid plots
# plot_women <- estimates_null %>%
#   filter(SEX=="Female")%>%
#   ggplot(aes(x=education_3_cats, y=estimated_grams)) +
#   geom_point(position=position_dodge(width=0.9), size=2) +
#   geom_errorbar(aes(ymin = grams_lwr,
#                     ymax = grams_upr),
#                 position=position_dodge(width=0.9), linewidth=0.8) +
#   facet_grid(cols=vars(race_6_cats),rows=vars(age_3_cats)) +
#   theme(axis.title.x = element_blank(), legend.position = "bottom",
#         axis.text=element_text(size=12),
#         strip.text.x = element_text(size = 12),
#         strip.text.y = element_text(size = 12),
#         legend.text=element_text(size=12),
#         legend.title=element_text(size=12),
#         axis.title.y =element_text(size=12, margin = margin(t = 0, r = 20, b = 0, l = 0)),
#         plot.title = element_text(size=18))+
#   scale_x_discrete(labels = label_wrap(width = 7))+
#   ggtitle("Women") +
#   labs(y= "Estimated grams per day")
# plot_women
# 
# plot_men <- test %>%
#   filter(SEX=="Male")%>%
#   ggplot(aes(x=education_3_cats, y=estimated_grams)) +
#   geom_point(position=position_dodge(width=0.9), size=2) +
#   geom_errorbar(aes(ymin = estimated_grams_lwr,
#                     ymax = estimated_grams_upr),
#                 position=position_dodge(width=0.9), linewidth=0.8) +
#   facet_grid(cols=vars(race_6_cats),rows=vars(age_3_cats)) +
#   theme(axis.title.x = element_blank(), legend.position = "bottom", 
#         axis.text=element_text(size=12),
#         strip.text.x = element_text(size = 12),
#         strip.text.y = element_text(size = 12),
#         legend.text=element_text(size=12),
#         legend.title=element_text(size=12),
#         axis.title.y =element_text(size=12, margin = margin(t = 0, r = 20, b = 0, l = 0)),
#         plot.title = element_text(size=18))+
#   scale_x_discrete(labels = label_wrap(width = 7))+ 
#   ggtitle("men") +
#   labs(y= "Estimated grams per day")
# plot_men


####################################################################################

## WORKINGS 1 - Alternate CIs:
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

#######################################################################################

# WORKINGS 2 - Check normality assumption of the random effects

# Plot a histogram of the standardised mother effects and superimpose an appropriately scaled normal density in order to assess the normality assumption
u0$condvalstd <- scale(u0$condval) # condval rescaled to have a mean of 0 and SD of 1
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

########################################################################################
# WORKINGS 3 - BASED ON AB'S SCRIPT

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


