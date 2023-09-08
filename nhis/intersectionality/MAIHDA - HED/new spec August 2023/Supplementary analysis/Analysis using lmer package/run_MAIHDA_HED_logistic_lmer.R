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
library(boot)

# Bias toward non-scientific notation
options(scipen=10)

# rename education categories
full_sample_intersections <- full_sample_intersections %>%
  mutate(education_3_cats = case_when(
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

# Generate a binary HED variable
data_intersections_MAIHDA <- data_intersections_MAIHDA %>%
  mutate(HED =
           case_when(ALC5UPYR >= 1 ~ 1,
                     ALC5UPYR == 0 ~ 0))

# Bias toward non-scientific notation
options(scipen=10)

data <- data_intersections_MAIHDA  %>% 
  group_by(SEX, race_6_cats, education_3_cats, age_3_cats) %>% 
  mutate(intersections = cur_group_id()) %>%
  group_by(intersections) %>%
  mutate(denominator=n()) %>%
  group_by(HED, intersections) %>%
  mutate(numerator=n(),
         proportion=numerator/denominator,
         percentage=proportion*100) %>%
  ungroup()

# Save subset of data for use in linear probability model (performed in STATA)
# LPM_data <- data %>% dplyr::select(intersections, HED, age_3_cats, SEX, 
#                                    race_6_cats, education_3_cats, numerator, denominator,
#                                    proportion, percentage) %>%
#   filter(HED==1) %>%
#   distinct(intersections, .keep_all=TRUE)
# write_dta(LPM_data, "SIMAH_workplace/nhis/intersectionality/cleaned_data/drinkers_data_for_LPM.dta")

# Subset data to keep only the variables of interest
temp <- data %>%
dplyr::select(intersections, HED, proportion, age_3_cats, SEX, race_6_cats, education_3_cats)

# Generate a summary table showing the proportion of HEDs by intersection
summary_table <- data %>%
  dplyr::select(intersections, HED, numerator, denominator, proportion, percentage) %>%
  filter(HED==1) %>%
  distinct()

# Identify any groups with zero HEDs
temp <- data %>% 
  ungroup() %>%
  dplyr::select(intersections, HED, age_3_cats, SEX, race_6_cats, education_3_cats, numerator, denominator, proportion, percentage)%>%
  filter(percentage==100)%>%
  distinct()
# Intersections: 105, 102	

# Generate a HED=0 row for these intersections
data_2 <- data %>% 
  add_row(intersections = 102, HED = 1, age_3_cats = "70+", SEX = "Female", race_6_cats = "AI/AN", education_3_cats = "High school max", numerator = 0, denominator = 124, proportion = 0, percentage = 0) %>% 
  add_row(intersections = 105, HED = 1, age_3_cats = "70+", SEX = "Female", race_6_cats = "AI/AN", education_3_cats = "some college", numerator = 0, denominator = 124, proportion = 0, percentage = 0)
  
# Subset the results for just one intersectional group and just HEDs
data_3 <- data_2 %>%
  filter(HED==1) %>%
dplyr::select(intersections, HED, age_3_cats, SEX, race_6_cats, education_3_cats, numerator, denominator, proportion, percentage)%>%
  distinct() %>%
  mutate(cons=1)

# Save results
write_dta(data_3, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/cleaned_data/HED_data_new_spec.dta")
saveRDS(data_3, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/cleaned_data/HED_data_new_spec.rds")

##### 1. RUN MAIHDA

model_data <- data_2

# Run the null model
null_model_hed <- glmer(HED ~ 1 + (1 | intersections), 
            data = model_data,
            family = binomial(link = "logit"))
summary(null_model_hed)


# Run the full model (including main effects)
full_model_hed <- glmer(HED ~ 1 + SEX + race_6_cats + education_3_cats + age_3_cats + (1 | intersections), 
                        data = model_data,
                        family = binomial(link = "logit"))
summary(full_model_hed)

# Save the model objects
saveRDS(null_model_hed, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/HED_null_new_spec.RDS")
saveRDS(full_model_hed, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/HED_full_new_spec.RDS")

#OR read in the model object
null_model_hed <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/HED_null_new_spec.RDS")
full_model_hed <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/HED_full_new_spec.RDS")

##### 2. CALCULATE THE VPCs AND PCV
v_null <- as.data.frame(VarCorr(null_model_hed))
v_full <- as.data.frame(VarCorr(full_model_hed))

VPC_null_hed <- v_null$vcov[v_null$grp == "intersections"] / (v_null$vcov[v_null$grp == "intersections"] + pi^2/3)
VPC_full_hed <- v_full$vcov[v_full$grp == "intersections"] / (v_full$vcov[v_full$grp == "intersections"] + pi^2/3)

##### 3. ESTIMATE HED PREVELANCE AND CIs FOR EACH INTERSECTION

# Fixed effects
fem <- fixef(null_model_hed)# estimated intercept
fem_SE <- se.fixef(null_model_hed)

# Random effects
u0 <- as.data.frame(ranef(null_model_hed)) # Where condval is the estimated random intercepts, condsd is the conditional standard deviation

# Estimates (converted to additive scale)
u0$p_hed <- inv.logit(fem + u0$condval)
u0$total_se <- (sqrt((fem_SE*fem_SE)+(u0$condsd*u0$condsd))) # calculate total SE buy adding the error around the intercept plus the error around the random effect
u0$plower <- inv.logit(fem + u0$condval - 1.96*u0$total_se) 
u0$pupper <- inv.logit(fem + u0$condval + 1.96*u0$total_se)

# Join estimates and intersections
u0 <- dplyr::rename(u0, "intersections" = grp)
estimates_null <- merge(u0, data_intersections_reference)
estimates_null <- estimates_null %>% dplyr::select(intersections, SEX, race_6_cats, age_3_cats, education_3_cats, p_hed, plower, pupper)

# Alternative options

#A.
predictions <- predictInterval(null_model_hed, level=0.95, newdata = data_intersections_reference, type="probability")
# option to add include.resid.var=0?

#B.
predictions_fixed_null <- ggpredict(
  null_model_hed,
  c("intersections [all]"),
  type = "fixed",
  ci.lvl = 0.95
)

predictions_random_null <- ggpredict(
  null_model_hed,
  c("intersections [all]"),
  type = "random",
  ci.lvl = 0.95
)

# full mode, fixed effects only
predictions_fixed <- ggpredict(
  full_model_hed,
  c("SEX", "race_6_cats", "age_3_cats", "education_3_cats"),
  type = "fixed",
  ci.lvl = 0.95
)

# full model, including both fixed and random effects
predictions_random <- ggpredict(
  full_model_hed,
  c("SEX", "race_6_cats", "age_3_cats", "education_3_cats"),
  type = "random",
  ci.lvl = 0.95
)

# type = "random" still returns population-level predictions, 
# however, unlike type = "fixed", intervals also consider the uncertainty in the variance 
# parameters (the mean random effect variance) and hence can be considered as prediction intervals.

# C.
prediction_table <- data_intersections_reference %>%
                    dplyr::select(race_6_cats, age_3_cats,education_3_cats, SEX, intersections)

pred1 <- predict(full_model_hed, newdata=data_intersections_reference)
pred2 <- predict(full_model_hed, newdata=data_intersections_reference, re.form=NA)  # will give you predictions that don't include the strata random effects
pred_comb <- as.data.frame(cbind(pred1, pred2))
pred_comb$total <- inv.logit(pred_comb$pred1)*100
pred_comb$additive_only <- inv.logit(pred_comb$pred2)*100
pred_comb$total_minus_main_effects <- pred_comb$total - pred_comb$additive_only

pred_ref <- add_predictions(prediction_table, full_model_hed) # will give you predictions including the strata random effects
predictions <- cbind(pred_comb, pred_ref)
predictions <- predictions %>% dplyr::select(intersections, race_6_cats, age_3_cats,education_3_cats, SEX, total, additive_only, total_minus_main_effects)

# Compare to u0j from full_model
u0j <- as.data.frame(ranef(full_model_hed)) %>% rename("intersections" = grp)
predictions_final <- merge(predictions, u0j)

# plot
ggplot(predictions_final, aes(x=total_minus_main_effects, y=inv.logit(condval))) + 
  geom_point() + geom_smooth(method=lm)


##### 4. CALCULATE INCIDENCE ATTRIBUTABLE TO INTERACTION

# Fixed effects
full_fem <- fixef(full_model_hed)
full_fem_SE <- se.fixef(full_model_hed)
full_fem_df <- as_tibble(full_fem, rownames=NA) %>% 
  t() %>%
  as.data.frame()

full_fem_df <- full_fem_df %>%
  dplyr::rename(bMale = SEXMale,
                bAsian = race_6_catsAsian)
# etc. so fixed_effects can then be obtained easily e.g.


# Random effects
full_u0 <- as.data.frame(ranef(full_model_hed)) # Where condval is the estimated random intercepts, condsd is the conditional standard deviation

# 4a. calculate total predicted incidences(main effects and interactive effects)  - as prob

# Estimates (converted to additive scale) - NEED TO ADD IN OTHER FIXED EFFECTS
full_u0$p_hed <- inv.logit(full_fem + full_u0$condval)
full_u0$plower <- inv.logit(full_fem + full_u0$condval - 1.96*full_u0$condsd) # This is based on intercept and re only - not fe
full_u0$pupper <- inv.logit(full_fem + full_u0$condval + 1.96*full_u0$condsd)

### need to generate dummy variables? see stata code as below
# generate p = 100*invlogit( ///
#                              b_cons*cons ///
#                              + b_female*female ///
#                              + b_younger_adult*younger_adult ///
#                              + b_older_adult*older_adult ///
#                              + b_Black*Black ///
#                              + b_Asian*Asian ///
#                              + b_Other*Other ///
#                              + b_Hispanic*Hispanic ///
#                              + b_med*med ///
#                              + b_high*high ///
#                              + b_second_decade*second_decade ///
#                              + u ///
# )

# 4b. calculate predicted incidences (main effects only) - as prob


# 4c. Calculate incidence attributable to interaction = total incidence – additive incidence






##### 4. PLOTS

# Prep data for plots
plot_data <- estimates_null %>%
  mutate(intersection_names = as.character(paste(SEX, race_6_cats, age_3_cats, education_3_cats)))

ggplot(plot_data, aes(x = fct_reorder(intersection_names, p_hed), y = p_hed)) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  geom_hline(yintercept = inv.logit(fem)) +
  geom_point() +
  geom_errorbar(aes(ymin = plower, ymax = pupper)) +
  geom_text(aes(label = intersection_names),
            angle=-90,
        #    position = position_dodge(width = 0.1),
            vjust = 0,
            hjust = 1.5,
            size = 3) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(x = "strata", y = "Proportion of group who drink heavily 1+ day a year")
