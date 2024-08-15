# This script runs a multilevel logistic regression with the binary variable of HED (1, 0)

# Load necessary packages
library(tidyr)
library(dplyr)
library(sjstats)
library(haven)
library(performance)
library(memisc)
library(gt)
library(R2MLwiN)
library(xlsx)
options(MLwiN_path="C:/Program Files/MLwiN v3.05/")

# Set working directory
setwd("C:/Users/cmp21seb/Documents/SIMAH/")

# Read in data:
drinkers <- readRDS("SIMAH_workplace/nhis/intersectionality/cleaned_data/nhis_alc_clean_drinkers_only.RDS")

# Generate a binary HED variable
drinkers <- drinkers %>%
  mutate(HED =
           case_when(ALC5UPYR >= 1 ~ 1,
                     ALC5UPYR == 0 ~ 0))

# Bias toward non-scientific notation
options(scipen=10)

data <- drinkers %>% 
  group_by(SEX, race_5_cats, education_3_cats, age_3_cats, decade) %>% 
  mutate(intersections = cur_group_id()) %>%
  group_by(intersections) %>%
  mutate(denominator=n()) %>%
  group_by(HED, intersections) %>%
  mutate(numerator=n(),
         proportion=numerator/denominator,
         percentage=proportion*100) %>%
  ungroup()

# Save subset of data for use in linear probability model (performed in STATA)
LPM_data <- data %>% dplyr::select(intersections, HED, age_3_cats, SEX, 
                                   race_5_cats, education_3_cats, decade, numerator, denominator,
                                   proportion, percentage) %>%
  filter(HED==1) %>%
  distinct(intersections, .keep_all=TRUE)
write_dta(LPM_data, "SIMAH_workplace/nhis/intersectionality/cleaned_data/drinkers_data_for_LPM.dta")

# Subset data to keep only the variables of interest
temp <- data %>%
dplyr::select(intersections, HED, proportion, age_3_cats, SEX, race_5_cats, education_3_cats, decade)

# Generate a summary table showing the proportion of HEDs by intersection
summary_table <- data %>%
  dplyr::select(intersections, HED, numerator, denominator, proportion) %>%
  filter(HED==1) %>%
  distinct()

# Identify any groups with zero HEDs
temp <- data %>% 
  ungroup() %>%
  dplyr::select(intersections, HED, age_3_cats, SEX, race_5_cats, education_3_cats, decade, numerator, denominator, proportion, percentage)%>%
  filter(percentage==100)%>%
  distinct()
# Intersections: 131, 137, 149, 161, 173	

# Generate a HED=0 row for these intersections
data_2 <- data %>% 
  add_row(intersections = 173, HED = 1, age_3_cats = "70+", SEX = "Female", race_5_cats = "Hispanic", education_3_cats = "some college", decade = "2000-2009", numerator = 0, denominator = 58, proportion = 0, percentage = 0) %>% 
  add_row(intersections = 149, HED = 1, age_3_cats = "70+", SEX = "Female", race_5_cats = "Non-Hispanic Other", education_3_cats = "high school or less", decade = "2000-2009", numerator = 0, denominator = 29, proportion = 0, percentage = 0) %>% 
  add_row(intersections = 161, HED = 1, age_3_cats = "70+", SEX = "Female", race_5_cats = "Non-Hispanic Other", education_3_cats = "4+ years college", decade = "2000-2009", numerator = 0, denominator = 5, proportion = 0, percentage = 0) %>% 
  add_row(intersections = 137, HED = 1, age_3_cats = "70+", SEX = "Female", race_5_cats = "Non-Hispanic Asian", education_3_cats = "some college", decade = "2000-2009", numerator = 0, denominator = 20, proportion = 0, percentage = 0) %>% 
  add_row(intersections = 131, HED = 1, age_3_cats = "70+", SEX = "Female", race_5_cats = "Non-Hispanic Asian", education_3_cats = "high school or less", decade = "2000-2009", numerator = 0, denominator = 34, proportion = 0, percentage = 0)

# Subset the results for just one intersectional group and just HEDs
data_3 <- data_2 %>%
  filter(HED==1) %>%
dplyr::select(intersections, HED, age_3_cats, SEX, race_5_cats, education_3_cats, decade, numerator, denominator, proportion, percentage)%>%
  distinct() %>%
  mutate(cons=1)

# Save results
write_dta(data_3, "SIMAH_workplace/nhis/intersectionality/cleaned_data/drinkers_HED_data.dta")
saveRDS(data_3, "SIMAH_workplace/nhis/intersectionality/cleaned_data/drinkers_HED_data.rds")

## Run MAIHDA logit model

# Prep data for use with Mlwin
model_data <- data %>%
  mutate(cons=1) %>% 
  arrange(intersections, NHISPID)

## MAIHDA MCMC

# Run the null model
(null_HED <- runMLwiN(logit(HED) ~ 1 + (1|intersections), D = "Binomial", data = model_data, estoptions=list(EstM=1, resi.store=TRUE)))

# Calculate the VPC
slotNames(null_HED)

VPC_HED_null <- print(VPC <- null_HED["RP"][["RP2_var_Intercept"]]/(pi^2/3 + null_HED["RP"][["RP2_var_Intercept"]]))
#0.302

# Run the full model (including main effects)
(full_HED <- runMLwiN(logit(HED) ~ 1 + + SEX + age_3_cats + race_5_cats + education_3_cats + decade + 
                        (1|intersections), 
                      D = "Binomial", data = model_data, estoptions=list(EstM=1, resi.store=TRUE)))

# Calculate the VPC
summary(full_HED)
VPC_full_HED <- print(VPC <- full_HED["RP"][["RP2_var_Intercept"]]/(pi^2/3 + full_HED["RP"][["RP2_var_Intercept"]]))
# 0.031

# Save the model objects
saveRDS(null_HED, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/HED_MCMC_null.RDS")
saveRDS(full_HED, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/HED_MCMC_full.RDS")

#OR read in the model object
null_HED <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/HED_MCMC_null.RDS")
full_HED <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/HED_MCMC_full.RDS")

# Generate a table of model coefficients, comparing the null and full models
temp_null <- getSummary(null_HED) 
temp_null <- as.data.frame(temp_null[["coef"]])
temp_null <- round(temp_null, 3) %>% dplyr::select(est,lwr,upr,p)
rownames(temp_null) <- c("intercept_FE_1","strata_RE_1","individuals_RE_1")

temp_main_effects <- getSummary(full_HED)
temp_main_effects <- as.data.frame(temp_main_effects[["coef"]])
temp_main_effects <- round(temp_main_effects, 3) %>% dplyr::select(est,lwr,upr,p)
rownames(temp_main_effects) <- c("intercept_FE_2","female","age 25-69", "age 70+", 
                                 "Non-Hispanic Black", "Non-Hispanic Asian", "Non-Hispanic Other", 
                                 "Hispanic", "Some college", "4+ years college","2010-2018", 
                                 "strata_RE_2", "individuals_RE_2")

coefs_table <- rbind(temp_null, temp_main_effects)
write.csv(coefs_table, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/HED - model coefficients and variance.csv")

# Create a gt object to enable addition of headers
gt_table <- gt(coefs_table, rownames_to_stub = TRUE) %>%
  tab_row_group(
    label = "Main effects model",
    rows = 4:16
  ) %>%
  tab_row_group(
    label = "Null model",
    rows = 1:3
  )

gt_table
gtsave(gt_table, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/HED - model coefficients and variance.html")

# Generate a table summarizing VPC
VPC_table <- data.frame(Model = c("null", "main effects"),
                        VPC = c(VPC_HED_null, VPC_full_HED))
saveRDS(VPC_table, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/MCMC_grams_HED.Rmd")

# Estimate log odds (and SEs) using predict function
data$yhat <- predict(full_HED, data)
# the predicted expected value (in this case the log odds of being a HED)
data$yhat_se <- predict(full_HED, data, se.fit=TRUE)$se.fit 

# Create table with results for just one intersection
processed_intersections_HED_MAIHDA <- data %>% 
  dplyr::select(intersections, SEX, education_3_cats, race_5_cats, age_3_cats, decade, yhat, yhat_se) %>% 
  unique()

# Extract  residuals from the Mlwin output object
processed_intersections_HED_MAIHDA$residuals <- full_HED@residual$lev_2_resi_est_Intercept 

# Extract the estimate of variance around the residuals, and take the sqrt of it to get the standard error around the residuals
processed_intersections_HED_MAIHDA$residuals_se <- sqrt(full_HED@residual$lev_2_resi_variance_Intercept) # standard error around residuals

# Estimate the overall mean combining additive effects (main effects) and multiplicative effects (residuals)
processed_intersections_HED_MAIHDA <- processed_intersections_HED_MAIHDA %>% 
  mutate(total_log_odds = yhat + residuals)

# Estimate the total standard error by combining the SE around the log_odds plus the SE around the residuals
processed_intersections_HED_MAIHDA <- processed_intersections_HED_MAIHDA %>% 
  mutate(total_se = (sqrt((yhat_se*yhat_se)+(residuals_se*residuals_se)))/2)

# Convert everything from the logit scale to get the predicted probability of being a HED
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

Estimated_probs_HED <- processed_intersections_HED_MAIHDA %>%
mutate(total_probability = logit2prob(total_log_odds)*100,
       CI_lower = logit2prob(total_log_odds-1.96*total_se)*100,
       CI_upper = logit2prob(total_log_odds+1.96*total_se)*100,
       additive_probability = logit2prob(yhat)*100,
       additive_CI_lower = logit2prob(yhat - 1.96*yhat_se)*100,
       additive_CI_upper = logit2prob(yhat + 1.96*yhat_se)*100,
       interaction_effects = total_probability - additive_probability,
       interaction_effects_CI_lower = interaction_effects - (logit2prob(1.96*residuals_se)),
       interaction_effects_CI_upper = interaction_effects + (logit2prob(1.96*residuals_se)))

# Save results 
saveRDS(Estimated_probs_HED,"C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/Estimated_probs_HED.Rds")
write.xlsx(Estimated_probs_HED,"C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/Estimated_probs_HED.xlsx")
