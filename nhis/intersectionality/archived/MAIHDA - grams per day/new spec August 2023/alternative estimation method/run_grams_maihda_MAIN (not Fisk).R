# MAIHDA of grams of alcohol per day

# MAIN - ALTERNATIVE METHOD FOR DERIVING ESTIMATES

# Set wd
setwd("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_code/nhis/intersectionality")

# Read in necessary R packages & functions
library(tidyverse)
library("R2MLwiN")

options(MLwiN_path="C:/Program Files/MLwiN v3.05/")

setwd("C:/Users/cmp21seb/Documents/SIMAH/")

options(scipen=10)

# Read in data that feeds into models
model_data <- readRDS("SIMAH_workplace/nhis/intersectionality/cleaned_data/new spec August 2023/grams/grams_data_pre_maihda_main.rds")

# Generate reference table with intersectional names
intersections_reference <- model_data %>%
  distinct(intersections, intersectional_names, mean_observed_grams)

# Read in the model objects
null_grams <- readRDS( "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/new spec August 2023/grams/null_grams_MAIN.rds")
full_grams <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/new spec August 2023/grams/full_grams_MAIN.rds")

##### EXTRACT MODEL ESTIMATES (alternative to Fisk method)

# Add intersectional group sizes as important indicator of expected level of shrinkage
model_data <- model_data %>% 
  group_by(intersections) %>%
  mutate(count=n()) %>%
  ungroup()

# generate dummy variables in order for predict to work
dums <- fastDummies::dummy_cols(model_data, select_columns=c("YEAR", "SEX", "age_diaz", "race_6_cats", "education_3_cats"))

# cols predict is looking for
sub("FP_", "", names(full_grams@FP))
# versus cols we have:
colnames(model_data)

# rename them to match....
new_data <- dplyr::rename(dums,
                            YEAR2000 = YEAR_2000,
                            YEAR2001 = YEAR_2001,
                            YEAR2002 = YEAR_2002,
                            YEAR2003 = YEAR_2003,
                            YEAR2004 = YEAR_2004,
                            YEAR2005 = YEAR_2005,
                            YEAR2006 = YEAR_2006,
                            YEAR2007 = YEAR_2007,
                            YEAR2008 = YEAR_2008,
                            YEAR2009 = YEAR_2009,
                            YEAR2010 = YEAR_2010,
                            YEAR2011 = YEAR_2011,
                            YEAR2012 = YEAR_2012,
                            YEAR2013 = YEAR_2013,
                            YEAR2014 = YEAR_2014,
                            YEAR2015 = YEAR_2015,
                            YEAR2016 = YEAR_2016,
                            YEAR2017 = YEAR_2017,
                            YEAR2018 = YEAR_2018,
                            SEXMale = SEX_Male,
                            SEXFemale = SEX_Female,
                            "age_diaz21-24" = "age_diaz_21-24", 
                            "age_diaz25-59" = "age_diaz_25-59", 
                            "age_diaz60+" = "age_diaz_60+",
                            "race_6_catsHispanic White" = "race_6_cats_Hispanic White",
                            "race_6_catsBlack" = "race_6_cats_Black",                
                            "race_6_catsAsian" = "race_6_cats_Asian",
                            "race_6_catsMultiple race" = "race_6_cats_Multiple race",      
                            "race_6_catsAI/AN" = "race_6_cats_AI/AN",
                            "education_3_catshigh_school_or_less" = "education_3_cats_high school or less",
                            "education_3_catssome college" = "education_3_cats_some college", 
                            "education_3_cats4+ years college" = "education_3_cats_4+ years college")

data_to_predict <- new_data %>% distinct(intersections, .keep_all=TRUE) %>% ungroup()          

# Estimate yhat (and SEs) using predict function
data_to_predict$yhat <- predict(full_grams, data_to_predict)
data_to_predict$yhat_se <- predict(full_grams, data_to_predict, se.fit=TRUE)$se.fit # the standard error of the predicted expected value

mdata_resutls <- data_to_predict

# Extract residuals from the Mlwin output object
mdata_results$residuals <- full_grams@residual$lev_2_resi_est_Intercept

# Extract the estimate of variance around the residuals, and take the sqrt of it to get the standard error around the residuals
mdata_results$residualsSE <- sqrt(full_grams@residual$lev_2_resi_variance_Intercept) # standard error around residuals

# Estimate the overall mean combining additive effects (main effects) and multiplicative effects (residuals)
mdata_results <- mdata_results %>% 
  mutate(estimate = yhat + residuals)

# Estimate the total standard error by combining the error around the mean plus the error around the residuals:
mdata_results <- mdata_results %>% 
  mutate(SE =(sqrt((yhat_se*yhat_se)+(residualsSE*residualsSE)))) # For now, assuming that division by 2 not required

# Generate results table
mdata_results <- mdata_results %>%
  dplyr::select(intersections, intersectional_names, count, SEX, race_6_cats, education_3_cats, 
                age_diaz, yhat, yhat_se, residuals, residualsSE, estimate, SE, mean_observed_grams)

# Back transform the estimates
mdata_results_2 <- mdata_results %>% 
  mutate(
    back_transformed_estimate = exp(estimate),
    back_transformed_CI_lower = exp(estimate - 1.96*SE),
    back_transformed_CI_upper = exp(estimate + 1.96*SE))

# Save estimates
saveRDS(mdata_results_2, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/grams/grams_MAIN_alt_ests.RDS")
write.csv(mdata_results_2, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/grams/grams_MAIN_alt_ests.csv")

# Summary results tables

# Read in results table
mdata_results_2 <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/grams/grams_MAIN_alt_ests.RDS")

# Summarise intersectional groups with the highest and lowest estimated grams
mdata_max_5_overall <- mdata_results_2 %>% ungroup %>% slice_max(back_transformed_estimate, n = 5) %>% 
  dplyr::select(intersectional_names, back_transformed_estimate, back_transformed_CI_lower, back_transformed_CI_upper)
mdata_min_5_overall <- mdata_results_2 %>% ungroup %>% slice_min(back_transformed_estimate, n = 5) %>% 
  dplyr::select(intersectional_names, back_transformed_estimate, back_transformed_CI_lower, back_transformed_CI_upper)
mdata_overall <- rbind(mdata_max_5_overall, mdata_min_5_overall)

write.csv(mdata_overall, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/grams/mdata_5_estimates_MAIN_alt_ests.csv")

# Summarise which intersectional groups have the largest residuals (interaction effects)
mdata_max_5_interactions <- mdata_results_2 %>% ungroup %>% slice_max(residuals, n = 5) %>% 
  dplyr::select(intersectional_names, residuals)
mdata_min_5_interactions <- mdata_results_2 %>% ungroup %>% slice_min(residuals, n = 5) %>% 
  dplyr::select(intersectional_names, residuals)
mdata_interactions <- rbind(mdata_max_5_interactions, mdata_min_5_interactions)

write.csv(mdata_interactions, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/grams/mdata_5_interactions_MAIN_alt_ests.csv")

##### Explore face validity of back-transformed data
temp <- mdata_results_2 %>% dplyr::select(intersectional_names, count, mean_observed_grams, back_transformed_estimate)
ggplot(temp, aes(x=mean_observed_grams, y=back_transformed_estimate)) + geom_point() +
  ggtitle("Comparisson of observed and estimated daily grams, 180 intersectional groups")
 ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/plots/new spec August 2023/grams/observed vs estimated grams_MAIN_alt_ests.png", 
       dpi=300, width=33, height=19, units="cm")

# Compare the ranking of intersectional groups based on observed and estimated grams
temp$rank_observed_grams <-rank(temp$mean_observed_grams)
temp$rank_estimated_grams <-rank(temp$back_transformed_estimate)
ggplot(temp, aes(x=rank_observed_grams, y=rank_estimated_grams)) + geom_point() + geom_smooth() +
  ggtitle("Comparisson of observed vs estimated drinking 'rank', 180 intersectional groups")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/plots/new spec August 2023/grams/observed vs estimated grams ranks_MAIN_alt_ests.png", 
        dpi=300, width=33, height=19, units="cm")
