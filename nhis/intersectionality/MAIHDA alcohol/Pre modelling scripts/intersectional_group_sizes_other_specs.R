# This script checks the group sizes for various potential intersectional group considerations.

setwd("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_code/nhis/intersectionality/")

# Load relevant packages
library(tidyr)
library(dplyr)

# Read in transformed data (for drinkers only)
transformed_drinkers <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/cleaned_data/log_transformed_drinkers.RDS")

### Check intersectional group sizes with various subgroup splits

## Option A - birth cohort and 3 age cats
data_intersections_A <- transformed_drinkers %>% 
  group_by(SEX, race_5_cats, education_3_cats, birth_cohort, age_3_cats) %>% 
  mutate(intersections = cur_group_id())
# Count people in each group
data_intersections_A <- data_intersections_A %>%
  group_by(intersections) %>%
  mutate(count=n()) %>%
  distinct(intersections, .keep_all = TRUE) %>% 
  dplyr::select(intersections, count, SEX, race_5_cats, education_3_cats, birth_cohort, age_3_cats) %>% arrange(count)
# Number of intersectional groups with more than 20 people in the group
sum(data_intersections_A$count >= 20)  # 229 out of 450 (51%)

## Option B - decade and age 3 cats
data_intersections_B <- transformed_drinkers %>% 
  group_by(SEX, race_5_cats, education_3_cats, decade, age_3_cats) %>% 
  mutate(intersections = cur_group_id())
# Count people in each group
data_intersections_B <- data_intersections_B %>%
  group_by(intersections) %>%
  mutate(count=n()) %>%
  distinct(intersections, .keep_all = TRUE) %>% 
  dplyr::select(intersections, count, SEX, race_5_cats, education_3_cats, decade, age_3_cats) %>% arrange(intersections)
# % of intersectional groups with more than 20 people in the group
sum(data_intersections_B$count >= 20) # 175 out of 180 (97%)

## Option C - decade and age 4 cats
data_intersections_C <- transformed_drinkers %>% 
  group_by(SEX, race_5_cats, education_3_cats, decade, age_4_cats) %>% 
  mutate(intersections = cur_group_id())
# Count people in each group
data_intersections_C <- data_intersections_C %>%
  group_by(intersections) %>%
  mutate(count=n()) %>%
  distinct(intersections, .keep_all = TRUE) %>% 
  dplyr::select(intersections, count, SEX, race_5_cats, education_3_cats, decade, age_4_cats) %>%
  arrange(count)
# Number of intersectional groups with less than 20 people in the group
sum(data_intersections_C$count >= 20) # 211 out of 240 (88%)

# Option B has highest % of intersectional groups with n>20
saveRDS(data_intersections_B, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/cleaned_data/intersections_count.RDS")

