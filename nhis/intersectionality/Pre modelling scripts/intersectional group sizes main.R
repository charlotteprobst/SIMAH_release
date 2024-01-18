# Generate table of intersectional group sizes: full sample

# Read in R packages 
library(tidyverse)

setwd("C:/Users/cmp21seb/Documents/SIMAH/")
data_main <- readRDS("~/SIMAH/SIMAH_workplace/nhis/intersectionality/cleaned_data/new spec August 2023/grams/grams_data_pre_maihda_main.rds")

# Drop individuals age <21
data_0 <- data_main %>% filter(age_diaz!="18-20")

# Keep only the 6 selected race and ethnicity groups
data_1 <- data_0 %>% filter(race_ethnicity==1|race_ethnicity==8|race_ethnicity==2|race_ethnicity==4|
                              race_ethnicity==7|race_ethnicity==3) 

data_1$race_6_cats <- factor(data_1$race_ethnicity,
                             levels = c(1,8,2,4,7,3),
                             labels = c("White", "Hispanic White", 
                                        "Black", "Asian", 
                                        "Multiple race", "AI/AN"))

# Generate intersections
data_2 <- data_1 %>% 
  group_by(SEX, race_6_cats, education_3_cats, age_diaz) %>% 
  mutate(intersection_ID = cur_group_id()) %>%
  mutate(Intersection = as.character(paste(SEX, age_diaz, race_6_cats, education_3_cats)))

# Full sample
# Check intersectional group sizes
temp <- data_2 %>% 
  group_by(Intersection) %>%
  mutate(count=n())

group_sizes <- temp %>% 
  distinct(Intersection, count) 
group_sizes$percent <- group_sizes$count/sum(group_sizes$count)*100 
group_sizes <- group_sizes %>% arrange(desc(percent))

write.csv(group_sizes, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/170124/intersectional group sizes full sample.csv")

# Drinkers only

# Check intersectional group sizes
temp_drinkers <- data_2 %>% 
  filter(ALCSTAT1 == "Current drinker") %>%
  group_by(Intersection) %>%
  mutate(count=n())

group_sizes_drinkers <- temp_drinkers %>% 
  distinct(Intersection, count) 
group_sizes_drinkers$percent <- group_sizes_drinkers$count/sum(group_sizes_drinkers$count)*100 
group_sizes_drinkers <- group_sizes_drinkers %>% arrange(desc(percent))

write.csv(group_sizes_drinkers, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/170124/intersectional group sizes drinkers.csv")
