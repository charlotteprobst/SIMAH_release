# Generate table of intersectional group sizes: full sample

# Read in R packages 
library(tidyverse)

setwd("C:/Users/cmp21seb/Documents/SIMAH/")
data <- readRDS("~/SIMAH/SIMAH_workplace/nhis/intersectionality/MAIHDA alcohol/inputs/grams_data_pre_maihda_drinkers.rds")

# Generate intersections
data_2 <- data %>% 
  group_by(SEX, race_6_cats, education_3_cats, age_diaz) %>% 
  mutate(intersection_ID = cur_group_id()) %>%
  mutate(Intersection = as.character(paste(SEX, age_diaz, race_6_cats, education_3_cats)))

# Check intersectional group sizes
temp <- data_2 %>% 
  group_by(Intersection) %>%
  mutate(count=n())

group_sizes <- temp %>% 
  distinct(Intersection, count) 
group_sizes$percent <- group_sizes$count/sum(group_sizes$count)*100 
group_sizes <- group_sizes %>%
  arrange(desc(percent)) %>%
  mutate(percent = round(percent, 1))

write.csv(group_sizes, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/MAIHDA alcohol/outputs/analytic sample/group sizes all intersections.csv")
