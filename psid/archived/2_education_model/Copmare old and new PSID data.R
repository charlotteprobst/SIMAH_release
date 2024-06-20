# Exploration of education categories by subgroup

library(dplyr)
library(ggplot2)
library(survey)
library(srvyr)
library(tidyverse)
setwd("C:/Users/cmp21seb/Documents/SIMAH")

old_data <- read.csv("SIMAH_workplace/PSID/cleaned data/alldata_old_1999_2019.csv")
new_data <- read.csv("SIMAH_workplace/PSID/cleaned data/psid_data_1999_2021.csv")

# How many individuals in each dataset
n_distinct(old_data$uniqueID) # 82,573
n_distinct(new_data$uniqueID) # 43,884

############## Compare how many individuals are in both datasets 
all_data <- full_join(old_data,new_data, by=join_by(uniqueID,year))
n_distinct(all_data$uniqueID) # 84,100

comparable_individuals <- all_data %>% filter(uniqueID%in%new_data$uniqueID & uniqueID%in%old_data$uniqueID) 
comparable_IDS <- unique(comparable_individuals$uniqueID) # 42,357
comparable_individuals_summary <- comparable_individuals %>% dplyr::select(uniqueID, year, race_new_unique, final_race_using_method_hierarchy)

# How many people are in the old data who are no longer in the new data
lost_individuals_from_old <- old_data %>% filter(!(uniqueID%in%comparable_individuals$uniqueID)) 
n_distinct(lost_individuals_from_old$uniqueID) # 40,216
  
# How many people are in the new data who weren't in the old data
lost_individuals_from_new <- new_data %>% filter(!(uniqueID%in%comparable_individuals$uniqueID)) 
n_distinct(lost_individuals_from_new$uniqueID) # 1,527

################## Explore who has been dropped from old

# no survey weight
lost_individuals_from_old_1 <- lost_individuals_from_old %>% filter(weight>0) # 572
n_distinct(lost_individuals_from_old_1$uniqueID) # 52 people remaining

# survey year not a noresponse year
lost_individuals_from_old_2 <- lost_individuals_from_old_1 %>% filter(year!=year_nonresponse) # 537
n_distinct(lost_individuals_from_old_2$uniqueID) # 52  people remaining

# age is negative
lost_individuals_from_old_3 <- lost_individuals_from_old_2 %>% filter(is.na(age)|age>0) # 527
n_distinct(lost_individuals_from_old_3$uniqueID) # 52  people remaining

# Summarise the relationship status of the remaining people
lost_individuals_from_old_3 %>% group_by(relationship) %>% count()

# Exclude if race is NA 
lost_individuals_from_old_4 <- lost_individuals_from_old_3 %>% filter(!is.na(race_new_unique)) #84
n_distinct(lost_individuals_from_old_4$uniqueID) # 9 people remaining, although this does not explain it 
# as not dropping NAs in the new data?

############# Explore differences in race between the comparable individuals

same_race <- comparable_individuals %>% filter(race_new_unique==final_race_using_method_hierarchy | (is.na(race_new_unique) & is.na(final_race_using_method_hierarchy)))%>%
  dplyr::select(uniqueID,race_new_unique,final_race_using_method_hierarchy)
same_race_IDS <- unique(same_race$uniqueID) # 40,307
same_race_percent <- length(same_race_IDS)/length(comparable_IDS)*100 # 95.2

different_race <- comparable_individuals %>% filter(race_new_unique!=final_race_using_method_hierarchy)%>%
  dplyr::select(uniqueID,race_new_unique,final_race_using_method_hierarchy)
different_race_IDS <- unique(different_race$uniqueID) # 689
different_race_percent <- length(different_race_IDS)/length(comparable_IDS)*100 # 1.6

# Check no-one with in both categories
conflicting_cats <- comparable_individuals %>% filter(uniqueID%in%same_race_IDS & uniqueID%in%different_race_IDS)

# People with a race in one dataset and NA in the other
race_one_NA <- comparable_individuals %>% filter((is.na(race_new_unique) & !(is.na(final_race_using_method_hierarchy))) | 
                                             (is.na(final_race_using_method_hierarchy) & !(is.na(race_new_unique))))%>%
  dplyr::select(uniqueID,race_new_unique,final_race_using_method_hierarchy)
one_NA_IDS <- unique(race_one_NA$uniqueID) # 10,143
one_NA_only <- comparable_individuals %>% filter((uniqueID%in%one_NA_IDS) &
                                                    !(uniqueID%in%same_race_IDS) & 
                                                        !(uniqueID%in%different_race_IDS))
one_NA_only_IDS <- unique(one_NA_only$uniqueID)
one_NA_percent <- length(one_NA_only_IDS)/length(comparable_IDS)*100 # 3.2
