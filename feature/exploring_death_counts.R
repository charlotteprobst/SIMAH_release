# Exploring death counts for each cause of death by subgroup, for the year 2000

# Setup
setwd("C:/Users/cmp21seb/Documents/SIMAH/")
DataDirectory <- "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/1_input_data/"
library(tidyverse)
library(ggplot2)

source("SIMAH_code/microsimpackage/R/load_death_counts.R")

PopulationSize <- 1000000
proportion <- 0.005001422
SelectedState <- "USA"

# Load death counts data
death_counts <- load_death_counts(model="SIMAH", proportion, SelectedState, DataDirectory)

# Filter by year 2000
death_counts_2000 <- death_counts %>% filter(year==2000)

# Disagregate 'cat' into unitary factors for easier filtering
death_counts_2000 <- death_counts_2000 %>% mutate(Race = ifelse(grepl("WHI",cat),"White",
                                                         ifelse(grepl("BLA", cat), "Black",
                                                         ifelse(grepl("SPA", cat),"Hispanic",
                                                         ifelse(grepl("OTH", cat), "Other",NA)))),
                                                  Sex = ifelse(grepl("f", cat), "Female", "Male"),
                                                  Education = ifelse(grepl("SomeC", cat), "Some college",
                                                              ifelse(grepl("College", cat),"College",
                                                              ifelse(grepl("LEHS", cat), "LEHS",NA))),
                                                  Education = fct_relevel(Education, "LEHS", "Some college", "College"),
                                                  Age = ifelse(grepl("18-24", cat), "18-24",
                                                        ifelse(grepl("25-29", cat), "25-29",
                                                        ifelse(grepl("30-34", cat), "30-24",
                                                        ifelse(grepl("35-39", cat), "35-39",
                                                        ifelse(grepl("40-44", cat), "40-44",
                                                        ifelse(grepl("45-49", cat), "45-49",
                                                        ifelse(grepl("50-54", cat), "50-54",
                                                        ifelse(grepl("55-59", cat), "55-59",
                                                        ifelse(grepl("60-64", cat), "60-64",
                                                        ifelse(grepl("65-69", cat), "65-69",
                                                        ifelse(grepl("70-74", cat), "70-74",
                                                        ifelse(grepl("75-79", cat), "75-79", NA)))))))))))))
                                                                                    
# Export into excel for conditional formatting
write.csv(death_counts_2000, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/death_counts_2000.csv")

######## Descriptive stats by subgroup

# NB. inflation factor of 100 and population size of 1,000,000

# Based on following interpretation (values TBC)
# count <0.5	 	        Very rare
# count >0.5 & <1	      Need to inflate
# count >10 & <=100     Some risk of cumsum >1
# count >100	          High risk of cumsum >1


### All diseases, one plot
other_diseases <- data_longer %>% filter(name!="RESTmort")
ggplot(other_diseases, aes(value)) + geom_histogram() + facet_wrap("name") + ggtitle("Mortality counts")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/death_counts_exploration/all_diseases.png",  dpi=300, width = 12, height = 7)

### Disease specific:

data_longer <- pivot_longer(death_counts_2000, cols=LVDCmort:RESTmort)

### IHD
IHD <- data_longer %>% filter(name=="IHDmort")

IHD <- IHD %>% mutate(Value_range = case_when(value<0.5 ~ "Value < 0.5",
                                              value>=0.5 & value<1  ~ "Value >=0.5 & <1",
                                              value>=1 & value<10 ~ "Value >=1, <10",
                                              value>=10 & value <100 ~ "Value >=10, <100",
                                              value>=100 ~ "Value >=100"),
                      Interpretation = case_when(value<0.5 ~ "Very rare - may not need to model",
                                                 value>=0.5 & value<1  ~ "Needs inflation",
                                                 value>=1 & value<10 ~ "Unlikely to be problematic",
                                                 value>=10 & value <100 ~ "Risk of cumsum >1",
                                                 value>=100 ~ "High risk of cumsum >1"))

IHD_problematic_counts <- IHD %>% filter(Interpretation == "Needs inflation"|
                                           Interpretation ==  "Risk of cumsum >1"|
                                           Interpretation ==  "High risk of cumsum >1")
write.csv(IHD_problematic_counts, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/death_counts_exploration/Summary tables/IHD_problematic_counts.csv")

IHD_summary <- IHD %>% group_by(Interpretation) %>% count() %>% ungroup() %>% mutate(percent=n/sum(n)*100)
write.csv(IHD_summary, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/death_counts_exploration/Summary tables/IHD_summary.csv")

# IHD by age
data_longer %>% filter(name=="IHDmort") %>% ggplot(aes(value)) + 
  geom_histogram() + facet_wrap("Age") + ggtitle("IHD mortality counts")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/death_counts_exploration/IHDmort_age.png",  dpi=300, width = 12, height = 7)

# IHD by sex
data_longer %>% filter(name=="IHDmort") %>% ggplot(aes(value)) + 
  geom_histogram() + facet_wrap("Sex") + ggtitle("IHD mortality counts")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/death_counts_exploration/IHDmort_sex.png",  dpi=300, width = 12, height = 7)

# IHD by SES
data_longer %>% filter(name=="IHDmort") %>% ggplot(aes(value)) + 
  geom_histogram() + facet_wrap("Education") + ggtitle("IHD mortality counts")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/death_counts_exploration/IHDmort_education.png",  dpi=300, width = 12, height = 7)


# DM
DM <- data_longer %>% filter(name=="DMmort")

DM <- DM %>% mutate(Value_range = case_when(value<0.5 ~ "Value < 0.5",
                                              value>=0.5 & value<1  ~ "Value >=0.5 & <1",
                                              value>=1 & value<10 ~ "Value >=1, <10",
                                              value>=10 & value <100 ~ "Value >=10, <100",
                                              value>=100 ~ "Value >=100"),
                      Interpretation = case_when(value<0.5 ~ "Very rare - may not need to model",
                                                 value>=0.5 & value<1  ~ "Needs inflation",
                                                 value>=1 & value<10 ~ "Unlikely to be problematic",
                                                 value>=10 & value <100 ~ "Risk of cumsum >1",
                                                 value>=100 ~ "High risk of cumsum >1"))

DM_problematic_counts <- DM %>% filter(Interpretation == "Needs inflation"|
                                           Interpretation ==  "Risk of cumsum >1"|
                                           Interpretation ==  "High risk of cumsum >1")
write.csv(DM_problematic_counts, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/death_counts_exploration/Summary tables/DM_problematic_counts.csv")

DM_summary <- DM %>% group_by(Interpretation) %>% count() %>% ungroup() %>% mutate(percent=n/sum(n)*100)
write.csv(DM_summary, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/death_counts_exploration/Summary tables/DM_summary.csv")

# DM by age
data_longer %>% filter(name=="DMmort") %>% ggplot(aes(value)) + 
  geom_histogram() + facet_wrap("Age") + ggtitle("DM mortality counts")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/death_counts_exploration/DMmort_age.png",  dpi=300, width = 12, height = 7)

# DM by sex
data_longer %>% filter(name=="DMmort") %>% ggplot(aes(value)) + 
  geom_histogram() + facet_wrap("Sex") + ggtitle("DM mortality counts")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/death_counts_exploration/DMmort_sex.png",  dpi=300, width = 12, height = 7)

# DM by SES
data_longer %>% filter(name=="DMmort") %>% ggplot(aes(value)) + 
  geom_histogram() + facet_wrap("Education") + ggtitle("DM mortality counts")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/death_counts_exploration/DMmort_education.png",  dpi=300, width = 12, height = 7)
