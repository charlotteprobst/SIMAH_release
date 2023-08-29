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

# Based on following interpretation (values TBC)
# count <0.5	 	        Very rare
# count >0.5 & <1	      Need to inflate
# count >10 & <=100     Some risk of cumsum >1
# count >100	          High risk of cumsum >1

# By disease and sex
ggplot(death_counts_2000, aes(LVDCmort)) + geom_histogram() + facet_wrap("Sex") + ggtitle("LVDC mortality counts")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/death_counts_exploration/LVDCmort_sex.png",  dpi=300, width = 12, height = 7)

ggplot(death_counts_2000, aes(HLVDCmort)) + geom_histogram() + facet_wrap("Sex") + ggtitle("HLVDC mortality counts")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/death_counts_exploration/HLVDCmort_sex.png",  dpi=300, width = 12, height = 7)

ggplot(death_counts_2000, aes(DMmort)) + geom_histogram() + facet_wrap("Sex") + ggtitle("DM mortality counts")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/death_counts_exploration/DMmort_sex.png",  dpi=300, width = 12, height = 7)

ggplot(death_counts_2000, aes(IHDmort)) + geom_histogram() + facet_wrap("Sex") + ggtitle("IHD mortality counts")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/death_counts_exploration/IHDmort_sex.png",  dpi=300, width = 12, height = 7)

ggplot(death_counts_2000, aes(ISTRmort)) + geom_histogram() + facet_wrap("Sex") + ggtitle("ISTR mortality counts")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/death_counts_exploration/ISTRmort_sex.png",  dpi=300, width = 12, height = 7)

ggplot(death_counts_2000, aes(HYPHDmort)) + geom_histogram() + facet_wrap("Sex") + ggtitle("HYPHD mortality counts")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/death_counts_exploration/HYPHDmort_sex.png",  dpi=300, width = 12, height = 7)

ggplot(death_counts_2000, aes(AUDmort)) + geom_histogram() + facet_wrap("Sex") + ggtitle("AUD mortality counts")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/death_counts_exploration/AUDmort_sex.png",  dpi=300, width = 12, height = 7)

ggplot(death_counts_2000, aes(UIJmort)) + geom_histogram() + facet_wrap("Sex") + ggtitle("UIJ mortality counts")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/death_counts_exploration/UIJmort_sex.png",  dpi=300, width = 12, height = 7)

ggplot(death_counts_2000, aes(MVACCmort)) + geom_histogram() + facet_wrap("Sex") + ggtitle("MVACC mortality counts")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/death_counts_exploration/MVACCmort_sex.png",  dpi=300, width = 12, height = 7)

ggplot(death_counts_2000, aes(IJmort)) + geom_histogram() + facet_wrap("Sex") + ggtitle("IJ mortality counts")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/death_counts_exploration/IJmort_sex.png",  dpi=300, width = 12, height = 7)

