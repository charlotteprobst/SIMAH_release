# Exploring death counts for each cause of death by subgroup, for the year 2000

# Setup
setwd("C:/Users/cmp21seb/Documents/SIMAH/")
DataDirectory <- "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/1_input_data/"

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
                                                                                    
# Export into excel
write.csv(death_counts_2000, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/death_counts_2000.csv")