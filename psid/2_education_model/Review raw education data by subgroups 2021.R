# Exploration of education categories by subgroup

library(dplyr)
library(ggplot2)
setwd("C:/Users/cmp21seb/Documents/SIMAH")

all_data <- read.csv("SIMAH_workplace/PSID/cleaned data/all_data_all_years_highest_priority_race.csv")

n_individuals_all_data <- n_distinct(all_data$uniqueID) # 84,121 individuals in the data

# Generate columns for education as want them named in the graph
plot_data <- all_data %>%
  mutate(education_cat=ifelse(grepl("College", education_cat), "4+ years of college",
                          ifelse(grepl("SomeC", education_cat), "Some college", 
                                 ifelse(grepl("LEHS", education_cat), "High school or less", NA))),
         education_cat=factor(education_cat,levels=c("4+ years of college", "Some college", "High school or less", NA)))

# Remove education NA
education_no_na <- plot_data %>% filter(!is.na(education_cat))
n_education_individuals <- n_distinct(education_no_na$uniqueID) # 58,576 individuals with education data
n_education_na_individuals <- n_individuals_all_data - n_education_individuals # 25,545 individuals with missing education data


### Plots

## Including NAs

# Full sample
g_na <- ggplot(plot_data, aes(x = year))
g_na + geom_bar(aes(fill=education_cat))+
  ggtitle("Trends in educational attainment over time")+
  scale_x_continuous(n.breaks=45, limits = c(1967,2022))+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y= "Count", colour = "Educational attainment")

# Excluding NAs
g <- ggplot(education_no_na, aes(x = year))
g + geom_bar(aes(fill=education_cat)) 

# By sex
g + geom_bar(aes(fill=education_cat)) +
  ggtitle("Trends in educational attainment over time")+
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(cols=vars(sex)) +
  labs(y= "Count", colour = "Educational attainment")

# By race
g + geom_bar(aes(fill=education_cat))+
  ggtitle("Trends in educational attainment over time")+
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(cols=vars(race_new)) +
  labs(y= "Count", colour = "Educational attainment")

# By age

# Explore issue with negative age values
review_age <- plot_data %>% filter(age<0) %>%
  select(year, birthyear, age, uniqueID)
n_distinct(review_age$uniqueID) # 45,102 individuals with incorrect age data (negative values) based on method of subtracting birth year from survey year
