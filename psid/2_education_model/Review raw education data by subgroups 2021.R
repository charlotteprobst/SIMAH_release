# Exploration of education categories by subgroup

library(dplyr)
library(ggplot2)
setwd("C:/Users/cmp21seb/Documents/SIMAH")

all_data <- read.csv("SIMAH_workplace/PSID/cleaned data/all_data_1999_2021_highest_priority_race_excl_non_responders.csv")

n_individuals_all_data <- n_distinct(all_data$uniqueID) # 43,884 individuals in the data

# Generate columns for education as want them named in the graph
plot_data <- all_data %>%
  mutate(education_cat=ifelse(grepl("College", education_cat), "4+ years of college",
                          ifelse(grepl("SomeC", education_cat), "Some college", 
                                 ifelse(grepl("LEHS", education_cat), "High school or less", NA))),
         education_cat=factor(education_cat,levels=c("4+ years of college", "Some college", "High school or less", NA)))

# Remove education NA
education_no_na <- plot_data %>% filter(!is.na(education_cat))
n_education_individuals <- n_distinct(education_no_na$uniqueID) # 31,924 individuals with education data
n_education_na_individuals <- n_individuals_all_data - n_education_individuals # 11,960 individuals with missing education data


### Plots

## Including NAs

# Full sample
g_na <- ggplot(plot_data, aes(x = year))
g_na + geom_bar(aes(fill=education_cat))+
  ggtitle("Trends in educational attainment over time")+
  scale_x_continuous(limits = c(1998,2022))+
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

# Line plots method

# draw a plot of all of all age groups
line_plot_data <- plot_data %>% group_by(year, sex, race_new, education_cat) %>% mutate(Tpop = n_distinct(uniqueID))
  
  summarise(TPop=sum(TPop)) %>% 
  mutate(sex = ifelse(sex==1, "Men","Women"), edclass = factor(edclass, levels=c("LEHS","SomeC","College")))

ggplot(data=allages, aes(x=year, y=TPop, colour=sex)) + geom_line(size=1) + facet_grid(cols=vars(edclass), scales="free") +
  theme_bw() + theme(legend.title=element_blank(), legend.position="bottom") + ylim(0,NA) + xlim(2010, 2021)
