
# SIMAH - NESARC Alcohol Transitions
# Data Analysis

library(tidyverse)  # data management
library(plotly)
library(htmlwidgets)


 
# Specify the data and output file locations
data    <- "C:/Users/klajd/Documents/2021 CAMH/SIMAH/SIMAH_workplace/nesarc/Processed data/"  # Location of data
output  <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nesarc/Beverage preferences/"  # Location of output

# Load data and edit data -----------------------------------------------------------------------------------------------
nesarc <- readRDS(paste0(data, "nesarc_clean.rds")) %>%
  dplyr::select(wave, race.factor, edu3, alc4.factor, age3, age7, female, coolers_prop, beers_prop, wine_prop, liquor_prop) %>%
  rename(race4 = race.factor, AlcUse = alc4.factor)
  
nesarc3 <- readRDS(paste0(data, "nesarc3_clean.rds")) %>%
  mutate(wave=3) %>%
  dplyr::select(wave, race.factor, edu3.factor, alc4.factor, age3, age7, female, coolers_prop, beers_prop, wine_prop, liquor_prop) %>%
  rename(edu3 = edu3.factor, race4 = race.factor, AlcUse = alc4.factor)

data <- rbind(nesarc, nesarc3) %>% 
  mutate(total_prop = rowSums(select(., coolers_prop, beers_prop, wine_prop, liquor_prop), na.rm=TRUE),
         sex = factor(female, levels=c(0,1), labels=c("Men", "Women"))) %>%
  filter(!is.na(total_prop) & total_prop!=0) %>%
  replace(is.na(.), 0) %>%
  dplyr::select(-total_prop) %>%
  mutate(wave = recode (wave, `1` = "NESARC I", `2` = "NESARC II", `3` = "NESARC III"),
          edu3 = fct_relevel(edu3, "Low", "Med", "High"),
          age7 = fct_relevel(age7, "18-20", "21-25", "26-29", "30-39", "40-49", "50-64", "65+"))


# Stratified by Wave -------------------------------------------------------------------------------------------------
# Proportion by education
edu <- data %>%
  group_by(wave, edu3) %>%
  summarise(across(ends_with("_prop"), mean, na.rm = TRUE)) %>%
  pivot_longer(cols=c("coolers_prop", "beers_prop", "wine_prop", "liquor_prop"), names_to = "type") %>%
  mutate(type = recode (type, "beers_prop" = "Beer", "liquor_prop" = "Liquor", "coolers_prop" = "Coolers", "wine_prop" = "Wine")) %>%
  rename(proportion = value) %>%
  ggplot(aes(type, edu3, fill= proportion)) + geom_tile() + facet_wrap(~wave)

ggplotly(edu) %>% saveWidget(file=paste0(output, "Fig 1A Edu by wave.html"))


# Proportion by race/ethnicity
race <- data %>%
  group_by(wave, race4) %>%
  summarise(across(ends_with("_prop"), mean, na.rm = TRUE)) %>%
  pivot_longer(cols=c("coolers_prop", "beers_prop", "wine_prop", "liquor_prop"), names_to = "type") %>%
  mutate(type = recode (type, "beers_prop" = "Beer", "liquor_prop" = "Liquor", "coolers_prop" = "Coolers", "wine_prop" = "Wine")) %>%
  rename(proportion = value) %>%
  ggplot(aes(type, race4, fill= proportion)) + geom_tile() + facet_wrap(~wave)

ggplotly(race) %>% saveWidget(file=paste0(output, "Fig 1B Race by wave.html"))


# Proportion by education and race/ethnicity
edu_race <- data %>%
  group_by(wave, edu3, race4) %>%
  summarise(across(ends_with("_prop"), mean, na.rm = TRUE)) %>%
  pivot_longer(cols=c("coolers_prop", "beers_prop", "wine_prop", "liquor_prop"), names_to = "type") %>%
  mutate(type = recode (type, "beers_prop" = "Beer", "liquor_prop" = "Liquor", "coolers_prop" = "Coolers", "wine_prop" = "Wine")) %>%
  rename(proportion = value)%>%
  ggplot(aes(type, race4:edu3, fill= proportion)) + geom_tile() + facet_wrap(~wave)

ggplotly(edu_race) %>% saveWidget(file=paste0(output, "Fig 1C Edu_Race by wave.html"))

# Proportion by age group
age7 <- data %>%
  group_by(wave, age7) %>%
  summarise(across(ends_with("_prop"), mean, na.rm = TRUE)) %>%
  pivot_longer(cols=c("coolers_prop", "beers_prop", "wine_prop", "liquor_prop"), names_to = "type") %>%
  mutate(type = recode (type, "beers_prop" = "Beer", "liquor_prop" = "Liquor", "coolers_prop" = "Coolers", "wine_prop" = "Wine")) %>%
  rename(proportion = value) %>%
  ggplot(aes(type, age7, fill= proportion)) + geom_tile() + facet_wrap(~wave)

ggplotly(age7) %>% saveWidget(file=paste0(output, "Fig 1D Age by wave.html"))



# Proportion by age group
sex <- data %>%
  group_by(wave, sex) %>%
  summarise(across(ends_with("_prop"), mean, na.rm = TRUE)) %>%
  pivot_longer(cols=c("coolers_prop", "beers_prop", "wine_prop", "liquor_prop"), names_to = "type") %>%
  mutate(type = recode (type, "beers_prop" = "Beer", "liquor_prop" = "Liquor", "coolers_prop" = "Coolers", "wine_prop" = "Wine")) %>%
  rename(proportion = value) %>%
  ggplot(aes(type, sex, fill= proportion)) + geom_tile() + facet_wrap(~wave)

ggplotly(sex) %>% saveWidget(file=paste0(output, "Fig 1E Sex by wave.html"))




# Stratified by Alcohol Use -----------------------------------------------------------------------------------------------
# Proportion by education
edu <- data %>%
  group_by(AlcUse, edu3) %>%
  summarise(across(ends_with("_prop"), mean, na.rm = TRUE)) %>%
  pivot_longer(cols=c("coolers_prop", "beers_prop", "wine_prop", "liquor_prop"), names_to = "type") %>%
  mutate(type = recode (type, "beers_prop" = "Beer", "liquor_prop" = "Liquor", "coolers_prop" = "Coolers", "wine_prop" = "Wine")) %>%
  rename(proportion = value) %>% 
  ggplot(aes(type, edu3, fill= proportion)) + geom_tile() + facet_wrap(~AlcUse)

ggplotly(edu) %>% saveWidget(file=paste0(output, "Fig 2A Edu by AlcUse.html"))


# Proportion by race/ethnicity
race <- data %>%
  group_by(AlcUse, race4) %>%
  summarise(across(ends_with("_prop"), mean, na.rm = TRUE)) %>%
  pivot_longer(cols=c("coolers_prop", "beers_prop", "wine_prop", "liquor_prop"), names_to = "type") %>%
  mutate(type = recode (type, "beers_prop" = "Beer", "liquor_prop" = "Liquor", "coolers_prop" = "Coolers", "wine_prop" = "Wine")) %>%
  rename(proportion = value) %>%
  ggplot(aes(type, race4, fill= proportion)) + geom_tile() + facet_wrap(~AlcUse)

ggplotly(race) %>% saveWidget(file=paste0(output, "Fig 2B Edu by AlcUse.html"))


# Proportion by education and race/ethnicity
edu_race <- data %>%
  group_by(AlcUse, edu3, race4) %>%
  summarise(across(ends_with("_prop"), mean, na.rm = TRUE)) %>%
  pivot_longer(cols=c("coolers_prop", "beers_prop", "wine_prop", "liquor_prop"), names_to = "type") %>%
  mutate(type = recode (type, "beers_prop" = "Beer", "liquor_prop" = "Liquor", "coolers_prop" = "Coolers", "wine_prop" = "Wine")) %>%
  rename(proportion = value)%>%
  ggplot(aes(type, race4:edu3, fill= proportion)) + geom_tile() + facet_wrap(~AlcUse)

ggplotly(edu_race) %>% saveWidget(file=paste0(output, "Fig 2C Edu_Race by AlcUse.html"))


# Proportion by age
age7 <- data %>%
  group_by(AlcUse, age7) %>%
  summarise(across(ends_with("_prop"), mean, na.rm = TRUE)) %>%
  pivot_longer(cols=c("coolers_prop", "beers_prop", "wine_prop", "liquor_prop"), names_to = "type") %>%
  mutate(type = recode (type, "beers_prop" = "Beer", "liquor_prop" = "Liquor", "coolers_prop" = "Coolers", "wine_prop" = "Wine")) %>%
  rename(proportion = value) %>%
  ggplot(aes(type, age7, fill= proportion)) + geom_tile() + facet_wrap(~AlcUse)

ggplotly(age7) %>% saveWidget(file=paste0(output, "Fig 2D Age by AlcUse.html"))


# Proportion by sex
sex <- data %>%
  group_by(AlcUse, sex) %>%
  summarise(across(ends_with("_prop"), mean, na.rm = TRUE)) %>%
  pivot_longer(cols=c("coolers_prop", "beers_prop", "wine_prop", "liquor_prop"), names_to = "type") %>%
  mutate(type = recode (type, "beers_prop" = "Beer", "liquor_prop" = "Liquor", "coolers_prop" = "Coolers", "wine_prop" = "Wine")) %>%
  rename(proportion = value) %>%
  ggplot(aes(type, sex, fill= proportion)) + geom_tile() + facet_wrap(~AlcUse)

ggplotly(sex) %>% saveWidget(file=paste0(output, "Fig 2E Sex by AlcUse.html"))


