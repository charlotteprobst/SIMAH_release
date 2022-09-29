
# SIMAH - NESARC Alcohol Transitions
# Data Analysis

library(tidyverse)   # data management
library(plotly)      # interactive plots
library(htmlwidgets) # interactive plots
library(skimr)       # descriptive statistics
library(janitor)     # descriptive statistics (tabyl function)
library(knitr)       # formatted tables
 

 
# Specify the data and output file locations
data    <- "C:/Users/klajd/Documents/2021 CAMH/SIMAH/SIMAH_workplace/nesarc/Processed data/"  # Location of data
output  <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nesarc/Beverage preferences/"  # Location of output

# Load data and edit data -----------------------------------------------------------------------------------------------

# Load NESARC I & II data and rename variables for consistency
nesarc1_2 <- readRDS(paste0(data, "nesarc_all.rds")) %>%
  dplyr::select(idnum, wave, race.factor, edu3, alc4.factor, age7, female, alc_daily_g, 
                coolers_prop, beers_prop, wine_prop, liquor_prop, coolers_daily_g, beers_daily_g, wine_daily_g, liquor_daily_g) %>%
  rename(race4 = race.factor, AlcUse4 = alc4.factor)
  
# Load NESARC III data and rename variables for consistency
nesarc3 <- readRDS(paste0(data, "nesarc3_all.rds")) %>%
  mutate(wave=3) %>%
  dplyr::select(idnum, wave, race.factor, edu3.factor, alc4.factor, age7, female, alc_daily_g,
                coolers_prop, beers_prop, wine_prop, liquor_prop, coolers_daily_g, beers_daily_g, wine_daily_g, liquor_daily_g) %>%
  rename(edu3 = edu3.factor, race4 = race.factor, AlcUse4 = alc4.factor)

# Combine data 
data <- rbind(nesarc1_2, nesarc3) %>% 
  mutate(sex = factor(female, levels=c(0,1), labels=c("Men", "Women")),
         total_prop = rowSums(select(., coolers_prop, beers_prop, wine_prop, liquor_prop), na.rm=TRUE), # calculate total proportion as a consistency check
         highest_prop = pmax(coolers_prop, beers_prop, wine_prop, liquor_prop, na.rm=TRUE),
         bev_pref = case_when( coolers_prop==50 & beers_prop==50 ~ "Multi",
                               coolers_prop==50 & wine_prop==50 ~ "Multi",
                               coolers_prop==50 & liquor_prop==50 ~ "Multi",
                               beers_prop==50 & wine_prop==50 ~ "Multi",
                               beers_prop==50 & liquor_prop==50 ~ "Multi",
                               wine_prop==50 & liquor_prop==50 ~ "Multi", 
                               coolers_prop >=50 ~ "Cooler",
                               beers_prop >=50 ~ "Beer",
                               wine_prop >=50 ~ "Wine",
                               liquor_prop >=50 ~ "Liquor",
                               TRUE ~ "No preferene")) %>%
  filter(!is.na(total_prop) & total_prop!=0) %>%            # remove non-drinkers
  mutate(across(c(coolers_prop, beers_prop, wine_prop, liquor_prop), ~ replace_na(.x, 0)),  # impute 0 for missing props
         wave = recode (wave, `1` = "NESARC I", `2` = "NESARC II", `3` = "NESARC III"),
         edu3 = fct_relevel(edu3, "Low", "Med", "High"),
         age7 = fct_relevel(age7, "18-20", "21-25", "26-29", "30-39", "40-49", "50-64", "65+")) %>% 
  dplyr::select(idnum, wave, sex, age7, edu3, race4, AlcUse4, bev_pref, 
                total_prop, coolers_prop, beers_prop, wine_prop, liquor_prop, highest_prop, 
                alc_daily_g, coolers_daily_g, beers_daily_g, wine_daily_g, liquor_daily_g)


# Check frequency of beverage preferences
data %>% tabyl(bev_pref) %>% adorn_pct_formatting(digits = 0)

# compare frequency by wave
data %>% 
  tabyl(bev_pref, wave) %>% 
  adorn_percentages("row") %>% adorn_pct_formatting(digits = 0) %>%
  adorn_ns() 


# mean proportions 
data %>% 
  group_by(wave) %>%
  select(coolers_prop, beers_prop, wine_prop, liquor_prop, 
         alc_daily_g, coolers_daily_g, beers_daily_g, wine_daily_g, liquor_daily_g) %>%
  skim() %>%
  write.csv("results.csv")

data %>%
  group_by(wave) %>%
  count()


# Stratified by Wave -------------------------------------------------------------------------------------------------
# Proportion by education
data %>%
  group_by(wave, edu3) %>%
  summarise(across(ends_with("_prop"), mean, na.rm = TRUE)) %>%
  pivot_longer(cols=c("coolers_prop", "beers_prop", "wine_prop", "liquor_prop"), names_to = "type") %>%
  mutate(type = recode (type, "beers_prop" = "Beer", "liquor_prop" = "Liquor", "coolers_prop" = "Coolers", "wine_prop" = "Wine")) %>%
  rename(proportion = value) %>%
  ggplot(aes(type, edu3, fill= proportion)) + geom_tile() + facet_wrap(~wave)

ggplotly() %>% saveWidget(file=paste0(output, "Fig 1A Edu by wave.html"))


# Proportion by race/ethnicity
data %>%
  group_by(wave, race4) %>%
  summarise(across(ends_with("_prop"), mean, na.rm = TRUE)) %>%
  pivot_longer(cols=c("coolers_prop", "beers_prop", "wine_prop", "liquor_prop"), names_to = "type") %>%
  mutate(type = recode (type, "beers_prop" = "Beer", "liquor_prop" = "Liquor", "coolers_prop" = "Coolers", "wine_prop" = "Wine")) %>%
  rename(proportion = value) %>%
  ggplot(aes(type, race4, fill= proportion)) + geom_tile() + facet_wrap(~wave)

ggplotly() %>% saveWidget(file=paste0(output, "Fig 1B Race by wave.html"))


# Proportion by age group
data %>%
  group_by(wave, age7) %>%
  summarise(across(ends_with("_prop"), mean, na.rm = TRUE)) %>%
  pivot_longer(cols=c("coolers_prop", "beers_prop", "wine_prop", "liquor_prop"), names_to = "type") %>%
  mutate(type = recode (type, "beers_prop" = "Beer", "liquor_prop" = "Liquor", "coolers_prop" = "Coolers", "wine_prop" = "Wine")) %>%
  rename(proportion = value) %>%
  ggplot(aes(type, age7, fill= proportion)) + geom_tile() + facet_wrap(~wave)

ggplotly() %>% saveWidget(file=paste0(output, "Fig 1C Age by wave.html"))


# Proportion by sex
data %>%
  group_by(wave, sex) %>%
  summarise(across(ends_with("_prop"), mean, na.rm = TRUE)) %>%
  pivot_longer(cols=c("coolers_prop", "beers_prop", "wine_prop", "liquor_prop"), names_to = "type") %>%
  mutate(type = recode (type, "beers_prop" = "Beer", "liquor_prop" = "Liquor", "coolers_prop" = "Coolers", "wine_prop" = "Wine")) %>%
  rename(proportion = value) %>%
  ggplot(aes(type, sex, fill= proportion)) + geom_tile() + facet_wrap(~wave)

ggplotly() %>% saveWidget(file=paste0(output, "Fig 1D Sex by wave.html"))



# Proportion by education and race/ethnicity
data %>%
  group_by(wave, edu3, race4) %>%
  summarise(across(ends_with("_prop"), mean, na.rm = TRUE)) %>%
  pivot_longer(cols=c("coolers_prop", "beers_prop", "wine_prop", "liquor_prop"), names_to = "type") %>%
  mutate(type = recode (type, "beers_prop" = "Beer", "liquor_prop" = "Liquor", "coolers_prop" = "Coolers", "wine_prop" = "Wine")) %>%
  rename(proportion = value)%>%
  ggplot(aes(type, race4:edu3, fill= proportion)) + geom_tile() + facet_wrap(~wave)

ggplotly() %>% saveWidget(file=paste0(output, "Fig 1E Edu_Race by wave.html"))



# Stratified by Alcohol Use -----------------------------------------------------------------------------------------------
# Proportion by education
data %>%
  group_by(AlcUse4, edu3) %>%
  summarise(across(ends_with("_prop"), mean, na.rm = TRUE)) %>%
  pivot_longer(cols=c("coolers_prop", "beers_prop", "wine_prop", "liquor_prop"), names_to = "type") %>%
  mutate(type = recode (type, "beers_prop" = "Beer", "liquor_prop" = "Liquor", "coolers_prop" = "Coolers", "wine_prop" = "Wine")) %>%
  rename(proportion = value) %>% 
  ggplot(aes(type, edu3, fill= proportion)) + geom_tile() + facet_wrap(~AlcUse4)

ggplotly() %>% saveWidget(file=paste0(output, "Fig 2A Edu by AlcUse.html"))


# Proportion by race/ethnicity
data %>%
  group_by(AlcUse4, race4) %>%
  summarise(across(ends_with("_prop"), mean, na.rm = TRUE)) %>%
  pivot_longer(cols=c("coolers_prop", "beers_prop", "wine_prop", "liquor_prop"), names_to = "type") %>%
  mutate(type = recode (type, "beers_prop" = "Beer", "liquor_prop" = "Liquor", "coolers_prop" = "Coolers", "wine_prop" = "Wine")) %>%
  rename(proportion = value) %>%
  ggplot(aes(type, race4, fill= proportion)) + geom_tile() + facet_wrap(~AlcUse4)

ggplotly() %>% saveWidget(file=paste0(output, "Fig 2B Edu by AlcUse.html"))


# Proportion by age
data %>%
  group_by(AlcUse4, age7) %>%
  summarise(across(ends_with("_prop"), mean, na.rm = TRUE)) %>%
  pivot_longer(cols=c("coolers_prop", "beers_prop", "wine_prop", "liquor_prop"), names_to = "type") %>%
  mutate(type = recode (type, "beers_prop" = "Beer", "liquor_prop" = "Liquor", "coolers_prop" = "Coolers", "wine_prop" = "Wine")) %>%
  rename(proportion = value) %>%
  ggplot(aes(type, age7, fill= proportion)) + geom_tile() + facet_wrap(~AlcUse4)

ggplotly() %>% saveWidget(file=paste0(output, "Fig 2C Age by AlcUse.html"))


# Proportion by sex
data %>%
  group_by(AlcUse4, sex) %>%
  summarise(across(ends_with("_prop"), mean, na.rm = TRUE)) %>%
  pivot_longer(cols=c("coolers_prop", "beers_prop", "wine_prop", "liquor_prop"), names_to = "type") %>%
  mutate(type = recode (type, "beers_prop" = "Beer", "liquor_prop" = "Liquor", "coolers_prop" = "Coolers", "wine_prop" = "Wine")) %>%
  rename(proportion = value) %>%
  ggplot(aes(type, sex, fill= proportion)) + geom_tile() + facet_wrap(~AlcUse4)

ggplotly() %>% saveWidget(file=paste0(output, "Fig 2D Sex by AlcUse.html"))



# Proportion by education and race/ethnicity
data %>%
  group_by(AlcUse4, edu3, race4) %>%
  summarise(across(ends_with("_prop"), mean, na.rm = TRUE)) %>%
  pivot_longer(cols=c("coolers_prop", "beers_prop", "wine_prop", "liquor_prop"), names_to = "type") %>%
  mutate(type = recode (type, "beers_prop" = "Beer", "liquor_prop" = "Liquor", "coolers_prop" = "Coolers", "wine_prop" = "Wine")) %>%
  rename(proportion = value)%>%
  ggplot(aes(type, race4:edu3, fill= proportion)) + geom_tile() + facet_wrap(~AlcUse4)

ggplotly() %>% saveWidget(file=paste0(output, "Fig 2E Edu_Race by AlcUse.html"))


