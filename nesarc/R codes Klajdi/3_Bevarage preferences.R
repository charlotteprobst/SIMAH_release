
# SIMAH - NESARC Alcohol Transitions
# Data Analysis

library(tidyverse)   # data management
library(plotly)      # interactive plots
library(htmlwidgets) # interactive plots
library(skimr)       # descriptive statistics
library(janitor)     # descriptive statistics (tabyl function)
library(survey)

 
# Specify the data and output file locations
#data    <- "C:/Users/klajd/Documents/2021 CAMH/SIMAH/SIMAH_workplace/nesarc/Processed data/"  # Location of data
#output  <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nesarc/Beverage preferences/"  # Location of output
data    <- "/Users/carolinkilian/Desktop/SIMAH_workplace/nesarc/2_Processed data/"  # Location of data
out  <- "/Users/carolinkilian/Desktop/SIMAH_workplace/nesarc/"  # Location of output

# Load data and edit data -----------------------------------------------------------------------------------------------

# Load NESARC I & II data and rename variables for consistency
#nesarc1_2 <- readRDS(paste0(data, "nesarc_clean.rds")) %>%
#  dplyr::select(idnum, wave, race.factor, edu3, alc4.factor, age3, age7, female, coolers_prop, beers_prop, wine_prop, liquor_prop) %>%
#  rename(race4 = race.factor, AlcUse4 = alc4.factor)

nesarc1 <- readRDS(paste0(data, "nesarc1_clean.rds")) %>%
  mutate(cluster = psu) %>%
  dplyr::select(idnum, cluster, stratum, weight, wave, race.factor, edu3.factor, alc4.factor, age3, age7, female, coolers_prop, beers_prop, beers_prop_wcoolers, wine_prop, liquor_prop) %>%
  rename(edu3 = edu3.factor, race4 = race.factor, AlcUse4 = alc4.factor)

# Load NESARC III data and rename variables for consistency
nesarc3 <- readRDS(paste0(data, "nesarc3_clean.rds")) %>%
  mutate(wave=3, psu = NA) %>%
  dplyr::select(idnum, cluster, stratum, weight, wave, race.factor, edu3.factor, alc4.factor, age3, age7, female, coolers_prop, beers_prop, beers_prop_wcoolers, wine_prop, liquor_prop) %>%
  rename(edu3 = edu3.factor, race4 = race.factor, AlcUse4 = alc4.factor)

# Combine data 
data <- rbind(nesarc1, nesarc3) %>% 
  #nesarc3 %>% 
  mutate(sex = factor(female, levels=c(0,1), labels=c("Men", "Women")),
         age3 = factor(case_when(age7 == "18-20" | age7 == "21-24" ~ "18-24",
                                 age7 == "25-29" | age7 == "30-39" | age7 == "40-49" ~ "25-49",
                                 age7 == "50-64" | age7 == "65+" ~ "50+")),
         total_prop = rowSums(dplyr::select(., coolers_prop, beers_prop, wine_prop, liquor_prop), na.rm=TRUE), # calculate total proportion as a consistency check
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
  filter(!is.na(total_prop) & total_prop!=0) %>%  # remove non-drinkers
  mutate_at(vars(c(coolers_prop, beers_prop, beers_prop_wcoolers, wine_prop, liquor_prop, 
                   total_prop, highest_prop)), ~ifelse(is.na(.), 0, .)) %>% 
  mutate(wave = recode (wave, `1` = "NESARC I", `2` = "NESARC II", `3` = "NESARC III"),
          edu3 = fct_relevel(edu3, "Low", "Med", "High"),
          age3 = fct_relevel(age3, "18-24", "25-49", "50+"), 
          age7 = fct_relevel(age7, "18-20", "21-24", "25-29", "30-39", "40-49", "50-64", "65+")) %>% 
  dplyr::select(idnum, cluster, stratum, weight, wave, sex, age3, edu3, race4, AlcUse4, bev_pref, coolers_prop, beers_prop, beers_prop_wcoolers, wine_prop, liquor_prop, highest_prop) %>%
  filter(AlcUse4 != "Non-drinker") # filter non-drinkers


# Check frequency of beverage preferences
data %>% tabyl(bev_pref) %>% adorn_pct_formatting(digits = 0)

# compare frequency by wave
data %>% 
  tabyl(bev_pref, wave) %>% 
  adorn_percentages("row") %>% adorn_pct_formatting(digits = 0) %>%
  adorn_ns()
                              
# count obs by category
obs <- data %>%
  group_by(sex, age3, edu3, race4, AlcUse4) %>% 
  summarise(n = n()) %>% ungroup() %>% 
  complete(sex, age3, edu3, race4, AlcUse4) %>% filter(AlcUse4 != "Non-drinker")

# select groups with n < 10
select <- obs %>% filter(n < 10 | is.na(n)) %>% mutate(cat = paste0(sex, age3, edu3, race4)) %>% pull(cat)

# combine Cat II and III for n < 10
data <- data %>% mutate(cat = paste0(sex, age3, edu3, race4),
                        AlcUse4 = case_when(
                          cat %in% select & AlcUse4 == "Category II" ~ "Cat II + III",
                          cat %in% select & AlcUse4 == "Category III" ~ "Cat II + III",
                          TRUE ~ AlcUse4))

# calculate new n by group
obs <- data %>% 
  group_by(sex, age3, edu3, race4, AlcUse4) %>% 
  summarise(n = n())

# Weighted proportions by subgroup and alcohol category --------------------------------------------------------------

# for svydesign RSQLite issue, see: https://www.rdocumentation.org/packages/survey/versions/4.4-2/topics/svydesign 
library(RSQLite)
options(survey.lonely.psu="adjust")
wdata <- svydesign(ids = ~cluster, strata = ~stratum, weights = ~weight, nest = T, data = data,
                   dbtype="SQLite", dbname=system.file("api.db", package="survey"))

summary(svyglm(beers_prop_wcoolers ~ wine_prop, design=wdata))
summary(svyglm(beers_prop_wcoolers ~ liquor_prop, design=wdata))

# get weighted mean and SEs
beer <- svyby(~beers_prop_wcoolers, by = ~sex+age3+edu3+race4+AlcUse4, design = wdata, svymean) %>% 
  mutate(beer_se = se) %>% dplyr::select(-se)
wine <- svyby(~wine_prop, by = ~sex+age3+edu3+race4+AlcUse4, design = wdata, svymean) %>% 
  mutate(wine_se = se) %>% dplyr::select(-se)
liquor <- svyby(~liquor_prop, by = ~sex+age3+edu3+race4+AlcUse4, design = wdata, svymean) %>% 
  mutate(liquor_se = se) %>% dplyr::select(-se)

out_data <- left_join(obs, beer) %>% left_join(., wine) %>% left_join(., liquor) %>%
  mutate(beers_prop_wcoolers = case_when(n < 10 ~ NA, TRUE ~ beers_prop_wcoolers), 
         wine_prop = case_when(n < 10 ~ NA, TRUE ~ wine_prop), 
         liquor_prop = case_when(n < 10 ~ NA, TRUE ~ liquor_prop)) 

# duplicate Cat II and III rows and add to output data
sub1 <- out_data %>% filter(AlcUse4 == "Cat II + III") %>% mutate(AlcUse4 = "Category II")
sub2 <- out_data %>% filter(AlcUse4 == "Cat II + III") %>% mutate(AlcUse4 = "Category III")

output <- out_data %>% filter(AlcUse4 != "Cat II + III") %>% rbind(., sub1, sub2) %>% 
  mutate(cat = paste0(sex, edu3, race4, AlcUse4))

# impute missing data by using props from age group 25-49
imp1 <- output %>% filter(is.na(beers_prop_wcoolers) & age3 == "18-24") %>% pull(cat)
imp1 <- output %>% filter(cat %in% imp1 & age3 == "25-49") %>% mutate(age3 = "18-24")

imp2 <- output %>% filter(is.na(beers_prop_wcoolers) & age3 == "50+") %>% pull(cat)
imp2 <- output %>% filter(cat %in% imp2 & age3 == "25-49") %>% mutate(age3 = "50+")
 
output <- output %>% filter(!is.na(beers_prop_wcoolers)) %>% rbind(., imp1, imp2) 

write.csv(output, paste0(out, "NESARC1+3_beverage preference.csv"))

# visualize beverage preferences

pdat <- output %>% dplyr::select(c("sex", "age3", "edu3", "race4", "AlcUse4", "n", "beers_prop_wcoolers", "wine_prop", "liquor_prop")) %>% 
  pivot_longer(cols = c("beers_prop_wcoolers", "wine_prop", "liquor_prop"), 
               names_to = "beverage", values_to = "prop")

ggplot(pdat[pdat$age3 == "25-49" & pdat$n >= 10,]) + 
  geom_col(aes(x = AlcUse4, y = prop, group = as.factor(beverage), fill = as.factor(beverage))) + 
  facet_grid(rows = vars(sex, edu3), cols = vars(race4)) + 
  ggtitle("25-49")

# OLD CODE BY KLAJDI -------------------------------------------------------------------------------------------------

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


