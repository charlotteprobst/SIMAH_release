#Wrapper code for generating population weights for base population for all states
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc()

######packages######
library(dplyr)
library(knitr)
library(ipfp)
library(tidyr)
library(janitor)
library(stringr)
library(readr)
library(sjmisc)
library(readxl)
library(foreign)
library(splitstackshape)

options(scipen=999)

###set working directory to the main folder
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
setwd(WorkingDirectory)

# which state?
selectedstate <- "USA"

# what size population
n <- 1000000

# read in the processed data containing the population weights to apply
# step (2) to generate the weights must have been run at least once before doing this
data <- read.csv("SIMAH_workplace/microsim/base_population/brfss_with_weights_USA.csv")

population <- data %>% 
  filter(State==selectedstate) %>% #filter on the state you want to model 
  ungroup() %>% 
  mutate(normalised = weight/sum(weight),
         new_weights = ceiling(normalised*n)) %>% 
  expandRows("new_weights") %>% 
  sample_n(n)

# process to make the population in the correct format for modelling 
processed_population <- population %>% 
  mutate(formerdrinker = ifelse(drinkingstatus_detailed=="Former drinker", 1,0),
         sex = ifelse(sex=="M","m","f"),
         # race = ifelse(race=="Black","BLA",
         #               ifelse(race=="White","WHI",
         #                      ifelse(race=="Hispanic","SPA",
         #                             ifelse(race=="Others","OTH",NA))))
         ) %>% 
  dplyr::select(age_var, race, sex, education, drinkingstatus,gramsperday, formerdrinker,
                household_income, BMI) %>% 
  rename(microsim.init.age=age_var, microsim.init.race=race, microsim.init.sex=sex,
         microsim.init.education=education, microsim.init.alc.gpd=gramsperday,
         microsim.init.drinkingstatus=drinkingstatus,
         microsim.init.income=household_income, microsim.init.BMI=BMI) %>% 
  mutate(microsim.init.spawn.year=2000,
         agecat = cut(microsim.init.age,
                      breaks=c(0,24,29,34,39,44,49,54,59,64,69,100),
                      labels=c("18-24","25-29","30-34","35-39","40-44","45-49",
                               "50-54","55-59","60-64","65-69","70-80")))

# save the base population in a sensible location
write.csv(processed_population, paste0("SIMAH_workplace/microsim/1_input_data/agent_files/", selectedstate, "basepop", n, "V4.csv"), row.names=F)

# # add alcohol categories 
processed_population <- code_alcohol_categories(processed_population)
# 
# basepop <- code_alcohol_categories(basepop)
# 
# brfss <- code_alcohol_categories(brfss)
# 
# brfss$year <- brfss$YEAR
# 
# brfsstarget <- generate_alcoholcat_summary(brfss) %>% mutate(type="target")
# 
# processed_population$year <- 2000
# 
# newbasepop <- generate_alcoholcat_summary(processed_population) %>% mutate(type="newbasepop")
# 
# basepop$year <- 2000
# 
# oldbasepop <- generate_alcoholcat_summary(basepop) %>% mutate(type="oldbasepop")
# 
# compare <- rbind(brfsstarget, newbasepop,oldbasepop) %>% 
#   filter(year_cat=="2000-2003")
# 
# ggplot(subset(compare,AlcCAT=="Medium risk"), aes(x=agecat, y=proportion, colour=type, group=type)) + 
#   geom_line() +
#   facet_grid(cols=vars(microsim.init.sex, microsim.init.race), rows=vars(microsim.init.education))
# 
# 
# 
# targets <- targets %>% filter(year_cat=="2000-2003")
#   

