# SIMAH October 2021 - code to take the processed BRFSS data file and up-shift to per-capita consumption on a state by state basis
# this code also adjusts the BRFSS monthly alcohol consumption to annual alcohol consumption using data from the NAS 
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

library(foreign)
library(readr)
library(dplyr)
library(tidyr)
library(labelled)
library(sjlabelled)
library(tidyverse)
library(naniar)
library(splitstackshape) 
library(truncnorm)

# CB laptop directory
# wd <- "~/Google Drive/SIMAH Sheffield/"
# CB desktop directory
# wd <- "G:/My Drive/SIMAH Sheffield/"
# SB desktop directory
wd <- "C:/Users/cmp21seb/Documents/SIMAH/"
setwd(wd)

####read in the joined up data files 
data <- readRDS("SIMAH_workplace/brfss/processed_data/brfss_full_2000_2022.RDS")
gc()
data <- do.call(rbind, data)

data <- data %>% filter(State!="Guam") %>% filter(State!="Puerto Rico") %>% filter(State!="territories")

data$StateOrig <- data$State

USA <- data %>% mutate(State="USA")

data <- rbind(data,USA)

source("SIMAH_code/brfss/1_upshift_data/upshift_functions.R")

# some people claim to be drinkers but quantity per occasion =0 
# solution (for now) is to allocate small amount of drinking per occasion 
# data$quantity_per_occasion <- ifelse(data$drinkingstatus==1 & data$gramsperday==0,
#                                      1, data$quantity_per_occasion)
data$gramsperday <- ((data$quantity_per_occasion*data$alc_frequency)/30)*14
data$drinkingstatus <- ifelse(data$gramsperday==0, 0, data$drinkingstatus)
data$drinkingstatus <- ifelse(data$alc_frequency==0, 0, data$drinkingstatus)
summary(data$gramsperday)
summary(data$alc_frequency)

# remove missing data for key variables - age, sex, race, drinking
data <- remove_missing_data(data)

# allocate individuals to be monthly/yearly/former drinkers or lifetime abstainers
data <- impute_yearly_drinking(data)

# check imputation has worked
summary <- data %>% filter(State=="USA") %>% group_by(YEAR, drinkingstatus_detailed) %>%
  tally() %>% ungroup() %>%
  group_by(YEAR) %>%
  mutate(percent = n/sum(n))
ggplot(data=summary, aes(x=YEAR, y=percent, fill=drinkingstatus_detailed)) +
  geom_bar(stat="identity",position="stack")

# impute the gpd for the non-30 day drinkers from the NAS data
NASGPD <- read.csv("SIMAH_workplace/brfss/processed_data/NAS_GPD_non30day.csv") %>% 
  rename(sex_recode = SEX, 
         agecat = AGECAT, 
         race_eth=RACE) %>% dplyr::select(-imputeddrinking) %>% 
  mutate(drinkingstatus_detailed = "Yearly drinker")

data <- left_join(data, NASGPD) %>% 
  mutate(gramsperday_new = ifelse(drinkingstatus_detailed=="Yearly drinker",
                              ALCGPD_non30, gramsperday),
         alc_frequency_new = ifelse(drinkingstatus_detailed=="Yearly drinker",
                                rtruncnorm(nrow(.), a=0, b=11, mean=2, sd=1), alc_frequency),
         alc_frequency_new = ifelse(drinkingstatus_detailed=="Yearly drinker" & alc_frequency_new<1, 1,
                                round(alc_frequency_new))) %>% 
  dplyr::select(-ALCGPD_non30)

# read in APC data - source = NIAAA 
# https://pubs.niaaa.nih.gov/publications/surveillance117/pcyr1970-2020.txt
APC <- process_APC(data)

# now join this up with the data
data <- left_join(data, APC)

# tally of drinkers in the BRFSS in each year in each State 

tally <- data %>% group_by(YEAR, State, drinkingstatus_updated) %>% 
  tally() %>% ungroup() %>% 
  group_by(YEAR, State) %>% 
  mutate(percentdrinkers = n/sum(n)) %>% filter(drinkingstatus_updated==1) %>% 
  dplyr::select(-c(n, drinkingstatus_updated))

# now calculate the percentage of drinkers in each year in each state
data <- left_join(data,tally)

# Calculate confidence interval for each group
conf_intervals <- data %>%
  group_by(YEAR, State) %>%
  summarize(sample_size = length(gramsperday_new),
            mean = mean(gramsperday_new),
            sd = sd(gramsperday_new),
            SE = sd/sqrt(sample_size),
            lower = mean - 1.96*SE,
            upper = mean + 1.96*SE)

# Join the confidence intervals back to the original dataset
data <- left_join(data, conf_intervals, by = c("YEAR", "State"))

# perform the up-shift 
test <- data %>% group_by(YEAR, State) %>% 
  mutate(BRFSS_APC = mean,
         BRFSS_APC_lower = lower,
         BRFSS_APC_upper = upper,
         adj_brfss_apc = BRFSS_APC/percentdrinkers,       # adjust the BRFSS APC value based on % of current drinkers
         adj_brfss_apc_lower = BRFSS_APC_lower/percentdrinkers,
         adj_brfss_apc_upper = BRFSS_APC_upper/percentdrinkers,
         gramspercapita_90 = gramspercapita_adj1*0.9, #adjust to 90% of APC
         quotient = (gramspercapita_90)/adj_brfss_apc, # adjust to 90% of the APC data
         cr_quotient = (quotient^(1/3)),                  # calculate cube root of quotient 
         quotient_lower = (gramspercapita_90)/adj_brfss_apc_lower, # adjust to 90% of the APC data
         cr_quotient_lower = (quotient_lower^(1/3)),                  # calculate cube root of quotient  
         quotient_upper = (gramspercapita_90)/adj_brfss_apc_upper, # adjust to 90% of the APC data
         cr_quotient_upper = (quotient_upper^(1/3)),                  # calculate cube root of quotient  
         gramsperday_upshifted= gramsperday_new*(cr_quotient^2),   # apply cube root quotient to gpd
         gramsperday_upshifted = ifelse(gramsperday_upshifted>200, 200, gramsperday_upshifted), #establish cap
         gramsperday_upshifted_upper = gramsperday_new*(cr_quotient_lower^2),   # apply cube root quotient to gpd
     #    gramsperday_upshifted_upper = ifelse(gramsperday_upshifted_lower>200, 200, gramsperday_upshifted), #establish cap
         gramsperday_upshifted_lower = gramsperday_new*(cr_quotient_upper^2),   # apply cube root quotient to gpd
       #  gramsperday_upshifted_lower = ifelse(gramsperday_upshifted_upper>200, 200, gramsperday_upshifted), #establish cap
         frequency_upshifted = alc_frequency_new*(cr_quotient^2),              # apply cube root quotient to frequency
         frequency_upshifted = round(frequency_upshifted),                #round upshifted frequency - can't drink on 0.4 of a day
         frequency_upshifted = ifelse(frequency_upshifted>30, 30, frequency_upshifted), # cap frequency at 30 days
         quantity_per_occasion_upshifted = gramsperday_upshifted/14*30/frequency_upshifted,
         quantity_per_occasion_upshifted = ifelse(gramsperday_upshifted==0, 0, quantity_per_occasion_upshifted)
         ) # recalculate drinks per occasion based on upshifted data 

test <- data %>% filter(drinkingstatus_updated==1) %>% filter(gramsperday_upshifted==0)
# adding the regions to the BRFSS 
data <- add_brfss_regions(data)

final_version <- data %>%
  dplyr::select(YEAR, surveymonth, surveyyear, State, region, race_eth, sex_recode, age_var,
                education_summary, household_income,
                employment, marital_status, BMI,
                drinkingstatus_detailed, drinkingstatus_updated,
                gramsperday, alc_frequency, quantity_per_occasion,
                gramsperday_upshifted,
                gramsperday_upshifted_lower,
                gramsperday_upshifted_upper,
                frequency_upshifted, 
                quantity_per_occasion_upshifted) %>% 
  rename(gramsperday_raw = gramsperday,
         frequency_raw = alc_frequency,
         quantity_per_occasion_raw = quantity_per_occasion,
         drinkingstatus = drinkingstatus_updated) %>% 
  mutate(gramsperday_upshifted = ifelse(gramsperday_upshifted>200, 200, gramsperday_upshifted),
         formerdrinker = ifelse(drinkingstatus_detailed=="formerdrinker",1,0)) %>% filter(YEAR>=2000)
  
saveRDS(final_version, "SIMAH_workplace/brfss/processed_data/BRFSS_upshifted_CIs_2000_2022_final.RDS")
