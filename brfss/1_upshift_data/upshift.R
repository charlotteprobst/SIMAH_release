# SIMAH October 2021 - code to take the processed BRFSS data file and up-shift to per-capita consumption on a state by state basis
# this code also adjusts the BRFSS monthly alcohol consumption to annual alcohol consumption using data from the NAS 
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

library(foreign)
library(SASxport)
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
wd <- "~/Google Drive/SIMAH Sheffield/"
# CB desktop directory
# wd <- "G:/My Drive/SIMAH Sheffield/"
setwd(wd)

####read in the joined up data files 
data <- readRDS("SIMAH_workplace/brfss/processed_data/brfss_full_selected.RDS")
gc()
data <- do.call(rbind, data)

data <- data %>% filter(State!="Guam") %>% filter(State!="Puerto Rico") %>% filter(State!="territories")

data$StateOrig <- data$State

USA <- data %>% mutate(State="USA")

data <- rbind(data,USA)

source("SIMAH_code/brfss/1_upshift_data/upshift_functions.R")

# some people claim to be drinkers but quantity per occasion =0 
# solution (for now) is to allocate small amount of drinking per occasion 
data$quantity_per_occasion <- ifelse(data$drinkingstatus==1 & data$gramsperday==0,
                                     0.01, data$quantity_per_occasion)
data$gramsperday <- ((data$quantity_per_occasion*data$alc_frequency)/30)*14
summary(data$gramsperday)
summary(data$alc_frequency)
# put cap of 200gpd on grams per day 
data$gramsperday <- ifelse(data$gramsperday>200, 200, data$gramsperday)

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
  mutate(gramsperday = ifelse(drinkingstatus_detailed=="Yearly drinker",
                              ALCGPD_non30, gramsperday),
         alc_frequency = ifelse(drinkingstatus_detailed=="Yearly drinker",
                                rtruncnorm(nrow(.), a=0, b=11, mean=2, sd=1), alc_frequency),
         alc_frequency = ifelse(drinkingstatus_detailed=="Yearly drinker" & alc_frequency<1, 1,
                                round(alc_frequency))) %>% 
  dplyr::select(-ALCGPD_non30)

# read in APC data - source = NIAAA 
# https://pubs.niaaa.nih.gov/publications/surveillance117/pcyr1970-2019.txt
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

# perform the up-shift 
data <- data %>% group_by(YEAR, State) %>% 
  mutate(BRFSS_APC = mean(gramsperday),                   # calculate BRFSS APC as mean grams per day
         adj_brfss_apc = BRFSS_APC/percentdrinkers,       # adjust the BRFSS APC value based on % of current drinkers
         gramspercapita_90 = gramspercapita_adj1*0.9, #adjust to 90% of APC
         quotient = (gramspercapita_90)/adj_brfss_apc, # adjust to 90% of the APC data
         cr_quotient = (quotient^(1/3)),                  # calculate cube root of quotient 
<<<<<<< Updated upstream
         gramsperday_upshifted_crquotient = gramsperday*(cr_quotient^2),   # apply cube root quotient to gpd
         frequency_upshifted = alc_frequency*(cr_quotient^2),              # apply cube root quotient to frequency
         frequency_upshifted = round(frequency_upshifted),                #round upshifted frequency - can't drink on 0.4 of a day
         frequency_upshifted = ifelse(frequency_upshifted>30, 30, frequency_upshifted), # cap frequency at 30 days
         quantity_per_occasion_upshifted = gramsperday_upshifted_crquotient/14*30/frequency_upshifted) # recalculate drinks per occasion based on upshifted data 

=======
         gramsperday_upshifted = gramsperday_adj*(cr_quotient^2),   # apply cube root quotient to gpd
         gramsperday_upshifted_cr = gramsperday_adj*cr_quotient,
         frequency_upshifted = alc_frequency_new*(cr_quotient^2),              # apply cube root quotient to frequency
         frequency_upshifted = round(frequency_upshifted),                #round upshifted frequency - can't drink on 0.4 of a day
         frequency_upshifted = ifelse(frequency_upshifted>30, 30, frequency_upshifted), # cap frequency at 30 days
         quantity_per_occasion_upshifted = gramsperday_upshifted/14*30/frequency_upshifted) # recalculate drinks per occasion based on upshifted data 
>>>>>>> Stashed changes
# adding the regions to the BRFSS 
data <- add_brfss_regions_wet(data)

# save the data with pre and post upshift for generating paper plots
<<<<<<< Updated upstream
# forpaper <- data %>% 
#   dplyr::select(YEAR, State, race_eth, sex_recode, age_var,
#                 education_summary, household_income,
#                 drinkingstatus, alc_frequency, quantity_per_occasion,
#                 gramsperday, 
#                 drinkingstatus_detailed, gramspercapita_adj1, gramspercapita, gramspercapita_90,
#                 gramsperday_upshifted_crquotient, 
#                 frequency_upshifted,
#                 quantity_per_occasion_upshifted)
# saveRDS(data, "SIMAH_workplace/brfss/processed_data/BRFSS_reweighted_upshifted_1984_2020.RDS")

=======
forpaper <- data %>%
  dplyr::select(YEAR, State, race_eth, sex_recode, age_var,
                education_summary, household_income,
                drinkingstatus, alc_frequency, quantity_per_occasion,
                gramsperday,
                drinkingstatus_detailed, gramspercapita_adj1, gramspercapita, gramspercapita_90,
                gramsperday_upshifted, gramsperday_upshifted_cr,
                frequency_upshifted,
                quantity_per_occasion_upshifted, hed)
saveRDS(forpaper, "SIMAH_workplace/brfss/processed_data/BRFSS_upshifted_1984_2020_paper.RDS")

summary <- forpaper %>% filter(State=="USA") %>% 
  group_by(YEAR, sex_recode) %>% 
  mutate(drinkingstatus_updated = ifelse(drinkingstatus_detailed=="Monthly drinker" |
                                           drinkingstatus_detailed=="Yearly drinker", 1,0)) %>% 
  filter(drinkingstatus_updated==1) %>% 
  summarise(monthly5plus = mean(hed, na.rm=T),
            sdmonthly5plus = sd(hed, na.rm=T),
            annual5plus = monthly5plus*12)
write.csv(summary, "SIMAH_workplace/brfss/processed_data/BRFSS_5plus.csv", row.names=F)

summary %>% group_by(sex_recode) %>% summarise(mean(annual5plus))
>>>>>>> Stashed changes
# select variables and save the upshifted data 
data <- data %>% dplyr::select(YEAR, State, StateOrig, region, race_eth, 
                               race_eth_detailed, sex_recode, age_var, employment, 
                               employment_detailed, marital_status_detailed,
                               marital_status,
                               education_summary, 
                               household_income, BMI,
                               mentalhealth,physicalhealth,
                               drinkingstatus_updated,
                               drinkingstatus_detailed, gramsperday_upshifted_crquotient,
                               frequency_upshifted, quantity_per_occasion_upshifted,
                               hed) %>% 
  rename(drinkingstatus = drinkingstatus_updated,
         gramsperday = gramsperday_upshifted_crquotient,
         frequency = frequency_upshifted,
         quantity_per_occasion = quantity_per_occasion_upshifted) %>% 
  mutate(formerdrinker = ifelse(drinkingstatus_detailed=="formerdrinker",1,0),
         gramsperday = ifelse(gramsperday>200, 200, gramsperday))

# data for Maddy APC 
USA <- data %>% filter(State=="USA") %>% ungroup() %>% 
  mutate(birth_year=YEAR-age_var,
         age_cat = cut(age_var,
                       breaks=c(0,20,25,30,40,50,60,70,100),
                       labels=c('18-20','21-25', '26-30', '31-40',
                                '41-50','51-60','61-70', '71+')),
         birth_cohort = cut(birth_year,
                            breaks=c(0,1899,1920,1925,1930,1935,1940,1945,
                                     1950,1955,1960,1965,1970,1975,1980,
                                     1985, 1990, 1995, 2000, 2002),
                            labels=c("<1900","1900-1920","1921-1925","1926-1930",
                                     "1931-1935","1936-1940","1941-1945","1946-1950",
                                     "1951-1955","1956-1960","1961-1965","1966-1970",
                                     "1971-1975","1976-1980","1981-1985","1986-1990",
                                     "1991-1995","1996-2000","2001-2002")),
         race_eth = ifelse(race_eth_detailed=="Non-Hispanic White","White",
                           ifelse(race_eth_detailed=="Non-Hispanic Black","Black",
                                  ifelse(race_eth_detailed=="Hispanic","Hispanic",
                                         ifelse(race_eth_detailed=="Non-Hispanic Asian/PI","Asian",
                                                ifelse(race_eth_detailed=="Non-Hispanic Native American","American Indian","Other ethnicity"))))),
         marital_status = ifelse(marital_status_detailed=="married","Married",
                                 ifelse(marital_status_detailed=="separated" | marital_status_detailed=="divorced", "Divorce/separated",
                                        ifelse(marital_status_detailed=="widowed","Widowed",
                                               ifelse(marital_status_detailed=="nevermarried" | marital_status_detailed=="unmarriedcouple","Never married",NA)))),
         employment_status = ifelse(employment_detailed=="employed","Employed",
                                    ifelse(employment_detailed=="retired","Retired",
                                           ifelse(employment_detailed=="homemaker" | employment_detailed=="unemployed - <1 year" |
                                                  employment_detailed=="unemployed - 1+ years" | employment_detailed=="student" |
                                                    employment_detailed=="unable to work","Unemployed", NA)))) %>% 
  dplyr::select(YEAR, StateOrig, region, sex_recode, race_eth, age_var, age_cat, birth_year, birth_cohort,
                household_income,
                employment_status, marital_status, education_summary, drinkingstatus,
                gramsperday, frequency, hed) %>% 
  rename(year=YEAR, sex=sex_recode, age=age_var, education=education_summary,
         drink_frequency=frequency)

# save the output to a .dta file 
write.dta(USA, "SIMAH_workplace/brfss/processed_data/BRFSS_for_APC_covariates.dta")
