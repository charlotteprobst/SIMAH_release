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
# library(tidyverse)
# library(naniar)
library(splitstackshape) 
library(truncnorm)

# CB laptop directory
# wd <- "~/Google Drive/SIMAH Sheffield/"
# CB desktop directory
# wd <- "G:/My Drive/SIMAH Sheffield/"
# CB server directory
wd <- "/home/cbuckley/"

setwd(wd)

####read in the joined up data files 
dataFiles <- readRDS("SIMAH_workplace/brfss/processed_data/brfss_selected_imputed.RDS")
gc()

source("SIMAH_code/brfss/1_upshift_data/upshift_functions.R")

# bind years together 
# data <- do.call(rbind, dataFiles)
data <- dataFiles
rm(dataFiles)

data <- data %>% filter(State!="Puerto Rico") %>% filter(State!="Guam") %>%
  filter(State!="territories")

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
data <- remove_missing(data)

# apply sample weights - using a factor otherwise dataset is too large 

# for SIMAH - only data post 2000 
data <- data %>% filter(YEAR>=2000)
summary(data$final_sample_weight)

data <- data %>% group_by(YEAR) %>% add_tally(name="originalsample")

data$new_weight <- round(data$final_sample_weight*0.0001,digits=0)

test <- data %>% mutate(flag = ifelse(new_weight<1, "missing","notmissing")) %>% group_by(YEAR, flag) %>% tally() %>% 
  pivot_wider(names_from=flag, values_from=n) %>% 
  mutate(missing = ifelse(is.na(missing), 0, missing),
         percentmissing = missing / (notmissing+missing))
total <- data %>% group_by(YEAR) %>% summarise(sum(new_weight))
sum(total$`sum(new_weight)`)

dataFiles <- list()
for(i in unique(data$YEAR)){
  dataFiles[[paste(i)]] <- data %>% filter(YEAR==i) %>% filter(new_weight>=1)
  dataFiles[[paste(i)]] <- expandRows(dataFiles[[paste(i)]], "new_weight")
  # dataFiles[[paste(i)]]$STATE <- "USA"
}

# resample back to the original sample size

sample_function <- function(data, n_sample){
  percentage <- n_sample/nrow(data)
  ratio <- round((1/percentage)-1)
  times <- ceiling(nrow(data)/ratio+1)
  seq <- c(rep(0, times=ratio),1)
  vector <- rep(seq, times=times)
  # order the data for representativeness
  data <- data[with(data, order(gramsperday, drinkingstatus, sex_recode, age_var, race_eth, education_summary)), ]
  data$tosample <- vector[1:nrow(data)]
  sampled <- data %>% filter(tosample==1)
  return(sampled)
}

sampled <- list()
for(i in names(dataFiles)){
  sampled[[paste(i)]] <- sample_function(dataFiles[[paste(i)]], 100)
}

sampled <- do.call(rbind, sampled)



gc()
# data$new_weight <- round(ifelse(data$YEAR==1984, data$final_sample_weight/1000, data$final_sample_weight/60),digits=0)


# summary(data$final_sample_weight)
# summary(data$new_weight)
# gc()

# create a new dataset with all states - label the state USA and join back with original data
# data <- rbind(data, USA)

# allocate individuals to be monthly/yearly/former drinkers or lifetime abstainers
dataFiles <- lapply(dataFiles, impute_yearly_drinking)
# data <- impute_yearly_drinking(data)

# check imputation has worked
# summary <- data %>% filter(State=="USA") %>% group_by(YEAR, drinkingstatus_detailed) %>%
#   tally() %>% ungroup() %>%
#   group_by(YEAR) %>%
#   mutate(percent = n/sum(n))
# ggplot(data=summary, aes(x=YEAR, y=percent, fill=drinkingstatus_detailed)) +
#   geom_bar(stat="identity",position="stack")

# impute the gpd for the non-30 day drinkers from the NAS data
NASGPD <- read.csv("SIMAH_workplace/brfss/processed_data/NAS_GPD_non30day.csv") %>% 
  rename(sex_recode = SEX, 
         agecat = AGECAT, 
         race_eth=RACE) %>% dplyr::select(-imputeddrinking) %>% 
  mutate(drinkingstatus_detailed = "Yearly drinker")

for(i in names(dataFiles)){

dataFiles[[paste(i)]] <- left_join(dataFiles[[paste(i)]], NASGPD) %>% 
  mutate(gramsperday = ifelse(drinkingstatus_detailed=="Yearly drinker",
                              ALCGPD_non30, gramsperday),
         alc_frequency = ifelse(drinkingstatus_detailed=="Yearly drinker",
                                rtruncnorm(nrow(.), a=0, b=11, mean=2, sd=1), alc_frequency),
         alc_frequency = ifelse(drinkingstatus_detailed=="Yearly drinker" & alc_frequency<1, 1,
                                round(alc_frequency))) %>% 
  dplyr::select(-ALCGPD_non30)
}


# read in APC data - source = NIAAA 
# https://pubs.niaaa.nih.gov/publications/surveillance117/pcyr1970-2019.txt
APC <- process_APC(dataFiles[[1]])

# now join this up with the data
for(i in names(dataFiles)){
dataFiles[[paste(i)]] <- left_join(dataFiles[[paste(i)]], APC)
}

# tally of drinkers in the BRFSS in each year in each State 
tally <- list()
for(i in names(dataFiles)){

tally[[paste(i)]] <- dataFiles[[paste(i)]] %>% group_by(YEAR, State, drinkingstatus_updated) %>% 
  tally() %>% ungroup() %>% 
  group_by(YEAR, State) %>% 
  mutate(percentdrinkers = n/sum(n)) %>% filter(drinkingstatus_updated==1) %>% 
  dplyr::select(-c(n, drinkingstatus_updated))
}

# now calculate the percentage of drinkers in each year in each state
for(i in names(dataFiles)){
dataFiles[[paste(i)]] <- left_join(dataFiles[[paste(i)]],tally[[paste(i)]])
}


# perform the up-shift 
for(i in names(dataFiles)){
dataFiles[[paste(i)]] <- dataFiles[[paste(i)]] %>% group_by(YEAR, State) %>% 
  mutate(BRFSS_APC = mean(gramsperday),                   # calculate BRFSS APC as mean grams per day
         adj_brfss_apc = BRFSS_APC/percentdrinkers,       # adjust the BRFSS APC value based on % of current drinkers
         gramspercapita_90 = gramspercapita_adj1*0.9, #adjust to 90% of APC
         quotient = (gramspercapita_90)/adj_brfss_apc, # adjust to 90% of the APC data
         cr_quotient = (quotient^(1/3)),                  # calculate cube root of quotient 
         gramsperday_upshifted_crquotient = gramsperday*(cr_quotient^2),   # apply cube root quotient to gpd
         frequency_upshifted = alc_frequency*(cr_quotient^2),              # apply cube root quotient to frequency
         frequency_upshifted = round(frequency_upshifted),                #round upshifted frequency - can't drink on 0.4 of a day
         frequency_upshifted = ifelse(frequency_upshifted>30, 30, frequency_upshifted), # cap frequency at 30 days
         quantity_per_occasion_upshifted = gramsperday_upshifted_crquotient/14*30/frequency_upshifted) # recalculate drinks per occasion based on upshifted data 
}


# select variables and save the upshifted data 
for(i in names(dataFiles)){
dataFiles[[paste(i)]] <- dataFiles[[paste(i)]] %>% dplyr::select(YEAR, State, race_eth, sex_recode, age_var, employment, 
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
  mutate(formerdrinker = ifelse(drinkingstatus_detailed=="Former drinker",1,0),
         gramsperday = ifelse(gramsperday>200, 200, gramsperday),
         quantity_per_occasion = ifelse(gramsperday==0, 0, quantity_per_occasion))
}

# save the data as an RDS 
saveRDS(dataFiles, "SIMAH_workplace/brfss/processed_data/upshifted_list_USA.RDS")
dataFiles <- read_rds("SIMAH_workplace/brfss/processed_data/upshifted_list_USA.RDS")

# sample a representative sample of 10,000 for each year 
sample_function <- function(data, n_sample){
  percentage <- n_sample/nrow(data)
  ratio <- round((1/percentage)-1)
  times <- ceiling(nrow(data)/ratio+1)
  seq <- c(rep(0, times=ratio),1)
  vector <- rep(seq, times=times)
  # order the data for representativeness
  data <- data[with(data, order(gramsperday, drinkingstatus_detailed, sex_recode, age_var, race_eth)), ]
  data$tosample <- vector[1:nrow(data)]
  sampled <- data %>% filter(tosample==1)
  return(sampled)
}


sampled <- list()
for(i in names(dataFiles)){
  sampled[[paste(i)]] <- sample_function(dataFiles[[paste(i)]], 100000)
}

sampled <- do.call(rbind, sampled)

# save representative sampled data as a .RDS
saveRDS(dataFiles, "SIMAH_workplace/brfss/processed_data/upshifted_sample_USA.RDS")

data <- readRDS("SIMAH_workplace/brfss/processed_data/brfss_selected_imputed.RDS")

# check comparison for un-weighted, full re-weighted and sample of re-weighted data
unweighted <- data %>% group_by(YEAR, sex_recode, race_eth, education_summary) %>% 
  filter(drinkingstatus==1) %>% 
  summarise(meanGPD= mean(gramsperday)) %>% mutate(type="unweighted") %>% filter(YEAR>=2000) %>% 
  mutate(race_eth = recode(race_eth, "Non-Hispanic White"="White",
                           "Non-Hispanic Black"="Black","Non-Hispanic Other"="Other",
                           "Hispanic"="Hispanic"),
         sex_recode = recode(sex_recode, "Men"="Male","Women"="Female"))

summary <- list()
for(i in names(dataFiles)){
  summary[[paste(i)]] <- dataFiles[[paste(i)]] %>% group_by(YEAR, sex_recode, race_eth, education_summary) %>% 
    filter(drinkingstatus==1) %>% 
    summarise(meanGPD = mean(gramsperday)) %>% mutate(type="full_weighted")
}
summary <- do.call(rbind,summary)

reweighted_sample <- read_rds("SIMAH_workplace/brfss/processed_data/upshifted_sample_USA.RDS") %>% 
  group_by(YEAR, sex_recode, race_eth, education_summary) %>% 
  filter(drinkingstatus==1) %>% 
  summarise(meanGPD = mean(gramsperday)) %>% mutate(type="sampled_weighted")

compare <- rbind(unweighted, reweighted_sample, summary) %>% pivot_wider(names_from=type, values_from=meanGPD)

compare$percentdiff <- ((compare$full_weighted - compare$sampled_weighted) / compare$full_weighted) * 100

library(ggplot2)
ggplot(data=summary, aes(x=YEAR, y=meangpd, colour=race_eth)) + geom_line() + 
  facet_grid(cols=vars(education_summary), rows=vars(sex_recode))
