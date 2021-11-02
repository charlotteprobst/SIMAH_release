# SIMAH October 2021 - code to take the processed BRFSS data file and up-shift to per-capita consumption on a state by state basis
# this code also adjusts the BRFSS monthly alcohol consumption to annual alcohol consumption using data from the NAS 

library(foreign)
library(SASxport)
library(readr)
library(dplyr)
library(tidyr)
library(labelled)
library(sjlabelled)
library(tidyverse)

# CB laptop directory
wd <- "~/Google Drive/SIMAH Sheffield/"
# CB desktop directory
# wd <- "G:/My Drive/SIMAH Sheffield/"
setwd(wd)

####read in the joined up data files 
dataFiles <- readRDS("SIMAH_workplace/brfss/processed_data/brfss_full_selected.RDS")
gc()

source("SIMAH_code/brfss/1_upshift_data/upshift_functions.R")

# not subsetting states - states from common regions are required for the microsim - so all states upshifted

# SIMAH_states <- c("California","Colorado","Florida","Indiana","Kentucky",
#                   "Louisiana","Massachusetts","Michigan","Minnesota","Missouri",
#                   "New York","Oregon","Pennsylvania","Tennessee","Texas")
# # first subset for selected SIMAH states 
# dataFiles <- lapply(dataFiles, subset_SIMAH_states)

# now remove any missing data for key variables 
dataFiles <- lapply(dataFiles, remove_missing)

# first exploring the data for our selected states and for the US current prevalence and quantity (monthly)
summary <- lapply(dataFiles, summariseprevalence)
summary <- do.call(rbind,summary) %>% filter(drinkingstatus==1) %>% 
  drop_na()
ggplot(data=summary, aes(x=YEAR, y=percentage, colour=sex_recode)) + 
  geom_line() + facet_wrap(~agecat+State) + theme_bw() + ylim(0,NA)
# reassign the list such that each state is one element of the list 
data <- do.call(rbind, dataFiles)

# some people claim to be drinkers but quantity per occasion =0 
# solution (for now) is to allocate small amount of drinking per occasion 
data$quantity_per_occasion <- ifelse(data$drinkingstatus==1 & data$gramsperday==0,
                                     0.01, data$quantity_per_occasion)
data$gramsperday <- ((data$quantity_per_occasion*data$alc_frequency)/30)*14
summary(data$gramsperday)

# put cap of 200gpd
data$gramsperday <- ifelse(data$gramsperday>200, 200, data$gramsperday)

USA <- data %>% mutate(State="USA")
data <- rbind(data, USA)

# dataFiles <- list()
# 
# for(i in SIMAH_states){
#   dataFiles[[paste(i)]] <- data %>% filter(State==i)
# }

# allocate individuals to be monthly/yearly/former drinkers or lifetime abstainers
data <- impute_yearly_drinking(data)

# check imputation has worked
summary <- data %>% group_by(YEAR, State, drinkingstatus_detailed) %>% 
  tally() %>% ungroup() %>% 
  group_by(YEAR, State) %>% 
  mutate(percent = n/sum(n))
ggplot(data=summary, aes(x=YEAR, y=percent, fill=drinkingstatus_detailed)) + 
  geom_bar(stat="identity",position="stack") + 
  facet_wrap(~State)

# impute the gpd for the non-30 day drinkers from the NAS data
NASGPD <- read.csv("SIMAH_workplace/brfss/processed_data/NAS_GPD_non30day.csv") %>% 
  rename(sex_recode = SEX, 
         agecat = AGECAT, 
         race_eth=RACE) %>% dplyr::select(-imputeddrinking) %>% 
  mutate(drinkingstatus_detailed = "Yearly drinker")

data <- left_join(data, NASGPD) %>% 
  mutate(gramsperday = ifelse(drinkingstatus_detailed=="Yearly drinker",
                              ALCGPD_non30, gramsperday)) %>% 
  dplyr::select(-ALCGPD_non30)

# read in APC data - source = NIAAA 
# https://pubs.niaaa.nih.gov/publications/surveillance117/pcyr1970-2019.txt
APC <- process_APC(data)

# now join this up with the data
data <- left_join(data, APC)

tally <- data %>% group_by(YEAR, State, drinkingstatus_updated) %>% 
  tally() %>% ungroup() %>% 
  group_by(YEAR, State) %>% 
  mutate(percentdrinkers = n/sum(n)) %>% filter(drinkingstatus_updated==1) %>% 
  dplyr::select(-c(n, drinkingstatus_updated))

# now calculate the percentage of drinkers in each year in each state
data <- left_join(data,tally)

# perform the up-shift 
data <- data %>% group_by(YEAR, State) %>% 
  mutate(BRFSS_APC = mean(gramsperday),
         adj_brfss_apc = BRFSS_APC/percentdrinkers,
         quotient = (gramsperday_adj1*0.9)/adj_brfss_apc,
         cr_quotient = (quotient^(1/3)),
         gramsperday_upshifted_quotient = gramsperday*(quotient),
         gramsperday_upshifted_crquotient = gramsperday*(cr_quotient^2),
         frequency_upshifted = alc_frequency*(cr_quotient^2),
         frequency_upshifted = ifelse(frequency_upshifted>30, 30, frequency_upshifted),
         quantity_per_occasion_upshifted = gramsperday_upshifted_crquotient/14*30/frequency_upshifted)

# adding the regions to the BRFSS 
data <- add_brfss_regions(data)

# now compare up-shifted to per capita mean data for each state 
GPDsummary <- data %>% group_by(YEAR, State, region) %>% filter(drinkingstatus==1) %>% 
  summarise(meanGPD = mean(gramsperday))

data <- left_join(data, GPDsummary)

compare <- data %>% 
  dplyr::select(YEAR, State, region, gramsperday_adj1, adj_brfss_apc, BRFSS_APC, gramsperday_upshifted_quotient,
                meanGPD,
                gramsperday_upshifted_crquotient) %>% 
  group_by(YEAR, State,region) %>% 
  summarise(SALES=mean(gramsperday_adj1), BASELINE=mean(meanGPD, na.rm=T))

percapita_adjusted <- data %>% 
  filter(drinkingstatus_updated==1) %>% 
  group_by(YEAR,State,region) %>% 
  summarise(UPSHIFTED=mean(gramsperday_upshifted_crquotient, na.rm=T))

compare <- left_join(compare, percapita_adjusted) %>% 
  pivot_longer(cols=SALES:UPSHIFTED)

for(i in unique(data$region)){
ggplot(data=subset(compare, region==i), aes(x=YEAR, y=value, colour=name)) + geom_line(size=1) +
  facet_wrap(~State, scales="fixed") + theme_bw() + theme(legend.title=element_blank(),
                                                         legend.position="bottom",
                                                         panel.background = element_rect(fill="white"),
                                                         strip.background = element_rect(fill="white"),
                                                         text = element_text(size=18)) + 
  ylim(0,NA) + xlim(2000,2020) + ylab("grams per day") + ggtitle(paste(i))
ggsave(paste0("SIMAH_Workplace/brfss/plots/upshift_compare",i,".png"), dpi=300, width=33, height=19, units="cm")
}

# select variables and save the upshifted data 
data <- data %>% dplyr::select(YEAR, State, region, race_eth, sex_recode, age_var, employment, 
                               education_summary, household_income, BMI, drinkingstatus_updated,
                               drinkingstatus_detailed, gramsperday_upshifted_crquotient,
                               frequency_upshifted, quantity_per_occasion_upshifted,
                               hed) %>% 
  rename(drinkingstatus = drinkingstatus_updated,
         gramsperday = gramsperday_upshifted_crquotient,
         frequency = frequency_upshifted,
         quantity_per_occasion = quantity_per_occasion_upshifted) %>% 
  mutate(formerdrinker = ifelse(drinkingstatus_detailed=="formerdrinker",1,0),
         gramsperday = ifelse(gramsperday>200, 200, gramsperday))


saveRDS(data, "SIMAH_workplace/brfss/processed_data/BRFSS_states_upshifted.RDS")

