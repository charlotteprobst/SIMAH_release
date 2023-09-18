#####SIMAH project 2022 - script for running SIMAH microsimulation model
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

library(devtools)
library(roxygen2)
library(dplyr)
library(tidyr)
library(purrr)
library(fitdistrplus)
library(lhs)
library(truncnorm)
library(data.table)
library(gridExtra)
library(foreach)
library(doParallel)
options(dplyr.summarise.inform = FALSE)

###set working directory to the main "SIMAH" folder in your directory 
# WorkingDirectory <- "U:/SIMAH/"
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "/home/cbuckley/"

DataDirectory <- paste0(WorkingDirectory, "SIMAH_workplace/microsim/1_input_data/")

# load in microsim R package
setwd(paste(WorkingDirectory))

install("SIMAH_code/microsimpackage", dep=T)

source("SIMAH_code/microsim/2_run_microsimulation/0_model_settings.R")

Output <- read_rds("SIMAH_workplace/microsim/2_output_data/policy_experiments/Output_policyexperiments_reps.RDS")

summary_list <- list()
for (disease in diseases) {
  # Generate and add the summary to the list with automatic naming
  summary_list[[paste0(disease)]] <- Output %>%
    ungroup() %>%
    group_by(samplenum, seed, year, agecat, sex, education) %>%
    # left_join(.,age2010) %>%
    summarise(!!paste0("observed_", disease) := (sum(!!sym(paste0(disease)))/inflation_factor),
              !!paste0("simulated_",disease) := (sum(!!sym(paste0("mort_", disease)))/inflation_factor))
  
}

summary <- Output %>% dplyr::select(samplenum,seed, year,agecat, sex,education) %>%
  distinct()

# now join together to make a diseases dataframe for that year
for(disease in diseases){
  summary <-
    left_join(summary, summary_list[[paste0(disease)]], by=c("samplenum","seed","year","sex","agecat","education"))
}

first <- paste0("observed_",diseases[1])
last <- paste0("simulated_", tail(diseases, n=1))

popcount <- Output %>%
  filter(samplenum==1) %>%
  group_by(year, sex, seed, agecat, education) %>%
  summarise(popcount = sum(popcount)) %>% 
  ungroup() %>% group_by(year, sex, agecat, education) %>% 
  summarise(popcount = mean(popcount))

summary <- left_join(summary, popcount)

summary <- summary %>%
  mutate(sex = ifelse(sex=="f","Women","Men"),
         education = factor(education, levels=c("LEHS","SomeC","College"))
  ) %>%
  pivot_longer(first:last) %>%
  separate(name, into=c("type","cause"))

summary <- summary %>%
  pivot_wider(names_from=type, values_from=value)

tojoin <- data.frame(samplenum=1:length(percentreductions), percentreduction=percentreductions)
summary <- left_join(summary, tojoin)

# calculate a crude rate
summary$observed_rate <- (summary$observed/summary$popcount)*100000
summary$simulated_rate <- (summary$simulated/summary$popcount)*100000

# now age standardise
agest <- summary %>% 
  filter(samplenum==1) %>% 
  filter(seed==1) %>% 
  filter(year==2010) %>% 
  filter(cause=="LVDC") %>% 
  group_by(agecat, sex, education) %>% 
  summarise(popcount=sum(popcount)) %>% 
  ungroup() %>% 
  group_by(sex, education) %>% 
  mutate(percent = popcount/sum(popcount)) %>% 
  dplyr::select(agecat, sex, education, percent) %>% distinct()

summary <- left_join(summary, agest)

summary_agest <- summary %>% 
  group_by(samplenum, seed, percentreduction, cause, year, sex, agecat, education) %>% 
  summarise(observed_rate = observed_rate*percent,
            simulated_rate = simulated_rate*percent,
            observed_count = sum(observed)*(1/proportion),
            simulated_count = sum(simulated)*(1/proportion)) %>% 
  ungroup() %>% 
  group_by(samplenum, seed, percentreduction, cause, year, sex, education) %>% 
  summarise(observed_rate = sum(observed_rate),
            simulated_rate = sum(simulated_rate),
            observed_count = sum(observed_count),
            simulated_count = sum(simulated_count)) %>% 
  ungroup() %>% group_by(samplenum, percentreduction, cause, year, sex, education) %>% 
  summarise(observed_rate = mean(observed_rate),
            simulated_rate = mean(simulated_rate),
            observed_count = mean(observed_count),
            simulated_count = mean(simulated_count))

summary_agest <- summary_agest %>% filter(samplenum<=4)
scaleFUN <- function(x) sprintf("%.1f", x)

# create the dataset for analysis
summary_agest <- summary_agest %>% 
  dplyr::select(percentreduction, year, cause, sex, education, observed_rate, simulated_rate, observed_count,
                simulated_count) %>% 
  mutate(percentreduction = paste0(percentreduction*100, "%"),
         education = recode(education, "LEHS"="High school or less",
                            "SomeC"="Some college",
                            "College"="College +"),
         cause = recode(cause, "AUD"="Alcohol use disorder",
                        "IJ"="Suicide","LVDC"="Liver cirrhosis"))
summary_agest <- summary_agest %>% filter(percentreduction=="0%")

write.csv(summary_agest, "SIMAH_workplace/microsim/2_output_data/policy_experiments/processed_policy_data.csv",
          row.names=F)

