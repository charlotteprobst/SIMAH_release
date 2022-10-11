# script for SIMAH project converting between categorical and continuous GPD 
# data exploration
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

library(tidyverse)
library(ggplot2)

# CB laptop directory
#wd <- "~/Google Drive/SIMAH Sheffield/"

# CK laptop directory
wd <- "/Users/carolinkilian/Desktop/"

setwd(wd)

# read data
dat <- readRDS("SIMAH_workplace/brfss/processed_data/BRFSS_upshifted_1984_2020_final.RDS") %>% filter(YEAR>=2000)

# alcohol categories (simah definition)
dat <- dat %>% 
  mutate(AlcCAT.simah = factor(ifelse(sex_recode=="Male" & gramsperday>0 &
                                       gramsperday<=40, "Low risk",
                                    ifelse(sex_recode=="Female" & gramsperday>0 &
                                             gramsperday<=20, "Low risk",
                                           ifelse(sex_recode=="Male" & gramsperday>40 &
                                                    gramsperday<=60, "Medium risk",
                                                  ifelse(sex_recode=="Female" & gramsperday>20 &
                                                           gramsperday<=40, "Medium risk",
                                                         ifelse(sex_recode=="Male" & gramsperday>60 &
                                                                  gramsperday<=100, "High risk",
                                                                ifelse(sex_recode=="Female" & gramsperday>40 &
                                                                         gramsperday<=60, "High risk", 
                                                                       ifelse(sex_recode=="Male" & gramsperday>100,
                                                                              "Very high risk", 
                                                                              ifelse(sex_recode=="Female" & gramsperday>60,
                                                                                     "Very high risk", NA)))))))), 
                        levels = c("Low risk", "Medium risk", "High risk", "Very high risk")))

# random sample for plotting
rand <- dat %>%
  filter(gramsperday>0) %>%
  group_by(YEAR, State) %>%
  sample_frac(0.1)
  
# distribution by alcohol category and sex
rand <- rand %>%
  group_by(sex_recode, AlcCAT.simah) %>%
  mutate(median=median(gramsperday))

ggplot(data=rand, aes(x=gramsperday)) +
  geom_histogram(aes(color=as.factor(sex_recode)), binwidth = 1, fill = "white") +
  geom_vline(aes(xintercept=median, color=as.factor(sex_recode)), linetype="dashed") + 
  facet_wrap(~ AlcCAT.simah, scales="free") +
  theme(legend.position="bottom")

# distribution by sociodemographic groups 
  
  # broad age group
  rand <- rand %>%
    mutate(ageCAT = ifelse(age_var<30, "18-29", ifelse(age_var>=30 & age_var<40, "30-39", ifelse(age_var>=40 & age_var<50, "40-49",
                        ifelse(age_var>=50 & age_var<60, "50-59", ifelse(age_var>=60 & age_var<70, "60-69", ifelse(age_var>=70, "70+", NA))))))) %>%
    group_by(sex_recode, ageCAT) %>%
    mutate(median=median(gramsperday))
  
  ggplot(data=rand[rand$gramsperday<100,], aes(x=gramsperday)) +
    geom_histogram(aes(color=as.factor(ageCAT)), binwidth = 1, fill = "white") +
    geom_vline(aes(xintercept=median, color=as.factor(ageCAT)), linetype="dashed") + 
    facet_wrap(~ as.factor(sex_recode), scales="free") +
    theme(legend.position="bottom")
  
  # education
  rand <- rand %>%
    group_by(sex_recode, ageCAT, education_summary) %>%
    mutate(median=median(gramsperday))
  
  ggplot(data=rand[rand$gramsperday<100 & rand$sex_recode=="Male",], aes(x=gramsperday)) +
    geom_histogram(aes(color=as.factor(education_summary)), binwidth = 0.5, fill = "white") +
    geom_vline(aes(xintercept=median, color=as.factor(education_summary)), linetype="dashed") + 
    facet_wrap(~ as.factor(ageCAT), scales="free") +
    theme(legend.position="bottom")
  
  # race/ethnicity
  rand <- rand %>%
    group_by(sex_recode, ageCAT, race_eth) %>%
    mutate(median=median(gramsperday))
  
  ggplot(data=rand[rand$gramsperday<100 & rand$sex_recode=="Male",], aes(x=gramsperday)) +
    geom_histogram(aes(color=as.factor(race_eth)), binwidth = 0.5, fill = "white") +
    geom_vline(aes(xintercept=median, color=as.factor(race_eth)), linetype="dashed") + 
    facet_wrap(~ as.factor(ageCAT), scales="free") +
    theme(legend.position="bottom")
  
# distribution by year
  rand <- rand %>%
    ungroup() %>%
    mutate(yearCAT = ifelse(YEAR<=2005, "2000-2005", ifelse(YEAR>2005 & YEAR<=2010, "2006-2010", 
                         ifelse(YEAR>2010 & YEAR<=2015, "2011-2015", ifelse(YEAR>2015, "2016-2020", NA))))) %>%
    group_by(sex_recode, ageCAT, yearCAT) %>%
    mutate(median=median(gramsperday))
  
  ggplot(data=rand[rand$gramsperday<100,], aes(x=gramsperday)) +
    geom_histogram(aes(color=as.factor(yearCAT)), binwidth = 0.5, fill = "white") +
    geom_vline(aes(xintercept=median, color=as.factor(yearCAT)), linetype="dashed") + 
    facet_grid(as.factor(ageCAT) ~ as.factor(sex_recode), scales="free") +
    theme(legend.position="bottom")
  



