# SIMAH project 2023 - calculating alcohol use by SES in NSDUH data

# Read in libraries
library(haven)
library(dplyr)
library(readr)
library(labelled)
library(survey)
library(tidyverse)
library(data.table)
library(foreign)
library(readstata13)

WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
setwd(WorkingDirectory)

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

names <- Sys.glob(c("SIMAH_workplace/nsduh/raw_data/*.DTA",
                    "SIMAH_workplace/nsduh/raw_data/*dta"))

source("SIMAH_code/drinking_by_SES/nsduh_functions.R")

dataFiles <- lapply(names, read.dta13, select.cols=c("EDUCCAT2", "eduhighcat", "EDUHIGHCAT",
                                                                    "AGE2", "IRSEX", "irsex", "NEWRACE2",
                                                                      "ALCYRTOT","alcyrtot", "ALCDAYS", "alcdays",
                                                                    "NODR30A","nodr30a", "ALCUS30D", "verep","vestr",
                                                                    "VEREP","VESTR","ANALWT_C", "ANALWTQ1Q4_C", "VESTRQ1Q4_C"))
names <- parse_number(names)
names <- gsub("-","",names)
names(dataFiles) <- names


for(i in names(dataFiles)){
  dataFiles[[i]]$Year <- as.numeric(names(dataFiles[i]))

}

dataFiles <- lapply(dataFiles, recode_education)
dataFiles <- lapply(dataFiles, recode_ageracesex)
dataFiles <- lapply(dataFiles, recode_drinking)
dataFiles <- lapply(dataFiles, recode_weights)

for(i in names(dataFiles)){
  print(i)
  print(summary(dataFiles[[i]]$analwt))
}

dataFiles <- lapply(dataFiles, selectvars)

data <- do.call(rbind, dataFiles) %>% filter(education!="12-17yo")

options(survey.lonely.psu = "adjust")

SVYobj <- svydesign(id = ~verep , strata = ~vestr , weights = ~analwt , data = data, nest = TRUE)

summarystats <- svyby(~alc_cat, ~Year+education+sex, SVYobj, svymean, na.rm=T)

alccat_summary <- summarystats %>% 
  pivot_longer(`alc_catCategory I`:`se.alc_catLifetime abstainer`) %>% 
  mutate(measure = ifelse(grepl("se", name), "SE","Mean"),
         alc_cat = gsub("se.alc_cat", "", name),
         alc_cat = gsub("alc_cat","",name),
         education = factor(education, levels=c("LEHS","SomeC","College"))) %>% 
  filter(measure=="Mean") %>% 
  dplyr::select(Year, alc_cat, sex, education, value) %>% 
  pivot_wider(names_from=c(sex, education), values_from=value) %>% 
  dplyr::select(Year, alc_cat, Men_LEHS, Men_SomeC,Men_College,Women_LEHS,Women_SomeC,Women_College)
write.csv(alccat_summary, "SIMAH_workplace/drinking_by_SES/NSDUH_mean_alc_cats.csv")

# now make a survey object for only drinkers and calculate grams per day 
SVYobj <-  svydesign(id = ~verep , strata = ~vestr , weights = ~analwt, data = subset(data,gramsperday>0), nest = TRUE )

summarystats <- svyby(~gramsperday, ~Year+education+sex, SVYobj, svymean, na.rm=T)

gpd_summary <- summarystats %>% 
  dplyr::select(-se) %>% 
  pivot_wider(names_from=c(sex,education), values_from =gramsperday) %>% 
  dplyr::select(Year, Men_LEHS, Men_SomeC,Men_College,Women_LEHS,Women_SomeC,Women_College)

write.csv(gpd_summary, "SIMAH_workplace/drinking_by_SES/NSDUH_mean_GPD.csv")

