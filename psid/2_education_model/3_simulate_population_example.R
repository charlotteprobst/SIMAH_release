# script to extract summary tables for education for education transitions paper
library(splitstackshape)
library(dplyr)
library(msm)
library(readr)
library(tidyr)
library(readxl)
library(doParallel)
library(foreach)
library(parallel)
library(ggplot2)
library(RColorBrewer)
library(ggalluvial)
library(patchwork)

# setwd("/home/cbuckley")
setwd("~/Google Drive/SIMAH Sheffield/")
# setwd("C:/Users/cmp21seb/Documents/SIMAH/")

# data <- read_csv("SIMAH_workplace/education_transitions/new_PSID_weighted_IDs.csv") # this file doesn't seem to be used anywhere?

source("SIMAH_code/psid/2_education_model/0_generate_population.R")

TPs <- read.csv("SIMAH_workplace/education_transitions/final_models/income_model_TP_6cat_16_new_noint.csv") %>% 
  mutate(racefinal=ifelse(racefinal=="white","White",
                          ifelse(racefinal=="black","Black",
                                 ifelse(racefinal=="hispanic","Hispanic",
                                        ifelse(racefinal=="other","Others",
                                               ifelse(racefinal=="Asian/PI","Others",
                                                      racefinal))))),
         racefinal = factor(racefinal,
                            levels=c("Black","Hispanic","White","AsianPI","Others")),
         sex = ifelse(sex=="male","Men","Women")) %>% filter(incomequintile==1 | incomequintile==5) %>% 
  mutate(incomecat = ifelse(incomequintile==1, "Lowest","Highest"))

population <- generate_population(TPs, 1000000) # population starts all aged 16.

# need to add incomecat to simulate_population function
simulatedpop1999 <- simulate_population(population, TPs, "1999-2009") # only up to age 26 as only simulating 20 years
simulatedpop2009<- simulate_population(population, TPs, "2009-2019") # only up to age 26 as only simulating 20 years

output <- rbind(simulatedpop1999, simulatedpop2009)

# by age 26 where have people ended up
# 6 categories instead of 5 
# Including income as a breakdown
totals <- output %>% filter(age==18 | age==21 | age==24 | age==26) %>% 
  filter(age==26)  %>%
  group_by(sex, racefinal, education, period, incomecat) %>% 
  tally(name="Npergroup") %>% ungroup() %>% group_by(sex, period, incomecat, .drop=FALSE) %>% 
  mutate(TotalN = sum(Npergroup)) %>% ungroup() %>% group_by(sex, racefinal, period, incomecat, .drop=FALSE) %>% 
  pivot_wider(names_from=education, values_from=Npergroup) %>% group_by(sex, period, racefinal, incomecat) %>%
  dplyr::select(TotalN, sex, period, racefinal, incomecat, LHS, HS, SomeC1, SomeC2, SomeC3,College) %>% 
  mutate(sumLHS = sum(c_across(`LHS`:`College`), na.rm=T), # added additional LHS category
         sumHS = sum(c_across(`HS`:`College`), na.rm=T),
         sum1YR = sum(c_across(`SomeC1`:`College`), na.rm=T),
         sum2YR = sum(c_across(`SomeC2`:`College`), na.rm=T),
         sum3YR = sum(c_across(`SomeC3`:`College`), na.rm=T),
         sumCollege = `College`,
         percentLHS = sumLHS/sumLHS,
         percentHS = sumHS/sumLHS,
         percent1yr = sum1YR/sumLHS,
         percent2yr = sum2YR/sumLHS,
         percent3yr = sum3YR/sumLHS,
         percentcollege = sumCollege/sumLHS)

write.csv(totals, "SIMAH_workplace/education_transitions/Summary_categories.csv", row.names=F)

