# SIMAH - protocol paper. Octber 2021
# This code reads in the microsimulation data for educational attainment proportions 
# and compares them to the observed data sources 
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(readr)
library(plotrix) 

k.wd <- c("C:/Users/Marie/Dropbox/NIH2020/")
k.wd <- c("~/Google Drive/SIMAH Sheffield")
setwd(k.wd)
# read in the summarised data - comparison of microsimulation and death rates data

df <- read.csv("SIMAH_workplace/protocol/output_data/2_microsim_education_summary.csv") %>% 
  mutate(datatype="microsim")

compare <- read.csv("SIMAH_workplace/protocol/output_data/2_summary_education_compare.csv") %>% 
  rename(sex = microsim.init.sex, 
         race = microsim.init.race,
         edclass = microsim.init.education) %>% 
  mutate(sex = recode(sex, "Female"="Women","Male"="Men"),
         race = recode(race, "BLA"="Non-Hispanic Black", "WHI"="Non-Hispanic White",
                       "SPA"="Hispanic","OTH"="Non-Hispanic Other"),
         edclass = recode(edclass, "LEHS"="High school degree or less",
                          "SomeC"="Some college", "College"="College degree or more"))

df <- rbind(df, compare)

# first calculate RMSE overall for differences between microsim and Census, PSID and ACS
# reported in-text in results section 
summary <- df %>% group_by(sex, edclass, datatype, year) %>% 
  summarise(n=sum(n)) %>% ungroup() %>% 
  group_by(sex, datatype, year) %>% 
  mutate(percent=n/sum(n)*100) %>% select(-n) %>% 
  pivot_wider(names_from=datatype, values_from=percent) %>% 
  mutate(errorcensus = Census - microsim,
         errorcensussq = errorcensus^2,
         errorACS = ACS - microsim,
         errorACSsq = errorACS^2,
         errorPSID = PSID - microsim,
         errorPSIDsq = errorPSID^2) %>% 
  ungroup() %>% mutate(RMSEcensus = sqrt(mean(errorcensussq, na.rm=T)),
                       RMSEACS = sqrt(mean(errorACSsq, na.rm=T)),
                       RMSEPSID = sqrt(mean(errorPSIDsq, na.rm=T))) %>% 
  dplyr::select(RMSEcensus, RMSEACS, RMSEPSID) %>% distinct()

# first without race breakdown 
summary <- df %>% group_by(sex, edclass, datatype, year) %>% 
  summarise(n=sum(n)) %>% ungroup() %>% 
  group_by(sex, datatype, year) %>% 
  mutate(percent=n/sum(n)) %>% select(-n) %>% 
  pivot_wider(names_from=datatype, values_from=percent) %>% 
  mutate(differenceACSsq = (abs(microsim-ACS)*100)^2,
         differenceCensussq = (abs(microsim-Census)*100)^2,
         differencePSIDsq = (abs(microsim-PSID)*100)^2) %>% 
  ungroup() %>% group_by(sex,edclass) %>% 
         mutate(RMSEACS = sqrt(mean(differenceACSsq, na.rm=T)),
         RMSEPSID = sqrt(mean(differencePSIDsq, na.rm=T)),
         RMSECensus = sqrt(mean(differenceCensussq, na.rm=T))) %>% 
  dplyr::select(sex, edclass, RMSEACS, RMSEPSID, RMSECensus) %>% distinct() %>% 
  pivot_longer(cols=c(RMSECensus, RMSEACS, RMSEPSID)) %>% 
  mutate(name = gsub("RMSE", "", name)) %>% 
  pivot_wider(names_from=sex, values_from=value)

write.csv(summary, "SIMAH_workplace/protocol/output_data/SuppTable2p1.csv", row.names=F)

# with race breakdown
summary <- df %>% group_by(race, edclass, datatype, year) %>% 
  summarise(n=sum(n)) %>% ungroup() %>% 
  group_by(race, datatype, year) %>% 
  mutate(percent=n/sum(n)) %>% select(-n) %>% 
  pivot_wider(names_from=datatype, values_from=percent) %>% 
  mutate(differenceACSsq = (abs(microsim-ACS)*100)^2,
         differenceCensussq = (abs(microsim-Census)*100)^2,
         differencePSIDsq = (abs(microsim-PSID)*100)^2) %>% 
  ungroup() %>% group_by(race,edclass) %>% 
  mutate(RMSEACS = sqrt(mean(differenceACSsq, na.rm=T)),
         RMSEPSID = sqrt(mean(differencePSIDsq, na.rm=T)),
         RMSECensus = sqrt(mean(differenceCensussq, na.rm=T))) %>% 
  dplyr::select(race, edclass, RMSEACS, RMSEPSID, RMSECensus) %>% distinct() %>% 
  drop_na() %>% 
  pivot_longer(cols=c(RMSECensus, RMSEACS, RMSEPSID)) %>% 
  mutate(name = gsub("RMSE", "", name)) %>% 
  pivot_wider(names_from=race, values_from=value) %>% 
  dplyr::select(edclass, name, `Non-Hispanic Black`, `Non-Hispanic White`,
                `Hispanic`, `Non-Hispanic Other`)

write.csv(summary, "SIMAH_workplace/protocol/output_data/SuppTable2p2.csv", row.names=F)

