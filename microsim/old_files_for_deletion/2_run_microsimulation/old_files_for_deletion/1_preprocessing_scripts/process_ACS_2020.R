# SIMAH project Dec 2021 - 
# Processing American Community Survey 2020 experimental data for comparison to microsim outputs
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc()

library(tidyverse)

###set working directory to the main "Microsimulation" folder in your directory 
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "/home/cbuckley/"
setwd(paste(WorkingDirectory))

# read in the data files - first household level files 
household1 <- read_csv("SIMAH_workplace/ACS/csv_hus/psam_husa.csv")
household2 <- read_csv("SIMAH_workplace/ACS/csv_hus/psam_husb.csv")

household <- rbind(household1, household2)
rm(household1, household2)


# now read in the individual level files
person1 <- read_csv("SIMAH_workplace/ACS/csv_pus/psam_pusa.csv")
person2 <- read_csv("SIMAH_workplace/ACS/csv_pus/psam_pusb.csv")

person <- rbind(person1, person2) 
rm(person1, person2)

# joining person and household - unsure if necessary 
combined <- left_join(person, household)

# calculating age / sex / race / splits 
names(combined)
summary <- combined %>% filter(AGEP>=18) %>% 
  mutate(RACE = ifelse(RAC1P==1, "White",
                       ifelse(RAC1P==2, "Black", "Other")),
         RACE = ifelse(HISP!="01", "Hispanic",RACE),
         AGECAT = cut(AGEP, breaks=c(0,24,34,44,54,64,74,79,1000),
                     labels=c("18-24","25-34","35-44",
                              "45-54","55-64","65-74","75-79","80+")),
         SEX = ifelse(SEX==1,"M","F")) %>% 
  group_by(SEX, RACE, AGECAT) %>% 
  summarise(target=sum(PWGTP)) %>% 
    mutate(YEAR=2020)

# now calculate SEs and 95% CIs 
summaryreps <- combined %>% filter(AGEP>=18) %>% 
  mutate(RACE = ifelse(RAC1P==1, "White",
                       ifelse(RAC1P==2, "Black", "Other")),
         RACE = ifelse(HISP!="01", "Hispanic",RACE),
         AGECAT = cut(AGEP, breaks=c(0,24,34,44,54,64,74,79),
                      labels=c("18-24","25-34","35-44",
                               "45-54","55-64","65-74","75-79")),
         SEX = ifelse(SEX==1,"M","F")) %>% 
  dplyr::select(SEX, RACE, AGECAT, PWGTP1:PWGTP80) %>% 
  pivot_longer(cols=c(PWGTP1:PWGTP80)) %>% 
  group_by(SEX, RACE, AGECAT,name) %>% 
  summarise(totalrep = sum(value))

compare <- left_join(summaryreps, summary) %>% 
  group_by(SEX, RACE, AGECAT) %>% 
  mutate(difference = abs(totalrep-target),
         differencesq = difference^2,
         sumdifferencesq = sum(differencesq),
         variance = 4/80*sumdifferencesq,
         SE = sqrt(variance),
         lower_ci = target - 1.645*SE,
         upper_ci = target + 1.645*SE) %>% 
  dplyr::select(YEAR, SEX, RACE, AGECAT, target, lower_ci, upper_ci) %>% 
  distinct()
         
write.csv(compare, "SIMAH_workplace/microsim/1_input_data/ACS_2020_summary.csv", row.names=F)
