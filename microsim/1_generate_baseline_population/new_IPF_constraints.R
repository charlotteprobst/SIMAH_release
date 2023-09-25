# processing ACS target data for education transitions calibration
library(tidyverse)
library(ipumsr)

WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
setwd(WorkingDirectory)

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi <- read_ipums_ddi("SIMAH_workplace/ACS/usa_00038.xml")
data <- read_ipums_micro(ddi)

targets <- data %>% 
  filter(AGE<=79) %>% 
  mutate(SEX = ifelse(SEX==1, "M","F"),
         AGECAT = cut(AGE, breaks=c(0,24,34,44,54,64,79),
                      labels=c("18.24","25.34","35.44","45.54","55.64",
                               "65.79")),
         RACE = ifelse(RACE==1, "WHI",
                       ifelse(RACE==2,"BLA","OTH")),
         RACE = ifelse(HISPAN>=1 & HISPAN<=4, "SPA",RACE),
         EDUC =  ifelse(EDUC<=6, "LEHS", ifelse(EDUC>6 & EDUC<=9, "SomeC","College"))) %>% 
  group_by(YEAR, SEX, AGECAT, RACE, EDUC) %>% 
  summarise(n=sum(PERWT)) %>% 
  mutate(STATE = "USA",
    cat = paste0(RACE,SEX,AGECAT,EDUC)) %>% 
  ungroup() %>% 
  dplyr::select(STATE, cat, n) %>% 
  pivot_wider(names_from=cat, values_from=n)



# save the target data
write.csv(targets, "SIMAH_workplace/microsim/1_generating_population/constraints_IPF_2023.csv",row.names=F)
