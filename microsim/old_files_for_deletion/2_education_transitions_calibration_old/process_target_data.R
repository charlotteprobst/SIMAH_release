# processing ACS target data for education transitions calibration
library(tidyverse)
library(ipumsr)

WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
setwd(WorkingDirectory)

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi <- read_ipums_ddi("SIMAH_workplace/ACS/usa_00037.xml")
data <- read_ipums_micro(ddi)

targets <- data %>% 
  filter(AGE<=79) %>% 
  mutate(SEX = ifelse(SEX==1, "Men","Women"),
         AGECAT = cut(AGE, breaks=c(0,24,34,44,54,64,79),
                      labels=c("18-24","25-34","35-44","45-54","55-64",
                               "65-79")),
         RACE = ifelse(RACE==1, "White",
                       ifelse(RACE==2,"Black","Others")),
         RACE = ifelse(HISPAN>=1 & HISPAN<=4, "Hispanic",RACE),
         EDUC =  ifelse(EDUC<=6, "LEHS", ifelse(EDUC>6 & EDUC<=9, "SomeC","College"))) %>% 
  group_by(YEAR, SEX, AGECAT, RACE, EDUC) %>% 
  summarise(n=sum(PERWT)) %>% ungroup() %>% 
  group_by(YEAR, SEX, AGECAT, RACE) %>% 
  mutate(prop=n/sum(n),
         SE = (prop*(1-prop)/sum(n)),
         SE = sqrt(SE))

# save the target data
write.csv(targets, "SIMAH_workplace/microsim/2_output_data/education_calibration/target_data.csv",row.names=F)
