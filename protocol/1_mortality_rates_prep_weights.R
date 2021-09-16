library(dplyr)
library(tidyr)
library(ggplot2)

k.wd <- c("C:/Users/Marie/Dropbox/NIH2020/")
k.wd <- c("~/Google Drive/SIMAH Sheffield/")
setwd(k.wd)

df <- read.csv("SIMAH_workplace/protocol/output_data/1_microsim_deaths_summary.csv") 

# only cause with all categories present in all year is all other causes ("REST")
age2018 <- df %>% filter(year==2018) %>% filter(cause=="REST") %>% 
      select(sex, edclass, raceeth, agecat, datatype, n)

# calculate percentage of individuals in each age category in 2018
age2018 <- age2018 %>% group_by(agecat, datatype) %>% #totals by year and datatype
  summarise(n=sum(n)) %>% ungroup() %>% group_by(datatype) %>% #group by everything except age to get proportions
  mutate(percent=n/sum(n)) %>% ungroup() %>% select(-c(n))

# check all add up to one for each sex/ed/data category
check <- age2018 %>% group_by(datatype) %>% summarise(sum=sum(percent))
summary(check$sum)

age2018 <- data.frame(age2018)

write.csv(age2018, "SIMAH_workplace/protocol/output_data/1_population_weights_2018.csv", row.names = F)

