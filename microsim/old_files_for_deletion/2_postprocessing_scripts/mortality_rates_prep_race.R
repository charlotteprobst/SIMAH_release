library(dplyr)
library(tidyr)
library(ggplot2)

df <- read.csv("3_output_data/microsim_deaths_summary_race.csv")

# group by only race (not split by sex)
df <- df %>% group_by(year, edclass, raceeth, agecat, cause, datatype) %>% summarise(totaldeaths=sum(totaldeaths),
                                                                                     n=sum(n))

# get population counts for each age category - independent of cause of death 
# which causes of death are present in every sub-group in every year? 
# within any cause -  3 edclass * 12 age * 19 year * 4 race * 2 datatype
sum <- df %>% group_by(year, cause, agecat, raceeth, edclass, .drop=FALSE) %>% filter(datatype=="microsim") %>% tally() %>% 
  group_by(cause) %>% mutate(sum=sum(n))

# only cause with all categories present in all year is all other causes ("REST")
age2018 <- df %>% filter(year==2018) %>% filter(cause=="REST") %>% select(edclass, raceeth, agecat, datatype, n)

# check that all categories are represented - should be 2 sex * 3 edclass * 12 age * 2 datatype rows

# calculate percentage of individuals in each age category in 2018
age2018 <- age2018 %>% group_by(edclass, raceeth, agecat, datatype) %>% #totals by year,sex, edclass, agecat and datatype
  summarise(n=sum(n)) %>% ungroup() %>% group_by(edclass, raceeth, datatype) %>% #group by everything except age to get proportions
  mutate(percent=n/sum(n)) %>% ungroup() %>%  select(-c(n))

# check all add up to one for each sex/ed/data category
check <- age2018 %>% group_by(edclass,raceeth, datatype) %>% summarise(sum=sum(percent))
summary(check$sum)

age2018 <- data.frame(age2018)

# join 2018 age percentages with data for all years - to only use age splits for 2018
df <- left_join(df, age2018)

options(digits=2)

# calculate death rate for each row (year, sex, edclass, agecat, cause) per 100,000
df$rate <- (df$totaldeaths/df$n)*100000

# calculate age specific rates - percentage in each age category multiplied by rate per 100,000 in that category
df$agespecificrate <- df$rate*df$percent

# calculate age standardised rates - sum of age-specific rates (i.e. weighted sum)
summary <- df %>% group_by(year, edclass, raceeth, cause, datatype) %>% 
  summarise(rate = sum(agespecificrate))

write.csv(summary, "3_output_data/1_mortality_rates_data_raceeducation.csv")
