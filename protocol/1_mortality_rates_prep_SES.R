library(dplyr)
library(tidyr)
library(ggplot2)

k.wd <- c("C:/Users/Marie/Dropbox/NIH2020/")
setwd(k.wd)

# read in the summarised data - comparison of microsimulation and death rates data
df <- read.csv("SIMAH_workplace/protocol/output_data/microsim_deaths_summary.csv") %>% select(-c(percent,rate))

df %>% group_by(year) %>% tally()

# get population counts for each age category - independent of cause of death 
# which causes of death are present in every sub-group in every year? 
# within any cause - 2 sex * 3 edclass * 12 age * 19 year = 1368
sum <- df %>% group_by(year, cause, agecat, edclass, Sex, .drop=FALSE) %>% filter(datatype=="microsim") %>% tally() %>% 
  group_by(cause) %>% mutate(sum=sum(n)) %>% filter(sum==1368)

# only cause with all categories present in all year is all other causes ("REST")
age2018 <- df %>% filter(year==2018) %>% filter(cause=="REST") %>% select(Sex, edclass, agecat, datatype, n)

# check that all categories are represented - should be 2 sex * 3 edclass * 12 age * 2 datatype rows
2*3*12*2==nrow(age2018)

# calculate percentage of individuals in each age category in 2018
age2018 <- age2018 %>% group_by(agecat, datatype) %>% # totals by agecat and datatype
  summarise(n=sum(n)) %>% ungroup() %>% group_by(datatype) %>% #group by everything except age to get proportions
  mutate(percent=n/sum(n)) %>% ungroup() %>%  select(-c(n))

# check all add up to one for each sex/ed/data category
age2018 %>% group_by(datatype) %>% summarise(sum=sum(percent))
age2018 <- data.frame(age2018)

# join 2018 age percentages with data for all years - to only use age splits for 2018
df <- left_join(df, age2018)

options(digits=2)

# check percent adds up to 1 for each cause of death 
check <- df %>% ungroup() %>% group_by(cause, year, datatype) %>% summarise(sum=sum(percent)) %>% ungroup() %>% 
  mutate(sum=round(sum, digits=2)) %>% filter(sum!=1) %>% mutate(cause = factor(cause))

# causes of death not summing to 1 - i.e. missing certain age categories
levels(check$cause)
levels(as.factor(df$cause))

# which causes of death are missing
summary(as.factor(check$cause))
# AUD and HYPHD missing 

# calculate death rate for each row (year, sex, edclass, agecat, cause) per 100,000
df$rate <- (df$totaldeaths/df$n)*100000

# calculate age specific rates - percentage in each age category multiplied by rate per 100,000 in that category
df$agespecificrate <- df$rate*df$percent

# calculate age standardised rates - sum of age-specific rates (i.e. weighted sum)
summary <- df %>% group_by(year, Sex, edclass, cause, datatype) %>% 
  summarise(rate = sum(agespecificrate))

write.csv(summary, "output_data/1_mortality_rates_data_SES.csv")
