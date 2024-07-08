# comparing alcohol consumption distributions in raw BRFSS data
library(tidyverse)

setwd("~/Google Drive/SIMAH Sheffield")
data <- read_rds("SIMAH_workplace/brfss/processed_data/BRFSS_raw_2000_2020_final.RDS")

deciles <- data %>% 
  drop_na(gramsperday) %>% 
  mutate(yearcat = cut(YEAR,
                       breaks=c(0,2003,2005,2007,2009,2011,2013,
                                2015,2017,2019,2021)),
         deciles = ntile(gramsperday, 10)) %>% 
  group_by(yearcat) %>% 
  mutate(deciles = ntile(gramsperday, 20),
         gramsperday = ifelse(gramsperday>200, 200, gramsperday),
         deciles = cut(gramsperday,
                       breaks=c(0,20,40,60,80,
                                100,120,140,160,180,200),
                       labels=c(1,2,3,4,5,6,7,8,9,10))) %>%
  group_by(yearcat, deciles, sex_recode) %>% 
  summarise(sum = sum(final_sample_weight)) %>% 
  drop_na() %>% 
  filter(yearcat=="(2011,2013]" | yearcat=="(2017,2019]") %>% 
  mutate(deciles = as.numeric(as.character(deciles))) %>% ungroup() %>% 
  group_by(yearcat, sex_recode) %>% 
  mutate(prop = sum / sum(sum))

ggplot(data=subset(deciles,deciles>=3), aes(x=deciles, y=prop, colour=yearcat, fill=yearcat)) +
  geom_bar(stat="identity",position="dodge") + 
  facet_grid(cols=vars(sex_recode))

data$gramsperday <- ifelse(data$gramsperday>200, 200, data$gramsperday)

drinkers <- data %>% filter(gramsperday>0) %>% drop_na(gramsperday) %>% 
  drop_na(sex_recode) %>% 
  mutate(yearcat = cut(YEAR,
                       breaks=c(0,2003,2005,2007,2009,2011,2013,
                                2015,2017,2019,2021))) %>% 
  filter(yearcat=="(2011,2013]" | yearcat=="(2017,2019]")
  

ggplot(data=subset(drinkers, gramsperday>50), 
       aes(x=gramsperday, y=..scaled..,colour=yearcat, weights=final_sample_weight)) + geom_density() +
  facet_grid(cols=vars(sex_recode))

ggplot(data=subset(drinkers, gramsperday>0), 
       aes(x=gramsperday,fill=yearcat, weights=final_sample_weight)) + geom_histogram(position="dodge") +
  facet_grid(cols=vars(sex_recode))

# compare the polarisation for the BRFSS data upshifted
# potentially use non upshifted with a correction (discuss in outcome modelling?)
# does the upshift change the time trends in category membership
# do categorical to cont transformation using old model (potentially non upshifted with adjustment for increasing mean to upshift)
# all of this on new ordinal version of the model (if it works)