# comparing the cirrhosis mortality rates for different causes of deaths
library(tidyverse)
setwd("~/Google Drive/SIMAH Sheffield/")

cirrhosis_by_cause <- read.delim("SIMAH_workplace/mortality/Underlying Cause of Death, 1999-2020-10yrage.txt") %>% 
  filter(Notes!="Total") %>% dplyr::select(-Notes) %>% 
  drop_na() %>% 
  dplyr::select(Year, Gender, Ten.Year.Age.Groups.Code,
                Cause.of.death.Code, Deaths, Population) %>% 
  rename(Age=Ten.Year.Age.Groups.Code, Sex=Gender, ICD=Cause.of.death.Code) %>% 
  mutate(Sex = ifelse(Sex=="Female","Women","Men")) %>% 
  mutate(ICD_code = substr(ICD, 1,3)) 

cirrhosisdeaths <- cirrhosis_by_cause %>% 
  group_by(Year, Sex, Age, ICD_code) %>% 
  summarise(Deaths = sum(Deaths),
            Population = unique(Population),
            Rate = (Deaths / Population) * 100000)

Pop2000 <- cirrhosis_by_cause %>% 
  filter(Year==2000) %>% 
  group_by(Year, Sex, Age) %>% 
  summarise(Population = mean(Population)) %>% ungroup() %>% 
  group_by(Year, Sex) %>% 
  mutate(percent = Population / sum(Population)) %>% 
  ungroup() %>% dplyr::select(Sex, Age, percent)

cirrhosisdeaths <- left_join(cirrhosisdeaths, Pop2000) %>% 
  group_by(Year, Sex, Age, ICD_code) %>% 
  mutate(weightedrate = ((Deaths / Population)*100000) * percent) %>% ungroup() %>% 
  group_by(Year, Sex,ICD_code) %>% summarise(agestrate = sum(weightedrate))

ggplot(data=cirrhosisdeaths, aes(x=Year, y=agestrate, colour=ICD_code)) + geom_line() + 
  facet_grid(cols=vars(Sex)) + theme_bw() + ylab("Age-st mortality per 100,000")

ggsave("SIMAH_workplace/mortality/cirrhosis_by_ICD.png", dpi=300, width=33, height=19,
       units="cm")
