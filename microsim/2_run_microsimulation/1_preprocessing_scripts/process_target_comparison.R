# processing target data for comparison to microsimulation outputs by State 

ACS <- read.csv("1_input_data/ACS_popcounts_unweighted_indage.csv") %>% filter(AGE<=79) %>% 
  mutate(AGECAT = cut(AGE, breaks=c(0,18,24,29,34,39,44,49,54,59,64,69,74,79),
                      labels=c("18","19-24","25-29","30-34","35-39","40-44",
                               "45-49","50-54","55-59","60-64","65-69","70-74","75-79"))) %>% 

  group_by(YEAR,SEX,RACE,
                                                                          AGECAT) %>% 
  filter(STATE==SelectedState) %>% summarise(n=sum(n)) %>% mutate(data = "ACS") %>% 
  filter(AGECAT!="80+") %>% mutate(n=n*proportion)

census2000 <- read.csv("1_input_data/processed_indagesexrace2000.csv") %>% filter(STATE==SelectedState) %>% 
  filter(Age<=79) %>% mutate(agecat = cut(Age, breaks=c(0,18,24,29,34,39,44,49,54,59,64,69,74,79),
                                          labels=c("18","19-24","25-29","30-34","35-39","40-44",
                                                   "45-49","50-54","55-59","60-64","65-69","70-74","75-79"))) %>% 
  group_by(sex, race, agecat) %>% summarise(n=sum(count)) %>% mutate(YEAR=2000, data="Census") %>% 
  rename(SEX=sex, RACE=race, AGECAT=agecat) %>% 
  mutate(RACE = recode(RACE, "BLA"="Black", "SPA"="Hispanic","WHI"="White","OTH"="Other")) %>% 
  mutate(n=n*proportion)
#   
census2010 <- read.csv("1_input_data/processed_indagesexrace2010.csv") %>% filter(STATE==SelectedState) %>% 
  filter(Age<=79) %>% mutate(agecat = cut(Age, breaks=c(0,18,24,29,34,39,44,49,54,59,64,69,74,79),
                                          labels=c("18","19-24","25-29","30-34","35-39","40-44",
                                                   "45-49","50-54","55-59","60-64","65-69","70-74","75-79"))) %>%
  group_by(sex, race, agecat) %>% summarise(n=sum(count)) %>% mutate(YEAR=2010, data="Census") %>% 
  rename(SEX=sex, RACE=race, AGECAT=agecat) %>% mutate(RACE = recode(RACE, "BLA"="Black", "SPA"="Hispanic","WHI"="White","OTH"="Other")) %>% 
  mutate(n=n*proportion)

target <- rbind(ACS, census2000, census2010) %>%
  pivot_wider(names_from=data, values_from=n) %>%
  mutate(target = ifelse(YEAR==2010 | YEAR==2000, Census, ACS),
         target = round(target)) %>%
  dplyr::select(YEAR, SEX, RACE, AGECAT, target)

target <- data.frame(target)

