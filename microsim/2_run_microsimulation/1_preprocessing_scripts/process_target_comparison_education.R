# processing target data for comparison to microsimulation outputs by State 

ACS <- read.csv("1_input_data/ACS_popcounts_unweighted_indage.csv") %>% filter(AGE<=79) %>% 
  mutate(AGECAT = cut(AGE, breaks=c(0,18,24,29,34,39,44,49,54,59,64,69,74,79),
                      labels=c("18","19-24","25-29","30-34","35-39","40-44",
                               "45-49","50-54","55-59","60-64","65-69","70-74","75-79"))) %>% 
  filter(AGE<=79) %>%

  group_by(YEAR,STATE,SEX,RACE,EDUC) %>% summarise(n=sum(n)) %>% mutate(data = "ACS")

census2000 <- read.csv("1_input_data/education_constraints.csv") %>% 
  rename(SEX=sex,
         EDUC=educationCAT,
         data=datatype,
         RACE = race,
         YEAR=year) %>% 
  mutate(RACE = recode(RACE, "BLA"="Black","SPA"="Hispanic","WHI"="White","OTH"="Other")) %>% 
  group_by(STATE, SEX, EDUC, RACE, YEAR, data) %>% summarise(n=sum(n))

census2010 <- read.csv("1_input_data/census2010education.csv") %>% 
  group_by(STATE, sex, educationCAT) %>% summarise(n = sum(value)) %>% ungroup() %>% 
  group_by(STATE, sex) %>%  
  rename(SEX = sex,
         EDUC=educationCAT) %>% 
  mutate(SEX=recode(SEX, "female"="F","male"="M"),
         data="Census",
         RACE = NA,
         YEAR=2010)

target <- rbind(ACS, census2000, census2010) %>% filter(STATE==SelectedState) %>% group_by(YEAR, SEX, EDUC, data) %>% 
  summarise(n=sum(n),
            n=n*proportion)

