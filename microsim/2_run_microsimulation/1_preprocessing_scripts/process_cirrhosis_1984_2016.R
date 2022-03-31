# first read in the data 1984 - 1998
cirrhosis1984_1998 <- read.delim("SIMAH_workplace/microsim/1_input_data/Cirrhosis_Compressed Mortality, 1979-1998.txt") %>% 
  filter(Notes!="Total") %>% dplyr::select(-Notes) %>% 
  mutate(sex = ifelse(Gender=="Female","f","m")) %>% rename(agegroup=Age.Group.Code) %>% 
  mutate(agegroup=ifelse(agegroup=="75-84","75.",agegroup),
         Deaths = ifelse(agegroup=="15-19",Deaths/5*2,
                         ifelse(agegroup=="75.",Deaths/10*6,
                                Deaths)),
         Population = ifelse(agegroup=="15-19",Population/5*2,
                             ifelse(agegroup=="75.", Population/10*6,Population)),
         Deaths = Deaths*proportion,
         Population=Population*proportion,
         Crude.Rate=parse_number(Crude.Rate),
         cat= paste(agegroup, sex, sep="_")) %>% dplyr::select(Year, cat, sex, agegroup, Deaths,Population) %>% 
  group_by(Year, sex, agegroup) %>% summarise(rate = sum(Deaths)/sum(Population)*100000,
                                              count=sum(Deaths)*100, Population=sum(Population))

cirrhosis1999_2016 <- read.delim("SIMAH_workplace/microsim/1_input_data/Cirrhosis_Compressed Mortality, 1999-2016.txt") %>% 
  filter(Notes!="Total") %>% dplyr::select(-Notes) %>% 
  mutate(sex = ifelse(Gender=="Female","f","m")) %>% rename(agegroup=Age.Group.Code) %>% 
  mutate(agegroup=ifelse(agegroup=="75-84","75.",agegroup),
         Deaths = ifelse(agegroup=="15-19",Deaths/5*2,
                         ifelse(agegroup=="75.",Deaths/10*6,
                                Deaths)),
         Population = ifelse(agegroup=="15-19",Population/5*2,
                             ifelse(agegroup=="75.", Population/10*6,Population)),
         Deaths = Deaths*proportion,
         Population=Population*proportion,
         Crude.Rate=parse_number(Crude.Rate),
         cat= paste(agegroup, sex, sep="_")) %>% dplyr::select(Year, cat, sex, agegroup, Deaths,Population) %>% 
  group_by(Year, sex, agegroup) %>% summarise(rate = sum(Deaths)/sum(Population)*100000,
                                              count=sum(Deaths)*100, Population=sum(Population))
cirrhosismortality <- rbind(cirrhosis1984_1998, cirrhosis1999_2016) %>% drop_na()
rm(cirrhosis1984_1998, cirrhosis1999_2016)

# age standardised target 
age2010 <- cirrhosismortality %>% filter(Year==2010) %>% 
  ungroup() %>% 
  group_by(Year, sex, agegroup) %>% 
  summarise(poptotal = mean(Population)) %>% ungroup() %>% 
  group_by(Year, sex) %>% 
  mutate(percent = poptotal / sum(poptotal)) %>% ungroup() %>% dplyr::select(sex, agegroup, percent)

cirrhosismortality_agest <- left_join(cirrhosismortality, age2010) %>% 
  group_by(Year, sex, agegroup) %>% 
  mutate(weightedrate = ((count/100 / Population)*100000) * percent) %>% ungroup() %>% 
  group_by(Year, sex) %>% summarise(agestrate = sum(weightedrate))
