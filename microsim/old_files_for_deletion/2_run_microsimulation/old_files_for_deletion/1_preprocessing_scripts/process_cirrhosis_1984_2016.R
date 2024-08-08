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


cirrhosis1999_2020 <- read.delim("SIMAH_workplace/microsim/1_input_data/Cirrhosis_Underlying Cause of Death, 1999-2020.txt") %>% 
  filter(Notes!="Total") %>% dplyr::select(-Notes) %>% 
  mutate(sex = ifelse(Gender=="Female","f","m")) %>% rename(agegroup=Five.Year.Age.Groups.Code) %>% 
  mutate(agegroup=ifelse(agegroup=="75-84","75.",agegroup),
         Deaths = ifelse(agegroup=="15-19",Deaths/5*2,
                         ifelse(agegroup=="80-84",Deaths/5,
                                Deaths)),
         Population = ifelse(agegroup=="15-19",Population/5*2,
                             ifelse(agegroup=="80-84", Population/5,Population)),
         Deaths = Deaths*proportion,
         Population=Population*proportion) %>% 
  mutate(agegroup = ifelse(agegroup=="25-29" | agegroup=="30-34","25-34",
                           ifelse(agegroup=="35-39" | agegroup=="40-44", "35-44",
                                  ifelse(agegroup=="45-49" | agegroup=="50-54","45-54",
                                         ifelse(agegroup=="55-59" | agegroup=="60-64", "55-64",
                                                ifelse(agegroup=="65-69" | agegroup=="70-74", "65-74",
                                                       ifelse(agegroup=="75-79" | agegroup=="80-84", "75.", agegroup))))))) %>% 
  dplyr::select(Year, sex, agegroup, Deaths,Population) %>% 
  group_by(Year, sex, agegroup) %>% summarise(rate = sum(Deaths)/sum(Population)*100000,
                                              count=sum(Deaths)*100, Population=sum(Population))


# cirrhosis1999_2020$type <- "2020"
# cirrhosis1999_2016$type <- "2016"
# test <- rbind(cirrhosis1999_2016, cirrhosis1999_2020)
# 
# ggplot(data=subset(test, Year<=2019), aes(x=Year, y=rate, colour=type)) + geom_line() + 
#   facet_grid(cols=vars(agegroup), rows=vars(sex))

cirrhosismortality <- rbind(cirrhosis1984_1998, cirrhosis1999_2020) %>% drop_na()

# ggplot(data=cirrhosismortality, aes(x=Year, y=rate, colour=sex)) + geom_line(size=1) + facet_grid(rows=vars(agegroup)) +
#   geom_vline(xintercept=2019, linetype="dashed")

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

# ggplot(data=subset(cirrhosismortality_agest, Year<=2019), aes(x=Year, y=agestrate)) + geom_line(size=1) +
#   facet_grid(rows=vars(sex)) + ylim(0,NA)
