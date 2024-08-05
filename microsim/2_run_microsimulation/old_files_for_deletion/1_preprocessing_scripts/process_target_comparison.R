# processing target data for comparison to microsimulation outputs by State 

ACS <- read.csv("SIMAH_workplace/microsim/1_input_data/ACS_popcounts_unweighted_indage.csv") %>% filter(AGE<80) %>% 
  mutate(AGECAT = cut(AGE, breaks=c(0,24,34,44,54,64,74,100),
                      labels=c("18-24","25-34","35-44",
                               "45-54","55-64","65-74","75-79")),
         AGECAT = cut(AGE, breaks=c(0,18,24,29,34,39,44,49,54,59,64,69,74,80),
                      labels=c("18","19-24","25-29","30-34","35-39",
                               "40-44","45-49","50-54","55-59",
                               "60-64","65-69","70-74","75-79"))
         ) %>% 

  group_by(YEAR,SEX,RACE,AGECAT) %>% 
  filter(STATE==SelectedState) %>% summarise(n=sum(n)) %>% mutate(data = "ACS") %>% 
  filter(AGECAT!="80+") %>% mutate(n=n*proportion)

census2010 <- read.csv("SIMAH_workplace/microsim/1_input_data/processed_indagesexrace2010.csv") %>% filter(STATE==SelectedState) %>% 
  filter(Age<=80) %>% mutate(agecat = cut(Age, breaks=c(0,24,34,44,54,64,74,100),
                                          labels=c("18-24","25-34","35-44",
                                                   "45-54","55-64","65-74","75-79")),
                             agecat = cut(Age, breaks=c(0,18,24,29,34,39,44,49,54,59,64,69,74,80),
                                          labels=c("18","19-24","25-29","30-34","35-39",
                                                   "40-44","45-49","50-54","55-59",
                                                   "60-64","65-69","70-74","75-79"))) %>%
  group_by(sex, race, agecat) %>% summarise(n=sum(count)) %>% mutate(YEAR=2010, data="Census") %>% 
  rename(SEX=sex, RACE=race, AGECAT=agecat) %>% mutate(RACE = recode(RACE, "BLA"="Black", "SPA"="Hispanic","WHI"="White","OTH"="Other")) %>% 
  mutate(n=n*proportion)

census1990 <- read.csv("SIMAH_workplace/microsim/census_data/overallcons1990.csv") %>% filter(STATE==SelectedState) %>% 
  filter(age<=80) %>% mutate(agecat = cut(age, breaks=c(0,24,34,44,54,64,74,100),
                                          labels=c("18-24","25-34","35-44",
                                                   "45-54","55-64","65-74","75-79")),
                             agecat = cut(age, breaks=c(0,18,24,29,34,39,44,49,54,59,64,69,74,80),
                                          labels=c("18","19-24","25-29","30-34","35-39",
                                                   "40-44","45-49","50-54","55-59",
                                                   "60-64","65-69","70-74","75-79")),
                             race = ifelse(race=="AmIn","Other",
                                           ifelse(race=="Asian","Other",
                                                  race)),
                             sex=ifelse(sex=="female","F","M")) %>% mutate(YEAR=1990, data="Census") %>% 
  rename(SEX=sex, RACE=race, AGECAT=agecat) %>% group_by(YEAR,data, SEX, RACE, AGECAT) %>% summarise(n=sum(value)*proportion)

target <- rbind(ACS, census2010, census1990) %>%
  pivot_wider(names_from=data, values_from=n) %>%
  mutate(target = ifelse(YEAR==2010 | YEAR==1990, Census, ACS),
         target = round(target)) %>%
  dplyr::select(YEAR, SEX, RACE, AGECAT, target)

if(model=="CASCADE"){
baseline <- basepop %>% mutate(AGECAT = cut(microsim.init.age, breaks=c(0,24,34,44,54,64,74,100),
                                            labels=c("18-24","25-34","35-44",
                                                     "45-54","55-64","65-74","75-79")),
                               AGECAT = cut(microsim.init.age, breaks=c(0,18,24,29,34,39,44,49,54,59,64,69,74,80),
                                            labels=c("18","19-24","25-29","30-34","35-39",
                                                     "40-44","45-49","50-54","55-59",
                                                     "60-64","65-69","70-74","75-79")),
                               microsim.init.sex=ifelse(microsim.init.sex=="m","M","F"),
                               microsim.init.race = recode(microsim.init.race, "BLA"="Black", "SPA"="Hispanic","WHI"="White","OTH"="Other")) %>% 
  group_by(AGECAT, microsim.init.sex, microsim.init.race) %>% tally(name="target") %>% 
  mutate(YEAR=1984) %>% rename(SEX=microsim.init.sex,
                               RACE=microsim.init.race)
}else if(model=="SIMAH"){
  baseline <- basepop %>% mutate(AGECAT = cut(microsim.init.age, breaks=c(0,24,34,44,54,64,74,100),
                                              labels=c("18-24","25-34","35-44",
                                                       "45-54","55-64","65-74","75-79")),
                                 microsim.init.sex=ifelse(microsim.init.sex=="m","M","F"),
                                 microsim.init.race = recode(microsim.init.race, "BLA"="Black", "SPA"="Hispanic","WHI"="White","OTH"="Other")) %>% 
    group_by(AGECAT, microsim.init.sex, microsim.init.race) %>% tally(name="target") %>% 
    mutate(YEAR=2000) %>% rename(SEX=microsim.init.sex,
                                 RACE=microsim.init.race)
  target <- target %>% filter(YEAR!=2000)
  
}
target <- rbind(target,baseline)

if(model=="CASCADE"){
  missing <- expand.grid(YEAR=c(1985:1989,1991:1999), SEX=c("M","F"), RACE=c("Black","Hispanic","White","Other"), 
                         AGECAT=c("18","19-24","25-29","30-34","35-39",
                                               "40-44","45-49","50-54","55-59",
                                               "60-64","65-69","70-74","75-79"),
                         target=NA)
  target <- rbind(target,missing)
  target <- target %>% group_by(SEX, RACE, AGECAT) %>% 
    arrange(YEAR) %>% mutate(target = na.approx(target))
}

# add in 2020 data - based on experimental estimates from ACS 

# ACS2020 <- read.csv("SIMAH_workplace/microsim/1_input_data/ACS_2020_summary.csv") %>%
#   mutate(target=target*proportion,
#          lower_ci = lower_ci*proportion,
#          upper_ci = upper_ci*proportion)
# target <- target %>% mutate(lower_ci=NA, upper_ci=NA)
# 
# target <- rbind(target, ACS2020)
target <- data.frame(target)



