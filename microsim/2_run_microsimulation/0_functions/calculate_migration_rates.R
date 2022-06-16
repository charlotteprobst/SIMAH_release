# function for calculating the baseline migration in and out rates (including 18 year olds to enter) 
# based on calculated totals from the ACS and Census 
calculate_migration_rates <- function(basepop, outwardmigrants, inwardmigrants, y){

summary <- basepop %>%
  mutate(n=1,
         agecat = cut(microsim.init.age, breaks=c(0,18,24,29,34,39,44,49,54,59,
                                                  64,69,74,100),
                      labels=c("18","19-24","25-29","30-34","35-39",
                               "40-44","45-49","50-54","55-59","60-64",
                               "65-69","70-74","75-79"))) %>% 
  complete(agecat, microsim.init.race, microsim.init.sex, fill=list(n=0)) %>%
  group_by(agecat, microsim.init.race, microsim.init.sex, .drop=FALSE) %>% 
  summarise(n=sum(n))

migin <- inwardmigrants %>% filter(Year==y) %>% filter(Age<=80) %>% mutate(agecat = cut(Age, breaks=c(0,18,24,29,34,39,44,49,54,59,
                                                                                                      64,69,74,100),
                                                                                        labels=c("18","19-24","25-29","30-34","35-39",
                                                                                                 "40-44","45-49","50-54","55-59","60-64",
                                                                                                 "65-69","70-74","75-79"))) %>% 
  rename(microsim.init.sex = Sex, microsim.init.race=Race, In = NetMigration) %>% ungroup() %>% 
  complete(agecat, microsim.init.race, microsim.init.sex, fill=list(In=0)) %>%
  group_by(microsim.init.sex, agecat, microsim.init.race, .drop=FALSE) %>% 
  summarise(In = sum(In),
            In = ifelse(is.na(In),0,In)) %>% mutate(microsim.init.sex=ifelse(microsim.init.sex=="female","f",
                                                                             ifelse(microsim.init.sex=="male","m",
                                                                                    ifelse(microsim.init.sex=="M","m",
                                                                                           ifelse(microsim.init.sex=="F","f",microsim.init.sex)))))
migout <- outwardmigrants %>% filter(Year==y) %>% filter(Age<=80) %>% mutate(agecat = cut(Age, breaks=c(0,18,24,29,34,39,44,49,54,59,
                                                                                                        64,69,74,100),
                                                                                          labels=c("18","19-24","25-29","30-34","35-39",
                                                                                                   "40-44","45-49","50-54","55-59","60-64",
                                                                                                   "65-69","70-74","75-79"))) %>% 
  rename(microsim.init.sex = Sex, microsim.init.race = Race, Out=NetMigration) %>% ungroup() %>% 
  mutate(microsim.init.sex=as.character(microsim.init.sex),
    microsim.init.sex=ifelse(microsim.init.sex=="female","f",
                                      ifelse(microsim.init.sex=="male","m",
                                             ifelse(microsim.init.sex=="F","f",
                                                    ifelse(microsim.init.sex=="M","m", microsim.init.sex))))) %>% 
  complete(agecat, microsim.init.race, microsim.init.sex, fill=list(Out=0)) %>% 
  group_by(microsim.init.sex, agecat, microsim.init.race, .drop=FALSE) %>% 
  summarise(Out=sum(Out), Out=ifelse(is.na(Out),0,Out))


summary <- left_join(summary, migin)
summary <- left_join(summary, migout)

summary <- summary %>% mutate(net = In - Out, 
                              MigrationInN = ifelse(net>0, abs(net)*(1/proportion),0),
                              MigrationOutN = ifelse(net<0, abs(net)*(1/proportion),0),
                              MigrationInRate = ifelse(net>0,
                                                       (abs(net)/n)*100000,
                                                       ifelse(n==0, net,
                                                       0)),
                              MigrationOutRate = ifelse(net<0,
                                                        (abs(net)/n)*100000,0),
                              Year = y) %>% dplyr::select(Year,agecat, microsim.init.sex,microsim.init.race,
                                                          MigrationInN, MigrationOutN)
return(summary)
}

