division1 <- c("Connecticut","Maine","Massachusetts","New Hampshire","Rhode Island","Vermont")
division2 <- c("New Jersey","New York","Pennsylvania")
division3 <- c("Illinois","Indiana","Michigan","Ohio","Wisconsin")
division4 <- c("Iowa","Kansas","Minnesota","Missouri","Nebraska","North Dakota","South Dakota")
division5 <- c("Delaware","Florida","Georgia","Maryland","North Carolina","South Carolina","Virginia","DC","West Virginia")
division6 <- c("Alabama","Kentucky","Mississippi","Tennessee")
division7 <- c("Arkansas","Louisiana","Oklahoma","Texas")
division8 <- c("Arizona","Colorado","Idaho","Montana","Nevada","New Mexico","Utah","Wyoming")
division9 <- c("Alaska","California","Hawaii","Oregon","Washington")

# USA 	California 	Colorado 	Florida	Indiana	Kentucky	
# Louisiana 	Massachusetts	Michigan	Minnesota 	Missouri 
# New York 	Oregon	Pennsylvania 	Tennessee 	Texas 

selectedregion <- ifelse(!is.na(match(SelectedState,division1)), "division1",
                       ifelse(!is.na(match(SelectedState,division2)),"division2",
                              ifelse(!is.na(match(SelectedState,division3)),"division3",
                                     ifelse(!is.na(match(SelectedState,division4)),"division4",
                                            ifelse(!is.na(match(SelectedState,division5)),"division5",
                                                   ifelse(!is.na(match(SelectedState,division6)),"division6",
                                                          ifelse(!is.na(match(SelectedState,division7)),"division7",
                                                                 ifelse(!is.na(match(SelectedState,division8)),"division8",
                                                                        ifelse(!is.na(match(SelectedState,division9)),"division9",
                                                                               NA)))))))))

brfss <- read_rds("SIMAH_workplace/brfss/processed_data/BRFSS_states_upshifted.RDS") %>% 
  filter(age_var<=79) %>% filter(YEAR>=2000) %>% 
  mutate(microsim.init.race = recode(race_eth,"White"="WHI", 
                                     "Black"="BLA", "Hispanic"="SPA", "Other"="OTH"),
         microsim.init.sex = recode(sex_recode,"Male"="m","Female"="f"),
         microsim.init.education = education_summary,
         agecat = cut(age_var,
                      breaks=c(0,24,34,44,54,64,79),
                      labels=c("18.24","25.34","35.44","45.54","55.64","65.79"))) %>% 
  rename(microsim.init.age = age_var, 
         microsim.init.drinkingstatus=drinkingstatus,
         microsim.init.alc.gpd=gramsperday,
         microsim.init.income = household_income) %>% 
  dplyr::select(YEAR, State, region, microsim.init.race, microsim.init.age,
                microsim.init.sex, microsim.init.education, microsim.init.drinkingstatus,
                microsim.init.alc.gpd, formerdrinker, microsim.init.income, agecat)