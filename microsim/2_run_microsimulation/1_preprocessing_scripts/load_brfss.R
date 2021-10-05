# division1 <- c("Connecticut","Maine","Massachusetts","New Hampshire","Rhode Island","Vermont")
# division2 <- c("New Jersey","New York","Pennsylvania")
# division3 <- c("Illinois","Indiana","Michigan","Ohio","Wisconsin")
# division4 <- c("Iowa","Kansas","Minnesota","Missouri","Nebraska","North Dakota","South Dakota")
# division5 <- c("Delaware","Florida","Georgia","Maryland","North Carolina","South Carolina","Virginia","DC","West Virginia")
# division6 <- c("Alabama","Kentucky","Mississippi","Tennessee")
# division7 <- c("Arkansas","Louisiana","Oklahoma","Texas")
# division8 <- c("Arizona","Colorado","Idaho","Montana","Nevada","New Mexico","Utah","Wyoming")
# division9 <- c("Alaska","California","Hawaii","Oregon","Washington")

brfss <- read_csv("SIMAH_workplace/microsim/brfss_data/BRFSS_upshift_BMIUSA.csv") %>% filter(YEAR>=2000)

# test <- brfss %>% mutate(agecat = cut(AGE, breaks=c(0,18,24,29,34,39,44,49,54,59,
#                                                                    64,69,74,79),
#                                        labels=c("18","19-24","25-29","30-34","35-39",
#                                                 "40-44","45-49","50-54","55-59","60-64",
#                                                 "65-69","70-74","75-79"))) %>% 
#   group_by(YEAR,SEX, agecat, RACE, EDUCATION) %>% sample_n(5, replace=T)
# 
# brfss <- test

# brfss$region <- ifelse(!is.na(match(brfss$STATE,division1)), "division1",
#                        ifelse(!is.na(match(brfss$STATE,division2)),"division2",
#                               ifelse(!is.na(match(brfss$STATE,division3)),"division3",
#                                      ifelse(!is.na(match(brfss$STATE,division4)),"division4",
#                                             ifelse(!is.na(match(brfss$STATE,division5)),"division5",
#                                                    ifelse(!is.na(match(brfss$STATE,division6)),"division6",
#                                                           ifelse(!is.na(match(brfss$STATE,division7)),"division7",
#                                                                  ifelse(!is.na(match(brfss$STATE,division8)),"division8",
#                                                                         ifelse(!is.na(match(brfss$STATE,division9)),"division9",
#                                                                                NA)))))))))

# SelectedRegion <- unique(subset(brfss, STATE==SelectedState)$region)
# 
# brfss <- brfss %>% filter(region==SelectedRegion) %>% dplyr::select(YEAR, region, STATE, AGE, RACE, SEX, EDUCATION, DRINKINGSTATUS_NEW, alcgpd_new,
#                                                                     INCOMENEW) %>% 
#   rename(microsim.init.age=AGE, microsim.init.race=RACE, microsim.init.sex=SEX, microsim.init.education=EDUCATION,
#          microsim.init.drinkingstatus=DRINKINGSTATUS_NEW, microsim.init.alc.gpd=alcgpd_new, microsim.init.income=INCOMENEW) %>% 
#   mutate(microsim.init.race = recode(microsim.init.race, "Black"="BLA","Hispanic"="SPA","Other"="OTH","White"="WHI"),
#          microsim.init.sex = recode(microsim.init.sex, "Female"="f","Male"="m"),
#          microsim.init.education = recode(microsim.init.education, "1"="LEHS","2"="SomeC","3"="College"))

brfss <- brfss %>% dplyr::select(YEAR, STATE, AGE, RACE, SEX, EDUCATION, DRINKINGSTATUS_NEW, alcgpd_new,
                                 imputeddrinking,INCOMENEW) %>%
  rename(microsim.init.age=AGE, microsim.init.race=RACE, microsim.init.sex=SEX, microsim.init.education=EDUCATION,
         microsim.init.drinkingstatus=DRINKINGSTATUS_NEW, microsim.init.alc.gpd=alcgpd_new, microsim.init.income=INCOMENEW) %>%
  mutate(microsim.init.race = recode(microsim.init.race, "Black"="BLA","Hispanic"="SPA","Other"="OTH","White"="WHI"),
         microsim.init.sex = recode(microsim.init.sex, "Female"="f","Male"="m"),
         microsim.init.education = recode(microsim.init.education, "1"="LEHS","2"="SomeC","3"="College"),
         formerdrinker=ifelse(imputeddrinking==1,1,0)) %>% 
  dplyr::select(-c(imputeddrinking))
