# functions for processing NSDUH data

recode_education <- function(data){
  data <- data %>% 
    mutate(education = ifelse(Year>=2000 & Year<=2014, EDUCCAT2,
                                  ifelse(Year==2020, EDUHIGHCAT, eduhighcat)),
           education = ifelse(education<=2, "LEHS",
                              ifelse(education==3, "SomeC",
                                     ifelse(education==4, "College", "12-17yo"))))
  return(data)
}

recode_ageracesex <- function(data){
  data <- data %>% 
    mutate(race = as.numeric(NEWRACE2),
      race = ifelse(race==1, "White",
                         ifelse(race==2, "Black",
                                ifelse(race==7, "Hispanic", "Other"))),
           sex = ifelse(Year==2020, IRSEX, 
                        ifelse(Year<=2001, IRSEX, irsex)),
           sex = ifelse(sex==1, "Men","Women"),
      age = as.numeric(AGE2),
      age = ifelse(age<=10, age+11,
                   ifelse(age==11,"22-23",
                          ifelse(age==12,"24-25",
                                 ifelse(age==13, "26-29",
                                        ifelse(age==14,"30-34",
                                               ifelse(age==15,"35-49",
                                                      ifelse(age==16, "50-64",
                                                             ifelse(age==17, "65+", NA)))))))))
  return(data)
}

recode_drinking <- function(data){
  data <- data %>% 
    mutate(drinksperoccasion = ifelse(Year<=2014, NODR30A,
                                      ifelse(Year>=2015, ALCUS30D, NA)),
           drinksperoccasion = ifelse(drinksperoccasion == 975, 5,
                                      ifelse(drinksperoccasion==985, NA,
                                             ifelse(drinksperoccasion==991, 0,
                                                    ifelse(drinksperoccasion==993, 0,
                                                           ifelse(drinksperoccasion>=994 & drinksperoccasion<=998, NA,
                                                                  drinksperoccasion))))),
           dayspermonth = ifelse(Year<=2001, ALCDAYS, 
                                 ifelse(Year==2020, ALCDAYS, alcdays)),
           dayspermonth = ifelse(dayspermonth==85, NA,
                                 ifelse(dayspermonth==91, 0,
                                        ifelse(dayspermonth==93, 0,
                                               ifelse(dayspermonth>=94 & dayspermonth<=98, NA, dayspermonth)))),
           drinkspermonth = drinksperoccasion * dayspermonth,
           gramsperday = drinkspermonth*14/30,
           drinkingstatus = ifelse(Year<=2001, ALCYRTOT,
                                   ifelse(Year==2020, ALCYRTOT, alcyrtot)),
           drinkingstatus = ifelse(drinkingstatus==991, "Lifetime abstainer",
                                   ifelse(drinkingstatus==993, "Former drinker",
                                          ifelse(drinkingstatus<=365, "current drinker", NA))),
           alc_cat = ifelse(drinkingstatus=="Lifetime abstainer", "Lifetime abstainer",
                            ifelse(drinkingstatus=="Former drinker", "Former drinker",
                                   ifelse(sex=="Women" & gramsperday<=20, "Category I",
                                          ifelse(sex=="Men" & gramsperday<=40, "Category I",
                                                 ifelse(sex=="Women" & gramsperday>20 & gramsperday<=40, "Category II",
                                                        ifelse(sex=="Men" & gramsperday>40 & gramsperday<=60, "Category II",
                                                               ifelse(sex=="Women" & gramsperday>40, "Category III",
                                                                      ifelse(sex=="Men" & gramsperday>60, "Category III", NA)))))))))
           
  return(data)
}

recode_weights <- function(data){
  data <- data %>% 
    mutate(verep = ifelse(Year<=2001, VEREP,
                          ifelse(Year==2020, VEREP,verep)),
           vestr = ifelse(Year<=2001, VESTR,
                          ifelse(Year==2020, VESTRQ1Q4_C,
                                 vestr)),
           analwt = ifelse(Year<2020, ANALWT_C,ANALWTQ1Q4_C))
  return(data)
}

selectvars <- function(data){
  data <- data %>% dplyr::select(Year,sex, age, education, gramsperday, alc_cat, vestr, verep, analwt)
  return(data)
}
