######FUNCTION FOR RUNNING MICROSIMULATION 
run_microsim <- function(seed,samplenum,basepop, outwardmigrants, inwardmigrants, deathrates, apply_death_rates,
                         updatingeducation, education_setup, transitionroles,
                         calculate_migration_rates, outward_migration, inward_migration, 
                         brfss,Rates,AlctransitionProbability,
                         transitions, PopPerYear, minyear, maxyear){
set.seed(seed)
Summary <- list()
DeathSummary <- list()
SummaryMissing <- list()
transitionyears <- seq(2002,2018, by=2)
for(y in minyear:maxyear){ 
####add the migrants for the year
  
if(y>=2001){
  basepop <- inward_migration(basepop,Rates,y, brfss)
  basepop <- outward_migration(basepop,Rates,y)
}
  
  
if(y>=2000){
basepop <- apply_death_rates(basepop, deathrates, y)
DeathSummary[[paste(y)]] <- basepop %>% filter(dead==1) %>% dplyr::select(agecat, microsim.init.race, microsim.init.sex, microsim.init.education,
                                              dead, cause) %>% mutate(year=y, seed=seed)
basepop <- basepop %>% filter(dead==0) %>% dplyr::select(-c(dead, cause, cat))
}
basepop$cat <- NULL
# transition education for individuals aged 34 and under
if(updatingeducation==1 & y>2000){
  totransition <- basepop %>% filter(microsim.init.age<=34)
  tostay <- basepop %>% filter(microsim.init.age>34)
  totransition <- education_setup(totransition,y)
  totransition <- totransition %>% group_by(cat) %>% do(transition_ed(., transitions))
  totransition$microsimnewED <- totransition$newED
  totransition$microsim.init.education <- ifelse(totransition$microsimnewED=="LEHS","LEHS",
                                                 ifelse(totransition$microsimnewED=="SomeC1","SomeC",
                                                        ifelse(totransition$microsimnewED=="SomeC2","SomeC",
                                                               ifelse(totransition$microsimnewED=="SomeC3","SomeC",
                                                                      ifelse(totransition$microsimnewED=="College","College",NA)
                                                                      ))))
  totransition <- totransition %>% ungroup() %>% dplyr::select(-c(prob, state, year, cat, newED))
  basepop <- rbind(totransition, tostay)
}
if(updatingalcohol==1 & y>2000){
  if(y %in% transitionyears==TRUE){
  basepop <- basepop %>% ungroup() %>% mutate(
    agecat = cut(microsim.init.age,
                 breaks=c(0,20,25,29,39,49,64,100),
                 labels=c("18-20","21-25","26-29","30-39","40-49","50-64","65+")),
    cat = paste(agecat, microsim.init.sex,
                                      microsim.init.race, microsim.init.education,
                                      AlcCAT, sep="_"),
                                prob = runif(nrow(.)))
   basepop <- basepop %>% group_by(cat) %>% do(transition_alcohol(., AlctransitionProbability))
  basepop <- basepop %>% 
    mutate(AlcCAT = newALC) %>% ungroup() %>% dplyr::select(-c(cat, prob, newALC))
  }
}
PopPerYear[[paste(y)]] <- basepop %>% mutate(year=y, seed=seed, samplenum=samplenum)

#delete anyone over 80
###then age everyone by 1 year and update age category
basepop <- basepop %>% mutate(microsim.init.age = microsim.init.age+1,
                              agecat = cut(microsim.init.age,
                                           breaks=c(0,19,24,34,44,54,64,74,100),
                                           labels=c("15-19","20-24","25-34","35-44","45-54","55-64",
                                                    "65-74","75-79")))
basepop <- subset(basepop, microsim.init.age<=79)



}

for(i in names(PopPerYear)){
Summary[[paste(i)]] <- PopPerYear[[paste(i)]] %>% mutate(agecat = cut(microsim.init.age,
                                                                      breaks=c(0,24,29,34,39,44,49,54,59,64,69,74,100),
                                                                      labels=c("18-24","25-29","30-34","35-39","40-44","45-49",
                                                                               "50-54","55-59","60-64","65-69","70-74","75-79"))) %>%
  group_by(microsim.init.sex, agecat, microsim.init.education, microsim.init.race) %>% tally() %>%
  mutate(year=i, seed=seed)
}

PopPerYear <- do.call(rbind,PopPerYear) %>% mutate(year=as.factor(as.character(year)),
                                                   samplenum=as.factor(samplenum),
                                                   microsim.init.sex=as.factor(microsim.init.sex),
                                                   microsim.init.race=as.factor(microsim.init.race),
                                                   microsim.init.education=as.factor(microsim.init.education),
                                                   # agecat = ifelse(microsim.init.age<=29, "18-29",
                                                   #                 ifelse(microsim.init.age>=30 & microsim.init.age<=49,"30-49",
                                                   #                        "50+")),
                                                   agecat=as.factor(agecat),
                                                   AlcCAT=as.factor(AlcCAT)) %>%
  group_by(year, samplenum, microsim.init.sex,microsim.init.race, microsim.init.education, agecat,
           AlcCAT, .drop=FALSE) %>% tally()
Summary <- do.call(rbind,Summary)
DeathSummary <- do.call(rbind, DeathSummary)
Summary <- list(Summary, DeathSummary)
return(Summary)
}

