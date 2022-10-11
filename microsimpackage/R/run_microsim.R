#' Runs microsimulation
#'
#' This function runs the microsimulation
#' @param
#' @keywords microsimulation
#' @export
#' @examples
#' run_microsim
run_microsim <- function(seed,samplenum,basepop,brfss,
                         death_rates,
                         updatingeducation, education_setup,
                         migration_rates,
                         updatingalcohol, alcohol_transitions,
                         catcontmodel,
                         policy=0, percentreduction=0.1,
                         minyear=2000, maxyear=2019, output="demographics"){
set.seed(seed)
Summary <- list()
DeathSummary <- list()
PopPerYear <- list()
transitionyears <- seq(2002,2018, by=2)
for(y in minyear:maxyear){
print(y)
# save a population summary
PopPerYear[[paste(y)]] <- basepop %>% mutate(year=y, seed=seed, samplenum=samplenum)
# add and remove migrants
if(y>=2001){
  basepop <- inward_migration(basepop,migration_rates,y, brfss)
  basepop <- outward_migration(basepop,migration_rates,y)
}

# apply death rates and summarise deaths by cause
if(y>=2000){
basepop <- apply_death_rates(basepop, death_rates, y)
DeathSummary[[paste(y)]] <- basepop %>% filter(dead==1) %>% dplyr::select(agecat, microsim.init.race, microsim.init.sex, microsim.init.education,
                                              dead, cause) %>% mutate(year=y, seed=seed)
basepop <- basepop %>% filter(dead==0) %>% dplyr::select(-c(dead, cause))
}

# transition education for individuals aged 34 and under
if(updatingeducation==1 & y>2000){
  totransition <- basepop %>% filter(microsim.init.age<=34)
  tostay <- basepop %>% filter(microsim.init.age>34)
  totransition <- education_setup(totransition,y)
  totransition <- totransition %>% group_by(cat) %>% do(transition_ed(., education_transitions))
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

# update alcohol use categories
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
  basepop <- basepop %>% group_by(cat) %>% do(transition_alcohol(., alcohol_transitions))
  basepop <- basepop %>%
    mutate(AlcCAT = newALC) %>% ungroup() %>% dplyr::select(-c(cat, prob, newALC))
  }
  # allocate a numeric gpd for individuals based on model
  # allocate every year even when transitions are only every two years?
  basepop <- allocate_gramsperday(basepop, y, catcontmodel, DataDirectory)
}

# if policy flag switched on - simulate a reduction in alcohol consumption
# if(policy==1){
# basepop <- reduce_consumption(basepop, percentreduction)
# }

#delete anyone over 79
###then age everyone by 1 year and update age category
basepop <- basepop %>% mutate(microsim.init.age = microsim.init.age+1,
                              agecat = cut(microsim.init.age,
                                           breaks=c(0,19,24,34,44,54,64,74,100),
                                           labels=c("15-19","20-24","25-34","35-44","45-54","55-64",
                                                    "65-74","75-79")))
basepop <- subset(basepop, microsim.init.age<=79)
}
# save output - depending on which was selected
if(output=="mortality"){
  Summary <- do.call(rbind, DeathSummary) %>%
    mutate(agecat = as.factor(agecat),
           microsim.init.sex=as.factor(microsim.init.sex),
           microsim.init.race=as.factor(microsim.init.race),
           microsim.init.education = as.factor(microsim.init.education),
           year = as.factor(year),
           cause=as.factor(cause)) %>%
    group_by(year, agecat, microsim.init.sex, microsim.init.race, microsim.init.education,
             cause, .drop=FALSE) %>% tally(name="ndeaths")

  PopSummary <- do.call(rbind,PopPerYear) %>%
    mutate(agecat = cut(microsim.init.age,
                                 breaks=c(0,24,29,34,39,44,49,54,59,64,69,74,100),
                                 labels=c("18-24","25-29","30-34","35-39",
                                 "40-44","45-49","50-54","55-59","60-64","65-69",
                                 "70-74","75-79")),
           microsim.init.sex=as.factor(microsim.init.sex),
           microsim.init.race=as.factor(microsim.init.race),
           microsim.init.education = as.factor(microsim.init.education),
           year = as.factor(year)) %>%
  group_by(year, agecat, microsim.init.sex, microsim.init.race, microsim.init.education, .drop=FALSE) %>%
           tally(name="totalpop")
Summary <- list(Summary,PopSummary)
  }else if(output=="demographics"){
  Summary <- do.call(rbind,PopPerYear) %>% mutate(year=as.factor(as.character(year)),
                                                  samplenum=as.factor(samplenum),
                                                  microsim.init.sex=as.factor(microsim.init.sex),
                                                  microsim.init.race=as.factor(microsim.init.race),
                                                  microsim.init.education=as.factor(microsim.init.education),
                                                  # agecat = ifelse(microsim.init.age<=29, "18-29",
                                                  #                 ifelse(microsim.init.age>=30 & microsim.init.age<=49,"30-49",
                                                  #                        "50+")),
                                                  agecat=as.factor(agecat),
                                                  AlcCAT=as.factor(AlcCAT)) %>%
    group_by(year, samplenum, microsim.init.sex, microsim.init.age, microsim.init.race, microsim.init.education,
             .drop=FALSE) %>% tally()
}else if(output=="alcohol"){
  CatSummary <- do.call(rbind,PopPerYear) %>% mutate(year=as.factor(as.character(year)),
                                                  samplenum=as.factor(samplenum),
                                                  microsim.init.sex=as.factor(microsim.init.sex),
                                                  microsim.init.race=as.factor(microsim.init.race),
                                                  microsim.init.education=as.factor(microsim.init.education),
                                                  agecat=as.factor(agecat),
                                                  AlcCAT=as.factor(AlcCAT)) %>%
    group_by(year, samplenum, microsim.init.sex,microsim.init.race, microsim.init.education, agecat,
             AlcCAT, .drop=FALSE) %>% tally()
  MeanSummary <- do.call(rbind,PopPerYear) %>% mutate(year=as.factor(as.character(year)),
                                                      samplenum=as.factor(samplenum),
                                                      microsim.init.sex=as.factor(microsim.init.sex),
                                                      microsim.init.race=as.factor(microsim.init.race),
                                                      microsim.init.education=as.factor(microsim.init.education),
                                                      agecat=as.factor(agecat)) %>%
    group_by(year, samplenum, microsim.init.sex, .drop=FALSE) %>%
    filter(microsim.init.alc.gpd!=0) %>%
    summarise(meangpd = mean(microsim.init.alc.gpd))
  Summary <- list(CatSummary, MeanSummary)
}
return(Summary)
}

