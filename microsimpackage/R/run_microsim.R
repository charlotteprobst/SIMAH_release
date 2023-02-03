#' Runs microsimulation
#'
#' This function runs the microsimulation
#' @param
#' @keywords microsimulation
#' @export
#' @examples
#' run_microsim
run_microsim <- function(seed,samplenum,basepop,brfss,
                         death_counts,
                         updatingeducation, education_setup,
                         migration_counts,
                         updatingalcohol, alcohol_transitions,
                         base_counts, diseases, lhs, liverinteraction,
                         policy=0, percentreduction=0.1, year_policy, inflation_factor,
                         minyear=2000, maxyear=2019, output="demographics"){
set.seed(seed)
Summary <- list()
DeathSummary <- list()
DiseaseSummary <- list()
PopPerYear <- list()
names <- names(lhs)
lhs <- as.numeric(lhs)
names(lhs) <- names
for(y in minyear:maxyear){
print(y)
# save a population summary
PopPerYear[[paste(y)]] <- basepop %>% mutate(year=y, seed=seed, samplenum=samplenum)
# add and remove migrants
if(y>=2001){
  basepop <- inward_migration(basepop,migration_counts,y, brfss)
  basepop <- outward_migration(basepop,migration_counts,y)
}

# apply death rates and summarise deaths by cause
if(y>=2000){
basepop <- apply_death_counts(basepop, death_counts, y, diseases)
DeathSummary[[paste(y)]] <- basepop %>% filter(dead==1) %>% dplyr::select(agecat, microsim.init.race, microsim.init.sex, microsim.init.education,
                                              dead, cause) %>% mutate(year=y, seed=seed)
# remove individuals due to death and remove columns no longer needed
basepop <- basepop %>% filter(dead==0) %>% dplyr::select(-c(dead, cause, overallrate))
}

# transition education for individuals aged 34 and under
if(updatingeducation==1 & y>=2000){
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
if(updatingalcohol==1 & y>=2000){
  # if(y %in% transitionyears==TRUE){
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
  # }
  # allocate a numeric gpd for individuals based on model
  # allocate every year even when transitions are only every two years?
  basepop <- allocate_gramsperday(basepop, y, catcontmodel, DataDirectory)

  if(policy==1 & y ==year_policy){
  basepop$microsim.init.alc.gpd <- basepop$microsim.init.alc.gpd - (basepop$microsim.init.alc.gpd*percentreduction)
  }
}

# simulate mortality from specific diseases
disease <- unique(diseases)
if("HLVDC" %in% diseases==TRUE){
basepop <- CirrhosisHepatitis(basepop,lhs)
}else if("LVDC" %in% diseases==TRUE){
  if(liverinteraction==1){
    basepop <- CirrhosisAllInteraction(basepop,lhs)
  }else if(liverinteraction==0){
  basepop <- CirrhosisAll(basepop,lhs)
  }
}

# calculate base rates if year = 2000)
if(y == 2000){
rates <- calculate_base_rate(basepop,base_counts,diseases)
}

basepop <- left_join(basepop, rates, by=c("cat")) %>%
  mutate(risk = RR*rate,
         prob = runif(nrow(.)),
         !!paste0("mort", quo_name(disease)) := ifelse(prob<risk, 1,0))

DiseaseSummary[[paste(y)]] <- basepop %>%
  group_by(cat) %>% add_tally() %>%
  mutate(!!paste0("mort", quo_name(disease)) := sum(!!as.name(paste0('mort',quo_name(disease))))) %>% ungroup() %>%
  mutate(year=y) %>% dplyr::select(year, cat, n, !!as.name(paste0('mort',quo_name(disease)))) %>%
  distinct()
# now sample the correct proportion of those to be removed (due to inflated mortality rate)
toremove <- basepop %>% filter(!!as.name(paste0('mort',quo_name(disease)))==1) %>% add_tally() %>%
    mutate(toremove=round(n/inflation_factor)) %>% do(dplyr::sample_n(.,size=unique(toremove), replace=F))
ids <- toremove$microsim.init.id
basepop <- basepop %>% filter(!microsim.init.id %in% ids)
basepop <- basepop %>% dplyr::select(-c(cat, RR, rate, risk, prob, !!as.name(paste0('mort',quo_name(disease)))))

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
  Summary <- postprocess_mortality(DiseaseSummary,diseases, death_counts, inflation_factor)
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
                                                  agecat = cut(microsim.init.age,
                                                                    breaks=c(0,20,25,29,39,49,64,100),
                                                                    labels=c("18-20","21-25","26-29","30-39","40-49","50-64","65+")),
                                                  agecat=as.factor(agecat),
                                                  AlcCAT=as.factor(AlcCAT)) %>%
    group_by(year, samplenum, microsim.init.sex,microsim.init.race, microsim.init.education,
             AlcCAT, .drop=FALSE) %>% tally()
  MeanSummary <- do.call(rbind,PopPerYear) %>% mutate(year=as.factor(as.character(year)),
                                                      samplenum=as.factor(samplenum),
                                                      microsim.init.sex=as.factor(microsim.init.sex),
                                                      microsim.init.race=as.factor(microsim.init.race),
                                                      microsim.init.education=as.factor(microsim.init.education),
                                                      agecat=as.factor(agecat)) %>%
    group_by(year, samplenum, microsim.init.sex, microsim.init.education, .drop=FALSE) %>%
    filter(microsim.init.alc.gpd!=0) %>%
    summarise(meangpd = mean(microsim.init.alc.gpd))
  Summary <- list(CatSummary, MeanSummary)
  Summary <- CatSummary
}
return(Summary)
}

