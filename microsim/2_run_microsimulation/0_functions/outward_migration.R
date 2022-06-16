outward_migration <- function(basepop, year_rates,y){
  # summarise the current population by age, sex and race
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
  # calculate a migration out rate for each age/sex/race group
  migout<- filter(year_rates, Year==y) %>% dplyr::select(agecat, microsim.init.sex,
                                                    microsim.init.race, MigrationOutN)
  # join the summary pop to the migration out rate
  summary <- left_join(summary, migout)
  # convert from a rate to the N to remove 
  summary <- summary %>% mutate(toremove = MigrationOutN*proportion,
                                toremove = ifelse(toremove>n, n, toremove)) %>% 
    dplyr::select(agecat, microsim.init.race, microsim.init.sex, toremove)
  basepop$agecat <- cut(basepop$microsim.init.age, breaks=c(0,18,24,29,34,39,44,49,54,59,
                                  64,69,74,100),
      labels=c("18","19-24","25-29","30-34","35-39",
               "40-44","45-49","50-54","55-59","60-64",
               "65-69","70-74","75-79"))
  basepop <- left_join(basepop,summary)
  basepop$toremove[is.na(basepop$toremove)] <- 0
  toremove <- basepop %>% group_by(agecat, microsim.init.race, microsim.init.sex) %>% sample_n(toremove)
  ids <- unique(toremove$microsim.init.id)
  basepopremoved <- basepop %>% filter(!microsim.init.id %in% ids) %>% dplyr::select(-toremove)
  return(basepopremoved)
}
