#' Deletes individuals due to migration in each simulated year
#' @param
#' @keywords microsimulation
#' @export
#' @examples
#' outward_migration

outward_migration_rate <- function(basepop, migration_rates,y){
  # summarise the current population by age, sex and race
  summary <- basepop %>%
    mutate(n=1,
           agecat = cut(microsim.init.age, breaks=c(0,18,24,29,34,39,44,49,54,59,
                                                    64,69,74,100),
                        labels=c("18","19-24","25-29","30-34","35-39",
                                 "40-44","45-49","50-54","55-59","60-64",
                                 "65-69","70-74","75-79"))) %>%
    # complete(agecat, microsim.init.race, microsim.init.sex, fill=list(n=0)) %>%
    group_by(agecat, microsim.init.race, microsim.init.sex, .drop=FALSE) %>%
    summarise(n=sum(n))
  # calculate a migration out rate for each age/sex/race group
  migout<- filter(migration_rates, year==y) %>% dplyr::select(agecat, microsim.init.sex,
                                                    microsim.init.race, migrationoutrate) %>% distinct() %>%
    drop_na()
  # join the summary pop to the migration out rate
  summary <- left_join(summary, migout, by=c("agecat","microsim.init.race","microsim.init.sex"))
  # convert from a rate to the N to remove
  summary <- summary %>% mutate(toremove = migrationoutrate*n,
                                toremove = ifelse(toremove>n, n, toremove)) %>%
    dplyr::select(agecat, microsim.init.race, microsim.init.sex, toremove)
  basepop$agecat <- cut(basepop$microsim.init.age, breaks=c(0,18,24,29,34,39,44,49,54,59,
                                  64,69,74,100),
      labels=c("18","19-24","25-29","30-34","35-39",
               "40-44","45-49","50-54","55-59","60-64",
               "65-69","70-74","75-79"))
  basepop <- left_join(basepop,summary, by=c("microsim.init.race","microsim.init.sex","agecat"))
  basepop$toremove[is.na(basepop$toremove)] <- 0
  if(length(unique(basepop$toremove))==1){
    basepopremoved <- basepop %>% dplyr::select(-toremove)
  }else if(length(unique(basepop$toremove))>1){
  toremove <- basepop %>% group_by(agecat, microsim.init.race, microsim.init.sex) %>%
    do(dplyr::sample_n(.,size=unique(toremove), replace=FALSE))
  ids <- unique(toremove$microsim.init.id)
  basepopremoved <- basepop %>% filter(!microsim.init.id %in% ids) %>% dplyr::select(-toremove)
  }
  return(basepopremoved)
}
