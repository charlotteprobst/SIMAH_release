#' Deletes individuals due to migration in each simulated year
#' @param
#' @keywords microsimulation
#' @export
#' @examples
#' outward_migration

outward_migration_estimate_rate <- function(basepop, migration_counts,y){
  # summarise the current population by age, sex and race
  denominator <- basepop %>%
    mutate(agecat = cut(age,
                        breaks=c(0,18,24,29,34,39,44,49,54,59,64,69,74,100),
                        labels=c("18","19-24","25-29","30-34","35-39","40-44",
                                 "45-49","50-54","55-59","60-64","65-69",
                                 "70-74","75-79"))) %>%
    group_by(race, sex,agecat) %>%
    tally()

  # calculate a migration out rate for each age/sex/race group
  migout<- filter(migration_counts, Year==y) %>% dplyr::select(agecat, sex,
                                                    race, MigrationOutN) %>% distinct()

  migration <- left_join(denominator, migout)
  migration$rate_out <- migration$MigrationOutN/migration$n
  migration <- migration %>% dplyr::select(race, sex,
                                           agecat, rate_out)
  # join the summary pop to the migration out rate
  summary <- left_join(denominator, migout, by=c("agecat","race","sex"))
  summary <- summary %>% mutate(toremove = MigrationOutN,
                                toremove = ifelse(toremove>n, n, toremove)) %>%
    dplyr::select(agecat, race, sex, toremove)
  basepop$agecat <- cut(basepop$age, breaks=c(0,18,24,29,34,39,44,49,54,59,
                                  64,69,74,100),
      labels=c("18","19-24","25-29","30-34","35-39",
               "40-44","45-49","50-54","55-59","60-64",
               "65-69","70-74","75-79"))
  basepop <- left_join(basepop,summary, by=c("race","sex","agecat"))
  basepop$toremove[is.na(basepop$toremove)] <- 0
  if(length(unique(basepop$toremove))==1){
    basepopremoved <- basepop %>% dplyr::select(-toremove)
  }else if(length(unique(basepop$toremove))>1){
  toremove <- basepop %>% group_by(agecat, race, sex) %>%
    do(dplyr::sample_n(.,size=unique(toremove), replace=FALSE))
  ids <- unique(toremove$ID)
  basepopremoved <- basepop %>% filter(!ID %in% ids) %>% dplyr::select(-toremove)
  }
  return(list(basepopremoved,migration))
}
