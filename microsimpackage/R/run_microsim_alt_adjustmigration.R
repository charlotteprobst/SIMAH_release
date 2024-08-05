#' Runs microsimulation - alternative version with different ordering
#' This version adjusts the migration COUNTS to match with the observed ACS data
#'
#' This function runs the microsimulation
#' @param
#' @keywords microsimulation
#' @import data.table
#' @export
#' @examples
#' run_microsim_alt_adjustmigration
run_microsim_alt_adjustmigration <- function(seed,samplenum,basepop,brfss,
                         death_counts,
                         updatingeducation, education_transitions,
                         migration_counts, population_counts,
                         updatingalcohol, alcohol_transitions,
                         catcontmodel, Hep, drinkingdistributions,
                         base_counts, diseases, lhs, liverinteraction,
                         policy=0, percentreduction=0.1, year_policy, inflation_factors,
                         age_inflated,
                         update_base_rate,
                         minyear=2000, maxyear=2019, output="demographics"){
set.seed(seed)
Summary <- list()
DeathSummary <- list()
DiseaseSummary <- list()
PopPerYear <- list()
# birth_rates <- list()
# migration_rates <- list()
names <- names(lhs)
lhs <- as.numeric(lhs)
names(lhs) <- names
for(y in minyear:maxyear){
print(y)

# save a population summary
PopPerYear[[paste(y)]] <- basepop %>% mutate(year=y, seed=seed, samplenum=samplenum)

# apply death rates - all other causes
basepop <- apply_death_counts(basepop, death_counts, y, diseases)

#delete anyone over 79
###then age everyone by 1 year and update age category
basepop <- basepop %>% mutate(microsim.init.age = microsim.init.age+1,
                              agecat = cut(microsim.init.age,
                                           breaks=c(0,19,24,34,44,54,64,74,100),
                                           labels=c("15-19","20-24","25-34","35-44","45-54","55-64",
                                                    "65-74","75-79")))
basepop <- subset(basepop, microsim.init.age<=79)

# add and remove migrants
if(y<2024){
basepop <- inward_births(basepop, migration_counts, y, brfss, model)
basepopsave <- basepop
  # adding in migrants
basepop <- inward_migration(basepop, migration_counts, y, brfss, model)
basepop <- outward_migration(basepop,migration_counts,y)
# comparing to the population counts for the next year (to calibrate migration in out rates)
compare <- population_counts %>% filter(Year==y)
popsummary <- basepop %>% group_by(agecat, microsim.init.sex, microsim.init.race) %>%
  tally()
compare <- left_join(compare, popsummary) %>%
  mutate(diff = TotalPop-n,
    pct_diff = (n - TotalPop) / TotalPop,
         flag = case_when(
           pct_diff > 0.001 ~ 1,
           pct_diff < -0.001 ~ 1,
           TRUE ~ 0
         )) %>%
  # filter(flag==1) %>%
  dplyr::select(Year,agecat,microsim.init.sex,microsim.init.race,TotalPop, n, diff)

# check to adjust migration in rates
adjusting <- migration_counts %>% filter(Year==y) %>%
  dplyr::select(-BirthsInN) %>% distinct() %>% drop_na()
compare <- left_join(compare,adjusting) %>%
  mutate(MigrationInN_new = MigrationInN+diff,
         MigrationOutN_new = ifelse(MigrationInN_new<0, abs(MigrationInN_new),
                                    MigrationOutN)) %>%
  dplyr::select(Year, agecat, microsim.init.sex, microsim.init.race,
                MigrationInN_new, MigrationOutN_new)
# now join back up with the migration counts file
migration_counts <- left_join(migration_counts, compare)
migration_counts <- migration_counts %>%
  mutate(MigrationInN_new = ifelse(MigrationInN_new<0, 0, MigrationInN_new),
         MigrationInN = ifelse(!is.na(MigrationInN_new), MigrationInN_new,
                                MigrationInN),
         MigrationOutN = ifelse(!is.na(MigrationOutN_new), MigrationOutN_new,
                                 MigrationOutN)) %>%
  dplyr::select(-c(MigrationOutN_new, MigrationInN_new)) %>% distinct()

# now apply the updated migration in and out rates to get the correct basepop for next year
basepop <- basepopsave
# adding in migrants
basepop <- inward_migration(basepop, migration_counts, y, brfss, model)
basepop <- outward_migration(basepop,migration_counts,y)
}

}
# save output - depending on which was selected
#### use a vector to contain the outputs we are interested in TODO
# indicator of how aggregated the results should be? - in the vector of outputs
if(output=="mortality"){
  Summary <- postprocess_mortality(DiseaseSummary,diseases, death_counts) %>%
    mutate(seed = seed, samplenum = samplenum)
  }else if(output=="demographics"){
    # add seed to the output file here TODO
  for(i in 1:length(PopPerYear)){
    PopPerYear[[i]]$agecat <- cut(PopPerYear[[i]]$microsim.init.age,
                                                  breaks=c(0,18,24,29,34,39,44,49,54,59,64,69,74,100),
                                                  labels=c("18","19-24","25-29","30-34","35-39","40-44",
                                                           "45-49","50-54","55-59","60-64","65-69",
                                                           "70-74","75-79"))
    PopPerYear[[i]] <- as.data.table(PopPerYear[[i]])
    PopPerYear[[i]] <- PopPerYear[[i]][, .(n = .N), by = .(year, samplenum, seed, microsim.init.sex, microsim.init.race, agecat)]
  }
    Summary <- do.call(rbind,PopPerYear)
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
  # add former drinkers and lifetime abstainers to this summary TODO
  Summary <- list(CatSummary, MeanSummary)
}
# migration_rates <- do.call(rbind,migration_rates)
# birth_rates <- do.call(rbind,birth_rates)
return(list(Summary,migration_counts))
}
