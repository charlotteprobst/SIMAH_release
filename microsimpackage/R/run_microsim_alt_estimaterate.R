#' Runs microsimulation - alternative version with different ordering
#'
#' This function runs the microsimulation
#' @param
#' @keywords microsimulation
#' @import data.table
#' @export
#' @examples
#' run_microsim_alt_estimaterate
run_microsim_alt_estimaterate <- function(seed,samplenum,basepop,brfss,
                         death_counts,
                         updatingeducation, education_transitions,
                         migration_rates,
                         updatingalcohol, alcohol_transitions,
                         catcontmodel, Hep, drinkingdistributions,
                         base_counts, diseases, lhs, liverinteraction,
                         policy=0, percentreduction=0.1, year_policy, inflation_factors,
                         age_inflated,
                         update_base_rate,
                         minyear=2000, maxyear=2022, output="demographics"){
set.seed(seed)
Summary <- list()
DeathSummary <- list()
DiseaseSummary <- list()
PopPerYear <- list()
birth_rates <- list()
migration_rates <- list()
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
basepop <- basepop %>% mutate(age = age+1,
                              agecat = cut(age,
                                           breaks=c(0,19,24,34,44,54,64,74,100),
                                           labels=c("15-19","20-24","25-34","35-44","45-54","55-64",
                                                    "65-74","75-79")))
basepop <- subset(basepop, age<=79)

# add and remove migrants
if(y<2024){
# basepop <- inward_births_rate(basepop, migration_rates, y, brfss, model)
# basepop <- inward_migration_rate(basepop, migration_rates, y, brfss)
files <- inward_births_estimate_rate(basepop, migration_counts, y, brfss)
basepop <- files[[1]]
birth_rates[[paste(y)]] <- files[[2]] %>%
  mutate(year=y)
files <- inward_migration_estimate_rate(basepop, migration_counts,y,brfss)
basepop <- files[[1]]
migration_rates[[paste(y)]] <- files[[2]] %>%
  mutate(year=y, rate_in=rate)

files <- outward_migration_estimate_rate(basepop, migration_counts,y)
basepop <- files[[1]]
migration_rates[[paste(y)]] <- left_join(migration_rates[[paste(y)]], files[[2]])

# basepop <- inward_migration(basepop,migration_counts,y, brfss,"SIMAH")
# basepop <- outward_migration(basepop,migration_counts,y)
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
    PopPerYear[[i]]$agecat <- cut(PopPerYear[[i]]$age,
                                                  breaks=c(0,18,24,29,34,39,44,49,54,59,64,69,74,100),
                                                  labels=c("18","19-24","25-29","30-34","35-39","40-44",
                                                           "45-49","50-54","55-59","60-64","65-69",
                                                           "70-74","75-79"))
    PopPerYear[[i]] <- as.data.table(PopPerYear[[i]])
    PopPerYear[[i]] <- PopPerYear[[i]][, .(n = .N), by = .(year, samplenum, seed, sex, race, agecat)]
  }
    Summary <- do.call(rbind,PopPerYear)
}else if(output=="alcohol"){
  CatSummary <- do.call(rbind,PopPerYear) %>% mutate(year=as.factor(as.character(year)),
                                                  samplenum=as.factor(samplenum),
                                                  sex=as.factor(sex),
                                                  race=as.factor(race),
                                                  education=as.factor(education),
                                                  agecat = cut(age,
                                                                    breaks=c(0,20,25,29,39,49,64,100),
                                                                    labels=c("18-20","21-25","26-29","30-39","40-49","50-64","65+")),
                                                  agecat=as.factor(agecat),
                                                  AlcCAT=as.factor(AlcCAT)) %>%
    group_by(year, samplenum, sex,race, education,
             AlcCAT, .drop=FALSE) %>% tally()
  MeanSummary <- do.call(rbind,PopPerYear) %>% mutate(year=as.factor(as.character(year)),
                                                      samplenum=as.factor(samplenum),
                                                      sex=as.factor(sex),
                                                      race=as.factor(race),
                                                      education=as.factor(education),
                                                      agecat=as.factor(agecat)) %>%
    group_by(year, samplenum, sex, education, .drop=FALSE) %>%
    filter(microsim.init.alc.gpd!=0) %>%
    summarise(meangpd = mean(microsim.init.alc.gpd))
  # add former drinkers and lifetime abstainers to this summary TODO
  Summary <- list(CatSummary, MeanSummary)
}
migration_rates <- do.call(rbind,migration_rates)
birth_rates <- do.call(rbind,birth_rates)
return(list(Summary,birth_rates,migration_rates))
}
